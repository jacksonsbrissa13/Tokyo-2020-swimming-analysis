# Tokyo 2020 Olympics Swimming Analysis

library(SwimmeR)
library(tidyverse)
library(ggrepel)

# SECTION 1 Data Import & Boxplot Visualisation:----

# Full import loop (Men's events) ----

mens_folder <- "Tokyo 2020 swimming data/Men's individual"
mens_files  <- list.files(path = mens_folder, pattern = "\\.pdf$", full.names = TRUE)
mens_list   <- list()

for (i in seq_along(mens_files)) {
  raw       <- read_results(mens_files[i])
  is_50m    <- grepl("50MFR", mens_files[i])
  df        <- swim_parse(raw, splits = !is_50m)
  df        <- mutate(df, across(everything(), as.character))
  df$Gender <- "Men"
  mens_list[[i]] <- df
}

mens_individual_df <- bind_rows(mens_list)

glimpse(mens_individual_df)
nrow(mens_individual_df)

#Repeat for Women's events

womens_folder <- "Tokyo 2020 swimming data/Women's individual"
womens_files  <- list.files(path = womens_folder, pattern = "\\.pdf$", full.names = TRUE)
womens_list   <- list()

for (i in seq_along(womens_files)) {
  raw       <- read_results(womens_files[i])
  is_50m    <- grepl("50MFR", womens_files[i])
  df        <- swim_parse(raw, splits = !is_50m)
  df        <- mutate(df, across(everything(), as.character))
  df$Gender <- "Women"
  womens_list[[i]] <- df
}

womens_individual_df <- bind_rows(womens_list)

glimpse(womens_individual_df)
nrow(womens_individual_df)


# Data cleaning ----

# remove DQ's
mens_individual_df <- mens_individual_df |> filter(DQ == "0")
womens_individual_df <- womens_individual_df |> filter(DQ == "0")

# converting times to numeric seconds using sec_format()

mens_individual_df <- mens_individual_df |> 
  mutate(Finals_sec = sec_format(Finals))

womens_individual_df <- womens_individual_df |> 
  mutate(Finals_sec = sec_format(Finals))

#converting split times to numeric class

mens_individual_df <- mens_individual_df |> 
  mutate(across(starts_with("Split_"), sec_format))

womens_individual_df <- womens_individual_df |> 
  mutate(across(starts_with("Split_"), sec_format))

# converting Reaction_Time and Place to numeric class type

mens_individual_df <- mens_individual_df |> 
  mutate(Reaction_Time = as.numeric(Reaction_Time), 
         Place = as.numeric(Place))

womens_individual_df <- womens_individual_df |> 
  mutate(Reaction_Time = as.numeric(Reaction_Time), 
         Place = as.numeric(Place))

# creating inquartile range (IQR) for both datasets

mens_individual_df <- mens_individual_df |>
  group_by(Event) |>
  mutate(
    Q1  = quantile(Finals_sec, 0.25, na.rm = TRUE),
    Q3  = quantile(Finals_sec, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  )

womens_individual_df <- womens_individual_df |>
  group_by(Event) |>
  mutate(
    Q1  = quantile(Finals_sec, 0.25, na.rm = TRUE),
    Q3  = quantile(Finals_sec, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  )

#removing outliers using IQR

mens_individual_df <- mens_individual_df |>
  filter(Finals_sec <= Q3 + 1.5 * IQR & Finals_sec >= Q1 - 1.5 * IQR) |> 
  ungroup() |> 
  select(-Q1, -Q3, -IQR)

womens_individual_df <- womens_individual_df |>
  filter(Finals_sec <= Q3 + 1.5 * IQR & Finals_sec >= Q1 - 1.5 * IQR) |> 
  ungroup() |> 
  select(-Q1, -Q3, -IQR)

# combining mens and womens datasets

full_individual_df <- bind_rows(mens_individual_df, womens_individual_df)

# adding category column (Sprint / Middle Distance / Distance) 

full_individual_df <- full_individual_df |>
  mutate(Category = case_when(
    grepl("50m|100m", Event) ~ "Sprint",
    grepl("200m|400m", Event) ~ "Middle Distance",
    grepl("800m|1500m", Event) ~ "Distance"
  ))

# stripping gender prefix from event names for cleaner facet labels 

full_individual_df <- full_individual_df |>
  mutate(Event_short = gsub("Men's |Women's ", "", Event))

# ordering events by distance for logical facet ordering
full_individual_df <- full_individual_df |>
  mutate(Event_short = factor(Event_short, levels = c(
    "50m Freestyle",
    "100m Backstroke", "100m Breaststroke", "100m Butterfly", "100m Freestyle",
    "200m Backstroke", "200m Breaststroke", "200m Butterfly", "200m Freestyle",
    "400m Freestyle",
    "800m Freestyle",
    "1500m Freestyle"
  )))

# Visualisations of final performance times by event and gender ----

# Faceted boxplot

figure_1 <- full_individual_df |>
  ggplot(aes(x = Gender, y = Finals_sec, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Event_short, scales = "free_y") +
  scale_y_continuous(n.breaks = 6) +
  scale_fill_manual(values = c("Men" = "skyblue", "Women" = "hotpink")) +
  labs(title = "Distribution of Final Times by Event and Gender — Tokyo 2020 Olympics (All Rounds)",
       x = NULL, y = "Time (seconds)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
  panel.border = element_rect(colour = "grey", fill = NA, linewidth = 0.5))

figure_1

# SECTION 2: Linear Regression Modelling ----

# Data Subsetting for Regression Analysis: ----

womens_400m_freestyle_df <- full_individual_df |>
  filter(Event == "Women's 400m Freestyle")

mens_1500m_freestyle_df <- full_individual_df |>
  filter(Event == "Men's 1500m Freestyle")

# Train/test split and simple linear regression models ----
# Womens 400m freestyle model
set.seed(123)
  sample_size_400w <- floor(0.7 * nrow(womens_400m_freestyle_df))
  train_index_400w <- sample(seq_len(nrow(womens_400m_freestyle_df)), size = sample_size_400w)
  train_400w <- womens_400m_freestyle_df[train_index_400w, ]
  test_400w  <- womens_400m_freestyle_df[-train_index_400w, ]

  womens_400m_mod <- lm(Finals_sec ~ Reaction_Time,
               data = train_400w)
  
  summary(womens_400m_mod)

# Mens 1500m freestyle model
set.seed(123)
  sample_size_1500m <- floor(0.7 * nrow(mens_1500m_freestyle_df))
  train_index_1500m <- sample(seq_len(nrow(mens_1500m_freestyle_df)), size = sample_size_1500m)
  train_1500m <- mens_1500m_freestyle_df[train_index_1500m, ]
  test_1500m  <- mens_1500m_freestyle_df[-train_index_1500m, ]

  mens_1500m_mod <- lm(Finals_sec ~ Reaction_Time,
                       data = train_1500m)

  summary(mens_1500m_mod)
  
# Model evaluation — MAE, RMSE and R² for both models ----
  
#Womens predictions
pred_train_400w <- predict(womens_400m_mod, newdata = train_400w)
pred_test_400w <- predict(womens_400m_mod, newdata = test_400w)
  
#Mens predictions
pred_train_1500m <- predict(mens_1500m_mod, newdata = train_1500m)
pred_test_1500m <- predict(mens_1500m_mod, newdata = test_1500m)

#Calculating MAE for womens 400m
MAE_train_400w <- mean(abs(pred_train_400w - train_400w$Finals_sec))
MAE_train_400w

MAE_test_400w <- mean(abs(pred_test_400w - test_400w$Finals_sec))
MAE_test_400w  
  
#Calculating MAE for mens 1500m  
MAE_train_1500m <- mean(abs(pred_train_1500m - train_1500m$Finals_sec))
MAE_train_1500m

MAE_test_1500m <- mean(abs(pred_test_1500m - test_1500m$Finals_sec))
MAE_test_1500m

# Women's 400m RMSE
RMSE_train_400w <- sqrt(mean((pred_train_400w - train_400w$Finals_sec)^2))
RMSE_train_400w

RMSE_test_400w <- sqrt(mean((pred_test_400w - test_400w$Finals_sec)^2))
RMSE_test_400w

# Men's 1500m RMSE
RMSE_train_1500m <- sqrt(mean((pred_train_1500m - train_1500m$Finals_sec)^2))
RMSE_train_1500m

RMSE_test_1500m <- sqrt(mean((pred_test_1500m - test_1500m$Finals_sec)^2))
RMSE_test_1500m

# R-squared
R2_400w <- summary(womens_400m_mod)$r.squared
R2_1500m <- summary(mens_1500m_mod)$r.squared

R2_400w
R2_1500m

# SECTION 3: Multiple Regression Modelling and Swim Event Time Prediction ----

# Data Merging and Processing:  ----
# Note: full_individual_df already created in Section 1
full_individual_df

# Extract Distance and Stroke from Event name convert Gender, Distance, Stroke to factors
full_individual_df <- full_individual_df |> 
  mutate(
    Distance = str_extract(Event, "\\d+"),
    Stroke = str_extract(Event, "Freestyle|Backstroke|Breaststroke|Butterfly"))

# Calculate mean_split_time (Finals_sec divided by number of 50m legs)

full_individual_df <- full_individual_df |> 
  mutate(mean_split_time = Finals_sec/(as.numeric(Distance)/50))


# Convert Gender, Distance and Stroke to factors

full_individual_df <- full_individual_df |>
  mutate(Stroke = as.factor(Stroke),
         Distance = as.numeric(Distance),
         Gender = as.factor(Gender))

# Multiple Linear Regression Modelling and Prediction: ----

set.seed(123)
sample_size_mv <- floor(0.7 * nrow(full_individual_df))
train_index_mv <- sample(seq_len(nrow(full_individual_df)), size = sample_size_mv)
train_mv <- full_individual_df[train_index_mv, ]
test_mv  <- full_individual_df[-train_index_mv, ]

# Log-log model: logging both sides linearises the power-law between
# distance and pace, enabling prediction beyond training distances.
mvmod <- lm(log(mean_split_time) ~ log(Distance) + Gender + Stroke, data = train_mv)

summary(mvmod)

# Model Evaluation — MAE, RMSE and R² for mvmod ----

pred_train_mvmod <- exp(predict(mvmod, newdata = train_mv))
pred_test_mvmod  <- exp(predict(mvmod, newdata = test_mv))

MAE_train_mvmod <- mean(abs(pred_train_mvmod - train_mv$mean_split_time))
MAE_test_mvmod <- mean(abs(pred_test_mvmod - test_mv$mean_split_time))

RMSE_train_mvmod <- sqrt(mean((pred_train_mvmod - train_mv$mean_split_time)^2))
RMSE_test_mvmod <- sqrt(mean((pred_test_mvmod - test_mv$mean_split_time)^2))

SS_res <- sum((train_mv$mean_split_time - pred_train_mvmod)^2)
SS_tot <- sum((train_mv$mean_split_time - mean(train_mv$mean_split_time))^2)
R2_mvmod <- 1 - SS_res / SS_tot

MAE_train_mvmod
MAE_test_mvmod
RMSE_train_mvmod
RMSE_test_mvmod
R2_mvmod

# Predict mean split time for Men's 100m Butterfly and Women's 5000m Freestyle ----
#men's 100m butterfly mean split prediction
mens_predicted_value <- exp(predict(mvmod, newdata = data.frame(
  Distance = 100, Gender = "Men", Stroke = "Butterfly")))
mens_predicted_value

#women's 5000m freestyle mean split prediction

predicted_5000w <- exp(predict(mvmod, newdata = data.frame(
  Distance = 5000, Gender = "Women", Stroke = "Freestyle")))

predicted_5000w

# SECTION 4: Data Visualisation of Ariarne Titmus' Performance ----
# Lead/lag vs. closest competitor at each 50m split (Heats vs Finals)----

# Filtering to titmus' heat and final only
titmus_race <- womens_400m_freestyle_df |>
  filter(Heat %in% c("Heat_4", "Final"))

# Converting to long format
titmus_long <- titmus_race |>
  pivot_longer(
    cols = num_range("Split_", seq(50, 400, by = 50)),
    names_to = "Distance",
    values_to = "split_sec"
  ) |>
  mutate(Distance = parse_number(Distance))

# Building cumulative time per swimmer per round (cumsum of splits)
titmus_long <- titmus_long |>
  group_by(Heat, Name) |>
  arrange(Distance, .by_group = TRUE) |>
  mutate(cum_time = cumsum(split_sec)) |>
  ungroup()

# Find closest competitor to Titmus and compute lead_lag
titmus_leadlag <- titmus_long |>
  group_by(Heat, Distance) |>
  mutate(titmus_cum = cum_time[Name == "TITMUS Ariarne"]) |>
  filter(Name != "TITMUS Ariarne") |>
  slice_min(abs(cum_time - titmus_cum), n = 1) |>
  ungroup() |>
  mutate(lead_lag = cum_time - titmus_cum) |>
  select(Heat, Distance, closest = Name, titmus_cum, competitor_cum = cum_time, lead_lag)

# ggplot2 visualisation ----
figure_2 <- titmus_leadlag |>
  ggplot(aes(x = Distance, y = lead_lag, colour = Heat)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#2e7d32", alpha = 0.06) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = "#c62828", alpha = 0.06) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = closest), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("Heat_4" = "blue", "Final" = "red")) +
  scale_x_continuous(breaks = seq(50, 400, by = 50)) +
  labs( title    = "Ariarne Titmus' Lead/Lag vs. Closest Competitor — Tokyo 2020 400m Freestyle",
  subtitle = "Positive = leading, Negative = lagging. Labels show the nearest competitor at each split.",
  x = "Distance (m)", y = "Lead (+) / Lag (−) vs. closest competitor (seconds)",
  colour = "Round"
  ) + theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.55),
    legend.background = element_rect(fill = "white")
  )

figure_2






