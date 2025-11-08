############################################################
# NBA Game Win Prediction Using Multivariable Regression
# Date: 2025-11-06
# Description: Predicts NBA game outcomes and point differentials
# using multivariable regression on differential team statistics.
############################################################

# --- Setup ---
# Ensures all code is shown when executed
knitr::opts_chunk$set(echo = TRUE)

# Loads required packages and dependencies for analysis
source("00_requirements.R")

# Runs the cleaning script that loads and preprocesses NBA data
source("Cleaning.Rmd")


# --- Data Overview ---
# Creates summary statistics for differential performance metrics
summary_stats <- games_clean %>%
  select(FG_diff, FT_diff, FG3_diff, AST_diff, REB_diff, PTS_diff) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))

# Displays the summary statistics table
print(summary_stats)

# --- Visualization Section ---

# Sets a 2x3 grid layout for multiple histograms
par(mfrow = c(2,3))

# Each histogram shows the distribution of differences between home and away teams
hist(games_clean$FG_diff, main = "Field Goal % Differential", xlab = "FG_diff", breaks = 30)
hist(games_clean$FG_diff, main = "Free Throw % Differential", xlab = "FT_diff", breaks = 30)
hist(games_clean$FG_diff, main = "3-Point % Differential", xlab = "FG3_diff", breaks = 30)
hist(games_clean$FG_diff, main = "Assists Differential", xlab = "AST_diff", breaks = 30)
hist(games_clean$FG_diff, main = "Rebounds Differential", xlab = "REB_diff", breaks = 30)
hist(games_clean$FG_diff, main = "Points Differential", xlab = "PTS_diff", breaks = 30)

# Denisty plot comparing point differential distributions for home vs away wins
games_clean %>%
  mutate(Win_Status = ifelse(HOME_TEAM_WINS == 1, "Home Win", "Away Win"))
  ggplot(aes(x = PTS_diff, fill = Win_Status)) +\
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Point Differential by Game Outcome",
    x = "Point Differential (Home - Away)",
    y = "Density") +
  theme_minimal()

# Box plots showing differential statistics separted by game outcome
games_clean %>%
  mutate(Win_Status = ifelse(HOME_TEAM_WINS == 1, "Home Win", "Away Win")) %>%
  select(Win_Status, FG_diff, FT_diff, FG3_diff, AST_diff, REB_diff) %>%
  pivot_longer(cols = -Win_Status, names_to = "Statistic", values_to = "Value") %>%
  ggplot(aes(x = Win_Status, y = Value, fill = Win_Status)) +
  geom_boxplot() +
  facet_wrap(~Statistic, scales = "free_y") +
  labs(title = "Distribution of Differential Statistics by Game Outcome",
       x = "Game Outcome",
       y = "Differential Value") +
  theme_minimal() +
  theme(legend.position = "none")

# Scatter plots showing linear trends between predictors and point differential

games_clean %>%
  select(FG_diff, FT_diff, FG3_diff, AST_diff, REB_diff, PTS_diff) %>%
  pivot_longer(cols = -PTS_diff, names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(x = Value, y = PTS_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(title = "Relationship Between Predictors and Point Differential",
       x = "Predictor Value",
       y = "Point Differential (Home - Away)") +
  theme_minimal()

# --- Modeling Section ---

# Logistic Regression: Predicts the probability that the home team wins
win_model <- glm(
  HOME_TEAM_WINS ~ FG_diff + FT_diff + FG3_diff + AST_diff + REB_diff,
  data = games_clean,
  family = binomial
)

# Displays model coefficients, p-values, and overall fit
summary(win_model)

# Linear Regression: Predicts the expected point differential (home - away)
differential_model <- glm(
  PTS_diff ~ FG_diff + FT_diff + FG3_diff + AST_diff + REB_diff,
  data = games_clean
)

# Shows model fit statistics and predictor significance
sumarry(differential_model)

# --- Prediction Section ---

# Generate predictions using both regression models
games_clean <- games_clean %>%
  mutate(
    pred_homewin_prob = predict(win_model, newdata = ., type = "response"),   # 🔵 Predicted probability of a home win
    pred_winner = if_else(pred_homewin_prob >= 0.5, HOME_ABBREVIATION, VISITOR_ABBREVIATION),  # 🔵 Predicted winning team
    pred_margin = as.numeric(predict(differential_model, newdata = .)),       # 🔵 Predicted point differential
    pred_side = if_else(pred_winner == HOME_ABBREVIATION, "HOME", "AWAY")     # 🔵 Predicted side (home/away)
  )

# Preview predictions for quick inspection
head(games_clean %>%
       select(GAME_ID, HOME_ABBREVIATION, VISITOR_ABBREVIATION,
              HOME_TEAM_WINS, pred_homewin_prob, pred_winner, pred_margin, pred_side))
