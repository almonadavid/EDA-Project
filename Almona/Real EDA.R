library(tidyverse)
theme_set(theme_light())
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")

# Potential Questions:
# Home Team Goals vs Away Team Goals
# Shot by location/position
# Shot angle

# Question 1
# Does shot angle influence goal likelihood, 
# and does this relationship depend on shot type?

## Bar chart showing Left shots vs Right shots
nhl_shots |>
  filter(!is.na(shooterLeftRight)) |>
  count(shooterLeftRight) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = shooterLeftRight, y = prop)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(y = "prop")
  # geom_label(aes(label = round(prop*100,2), hjust = 1))
#   
# chisq.test(table(nhl_shots$shooterLeftRight))
# 
# 
# test <- nhl_shots |> 
#   filter(!is.na(shotType)) |> 
#   mutate(shotSector = case_when( #grouping shot angles
#     shotAngle > 60 ~ 1,   # far left
#     shotAngle >= -60 & shotAngle <= 60 ~ 2,  # center
#     shotAngle < -60 ~ 3   # far right
#   )) 
# 
# table("Shot Sector" = test$shotSector,
#       "Shot Type" = test$shotType)
# 
# ## Heatmap of shotType & shotSector
# nhl_shots |> 
#   filter(!is.na(shotType)) |> 
#   mutate(shotSector = cut(
#     shotAngle,
#     breaks = seq(-90, 90, by = 30), # 6 bins
#     include.lowest = TRUE,
#     labels = c("Far Right", "Right Wing", "Right-Center", "Left-Center", "Left Wing", "Far Left")
#   )) |> 
#   group_by(shotSector, shotType) |> 
#   summarize(freq = n(),
#             joint = n()/nrow(nhl_shots)) |> 
#   ggplot(aes(x = shotSector, y = shotType)) +
#   geom_tile(aes(fill = freq), color = "white") +
#   geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
#   scale_fill_gradient2()
#   
# 
# ## Heatmap of Event & shotSector
# nhl_shots |> 
#   # filter(!is.na(shotType)) |> 
#   mutate(shotSector = cut(
#     shotAngle,
#     breaks = seq(-90, 90, by = 30), # 6 bins
#     include.lowest = TRUE,
#     labels = c("Far Right", "Right Wing", "Right-Center", "Left-Center", "Left Wing", "Far Left")
#   )) |> 
#   group_by(shotSector, event) |> 
#   summarize(freq = n(),
#             joint = n()/nrow(nhl_shots)) |> 
#   ggplot(aes(x = shotSector, y = event)) +
#   geom_tile(aes(fill = freq), color = "white") +
#   geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
#   scale_fill_gradient2()
#   
# 
# ## Heatmap of Event & shooterLeftRight
# nhl_shots |> 
#   filter(!is.na(shooterLeftRight)) |> 
#   group_by(shooterLeftRight, event) |> 
#   summarize(freq = n(),
#             joint = n()/nrow(nhl_shots)) |> 
#   ggplot(aes(x = shooterLeftRight, y = event)) +
#   geom_tile(aes(fill = freq), color = "white") +
#   geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
#   scale_fill_gradient2()
# 
# ## Histogram shot angle
# nhl_shots |> 
#   ggplot(aes(x = shotAngle)) +
#   geom_histogram() +
#   geom_rug(aes(color = shotAngle), alpha = 0.2)

## Shooter Handedness vs Shot Side ####

nhl_shots |>
  filter(!is.na(shooterLeftRight)) |> 
  mutate(shotSide = 
           ifelse(shotAngle < 0, "left", "right")) |> 
  count(shooterLeftRight, shotSide) |> 
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  labs(x = "Shooter Handedness", 
       y = "Proportion", 
       fill = "Shot Side",
       title = "All Shots") +
  theme_minimal()


nhl_shots |>
  filter(!is.na(shooterLeftRight) & event == "GOAL") |> 
  mutate(shotSide = 
           ifelse(shotAngle < 0, "left", "right")) |> 
  count(shooterLeftRight, shotSide) |> 
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  labs(x = "Shooter Handedness", 
       y = "Proportion", 
       fill = "Shot Side",
       title = "Only Goals") +
  theme_minimal()



nhl_shots |>
  filter(!is.na(shooterLeftRight)) |> 
  mutate(shotSide = 
           ifelse(shotAngle < 0, "left", "right")) |> 
  ggplot(aes(x = shooterLeftRight, fill = shotSide)) +
  geom_bar(position = "stack") +
  facet_wrap(~ event)



nhl_shots |>
  filter(!is.na(shooterLeftRight)) |> 
  mutate(shotSide = ifelse(shotAngle < 0, "left", "right")) |> 
  count(event, shooterLeftRight, shotSide) |> 
  group_by(event, shooterLeftRight) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  facet_wrap(~ event) +
  labs(
    x = "Shooter Handedness", 
    y = "Proportion of Shots", 
    fill = "Shot Side",
    title = "Shot Side by Shooter Handedness (Proportions by Event)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


## Team code vs shot distance
nhl_shots |> 
  filter(shotOnEmptyNet == 0) |> 
  ggplot(aes(x = teamCode, y = shotDistance)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Shot Distance by Team (excluding empty net shots)",
    x = "Team",
    y = "Shot Distance (feet)"
  ) +
  theme_minimal()

## Do teams shoot differently depending on score differential?
# shot frequency by score differential
nhl_shots |>
  mutate(score_diff = ifelse(isHomeTeam == 1,
                             homeTeamGoals - awayTeamGoals,
                             awayTeamGoals - homeTeamGoals)) |> 
  filter(shotOnEmptyNet == 0) |>
  group_by(score_diff) |>
  summarise(total_shots = n()) |>
  ggplot(aes(x = score_diff, y = total_shots)) +
  geom_col() +
  labs(
    title = "Total Shots by Score Differential",
    x = "Score Differential (Shooting Team Perspective)",
    y = "Number of Shots"
  ) +
  theme_minimal()

# shot disctance by score differential
nhl_shots |>
  mutate(score_diff = ifelse(isHomeTeam == 1,
                             homeTeamGoals - awayTeamGoals,
                             awayTeamGoals - homeTeamGoals)) |> 
  filter(shotOnEmptyNet == 0) |>
  ggplot(aes(x = factor(score_diff), y = shotDistance)) +
  geom_boxplot() +
  labs(
    title = "Shot Distance by Score Differential",
    x = "Score Differential (Shooting Team Perspective)",
    y = "Shot Distance (feet)"
  ) +
  theme_minimal()
