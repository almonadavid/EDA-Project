library(tidyverse)
library(sportyR)
library(ggplot2)
theme_set(theme_light())
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")


## Bar chart showing shot handedness distribution
nhl_shots |>
  filter(!is.na(shooterLeftRight)) |>
  count(shooterLeftRight) |>
  mutate(prop = n/sum(n)) |>
  ggplot(aes(x = shooterLeftRight, y = prop)) +
  geom_col(fill = "blue") +
  scale_x_discrete(labels = c("Left", "Right")) +
  labs(y = "Proportion",
       x = "Shooter Handedness",
       caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))

# Chi square test shows this is statistically significant
chisq.test(table(nhl_shots$shooterLeftRight))


## Shooter Handedness vs Shot Side ####

nhl_shots |>
  filter(!is.na(shooterLeftRight)) |> 
  mutate(shotSide = 
           ifelse(shotAngle < 0, "left", "right")) |> 
  count(shooterLeftRight, shotSide) |> 
  mutate(prop = n/sum(n), .by = shooterLeftRight) |>
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  labs(x = "Shooter Handedness", 
       y = "Proportion", 
       fill = "Shot Side",
       title = "Shooter Handedness vs Shot Side (All Shots)",
       caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))


nhl_shots |>
  filter(!is.na(shooterLeftRight) & event == "GOAL") |> 
  mutate(shotSide = 
           ifelse(shotAngle < 0, "left", "right")) |> 
  count(shooterLeftRight, shotSide) |> 
  mutate(prop = n/sum(n), .by = shooterLeftRight) |>
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  labs(x = "Shooter Handedness", 
       y = "Proportion", 
       fill = "Shot Side",
       title = "Shooter Handedness vs Shot Side (Only Goals)",
       caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))

## Facet Wrap Version of Bar Chart Above
# First, create a summary of GOAL events
goal_summary <- nhl_shots |>
  filter(!is.na(shooterLeftRight), event == "GOAL") |> 
  mutate(shotSide = ifelse(shotAngle < 0, "left", "right")) |>
  count(event, shooterLeftRight, shotSide)

# Next, create a summary of TOTAL events by counting across all events
total_summary <- nhl_shots |>
  filter(!is.na(shooterLeftRight)) |> 
  mutate(shotSide = ifelse(shotAngle < 0, "left", "right")) |>
  # Count by handedness and shot side, but NOT by event
  count(shooterLeftRight, shotSide) |> 
  # Manually add the 'event' column to label this data as "TOTAL"
  mutate(event = "TOTAL")

# Combine the two summaries into one dataframe
final_data <- bind_rows(goal_summary, total_summary) |>
  # Now, calculate the proportion within each group (GOAL and TOTAL)
  group_by(event, shooterLeftRight) |>
  mutate(prop = n / sum(n))


# 2. --- Plotting ---

# Pipe the final prepared data into ggplot
final_data |>
  ggplot(aes(x = shooterLeftRight, y = prop, fill = shotSide)) +
  geom_col(position = "dodge") +
  # Facet by the 'event' column, which now contains "GOAL" and "TOTAL"
  facet_wrap(~ event) +
  labs(
    x = "Shooter Handedness", 
    y = "Proportion", 
    fill = "Shot Side",
    title = "Proportion of Shots by Side for Goals vs. All Shots",
    caption = "Data courtesy of MoneyPuck.com."
  ) +
  theme(plot.caption = element_text(face = "italic")) +
  scale_y_continuous(labels = scales::percent)


## Team code vs shot distance
nhl_shots |> 
  filter(shotOnEmptyNet == 0) |> 
  ggplot(aes(x = teamCode, y = shotDistance)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Shot Distance by Team (excluding empty net shots)",
    x = "Team",
    y = "Shot Distance (feet)",
    caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))


## Shots vs Goals Per Game by Team (excluding empty net shots)
nhl_shots |> 
  filter(shotOnEmptyNet == 0) |> 
  group_by(teamCode) |> 
  summarise(numGames = n_distinct(game_id),
            numShots = n(),
            numGoals = sum(event == "GOAL", na.rm = TRUE),
            shotspergame = numShots / numGames,
            goalspergame = numGoals / numGames) |>
  mutate(
    madeplayoffs = ifelse(teamCode %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                      "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                      "STL", "VGK", "LAK", "EDM"), "Yes", "No")) |>
  ggplot(aes(x = shotspergame, y = goalspergame)) +
  geom_point(aes(color = madeplayoffs), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  labs(
    title = "Shots vs Goals Per Game by Team (excluding empty net shots)",
    x = "Shots Per Game",
    y = "Goals Per Game",
    color = "Made Playoffs?",
    caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))


## Shots vs Goals Per Game by Team with TEAM LABEL
nhl_shots |> 
  filter(shotOnEmptyNet == 0) |> 
  group_by(teamCode) |> 
  summarise(numGames = n_distinct(game_id),
            numShots = n(),
            numGoals = sum(event == "GOAL", na.rm = TRUE),
            shotspergame = numShots / numGames,
            goalspergame = numGoals / numGames) |>
  mutate(
    madeplayoffs = ifelse(teamCode %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                          "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                          "STL", "VGK", "LAK", "EDM"), "Yes", "No")) |>
  ggplot(aes(x = shotspergame, y = goalspergame)) +
  geom_point() +
  geom_hline(yintercept = 2.82, linetype = "dashed") +
  geom_vline(xintercept = 42.40, linetype = "dashed") +
  geom_label(aes(label = teamCode, color = madeplayoffs)) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  labs(
    title = "Shots vs Goals Per Game by Team (excluding empty net shots)",
    x = "Shots Per Game",
    y = "Goals Per Game",
    color = "Made Playoffs?",
    caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))


## Shots made vs Shot Conceded (color by playoffs)
nhl_shots |>
  group_by(game_id, homeTeamCode, awayTeamCode, isHomeTeam) |>
  summarise(shots = n(), .groups = "drop") |>
  pivot_wider(
    names_from = isHomeTeam,
    values_from = shots,
    names_prefix = "shots_") |>
  rename(homeShots = shots_1,
         awayShots = shots_0) |> 
  select(game_id, homeTeamCode, awayTeamCode, homeShots, awayShots) |>
  group_by(homeTeamCode) |> 
  summarise(shotsMade = sum(homeShots, na.rm = TRUE),
            shotsConceded = sum(awayShots, na.rm = TRUE)) |> 
  rename(Team = homeTeamCode) |> 
  mutate(
    madeplayoffs = ifelse(Team %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                          "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                          "STL", "VGK", "LAK", "EDM"), "Yes", "No")) |>
  ggplot(aes(x = shotsMade, y = shotsConceded)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  geom_label(aes(label = Team, color = madeplayoffs)) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "red")) +
  labs(
    title = "Shots Made vs Shots Conceded by Team",
    x = "Shots Made",
    y = "Goals Conceded",
    color = "Made Playoffs?",
    caption = "Data courtesy of MoneyPuck.com.") +
  theme(plot.caption = element_text(face = "italic"))



## sportyR package
geom_hockey(league = "NHL") +
  geom_point(data = subset(nhl_shots, event == "GOAL" & shotOnEmptyNet == 0), 
             aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord))



## Do teams shoot differently depending on score differential?
# shot frequency by score differential
# nhl_shots |>
#   mutate(score_diff = ifelse(isHomeTeam == 1,
#                              homeTeamGoals - awayTeamGoals,
#                              awayTeamGoals - homeTeamGoals)) |> 
#   filter(shotOnEmptyNet == 0) |>
#   group_by(score_diff) |>
#   summarise(total_shots = n(),
#             timeinscorediff = mean(time)) |> View()
#   ggplot(aes(x = score_diff, y = total_shots)) +
#   geom_col() +
#   labs(
#     title = "Total Shots by Score Differential",
#     x = "Score Differential (Shooting Team Perspective)",
#     y = "Number of Shots"
#   ) +
#   theme_minimal()
# 
# # shot distance by score differential
# nhl_shots |>
#   mutate(score_diff = ifelse(isHomeTeam == 1,
#                              homeTeamGoals - awayTeamGoals,
#                              awayTeamGoals - homeTeamGoals)) |> 
#   filter(shotOnEmptyNet == 0) |>
#   ggplot(aes(x = factor(score_diff), y = shotDistance)) +
#   geom_boxplot() +
#   labs(
#     title = "Shot Distance by Score Differential",
#     x = "Score Differential (Shooting Team Perspective)",
#     y = "Shot Distance (feet)"
#   ) +
#   theme_minimal()