library(tidyverse)
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")

# need to find a better way to do total column
hometeamshotsgoals_total <- nhl_shots %>%
  filter(team == "HOME", event %in% c("GOAL", "SHOT")) %>%
  summarise(total = n())


hometeamshots_total <- nhl_shots %>%
  filter(team == "HOME") %>%
  summarise(total = n())

 
hometeamshotsongoal_percentage<- hometeamshotsgoals_total/ hometeamshots_total
print(hometeamshotsongoal_percentage)

awayteamshotsgoals_total <- nhl_shots %>%
  filter(team == "AWAY", event %in% c("GOAL", "SHOT")) %>%
  summarise(total = n())


awayteamshots_total <- nhl_shots %>%
  filter(team == "AWAY") %>%
  summarise(total = n())


awayteamshotsongoal_percentage<- awayteamshotsgoals_total/ awayteamshots_total
print(awayteamshotsongoal_percentage)


print(hometeamshotsgoals_total)
print(hometeamshots_total)
print(awayteamshotsgoals_total)
print(awayteamshots_total)

