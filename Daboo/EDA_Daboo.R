
# loading the data
library(tidyverse)
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")


nhl_goals<- nhl_shots|>
  filter(event %in% "GOAL" )

library(ggplot2)

nhl_goals|>
  count(event, period)

# visually looking at the goals scored in each period with a bar chart
nhl_goals|>
  select(event, period)|>
  ggplot(aes(period))+
  geom_bar()

# creating a data set with just the goals scored in regulation
nhl_goals_reg <- nhl_goals |>
  filter(period %in% c(1, 2, 3))

# bar chart with counts and proportions
nhl_goals_reg|>
  select(event, period)|>
  count(period)|>
  mutate(prop=n/sum(n))|>
  ggplot(aes(period, prop))+
  geom_col()+
  geom_label(aes(label=n))

# bar chart showing how many goals each teams scores in each period
nhl_goals_reg|>
  select(event, period, teamCode)|>
  ggplot(aes(period, color=teamCode))+
  geom_bar()

# table showing how many goals each team scores in each period
nhl_goals_reg|>
  select(event, period, teamCode)|>
  count(period, teamCode) |>
  pivot_wider(names_from=teamCode, values_from=n)

# table showing the proportions of goals in each period that each team scores 
nhl_goals_reg_period_team<- nhl_goals_reg|>
  select(event, period, teamCode)|>
  count(period, teamCode) |>
  group_by(period)|>
  mutate(prop=n/sum(n))|>
  select(period, teamCode, prop)|>
  pivot_wider(names_from=teamCode, values_from=prop)|>
  ungroup()|>
  mutate(total=rowSums(across(-period)))

# Looking at who were the best shooters by SOG%

nhl_shots_reg<- nhl_shots|>
  filter(period %in% c(1, 2, 3))

class(nhl_shots_reg$event)
