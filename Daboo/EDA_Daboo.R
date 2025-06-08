
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



nhl_goals_5v5<- nhl_goals|>
  filter(period %in% c(1, 2, 3))|>
  filter(shotOnEmptyNet==0)

nhl_goals_5v5|>
  select(event, period)|>
  ggplot(aes(period))+
  geom_bar()

# proportion of goals by each team in each period
nhl_goals_5v5_period_team<- nhl_goals_reg|>
  select(event, period, teamCode)|>
  count(period, teamCode) |>
  group_by(period)|>
  mutate(prop=n/sum(n))|>
  select(period, teamCode, prop)|>
  pivot_wider(names_from=teamCode, values_from=prop)|>
  ungroup()|>
  mutate(total=rowSums(across(-period)))

# Looking at who were the best shooters by SOG%
nhl_shots_5v5<- nhl_shots |>
  filter(shotOnEmptyNet==0)
names(nhl_shots)

# filtering out empty net shots
nhl_shots_5v5 <- nhl_shots |>
  filter(shotOnEmptyNet == 0)

# calculating a players sog_pct of players who took at least 20 shots
sog_stats<-nhl_shots_5v5|>
  group_by(shooterName)|>
  summarise(total_shots=n(),
            shots_on_goal=sum(shotWasOnGoal==1, na.rm=TRUE),
            sog_pct=shots_on_goal/total_shots)|>
  filter(total_shots>=20)

# top 10 players only by sog_pct
sog_stats|>
  slice_max(sog_pct, n=10)

# worst 10 players by sog_pct
sog_stats|>
  slice_min(sog_pct, n=10)

# finding top 10 in sog_pct but giving weight to taking more shots
sog_stats|>
  mutate(weighted_score=sog_pct * log(total_shots))|>
  arrange(desc(weighted_score))

# finding the bottom 10 in sog_pct but giving weight to taking more shots
sog_stats|>
  mutate(weighted_score=sog_pct * log(total_shots))|>
  arrange(weighted_score)

sog_stats|>
  mutate(weighted_score=sog_pct * sqrt(total_shots))|>
  arrange(desc(weighted_score))
