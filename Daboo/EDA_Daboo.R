
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
  filter(total_shots>=50)

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

sog_stats|>
  ggplot(aes(sog_pct, shots_on_goal))+
  geom_point()+
  geom_text(aes(label=shooterName), size=3)

## Looking at the most efficient teams shooting

# creating the team shot on goal statistics
sog_team_stats<- nhl_shots_5v5|>
  group_by(teamCode)|>
  summarise(total_shots=n(),
            sog=sum(shotWasOnGoal==1, na.rm=TRUE),
            sog_pct=100*(sog/total_shots), 
            goals=sum(event=="GOAL"))

# plotting sog_pct with sog, along with the number of goals and if the team made the playoffs
sog_team_stats|>
  mutate(
    madeplayoffs = ifelse(teamCode %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                          "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                          "STL", "VGK", "LAK", "EDM"), "Yes", "No")) |>
  mutate(goal_bin = case_when(
    goals < 200 ~ "<200",
    goals >= 200 & goals <= 250 ~ "200-250",
    goals > 250 ~ ">250"
  ), goal_bin=factor(goal_bin, levels=c("<200", "200-250", ">250")))|>
  ggplot(aes(sog_pct, total_shots))+
  geom_point(alpha=.5, aes(size=goal_bin, color=madeplayoffs))+
  geom_hline(yintercept=mean(sog_team_stats$total_shots), linetype="dashed", color="red")+
  geom_vline(xintercept=mean(sog_team_stats$sog_pct), linetype="dashed", color="blue")+
  geom_text(aes(label=teamCode), vjust=-1, size=3)+
  scale_color_manual(values=c("Yes"="blue", "No"="red"))+
  scale_size_manual(values=c("<200"=2, "200-250"=4, ">250"=6))+
  labs(x="SOG %", y="Total Shots", 
       title="Shot Efficiency to Evaluate Goals and Whether a team made the playoffs", 
       color="Made Playoffs",
       size="Goals")+
  theme_light()+
  theme(plot.title = element_text(hjust=.5, face="bold"))

# plot but with the dot size for goals getting bigger, rather than on a set scale
sog_team_stats|>
  mutate(
    madeplayoffs = ifelse(teamCode %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                          "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                          "STL", "VGK", "LAK", "EDM"), "Yes", "No")) |>
  ggplot(aes(sog_pct, total_shots))+
  geom_point(alpha=.5, aes(size=goals, color=madeplayoffs))+
  geom_smooth(method="lm", se=FALSE, color="green")+
  geom_hline(yintercept=mean(sog_team_stats$total_shots), linetype="dashed", color="orange")+
  geom_vline(xintercept=mean(sog_team_stats$sog_pct), linetype="dashed", color="purple")+
  geom_text(aes(label=teamCode), vjust=-1, size=3)+
  scale_color_manual(values=c("Yes"="blue", "No"="red"))+
  scale_size_continuous(range=c(2, 10))+
  labs(x="SOG %", y="Total Shots", 
       title="Shot Efficiency to Evaluate Goals and Whether a team made the playoffs", 
       color="Made Playoffs",
       size="Goals", caption="Data from MoneyPuck.com")+
  theme_light()+
  theme(plot.title = element_text(hjust=.5, face="bold"))

# manually changing the location of the teamCode labels to they are all visible
sog_team_stats|>
  mutate(
    madeplayoffs = ifelse(teamCode %in% c("TOR", "TBL", "FLA", "OTT", "MTL", "WSH",
                                          "CAR", "NJD", "WPG", "DAL", "COL", "MIN",
                                          "STL", "VGK", "LAK", "EDM"), "Yes", "No"),
  label_x=case_when(
    teamCode=="PIT"~sog_pct+.25,
    TRUE~sog_pct
  ), 
  label_y=case_when(
    teamCode=="PIT"~total_shots+20,
    TRUE~total_shots
  )
  )|>
  ggplot(aes(sog_pct, total_shots))+
  geom_point(alpha=.5, aes(size=goals, color=madeplayoffs))+
  geom_smooth(method="lm", se=FALSE, color="green")+
  geom_hline(yintercept=mean(sog_team_stats$total_shots), linetype="dashed", color="orange")+
  geom_vline(xintercept=mean(sog_team_stats$sog_pct), linetype="dashed", color="purple")+
  geom_text(aes(x=label_x, y=label_y, label=teamCode), size=3)+
  scale_color_manual(values=c("Yes"="blue", "No"="red"))+
  scale_size_continuous(range=c(2, 10))+
  labs(x="SOG %", y="Total Shots", 
       title="Shot Efficiency to Evaluate Goals and Whether a team made the playoffs", 
       color="Made Playoffs",
       size="Goals", caption="Data from MoneyPuck.com")+
  theme_light()+
  theme(plot.title = element_text(hjust=.5, face="bold"))

## Clustering Analysis

# looking at the shape of variables to cluster
#shotAngle
nhl_shots|>
  ggplot(aes(shotAngle))+
  geom_histogram()
# shotDistance
nhl_shots|>
  ggplot(aes(shotDistance))+
  geom_histogram()

# transforming shot distance to get it normally distributed
nhl_shots|>
  mutate(sqrt_shotDistance=sqrt(shotDistance))|>
  ggplot(aes(sqrt_shotDistance))+
  geom_histogram()


# goal density map
nhl_shots |>
  filter(!is.na(arenaAdjustedXCord), !is.na(arenaAdjustedYCord), event == "GOAL") |>
  ggplot(aes(arenaAdjustedXCord, arenaAdjustedYCord)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Goal Density Map", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()


  