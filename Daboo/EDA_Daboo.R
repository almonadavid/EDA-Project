
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



nhl_goals_with_goalie<- nhl_goals|>
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

# goals in regions of the ice
nhl_goals_with_goalie|>
  ggplot(aes(shotAngle))+
  geom_histogram()
  
nhl_goals_with_goalie|>
  ggplot(aes(shotDistance))+
  geom_histogram()

nhl_goals_with_goalie|>
  ggplot(aes(shotDistance))+
  geom_boxplot()

nhl_goals_with_goalie|>
  summarise(max_distance=max(shotDistance), min_distance=min(shotDistance))

# because shot distance is heabily skewed right, set manual bin widths shorter 
# towards 0 feet, and farther widths larger than 0

distance_bins<-c(0, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
nhl_goals_with_goalie<-nhl_goals_with_goalie|>
  mutate(distance_bin=cut(shotDistance, breaks=distance_bins, include.lowest=TRUE),
         angle_bin=cut(shotAngle, breaks=seq(-90, 90, by=15), include.lowest=TRUE))
angle_distance_counts<-nhl_goals_with_goalie|>
  count(distance_bin, angle_bin)|>
  mutate(props=n/sum(n))

angle_distance_counts|>
  ggplot(aes(distance_bin, angle_bin, fill=n))+
  geom_tile(color="white")+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(label=scales::percent(props, accuracy=.1)), size=3)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

# best and worst goalies for 3.2% goal area
nhl_shots|>
  filter(shotDistance>5 & shotDistance<=10, shotAngle<=0 & shotAngle>-15, 
         event %in% c("GOAL", "SHOT"))|>
  group_by(goalieNameForShot) |>
  summarise(total_shots=n(),
            goals_allowed=sum(event=="GOAL"),
            save_pct=1- goals_allowed/total_shots)|>
  filter(total_shots>=10)|>
  arrange(desc(save_pct))|>
  arrange(save_pct)

# best and worst goalies for 2.9% goal area
nhl_shots|>
  filter(shotDistance >5 & shotDistance<=10, shotAngle>30 & shotAngle<=45, 
         event %in% c("GOAL", "SHOT"))|>
  group_by(goalieNameForShot) |>
  summarise(total_shots=n(),
            goals_allowed=sum(event=="GOAL"),
            save_pct=1- goals_allowed/total_shots)|>
  filter(total_shots>=10)|>
  arrange(desc(save_pct))|>
  arrange(save_pct)

# plotting for Tristan Jarry
distance_bins <- c(0, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100)
angle_bins <- seq(-90, 90, by = 15)
goalie_bin_stats<-nhl_shots|>
  filter(goalieNameForShot=="Tristan Jarry", event %in% c("GOAL", "SHOT"))|>
  mutate(distance_bin=cut(shotDistance, breaks=distance_bins, include.lowest=TRUE),
         angle_bin=cut(shotAngle, breaks=seq(-90, 90, by=15), include.lowest=TRUE))|>
  group_by(distance_bin, angle_bin)|>
  summarise(total_shots=n(),
            goals_allowed=sum(event=="GOAL"),
            save_pct=1- goals_allowed/total_shots)|>
  ungroup()
goalie_bin_stats|>
  ggplot(aes(distance_bin, angle_bin, fill=save_pct))+
  geom_tile(color="white")+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(label=scales::percent(save_pct, accuracy=.1)), size=3)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

# plotting Jarry's stats vs. league average
distance_bins <- c(0, 10, 20, 30, 50, 75, 100)
angle_bins <- seq(-90, 90, by = 30)
nhl_shots_binned<- nhl_shots|>
  filter(event %in% c("GOAL", "SHOT"))|>
  mutate(
    distance_bin = cut(shotDistance, breaks = distance_bins, include.lowest = TRUE),
    angle_bin = cut(shotAngle, breaks = angle_bins, include.lowest = TRUE)
  )

league_avg<-nhl_shots_binned|>
  group_by(distance_bin, angle_bin)|>
  summarise(
    league_total = n(),
    league_goals = sum(event == "GOAL"),
    league_save_pct = 1 - league_goals / league_total)|>
  ungroup()

jarry_stats<-nhl_shots_binned|>
  filter(goalieNameForShot=="Tristan Jarry")|>
  group_by(distance_bin, angle_bin)|>
  summarise(jarry_total=n(),
            jarry_goals=sum(event=="GOAL"),
            jarry_save_pct=1-jarry_goals/jarry_total)|>
  ungroup()

jarry_vs_league<-jarry_stats|>
  inner_join(league_avg, by=c("distance_bin", "angle_bin"))|>
  mutate(save_pct_diff=jarry_save_pct-league_save_pct)
sum(jarry_vs_league$save_pct_diff)

jarry_vs_league|>
  mutate(text_color=ifelse(save_pct_diff>=0, "white", "black"))|>
  ggplot(aes(distance_bin, angle_bin, fill=save_pct_diff))+
  geom_tile(color="white")+
  scale_fill_viridis_c(name="Save % \nDifference", option="D", direction=-1)+
  geom_text(aes(label=scales::percent(save_pct_diff, accuracy=.1),
            color=text_color), size=3)+
  scale_color_identity()+
  scale_y_discrete(limits=rev)+
  geom_point(data=data.frame(distance_bin=1.5, angle_bin=3.5),
             aes(distance_bin, angle_bin),
             color="black", size=7, shape=21, fill="red")+
  labs(
    title = paste("Save% Compared to the League Average for Tristan Jarry"),
    x = "Distance Bin", y = "Angle Bin",
    caption=paste0("Total Save% Difference: ",
                   scales::percent(sum(jarry_vs_league$save_pct_diff, na.rm=TRUE)))
    )+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        plot.caption = element_text(size=12))

# plotting on an nhl_rink
jarry_rink<-geom_hockey(league="NHL", display_range = "offense")
get_bin_center<-function(bin_labels){
  sapply(as.character(bin_labels), function(x) {
    nums <- as.numeric(gsub("[^0-9.-]", "", unlist(strsplit(x, ","))))
    if (length(nums) == 2) return(mean(nums))
    else return(as.numeric(nums))  # fallback
  })
}
  
jarry_vs_league<-jarry_vs_league|>
  mutate(dist_center=get_bin_center(distance_bin),
         angle_center=get_bin_center(angle_bin),
         theta_rad = angle_center * pi / 180,
         x=89-(dist_center*cos(theta_rad)),
         y=dist_center*sin(theta_rad))
         
jarry_rink+
  geom_hex(data = jarry_vs_league, aes(x = x, y = y, fill = save_pct_diff), 
            width = 8, height = 8, color = "white") +
  scale_fill_viridis_c(name="Save %\nDifference", option="D", direction=-1) +
  geom_text(data = jarry_vs_league, 
            aes(x = x, y = y, 
                label = scales::percent(save_pct_diff, accuracy = 0.1),
                color = ifelse(save_pct_diff >= 0, "white", "black")),
            size = 3) +
  scale_color_identity() +
  labs(
    title = "Save% Compared to League Average for Tristan Jarry",
    caption = paste0("Total Save% Difference: ",
                     scales::percent(sum(jarry_vs_league$save_pct_diff, na.rm = TRUE)))
  ) +
  theme_minimal()

# plotting all of the shots taken on Tristan Jarry
shots_on_jarry<-nhl_shots|>
  filter(goalieNameForShot=="Tristan Jarry", event %in% c("GOAL", "SHOT"))|>
  mutate(x=abs(arenaAdjustedXCord), y=ifelse(
    arenaAdjustedXCord<0, -arenaAdjustedYCord, arenaAdjustedYCord))

goals_on_jarry<-shots_on_jarry|>
  filter(event %in%c("GOAL"))
         
ozone_rink<-geom_hockey(league="NHL", display_range="offense")
ozone_rink+
  geom_point(data=shots_on_jarry, aes(x, y, color=event))
# plotting all of the goals on Jarry
jarry_goals<-ozone_rink+
  geom_hex(data=goals_on_jarry, aes(x, y), binwidth=c(5,5), alpha=.8)+
  scale_fill_gradient(low="midnightblue", high="red")+
  labs(title="Goals Given up by Tristan Jarry")

nhl_goals<-nhl_goals|>
  mutate(x=abs(arenaAdjustedXCord), y=ifelse(
    arenaAdjustedXCord<0, -arenaAdjustedYCord, arenaAdjustedYCord))

# plotting all of the goals in the NHL
league_goals<-ozone_rink+
  geom_hex(data=nhl_goals, aes(x=x, y=y), binwidth=c(5,5), alpha=.8)+
  scale_fill_gradient(low="midnightblue", high="red")+
  labs(title="Goals Given up by all NHL Goalies")

# plotting goals against Hellebuyck
goals_on_helly<-nhl_goals|>
  filter(goalieNameForShot=="Connor Hellebuyck")
helly_goals<-ozone_rink+
  geom_hex(data=goals_on_helly, aes(x, y), binwidth=c(5,5), alpha=.8)+
  scale_fill_gradient(low="midnightblue", high="red")+
  labs(title="Goals Given up by Connor Hellebuyck")
  

library(patchwork)
# combining the two plots
jarry_goals + league_goals + plot_layout(guides="collect")
jarry_goals+helly_goals+plot_layout(guides="collect")
