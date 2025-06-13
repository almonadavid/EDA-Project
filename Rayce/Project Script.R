library(tidyverse)
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")

# Home Team vs. Away Team Shots on Goal
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


#Shooter Time of Ice vs. Goals
goalsonly<-nhl_shots |>
  filter(event %in% c("GOAL"), shooterTimeOnIce < 100)
         

print(which.max(goalsonly$shooterTimeOnIce))  

library(ggplot2)       
hist(goalsonly$shooterTimeOnIce,
     xlim = c(0, 100),
     breaks = 200,
     main = "Histogram of Shooter Time on Ice for Goals",
     xlab = "Time on Ice (seconds)",
     col = "skyblue",
     border = "white")

timeonice<-nhl_shots |>
  filter(shooterTimeOnIce < 100)
library(ggplot2)       
hist(timeonice$shooterTimeOnIce,
     xlim = c(0, 100),
     breaks = 200,
     main = "Histogram of Shooter Time on Ice",
     xlab = "Time on Ice (seconds)",
     col = "red",
     border = "white")

#Best Goalie
goalietotalshots<-nhl_shots|>
  group_by(goalieNameForShot) |>
  summarise(totalshots=n())

goaliesgoalscoredon<-nhl_shots|>
  filter(event == "GOAL") |>
  group_by(goalieNameForShot) |>
  summarise(totalgoals=n())

goaliedata <- merge(goalietotalshots, goaliesgoalscoredon, by = "goalieNameForShot") |>
  mutate(goal_perc = totalgoals/totalshots)|>
  arrange(goal_perc)
  
#Shot Type vs. Event(Goal, Miss, Shot), vs. Distance

nhl_shots |>
  count(event, shotType) |>
  ggplot(aes(x = shotType, y = n, fill = event)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(
    title = "Goal vs. Shot Type",
    x = "Shot Type",
    y = "Count",
    fill = "Goal?"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

nhl_shots |>
  filter(!is.na(event), !is.na(shotDistance)) |>
  ggplot(aes(x = event, y = shotDistance, fill = event)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Shot Distance by Event Type",
    x = "Event",
    y = "Shot Distance (feet)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 

nhl_shots |>
  filter(!is.na(event), !is.na(shotAngle)) |>
  ggplot(aes(x = event, y = shotAngle, fill = event)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Shot Angle by Event Type",
    x = "Event",
    y = "Shot Angle (feet)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 

nhl_shots |>
  filter(!is.na(event), !is.na(shotDistance)) |>
  ggplot(aes(x = shotDistance, fill = event)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Shot Distance by Event",
    x = "Shot Distance (feet)",
    y = "Density",
    fill = "Event"
  )

nhl_shots |>
  filter(!is.na(event), !is.na(shotAngle)) |>
  ggplot(aes(x = shotAngle, fill = event)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Shot Angle by Event",
    x = "Shot Distance (feet)",
    y = "Density",
    fill = "Event"
  )
nhl_shots |>
  filter(!is.na(shotType), !is.na(shotDistance), !is.na(event)) |>
  group_by(event, shotType) |>
  summarise(mean_distance = mean(shotDistance, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(y = event, x = mean_distance, fill = shotType)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Mean Shot Distance by Event and Shot Type",
    x = "Mean Shot Distance (feet)",
    y = "Event",
    fill = "Shot Type"
  )


nhl_shots |>
  filter(!is.na(event), !is.na(shotDistance), !is.na(shotAngle)) |>
  ggplot(aes(x = shotDistance, y = shotAngle, color = event)) +
  geom_point(alpha = 0.5, size = 1.5)


library(ggplot2)
library(dplyr)

nhl_shots %>%
  filter(!is.na(event), !is.na(shotDistance), !is.na(shotAngle)) %>%
  mutate(posAngle = abs(shotAngle)) %>%
  # count(shotDistance, posAngle) %>% 
  # ggplot(aes(x=n))+geom_histogram()
  ggplot(aes(x = shotDistance, y = posAngle, color = event)) +
  geom_point(size = 1.2) +
  scale_color_manual(values = c("GOAL" = "hotpink", "MISS" = "black", "SHOT" = "black")) +
  labs(
    x = "Shot Distance",
    y = "Shot Angle",
    color = "Event",
    title = "NHL Shot Events by Distance and Angle"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Clustering
library(tidyverse)
theme_set(theme_light())
library(dslabs)

nhl_shots |> 
  ggplot(aes(x = shotDistance)) + 
  geom_histogram()
  
nhl_shots |> 
  ggplot(aes(x = shotAngle) + 
           geom_histogram()

library(tidyverse)
theme_set(theme_light())
library(dslabs)

# Histogram of shot distances
nhl_shots |> 
  ggplot(aes(x = shotDistance)) + 
  geom_histogram()

# Histogram of shot angles (fix: missing parenthesis)
nhl_shots |> 
  ggplot(aes(x = shotAngle)) + 
  geom_histogram()

# Cluster players by mean angle and distance
player_cluster <- nhl_shots |>
  mutate(abs_shotAngle = abs(shotAngle)) |>
  group_by(shooterName) |> 
  summarize(
    meanangle = mean(abs_shotAngle, na.rm = TRUE), 
    meandistance = mean(shotDistance, na.rm = TRUE)
  ) |>
  ungroup() |> 
  mutate(
    std_meanangle = as.numeric(scale(meanangle)),
    std_meandistance = as.numeric(scale(meandistance))
  )

# K-means clustering
shots_kmeans <- player_cluster |> 
  select(std_meanangle, std_meandistance) |> 
  kmeans(centers = 4, nstart = 30, algorithm = "Lloyd")

# Add cluster labels
player_cluster <- player_cluster |> 
  mutate(shot_clusters = as.factor(shots_kmeans$cluster))

# Plot clusters
ggplot(player_cluster, aes(x = meanangle, y = meandistance, color = shot_clusters)) +
  geom_point(size = 4, alpha = 0.8) + 
  ggthemes::scale_color_colorblind() +
  labs(
    #title = "Shot Clusters by Angle and Distance for Individual Players",
    x = "Mean Absolute Shot Angle",
    y = "Mean Shot Distance",
    color = "Cluster"
  ) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Calculate shooting percentages
player_shot_pct <- nhl_shots |>
  group_by(shooterName) |>
  summarize(
    total_shots = n(),
    goals = sum(event == "GOAL", na.rm = TRUE),
    shot_pct = goals / total_shots
  ) |>
  ungroup()

# Combine cluster data with shooting stats
player_cluster_with_stats <- player_cluster |> 
  left_join(player_shot_pct, by = "shooterName")

# Average shot percentage per cluster
cluster_percentage <- player_cluster_with_stats |> 
  group_by(shot_clusters) |> 
  summarize(avg_shotperc_cluster = mean(shot_pct, na.rm = TRUE))


#--------------------------------------------------------------------------------------------------



library(tidyverse)
theme_set(theme_light())
library(dslabs)

# Histogram of shot distances
nhl_shots |> 
  ggplot(aes(x = shotDistance)) + 
  geom_histogram()

# Histogram of shot angles
nhl_shots |> 
  ggplot(aes(x = shotAngle)) + 
  geom_histogram()

# Prepare shot-level data with cleaned variables
shot_data <- nhl_shots |>
  filter(!is.na(shotDistance), !is.na(shotAngle)) |>
  mutate(
    abs_shotAngle = abs(shotAngle),
    std_angle = as.numeric(scale(abs_shotAngle)),
    std_distance = as.numeric(scale(shotDistance))
  )

# K-means clustering on individual shots
shot_kmeans <- shot_data |> 
  select(std_angle, std_distance) |> 
  kmeans(centers = 4, nstart = 30, algorithm = "Lloyd")

# Add cluster labels to each shot
shot_data <- shot_data |> 
  mutate(shot_cluster = as.factor(shot_kmeans$cluster))

# Plot clusters for individual shots
ggplot(shot_data, aes(x = abs_shotAngle, y = shotDistance, color = shot_cluster)) +
  geom_point(alpha = 0.5, size = 1.5) +
  ggthemes::scale_color_colorblind() +
  labs(
    #title = "Shot Clusters by Angle and Distance (Individual Shots)",
    x = "Absolute Shot Angle",
    y = "Shot Distance",
    color = "Cluster"
  ) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Optional: Shot percentage by cluster (if keeping goal info)
cluster_shot_stats <- shot_data |> 
  group_by(shot_cluster) |> 
  summarize(
    total_shots = n(),
    goals = sum(event == "GOAL", na.rm = TRUE),
    shot_pct = goals / total_shots
  )


#Making a hockey rink, this not correct lol
library(sportyR)

?geom_hockey


library(ggplot2)
library(sportyR)  
library(dplyr)
library(ggthemes)

# Step 1: Prepare and cluster the player data
player_cluster <- nhl_shots |>
  mutate(abs_shotAngle = abs(shotAngle)) |>
  group_by(shooterName) |> 
  summarize(
    meanangle = mean(abs_shotAngle, na.rm = TRUE), 
    meandistance = mean(shotDistance, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    std_meanangle = as.numeric(scale(meanangle)),
    std_meandistance = as.numeric(scale(meandistance))
  )

# Step 2: Run K-means on standardized values
set.seed(47)
shots_kmeans <- player_cluster |> 
  select(std_meanangle, std_meandistance) |> 
  kmeans(centers = 4, nstart = 30)

# Step 3: Add cluster assignments
player_cluster <- player_cluster |> 
  mutate(shot_clusters = as.factor(shots_kmeans$cluster))

# Step 4: Convert polar to Cartesian for rink overlay
plotrinkdata <- player_cluster |>
  # filter (shooterName ==
  #           "Igor Shesterkin" ) |>
  mutate(
    angle_rad = meanangle * pi / 180,
    x = meandistance * cos(angle_rad),
    y = meandistance * sin(angle_rad)
  )

# Step 5: Build plot
base_rink_plot <- geom_hockey(league = "NHL", display_range = "offensive")

base_rink_plot +
  geom_point(
    data = plotrinkdata,
    aes(x = x-90, y = y, color = shot_clusters),
    size = 3, alpha = .6
  ) +
  ggthemes::scale_color_colorblind() +
  coord_fixed(xlim = c(-100, -20)) +
  theme_void() +
  labs(
    #title = "Clustered Player Shot Tendencies on NHL Rink",
    color = "Cluster"
  ) +
  theme(
    #legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
    
  )



