library(tidyverse)
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
  
chisq.test(table(nhl_shots$shooterLeftRight))


test <- nhl_shots |> 
  filter(!is.na(shotType)) |> 
  mutate(shotSector = case_when( #grouping shot angles
    shotAngle > 60 ~ 1,   # far left
    shotAngle >= -60 & shotAngle <= 60 ~ 2,  # center
    shotAngle < -60 ~ 3   # far right
  )) 

table("Shot Sector" = test$shotSector,
      "Shot Type" = test$shotType)

## Heatmap of shotType & shotSector
nhl_shots |> 
  filter(!is.na(shotType)) |> 
  mutate(shotSector = cut(
    shotAngle,
    breaks = seq(-90, 90, by = 30), # 6 bins
    include.lowest = TRUE,
    labels = c("Far Right", "Right Wing", "Right-Center", "Left-Center", "Left Wing", "Far Left")
  )) |> 
  group_by(shotSector, shotType) |> 
  summarize(freq = n(),
            joint = n()/nrow(nhl_shots)) |> 
  ggplot(aes(x = shotSector, y = shotType)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
  scale_fill_gradient2()
  

## Heatmap of Event & shotSector
nhl_shots |> 
  # filter(!is.na(shotType)) |> 
  mutate(shotSector = cut(
    shotAngle,
    breaks = seq(-90, 90, by = 30), # 6 bins
    include.lowest = TRUE,
    labels = c("Far Right", "Right Wing", "Right-Center", "Left-Center", "Left Wing", "Far Left")
  )) |> 
  group_by(shotSector, event) |> 
  summarize(freq = n(),
            joint = n()/nrow(nhl_shots)) |> 
  ggplot(aes(x = shotSector, y = event)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
  scale_fill_gradient2()
  

## Heatmap of Event & shooterLeftRight
nhl_shots |> 
  filter(!is.na(shooterLeftRight)) |> 
  group_by(shooterLeftRight, event) |> 
  summarize(freq = n(),
            joint = n()/nrow(nhl_shots)) |> 
  ggplot(aes(x = shooterLeftRight, y = event)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
  scale_fill_gradient2()

# Question 2
# Does handedness affect shooting effectiveness from different angles?





summary(test$arenaAdjustedYCord)