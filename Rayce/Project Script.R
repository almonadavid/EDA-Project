library(tidyverse)
nhl_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nhl_shots.csv")

# need to find a better way to do total column
hometeamgoals <- nhl_shots |> 
  filter(team == "HOME", event == "GOAL") |> 
  mutate(total = n())

  
  

 
  