# ----- B1705 Week 10 | Cluster Analysis: Practical | 20.03.2024 -----
rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/8sf5s4uloq8rgu0tbf45y/sco_prem_2024.csv?rlkey=qvoewxn2nd1f6clqbwo0aib7f&dl=1')

# For example....

selected_data <- scale(df[, c("home_team_win_ratio", "away_team_win_ratio", "home_team_points_ratio", "away_team_points_ratio")])




