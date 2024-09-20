library(tidyverse)
library(maps)
library(ggplot2)


soccer_data <- read.csv("matches_1930_2022.csv") %>%
  select(home_team, away_team, home_score, away_score)

home_data <- soccer_data %>%
  select(Team = home_team, Score = home_score)

away_data <- soccer_data %>%
  select(Team = away_team, Score = home_score)

combined_data <- rbind(home_data, away_data) %>%
  mutate(Team = case_when(
    Team == "West Germany" ~ "Germany",
    Team == "China PR" ~ "China",
    Team == "United States" ~ "USA",
    Team == "IR Iran" ~ "Iran",
    Team == "England" ~ "UK",
    Team == "Türkiye" ~ "Turkey",
    Team == "West Germany" ~ "Germany",
    TRUE ~ Team
  )) %>%
  group_by(Team) %>%
  mutate(total_goals = sum(Score)) 

# We separated home and away teams and them added them back together so that we have the total goals scored by each team when they were home and when they were away. We then grouped by team and added up the goals of each game to get total goals. We also changed names of Countries to match names in maps data.

maps.data <- map_data("world")

maps_combined_data <- full_join(maps.data, fixed_data, by = c("region" = "Team")) 
goal_plot <- ggplot(maps_combined_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = total_goals), color = "gray40", linewidth = .1) +
  scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "gray40") +
  theme_void() +
  labs(title = "Total Goals Scored by Each Country in the World Cup (1930-2022)", caption = "Source: Petro via Kaggle", fill = "Goal Count") +
  theme(plot.title = element_text(hjust=0.5, size=10, face="bold"))

wins <- read_csv("world_cup.csv", show_col_types = FALSE) 

#Reading in world_cup data.

wins <- wins %>%
  select(Champion) %>%
  mutate(Champion = case_when(
    Champion == "China PR" ~ "China",
    Champion == "United States" ~ "USA",
    Champion == "Germany DR" ~ "Germany",
    Champion == "IR Iran" ~ "Iran",
    Champion == "IR Iran" ~ "Iran",
    Champion == "England" ~ "UK",
    Champion == "Türkiye" ~ "Turkey",
    Champion == "West Germany" ~ "Germany",
    TRUE ~ Champion
  )) 

# Changing names to match maps.data

wins <- wins %>%  
  group_by(Champion) %>%
  mutate(Num_wins = n())

world_cup_combined <- full_join(wins, maps.data, by = c("Champion" = "region"))
win_plot <- ggplot(world_cup_combined, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Num_wins), color = "gray40", linewidth = .1) +
  scale_fill_continuous(na.value = "white", name = "Total Wins", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Total World Cup Wins by Country", caption = "Source: Petro via Kaggle") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5, size=15))

ggsave("win_plot.pdf", plot = win_plot, width=4.5, height=4.5)
ggsave("goal_plot.pdf", plot = goal_plot, width=4.5, height=4.5)

