# remotes::install_github('ewenme/understatr')

library(tidyverse)
library(understatr)
library(here)
library(ggrepel)
library(ggsoccer)
library(glue)

leagues <- get_leagues_meta()

unique(leagues$league_name)

leagues <- leagues %>% 
  filter(!league_name == "RFPL")

team_data <- map_dfr(unique(leagues$league_name), get_league_teams_stats, year = 2021)

player_data <- map_dfr(unique(team_data$team_name), get_team_players_stats, year = 2021)

summary <- player_data %>% 
  mutate( nineties = time / 90,
          npxg_p90 = npxG / nineties,
          xa_p90 = xA / nineties) %>% 
  filter(time>1800)

summary %>% 
  filter(time > 1800) %>% 
  ggplot(aes(x = npxg_p90, y = xa_p90)) + 
  geom_point() + 
  geom_text_repel(data = summary %>% 
                    filter(npxg_p90 > 0.4 | xa_p90>0.4),
                  aes(label= player_name)) +
  labs(title = "Attacking Contributions",
       subtitle = "European Big 5 Leagues >900 mins",
       x = "Non-Penalty xG per 90",
       y = "xA per 90")

## Player Shot Map
players <- c(player_data$player_id)

shot_data <- players %>% 
  map_dfr(.,possibly(get_player_shots, otherwise = NULL))

shots_data_2021 <- shot_data %>% 
  filter(year == 2021)

player_name <- "Mohamed Salah"

shots_data_2021 %>% 
  filter(player == {player_name},
         !situation == "Penalty") %>% 
  mutate(X = X*100,
         Y = Y*100) %>% 
  ggplot(aes(x=X, y = 100-Y)) + 
  annotate_pitch() + 
  geom_point(aes(color = result, size = xG)) + 
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) + 
  theme_pitch() + 
  labs(title = glue({player_name}),
       subtitle = "non-Penalty Shot locations 2021/22")
