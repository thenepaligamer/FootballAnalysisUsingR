library(tidyverse)
library(worldfootballR)

# https://fbref.com/en/comps/9/Premier-League-Stats#stats_squads_defense_for
prem_2022_defense <- get_season_team_stats(
  country = "ENG", gender = "M", season_end_year = "2022", tier = "1st",
  stat_type = "defense"
)
# dplyr::glimpse(prem_2022_defense)

prem_2022_defense %>%
  filter(Team_or_Opponent == "team") %>%
  ggplot(aes(x = Sh_Blocks, y = ShSv_Blocks)) +
  geom_point(size = 4, colour = "midnightblue", fill = "midnightblue", alpha = 0.2, shape = 21) +
  ggrepel::geom_text_repel(aes(label = Squad))
