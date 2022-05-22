library(tidyverse)
library(worldfootballR)

xg_data <- get_match_results(country = "ENG", gender = "M", season_end_year = c(2018:2021))

end_season_summary <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = c(2018:2021),tier = "1st", stat_type = "league_table")

