library(openxlsx)
library(tidyverse)

#devtools::install_github("JaseZiv/worldfootballR", ref = "main")
library(worldfootballR)

options(ggrepel.max.overlaps = Inf)

prem_2021_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "shooting" )

prem_2021_shooting %>% 
  filter(Team_or_Opponent == "team") %>% 
  mutate(non_P_Goals = Gls_Standard - PK_Standard) %>% 
  ggplot(aes(x = npxG_Expected, y = non_P_Goals)) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
  geom_point(size = 5, colour = "midnightblue", fill="midnightblue", alpha=0.4, shape = 21) + 
  ggrepel::geom_text_repel(aes(label = Squad), color = "midnightblue", size=5) + 
  scale_x_continuous(limits = c(10,100), name = "Non-Penalty xG") + 
  scale_y_continuous(limits = c(10,100), name = "Non-Penalty Goals") +
  ggtitle("Did teams score as expected?",
          subtitle = "Team above line exceeded their xG while teams below didn't") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 25, face = "bold"), plot.subtitle = element_text(size = 17, colour = "grey30"),
        plot.title.position = "plot", plot.caption.position = "plot",
        axis.title = element_text(size = 16), axis.text = element_text(size = 14)
        )
