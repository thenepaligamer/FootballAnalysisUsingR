library(tidyverse)
library(worldfootballR)

# Extracting squad shooting data
prem_2022_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "shooting" )

# Plotting data
prem_2022_shooting %>% 
  # filtering only team shooting data not opponent
  filter(Team_or_Opponent == "team") %>% 
  # create a new column to add a variable that removes penalties from team's goal total
  mutate(non_Pen_Goals = Gls_Standard - PK_Standard) %>% 
  # Plotting
  ggplot(aes(x = npxG_Expected, y = non_Pen_Goals)) + 
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype=2) + 
  geom_point(size = 6, colour = "midnightblue", fill = "midnightblue", alpha=0.4, shape=21) + 
  ggrepel::geom_text_repel(aes(label = Squad), colour = "midnightblue", size = 4) + 
  # Limiting x an y axis
  scale_x_continuous(limits = c(10,100), name = "Non Penalty xG") + 
  scale_y_continuous(limits = c(10,100), name = "Non Penalty Goals") + 
  # Adding title and subtitle
  ggtitle("Do Teams score as expected?",
          subtitle = "Teams above dashed line exceeded their xG, while team below don't ") +
  # applying pre-programmed general theme
  theme_minimal()+
  theme(plot.title = element_text(size=23, face="bold"), plot.subtitle = element_text(size=17, colour="grey30"),
        # and change where the plot is aligned - in this case it's left-aligned
        plot.title.position = "plot", plot.caption.position = "plot",
        # change the size of axis titles and text
        axis.title = element_text(size=15), axis.text = element_text(size = 13))
