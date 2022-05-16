competition <- FreeCompetitions() %>% 
  filter(competition_name == "Champions League" & season_name=="2017/2018")

competition_data <- FreeMatches(competition)

single_match_data <- StatsBombFreeEvents(MatchesDF = competition_data, Parallel = T)

single_match_data <- allclean(single_match_data)

unique(single_match_data$type.name)
single_team <- single_match_data %>% 
  filter(team.name == "Real Madrid", type.name == "Shot")

single_team_shots <- single_team %>% 
  select(period, minute, type.name, pass.length, pass.angle, player.name, location.x, location.y, shot.statsbomb_xg, shot.technique.name, shot.body_part.name, shot.type.name,
         shot.outcome.name, shot.end_location.x, shot.end_location.y, shot.end_location.z)

# -------- Create goal outcome column
single_team_shots <- single_team_shots %>% 
  mutate(
    goal = case_when(
      shot.outcome.name == "Goal" ~ "True",
      shot.outcome.name != "Goal" ~ "False",
    )
  )
# -------- filter out an penalty shootout shot (period 5 refers to shootouts)
single_team_shots <- single_team_shots %>%
  filter(period != 5)

# Shot map plot
# --------------------------------- plotting and theme
# create pitch and theme
p1 <- create_Pitch(grass_colour = "#224C56", background_colour = "#224C56", line_colour = "#B3CED9") +
  # plot a point for each shot by x and y location ()
  geom_point(single_team_shots, mapping = aes(x = location.x, y = location.y, fill = goal, size = shot.statsbomb_xg), 
             color = "gray60", pch=21) +
  # set the scale for the size and labels of the expected goal for each shot
  scale_size_continuous(limits=c(0,1), breaks = c(.25,.5,.75, 1), labels=c(".25", ".5", ".75", "1")) + 
  # set the color and legend label manually for the fill value (goal or no goal)
  scale_fill_manual(breaks=c("True", "False"), values = c("green3", "gray15"), labels=c("Goal", "No Goal")) +
  # set the limits for the x-axis
  scale_x_continuous(limits = c(0, 120)) +
  # set the limits for the y-axis
  scale_y_continuous(limits = c(0, 80)) +
  # theme elements for the plot
  theme(
    plot.background = element_rect(colour = "#224C56", fill = "#224C56"),
    plot.title = element_text(color = "white", hjust = .5, size = 20, family = "Comic Sans MS", face = "bold", vjust = -1),
    plot.subtitle = element_text(color = "lightgrey", hjust = .5, size = 8, family = "Comic Sans MS", face = "bold", vjust = -2),
    plot.caption = element_text(color = "white", hjust = .5, size = 6, family = "Comic Sans MS", face = "bold", vjust = 4),
    legend.position = c(.5,.2),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill = "gray20", colour = "transparent"),
    legend.title = element_text(hjust = .4, vjust = .5, size = 10, family = "Comic Sans MS", face = "bold", colour = "white"),
    legend.text = element_text(hjust = .4, vjust = .5, size = 8, family = "Comic Sans MS", face = "bold", colour = "white"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.margin = margin(t = .1, b=.1, r=.1, l=.1, unit='cm')
  ) +
  # create the plot's title, subtitle, and legend titles
  labs(title = "Real Madrid - 2018 CL Final Shot Map",
       subtitle = "Includes all open-play and set piece shots // Data via: StatsBombR // @Thenepaligamer",
       fill = "Outcome",
       size = "xG") +
  # flip the horizontal pitch to a vertical layout
  coord_flip(xlim = c(60, 120), ylim = c(0,80)) +
  # order the legends, setting fill 1st and size 2nd
  guides(fill = guide_legend(order = 1))
p1 + draw_image(x= 110, y = 1, width = 7.5, height = 7.5) 
