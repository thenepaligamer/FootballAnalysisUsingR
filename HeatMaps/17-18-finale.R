pacman::p_load(ggplot2, tidyverse, extrafont, cowplot, StatsBombR, SBpitch, paletteer)

comps <- FreeCompetitions() %>%
  filter(competition_name == "Champions League" & season_name == "2017/2018")

comp_data <- FreeMatches(comps)

ucl19 <- StatsBombFreeEvents(MatchesDF = comp_data, Parallel = T)
ucl19 <- allclean(ucl19)

liverpool <- ucl19 %>%
  filter(team.name == "Liverpool", type.name == "Pass")
liverpool <- liverpool %>%
  filter(is.na(pass.outcome.name))
liverpool1 <- liverpool %>%
  select(
    period, minute, type.name, pass.length, pass.angle, player.name, pass.recipient.name, pass.outcome.name,
    pass.height.name, pass.body_part.name, location.x, location.y, pass.end_location.x, pass.end_location.y,
    carry.end_location.x, carry.end_location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z
  )

real_madrid <- ucl19 %>%
  filter(team.name == "Real Madrid", type.name == "Pass")
real_madrid <- real_madrid %>%
  filter(is.na(pass.outcome.name))
real_madrid1 <- real_madrid %>%
  select(
    period, minute, type.name, pass.length, pass.angle, player.name, pass.recipient.name, pass.outcome.name,
    pass.height.name, pass.body_part.name, location.x, location.y, pass.end_location.x, pass.end_location.y,
    carry.end_location.x, carry.end_location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z
  )

palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = 1)

p1 <- create_Pitch(grass_colour = "gray15", background_colour = "gray15", line_colour = "white") +
  geom_density_2d_filled(
    data = liverpool1, aes(x = pass.end_location.x, y = pass.end_location.y, fill = ..level.., ), alpha = .4,
    contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
  theme(
    legend.position = "none",
    plot.background = element_rect(colour = "gray15", fill = "gray15"),
    plot.title = element_text(color = "white", hjust = .5, size = 20, family = "Comic Sans MS", face = "bold", vjust = -3),
    plot.subtitle = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -4),
    plot.caption = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = 4)
  ) +
  labs(
    title = "Liverpool Avg. Pass Reception Heatmap vs. Real Madrid",
    subtitle = "UCL Final vs. Real Madrid - May 26th, 2018",
    caption = "@Thenepaligamer"
  )

ggdraw(p1) + theme(plot.background = element_rect(fill = "gray15", colour = NA))

p2 <- create_Pitch(grass_colour = "gray15", background_colour = "gray15", line_colour = "white") +
  geom_density_2d_filled(
    data = real_madrid1, aes(x = pass.end_location.x, y = pass.end_location.y, fill = ..level.., ), alpha = .4,
    contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
  theme(
    legend.position = "none",
    plot.background = element_rect(colour = "gray15", fill = "gray15"),
    plot.title = element_text(color = "white", hjust = .5, size = 20, family = "Comic Sans MS", face = "bold", vjust = -3),
    plot.subtitle = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -4),
    plot.caption = element_text(color = "white", hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = 4)
  ) +
  labs(
    title = "Real Madrid Avg. Pass Reception Heatmap vs. Liverpool",
    subtitle = "UCL Final vs. Liverpool - May 26th, 2018",
    caption = "@Thenepaligamer"
  )

ggdraw(p2) + theme(plot.background = element_rect(fill = "gray15", colour = NA))
