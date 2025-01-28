library(worldfootballR)
library(tidyverse)
library(forcats)
library(glue)
library(extrafont)

nacho <- fb_player_scouting_report("https://fbref.com/en/players/2946f9a1/Nacho", pos_versus = "primary")

print(nacho$Statistic)

footballer_selected <- nacho[c(27, 29, 31, 43, 44, 47, 96, 87, 106, 102, 108, 110, 122, 126, 149), ]

footballer_selected

footballer_selected <- footballer_selected %>%
  mutate(stat = case_when(
    Statistic == "Passes Completed" |
      Statistic == "Pass Completion %" |
      Statistic == "Progressive Passing Distance" |
      Statistic == "Key Passes" |
      Statistic == "Passes into Final Third" |
      Statistic == "Touches" |
      Statistic == "Carries" |
      Statistic == "Carries into Final Third" |
      Statistic == "Dispossessed" |
      Statistic == "Penalty Kicks Conceded" |
      Statistic == "Progressive Passes" ~ "Possession",
    TRUE ~ "Defending"
  ))
footballer_selected

temp <- (360 / (nrow(footballer_selected)) / 2)
myAng <- seq(-temp, -360 + temp, length.out = nrow(footballer_selected))
ang <- ifelse(myAng < -90, myAng + 180, myAng)
ang <- ifelse(ang < -90, ang + 180, ang)

footballer_selected$Statistic <- gsub(" ", "\n", footballer_selected$Statistic)

ggplot(footballer_selected, aes(fct_reorder(Statistic, stat), Percentile)) +
  geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, colour = "white", alpha = 0.5) +
  geom_bar(stat = "identity", width = 1, aes(fill = stat), colour = "white") +
  coord_polar() +
  geom_label(aes(label = Per90, fill = stat), size = 2, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Possession" = "#D70232",
    "Defending" = "#FF9300"
  )) +
  scale_y_continuous(limits = c(-20, 100)) +
  labs(
    fill = "",
    caption = "Data from StatsBomb via FBref| By @thenepaligamer",
    title = glue("{footballer_selected$Player[1]} | Real Madrid FC"),
    subtitle = glue::glue("2021/2022 | Compared to defenders Top 5 competitions | Stats per 90")
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#130523", color = "#130523"),
    panel.background = element_rect(fill = "#130523", color = "#130523"),
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = ang, colour = "#ffffff"),
    text = element_text(family = "Inter", size = 15, color = "#ffffff"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 10, family = "Fira Code"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 2, 2, 2)
  )

# ggsave("nacho-pizzachart.png", width = 3000, height = 3000, units = "px")
