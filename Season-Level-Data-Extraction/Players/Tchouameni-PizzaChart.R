# Load necessary packages
pacman::p_load(worldfootballR, tidyverse, forcats, glue, sysfonts)

# Set font for plot
font_add_google(name = "IBM Plex Sans", family = "IBM")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

# Extract data from FBRef for a particular player
footballer <- fb_player_scouting_report(
  "https://fbref.com/en/players/4f255115/Aurelien-Tchouameni", # nolint
  pos_versus = "primary")

# Select the attributes you want to plot in plot
footballer_selected <- footballer[
  c(83, 84, 87, 91, 94, 95, 96, 97, 133, 135, 234, 257, 258, 267, 302, 305), ] # nolint

# Categorize above attributes into major groups
footballer_selected <- footballer_selected |>
  mutate(stat = case_when(
    Statistic == "Tackles Won" |
      Statistic == "Tackles (Def 3rd)" |
      Statistic == "Dribblers Tackled" |
      Statistic == "Blocks" |
      Statistic == "Interceptions" |
      Statistic == "Tkl+Int" |
      Statistic == "Clearances" |
      Statistic == "Errors" ~ "Defensive Actions",
    Statistic == "Aerials Won" |
      Statistic == "% of Aerials Won" ~ "Aerial",
    Statistic == "Touches (Def Pen)" |
      Statistic == "Ball Recoveries" ~ "Positioning and Recoveries",
    Statistic == "Fouls Committed" |
      Statistic == "Second Yellow Card" ~ "Discipline",
    TRUE ~ "Passing"
  ))

# Creating circle for plot
temp <- (360 / (nrow(footballer_selected)) / 2)
my_ang <- seq(-temp, -360 + temp, length.out = nrow(footballer_selected))
ang <- ifelse(my_ang < -90, my_ang + 180, my_ang)
ang <- ifelse(ang < -90, ang + 180, ang)

# Replaces all space with new line character
footballer_selected$Statistic <- gsub(" ", "\n", footballer_selected$Statistic)

# Plotting the graph
p <- ggplot(
            footballer_selected,
            aes(fct_reorder(Statistic, stat),
                Percentile)) +
  geom_bar(
    aes(y = 100, fill = stat),
    stat = "identity",
    width = 1,
    colour = "white",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity", width = 1, aes(fill = stat), colour = "white") +
  coord_polar() +
  geom_label(
    aes(label = Per90, fill = stat),
    size = 2,
    color = "white",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Defensive Actions" = "#2A6F56",
    "Aerial" = "#74B3CE",
    "Positioning and Recoveries" = "#4F759B",
    "Discipline" = "#5D5179",
    "Passing" = "#571F4E"
  )) +
  scale_y_continuous(limits = c(-20, 100)) +
  labs(
    fill = "",
    caption = "Data from StatsBomb via FBref\nBy @thenepaligamer",
    title = glue("{footballer_selected$Player[1]} | Real Madrid"),
    subtitle = glue(
      "2024/2025 | Compared to Defenders in Top 5 competitions | Stats per 90"
    )
  ) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = ang),
    text = element_text(size = 15, family = font),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, b = 20)
  )

ggsave(
  plot = p,
  filename = here::here("Season-Level-Data-Extraction",
    "Players",
    glue("{footballer_selected$Player[1]}-comparisons.png")
  ),
  height = 8,
  width = 8,
  bg = "#f5f5f5"
)
