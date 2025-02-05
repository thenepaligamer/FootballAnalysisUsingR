# Load packages
pacman::p_load(tidyverse, worldfootballR, dplyr, stringr, rcartocolor, sysfonts, showtext)

# Set font for plot
font_add_google(name = "IBM Plex Sans", family = "IBM")
showtext_auto()
showtext_opts(dpi = 300)

font <- "IBM"

# Extract match data from the league
matches <- fb_match_urls(
  country = "AUT",
  gender = "M",
  season_end_year = 2024,
  tier = "1st"
) |>
  as_tibble() |>
  filter(str_detect(value, "Linz"))

# Extract player minutes
mins <- fb_match_lineups(match_url = matches$value)

# Select team name
team_name <- "Blau-Weiß Linz"

# Filter minutes for the team above
filtered_mins <- mins |>
  select(Player_Name, Min, Team, Matchday, MatchURL) |>
  filter(Team == team_name)

filtered_mins$Matchday <- as.character(filtered_mins$Matchday)

players <- unique(filtered_mins$Player_Name)
match_dates <- unique(filtered_mins$Matchday)

# Create all matchday possibilities for all squad players
dat <- crossing(players, match_dates) |>
  rename("Player_Name" = players, "Matchday" = match_dates)

dat_join <- full_join(filtered_mins, dat, by = c("Player_Name", "Matchday"))

# Replace NA values with 0
dat_join$Min[is.na(dat_join$Min)] <- 0

# Find match days and dates
match_dates <-
  tibble(Matchday = unique(dat_join$Matchday)) |>
  rownames_to_column(var = "match")

# Join data
data_combo <- left_join(dat_join, match_dates, by = "Matchday") |>
  arrange(match) |>
  mutate(match = as.numeric(match))

# Summarizes total played mins for every player
summary <- data_combo |>
  group_by(Player_Name) |>
  summarise(total_mins = sum(Min, na.rm = TRUE))

data_combo <- left_join(data_combo, summary, by = "Player_Name")

# For fancy tag for y axis
full_data <-
  data_combo |>
  mutate(
    player_mins = glue::glue(
      "<span style = 'font-size:10pt;'>{Player_Name}</span>
      <span style = 'color:#9b111e'>({total_mins})</span>"
    )
  )

# Plot
p <- full_data |>
  ggplot(aes(
    x = match,
    y = player_mins,
    fill = Min
  )) +
  geom_tile(aes(height = 0.9, width = 0.9)) +
  geom_text(aes(label = Min), size = 3) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      keywidth = 3,
      nrow = 1,
      title = "Minutes Played"
    )
  ) +
  labs(
    title = "Blau-Weiß Linz - Player Utilisation",
    subtitle = glue::glue(
      "2023/24 Austrian Bundesliga
      <span style = 'color:#9b111e'>Total Minutes Played</span> "
    ),
    x = "Matchday",
    y = "",
    caption = "Viz: @thenepaligamer\nData: Fbref.com"
  ) +
  scale_x_continuous(
    expand = c(0, 0.3),
    breaks = seq(1, max(data_combo$match), by = 1)
  ) +
  scale_fill_carto_c(palette = "TealGrn",
                     breaks = c(0, 15, 30, 45, 60, 75, 90)) +
  theme_minimal() +
  theme(
    text = element_text(family = font),
    plot.title = element_text(size = 26, hjust = 0.5),
    plot.subtitle = ggtext::element_textbox(size = 13, hjust = 0.5),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.y = ggtext::element_markdown()
  )

# Plot title for file
title <- team_name |> str_to_lower() |> str_replace(" ", "_")

# Save plot
ggsave(
  plot = p,
  filename = here::here("Season-Level-Data-Extraction",
                        "Teams",
                        glue::glue("{title}_squad_usage.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)
