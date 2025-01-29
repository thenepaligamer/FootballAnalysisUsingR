# Load packages
pacman::p_load(tidyverse, worldfootballR, dplyr, stringr)

matches <- worldfootballR::fb_match_urls(
  country = "AUT",
  gender = "M",
  season_end_year = 2024,
  tier = "1st"
) |>
  as_tibble() |>
  filter(str_detect(value, "Linz"))

mins <- worldfootballR::fb_match_lineups(match_url = matches$value)

team_name <- "Blau-Weiß Linz"

filtered_mins <- mins |>
  select(Player_Name, Min, Team, Matchday, MatchURL) |>
  filter(Team == team_name)

filtered_mins$Matchday <- as.character(filtered_mins$Matchday)

players <- unique(filtered_mins$Player_Name)
match_dates <- unique(filtered_mins$Matchday)

dat <- crossing(players, match_dates) |>
  rename("Player_Name" = players, "Matchday" = match_dates)

dat_join <- full_join(filtered_mins, dat, by = c("Player_Name", "Matchday"))

dat_join$Min[is.na(dat_join$Min)] <- 0

match_dates <-
  tibble(Matchday = unique(dat_join$Matchday)) |>
  rownames_to_column(var = "match")

data_combo <- left_join(dat_join, match_dates, by = "Matchday") |>
  arrange(match) |>
  mutate(match = as.numeric(match))

summary <- data_combo |>
  group_by(Player_Name) |>
  summarise(total_mins = sum(Min, na.rm = TRUE))

data_combo <- left_join(data_combo, summary, by = "Player_Name")

p <- data_combo |>
  ggplot(aes(
    x = match,
    y = Player_Name,
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
    subtitle = "2023-2024 Austrian Bundesliga ",
    x = "Matchday",
    y = "",
    caption = "Viz: @thenepaligamer\nData: Fbref.com"
  ) +
  scale_x_continuous(
    expand = c(0, 1),
    breaks = seq(1, max(data_combo$match), by = 1)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    panel.grid = element_blank()
  )