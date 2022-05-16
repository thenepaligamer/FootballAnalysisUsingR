library(StatsBombR)
library(tidyverse)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("ggsoccer")
library(ggsoccer)
#install.packages("RColorBrewer")
library(RColorBrewer)
remotes::install_github("torvaney/ggsoccer")

# Load competitions
competition <- FreeCompetitions() %>% 
  filter(competition_id == 37, season_name == "2020/2021")

# Load matches
matches <- FreeMatches(competition)

# Pull data for all matches and clean
data <- StatsBombFreeEvents(MatchesDF = matches, Parallel = T) %>% 
  allclean()

data
