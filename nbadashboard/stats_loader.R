


#---------------------------- Dependencies -------------------------------------------

library(rvest)
library(dplyr)
library(plyr)
library(parallel)
library(janitor)
library(jsonlite)
library(gapminder)
library(stringr)

# ------------------------------------ Useful Links -------------------------------
# updateSelectInput:
# https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices


# ------------------------- remove conflict ------------------------------------------

conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")

# ----------------------------- Player stats by season -----------------------------------

# create links to website
seasons = 2018:2019
urls_regular_stats <-
  paste0("https://www.basketball-reference.com/leagues/NBA_",
         seasons,
         "_per_game.html")
urls_advanced_stats <-
  paste0("https://www.basketball-reference.com/leagues/NBA_",
         seasons,
         "_advanced.html")


# function to call to get stats from website
getStats <-  function(url, season) {
  mat = url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table() %>%
    bind_rows() %>%
    remove_empty(which = c("rows", "cols")) %>%
    select(-c(Rk)) %>%
    distinct(Player, .keep_all = TRUE) %>%
    filter(!Player == "Player") %>%
    mutate_all(na_if, "") %>%
    rename(Team = Tm)
  
  iteration = parent.frame()$i[]
  
  season = paste0((seasons[iteration] - 1), "-", seasons[iteration])
  
  mat = mat %>%
    mutate(Season = as.factor(season))
}

# Check if number only
f_numbers_only <- function(x)
  ! grepl("\\D", x)



# getting stats from website
d_regular_raw <-
  mclapply(urls_regular_stats, getStats, seasons) %>%
  bind_rows() %>%
  dplyr::rename(MPG = MP)

d_advanced_raw <-
  mclapply(urls_advanced_stats, getStats,  seasons) %>% bind_rows()


# merging regular and advanced stats
d_season_combined <-
  left_join(d_regular_raw,
            d_advanced_raw,
            by = c("Player", "Pos", "Age", "Team", "G",  "Season"))

# converting types
d_season_combined$Pos <- as.factor(d_season_combined$Pos)
d_season_combined$Team <- as.factor(d_season_combined$Team)
d_season_combined$Season <- as.factor(d_season_combined$Season)
d_season_combined$Player <- as.factor(d_season_combined$Player)

d_season_combined <- d_season_combined %>%
  mutate_if(is.character, as.numeric) 

d_season_combined$Player <- as.character(d_season_combined$Player)

# write.csv(d_season_combined, "season_combined.csv")

# ----------------------------- Player All-Time stats -----------------------------------




# ------------------------------ Player salaries ----------------------------------------



# ---------------------------------- League average----------------------------

d_league_average <- d_season_combined %>%
  select(-c(Player, Team, Pos, Season)) %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(mean, na.rm = TRUE)

# ----------------------------- residual around league average mean ---------------------
#
#  d_residuals <- d_season_combined %>%
#    summarise_if()


# -------------------
