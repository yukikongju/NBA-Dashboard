





#---------------------------- Dependencies -------------------------------------------

library(rvest)
library(dplyr)
library(plyr)
library(parallel)
library(janitor)
library(jsonlite)


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
    mutate_all(na_if, "")
  
  iteration = parent.frame()$i[]
  
  season = paste0((seasons[iteration] - 1), "-", seasons[iteration])
  
  mat = mat %>%
    mutate(Season = as.factor(season))
}

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
            by = c("Player", "Pos", "Age", "Tm", "G",  "Season"))

write.csv(d_season_combined, "season_combined.csv")

# ----------------------------- Player All-Time stats -----------------------------------




# ------------------------------ Player salaries ----------------------------------------



# ---------------------------------- League average----------------------------

d_league_average <- d_season_combined %>%
  select(-c(Player, Tm, Pos)) %>%
  mutate_if(is.character, as.numeric) %>%
  summarise_all(list(median = median, mean = mean), is.na =
                  TRUE)

# ----------------------------- residual around league average mean ---------------------

# d_residuals <- d_season_combined %>%
#   revalue()
