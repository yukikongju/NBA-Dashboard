
# ----------------------------------Dependencies ----------------------------------------

library(shiny)
library(dplyr)


# ------------------- Import data ----------------------


d_season_combined <- d_season_combined

# ------------------- UI ----------------------------

ui <- navbarPage(
    title = "NBA Dashboard",
    
    # ----------- Leaderboard ---------------
    
    tabPanel("Leaderboard",
             fluidPage(
                 titlePanel("Top 100"),
                 sidebarLayout(
                     sidebarPanel(tableOutput("leaderboard_table")),
                     mainPanel(fluidRow(
                         column(3,
                                selectInput(
                                    "leaderboard_seasonInput", "Season", c(levels(as.factor(
                                        d_season_combined$Season
                                    )))
                                )),
                         column(
                             3,
                             varSelectInput(
                                 "leaderboard_statsInput",
                                 "Stats",
                                 d_season_combined %>% select(-c(Player, Pos, Age, Tm, Season))
                             )
                         )
                     ),
                     fluidRow(plotOutput("leaderboard_plot")),
                     )
                 )
             )),
    
    # ------------ Player evolution ------------------
    
    tabPanel("Player Evolution",
             fluidPage(
                 fixedRow(column(
                     12,
                     selectizeInput("search_player", choices = d_players_name, label = "Search player")
                 )),
                 fluidRow(column(
                     6,
                     fluidRow(imageOutput("player_image")),
                     fluidRow(textOutput("player_team")),
                     fluidRow(textOutput("player_age")),
                     fluidRow(textOutput("player_pos"))
                     
                 ),
                 column(6, plotOutput("evolution_plot"))),
                 fluidRow(
                     tabsetPanel(type="tabs",
                        tabPanel("Regular Stats",tableOutput("stats_table_regular")),
                        tabPanel("Advanced Metrics",tableOutput("stats_table_advanced"))
                        )
                 )
                 
             )),
    
    # ---------------- Comparing Player ----------------------
    tabPanel("Comparing Player",)
)
