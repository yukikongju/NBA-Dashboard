



# ----------------------------------Dependencies ----------------------------------------

library(shiny)
library(dplyr)


# ------------------- Import data ----------------------


d_season_combined <- d_season_combined

#  ------------------ conflicts ----------------


# ------------------- UI ----------------------------

ui <- navbarPage(
    title = "NBA Dashboard",
    
    # ----------- Leaderboard ---------------
    
    tabPanel("Leaderboard", fluidPage(
        titlePanel("Leaderboard"),
        wellPanel(fluidRow(
            column(
                4,
                selectInput(
                    "leaderboard_dataset_input",
                    choices = c(
                        "Players Stats" = "players",
                        "Teams Stats" = "teams",
                        "Salaries" = "salaries",
                        "Drafts" = "drafts"
                    ),
                    selected = "players",
                    label = "1. Choose a dataset"
                )
            ),
            column(4,
                   uiOutput("leaderboard_season_choices")),
            column(4,
                   uiOutput("leaderboard_stats_choices"))
        ),
        sliderInput("leaderboard_slider", min = 10, max=60, label = "Top :", value = 20)
       ),
       fluidRow(
           splitLayout(
              DT:: dataTableOutput("leaderboard_table"),
               plotOutput("leaderboard_plot")
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
                 fluidRow(
                     column(
                         6,
                         fluidRow(imageOutput("player_image")),
                         fluidRow(textOutput("player_team")),
                         fluidRow(textOutput("player_age")),
                         fluidRow(textOutput("player_pos"))
                         
                     ),
                     column(6, plotOutput("evolution_plot"))
                 ),
                 fluidRow(tabsetPanel(
                     type = "tabs",
                     tabPanel("Regular Stats", tableOutput("stats_table_regular")),
                     tabPanel("Advanced Metrics", tableOutput("stats_table_advanced"))
                 ))
                 
             )),
    
    # ---------------- Comparing Player ----------------------
    tabPanel("Comparing Player", ),
    
    # ---------------  Screener ----------------
    
    tabPanel(
        " Screener",
        fluidPage(
            titlePanel("Screener"),
            wellPanel(
                fluidRow(column(
                    6,
                    selectInput(
                        "screener_dataset_input",
                        label = "1. Select a dataset to view",
                        choices =  c(
                            "Players Stats" = "players",
                            "Teams Stats" = "teams",
                            "Draft" = "drafts",
                            "Salaries" = "salaries"
                        ),
                        selected = "players"
                    ),
                ),
                column(6,
                       uiOutput("screener_season"))),
                fluidRow(
                    column(3, uiOutput("screener_var1")),
                    column(3, numericInput(
                        "screener_value1", value = 0, label = ""
                    )),
                    column(3, uiOutput("screener_var3")),
                    column(3, numericInput(
                        "screener_value3", value = 0, label = ""
                    ))
                    
                ),
                
                fluidRow(
                    column(3, uiOutput("screener_var2")),
                    column(3, numericInput(
                        "screener_value2", value = 0, label = ""
                    )),
                    column(3, uiOutput("screener_var4")),
                    column(3, numericInput(
                        "screener_value4", value = 0, label = ""
                    ))
                )
            ),
            DT::dataTableOutput("screener_table")
        )
    ),
    # ----------------------------- View DataSet -------------------
    
    tabPanel("View Dataset",
             fluidPage(
                 titlePanel("View Dataset"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "select_dataset",
                             label = "1. Select a dataset to view",
                             choices =  c(
                                 "Players Stats" = "players",
                                 "Teams Stats" = "teams",
                                 "Salaries" = "salaries",
                                 "Draft" = "drafts"
                             ),
                             selected = "players"
                         ),
                         uiOutput("dataset_seasons"),
                         uiOutput("dataset_columns_x"),
                         uiOutput("dataset_columns_y"),
                         sliderInput(
                             "dataset_bins_slider",
                             label = "5. Select a bins number to make the histogram",
                             min = 5,
                             max = 25,
                             value = 15
                         ),
                         splitLayout(
                             radioButtons(
                                 "dataset_export_img",
                                 choices = c("png", "pdf"),
                                 selected = "png",
                                 label = "6. Export image in:"
                             ),
                             radioButtons(
                                 "dataset_export_table",
                                 choices = c("csv", "txt"),
                                 selected = "csv",
                                 label = "7. Export table in: "
                             )
                         )
                     ),
                     mainPanel(
                         tabsetPanel(
                             type = "tab",
                             tabPanel(
                                 "Histogram",
                                 plotOutput("dataset_histogram"),
                                 downloadButton("dataset_download_hist", label = "Download Histogram")
                             ),
                             tabPanel(
                                 "Scatter Plot",
                                 plotlyOutput("dataset_scatterplot"),
                                 verbatimTextOutput("dataset_scatterplot_summary"),
                                 downloadButton("dataset_download_scatplot", label = "Download Scatterplot")
                             ),
                             tabPanel(
                                 "Summary",
                                 tableOutput("dataset_summary"),
                                 downloadButton("dataset_download_summary", label = "Download Summary")
                             ),
                             tabPanel("Facets",),
                             tabPanel(
                                 "Raw Data",
                                 DT::dataTableOutput("dataset_rawdata"),
                                 downloadButton("dataset_download_dataset", label = "Download Dataset")
                             )
                         )
                     )
                 )
             ))
)
