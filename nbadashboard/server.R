

# ----------------- Dependencies ----------------
library(shiny)
library(ggplot2)
library(dplyr)
library(conflicted)

# -------------- remove conflict ----------------

conflict_prefer("arrange", "dplyr")


# ------------------ Server -----------------

server <- function(input, output) {
    
    # ---------------- Leaderboard --------------
    
    stats <- reactive({
        input$leaderboard_statsInput
    })
    
    output$leaderboard_table <- renderTable({
        print(input$leaderboard_seasonInput)
        print(input$leaderboard_statsInput)
        
        d_season_combined %>% select(Player, Tm, !!!stats()) %>% 
            arrange(desc(!!!stats())) %>% 
            top_n(100)
        
    }, include.rownames=TRUE)
    
    output$leaderboard_plot <- renderPlot({
        
    })
    
    stats_names <- d_season_combined %>% select(-c(Player, Pos, Age, Tm, Season))
    
    
    # ------------ Player evolution ------------------
    
    
    d_players_name <- d_season_combined %>% select(Player) %>% 
        distinct(Player, .keep_all = TRUE) %>%
        arrange(Player)
    
    player <- reactive({
        input$search_player
    })
    
    output$stats_table_regular <- renderTable({
        d_regular_raw %>% 
            filter(Player==player()) %>% 
            select(-c(Player, Age, Pos)) %>% 
            arrange(desc(Season)) %>% 
            select(Season, everything())
    })
    
    output$stats_table_advanced <- renderTable({
        d_advanced_raw %>% 
            filter(Player==player()) %>% 
            select(-c(Player, Age, Pos)) %>% 
            arrange(desc(Season)) %>% 
            select(Season, everything())
    })
    
    
    output$player_team <- renderText({
        team=(d_season_combined %>% filter(Player==player()) %>% select(Tm))[1,]
        paste("Team : ", team )
    })
    
    output$player_age <- renderText({
        age=(d_season_combined %>% filter(Player==player()) %>% select(Age))[1,]
        paste("Age : ", age )
    })
    
    output$player_pos <- renderText({
        position=(d_season_combined %>% filter(Player==player()) %>% select(Pos))[1,]
        paste("Position : ", position )
    })
    
    output$evolution_plot <- renderPlot({
        
    })
    
    
    # ------------- Comparing players ----------------
    
    
}
