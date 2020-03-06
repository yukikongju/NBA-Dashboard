

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
        
        
    })
    
    
    
    output$leaderboard_plot <- renderPlot({
        
    })
    
    
    
    
    # ------------ Player evolution ------------------
    
    
    d_players_name <- d_season_combined %>% select(Player) %>% 
        distinct(Player, .keep_all = TRUE) %>%
        arrange(Player)
    
    player <- reactive({
        input$search_player
    })
    
    output$stats_table <- renderTable({
        d_season_combined %>% 
            filter(Player==player()) %>% 
            select(-c(Player, Age, Pos)) %>% 
            arrange(desc(Season)) 
    })
    
    output$player_team <- renderText({
        team=(d_season_combined %>% filter(Player==player()) %>% select(Tm))[1,]
        paste("Team : ", team )
    })
    
    output$evolution_plot <- renderPlot({
        
    })
    
    
    # ------------- Comparing players ----------------
    
    
}
