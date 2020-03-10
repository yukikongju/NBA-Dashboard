



# ----------------- Dependencies ----------------
library(shiny)
library(ggplot2)
library(dplyr)
library(conflicted)

# -------------- remove conflict ----------------

conflict_prefer("arrange", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")

# --------------- Abreviations ------------------






# ------------------ Server -----------------

server <- function(input, output, session) {
    # ---------------- Leaderboard --------------
    
    stats <- reactive({
        input$leaderboard_statsInput
    })
    
    output$leaderboard_table <- renderTable({
        print(input$leaderboard_seasonInput)
        print(input$leaderboard_statsInput)
        
        d_season_combined %>%
            select(Player, Team, !!!stats()) %>%
            arrange(desc(!!!stats())) %>%
            top_n(100)
        
    }, include.rownames = TRUE)
    
    output$leaderboard_plot <- renderPlot({
        # mean=d_season_combined %>%
        #     select(-c(Player, Team, Pos, Season)) %>%
        #     mutate_if(is.character, as.numeric) %>%
        #     select(stats()) %>% summarise_all(mean)
        # #
        # mean.lm=lm(stats()~mean)
        
        print(mean)
        
        
    })
    
    output$leaderboard_summary <- renderPrint({
        d_season_combined %>% select(!!!stats()) %>% as.factor() %>% summary()
    })
    
    
    
    stats_names <-
        d_season_combined %>% select(-c(Player, Pos, Age, Team, Season))
    
    # season_levels <- d_season_combined %>% select(Season)
    
    # ------------ Player evolution ------------------
    
    
    d_players_name <- d_season_combined %>% select(Player) %>%
        distinct(Player, .keep_all = TRUE) %>%
        arrange(Player)
    
    player <- reactive({
        input$search_player
    })
    
    output$stats_table_regular <- renderTable({
        d_regular_raw %>%
            filter(Player == player()) %>%
            select(-c(Player, Age, Pos)) %>%
            arrange(desc(Season)) %>%
            select(Season, everything())
    })
    
    output$stats_table_advanced <- renderTable({
        d_advanced_raw %>%
            filter(Player == player()) %>%
            select(-c(Player, Age, Pos)) %>%
            arrange(desc(Season)) %>%
            select(Season, everything())
    })
    
    
    output$player_team <- renderText({
        team = (d_season_combined %>% filter(Player == player()) %>% select(Team))[1,]
        paste("Team : ", team)
    })
    
    output$player_age <- renderText({
        age = (d_season_combined %>% filter(Player == player()) %>% select(Age))[1,]
        paste("Age : ", age)
    })
    
    output$player_pos <- renderText({
        position = (d_season_combined %>% filter(Player == player()) %>% select(Pos))[1,]
        paste("Position : ", position)
    })
    
    output$evolution_plot <- renderPlot({
        
    })
    
    
    # ------------- Comparing players ----------------
    
    # ---------------- Player Screener ---------------
    
    # ------------- View Dataset ---------------------
    
    datasetInput <- reactive({
        switch(input$select_dataset,
               "players"=d_season_combined)
    })
    
    column_names <- reactive({
        names(datasetInput())
    })
    
    output$dataset_columns <- renderUI({
        selectInput("variables", "2. Select a variable", choices = column_names())
    })
    
    season_level <- reactive({
        
    })
    
    output$dataset_seasons <- renderUI({
        selectInput("variables", "3. Select a season", choices = season_level())
    })
    

    bins <- reactive({
        input$dataset_bins_slider
    })
    
    
    
    
    output$dataset_histogram <- renderPlot({
        datasetInput() %>% ggplot()+geom_bar(aes(Pos, fill=Pos))
    })
    
}
