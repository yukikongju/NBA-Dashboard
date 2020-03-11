






# ----------------- Dependencies ----------------
library(shiny)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(plotly)
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
    
    season <- reactive({
        input$leaderboard_seasonInput
    })
    
    output$leaderboard_table <- renderTable({
        print(input$leaderboard_seasonInput)
        print(input$leaderboard_statsInput)
        
        d_season_combined %>%
            select(Player, Team,!!!stats()) %>%
            # filter(Season==!!season()) %>%
            arrange(desc(!!!stats())) %>%
            top_n(100)
        
    }, include.rownames = TRUE)
    
    output$leaderboard_plot <- renderPlot({
        
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
        team = (d_season_combined %>% filter(Player == player()) %>% select(Team))[1, ]
        paste("Team : ", team)
    })
    
    output$player_age <- renderText({
        age = (d_season_combined %>% filter(Player == player()) %>% select(Age))[1, ]
        paste("Age : ", age)
    })
    
    output$player_pos <- renderText({
        position = (d_season_combined %>% filter(Player == player()) %>% select(Pos))[1, ]
        paste("Position : ", position)
    })
    
    output$evolution_plot <- renderPlot({
        
    })
    
    
    # ------------- Comparing players ----------------
    
    # ---------------- Screener ---------------
    
    screenerInput <- reactive({
        switch(input$select_dataset,
               "players" = d_season_combined)
    })
    
    screenerColumnNames <- reactive({
        datasetInput() %>%
            select(-Player) %>%
            names()
    })
    
    screener_season_level <- reactive({
        levels(screenerInput()$Season)
    })
    
    output$screener_season <- renderUI({
        selectInput("screener_season", " 2. Select a season",
                    choices = screener_season_level())
        
    })
    
    output$screener_var1 <- renderUI({
        selectInput("screener_v1", " Select a variable", choices = screenerColumnNames())
    })
    
    output$screener_var2 <- renderUI({
        selectInput("screener_v2", " Select a variable", choices = screenerColumnNames())
    })
    
    output$screener_var3 <- renderUI({
        selectInput("screener_v3", " Select a variable", choices = screenerColumnNames())
    })
    
    output$screener_var4 <- renderUI({
        selectInput("screener_v4", " Select a variable", choices = screenerColumnNames())
    })
    
    
    output$screener_table <- renderDataTable({
        d_season_combined %>% filter(Season == "2018-2019")
    })
    
    # ------------- View Dataset ---------------------
    
    datasetInput <- reactive({
        switch(input$select_dataset,
               "players" = d_season_combined)
    })
    
    numeric_columns <- reactive({
        datasetInput() %>%
            select(-c(Player, Season, Team, Pos)) %>%
            names()
    })
    
    output$dataset_columns_x <- renderUI({
        selectInput("dataset_variable_x", "3. Select a variable in x", choices = numeric_columns())
    })
    
    output$dataset_columns_y <- renderUI({
        selectInput("dataset_variable_y", "4. Select a variable in y", choices = numeric_columns())
    })
    
    season_level <- reactive({
        levels(datasetInput()$Season)
    })
    
    output$dataset_seasons <- renderUI({
        selectInput("dataset_season", "2. Select a season", choices = season_level())
    })
    
    binsInput <- reactive({
        input$dataset_bins_slider
    })
    
    variableInputX <- reactive({
        input$dataset_variable_x
        
    })
    
    variableInputY <- reactive({
        input$dataset_variable_y
        
    })
    
    seasonInput <- reactive({
        input$dataset_season
    })
    
    output$dataset_histogram <- renderPlot({
       datasetInput() %>%
             filter(Season==seasonInput()) %>%
             ggplot(aes(x = get(variableInputX()))) +
             geom_histogram(bins = binsInput(), fill="#69b3a2", color="#e9ecef", alpha=0.8) +
             xlab(toString(variableInputX())) +
            ggtitle(paste0("Histogram of ", variableInputX(), " in ", seasonInput())) 
    })
    
    output$dataset_scatterplot <- renderPlotly({
       scat <-  datasetInput() %>% 
            filter(Season==seasonInput()) %>% 
            ggplot(aes(text=Player, get(variableInputX()), get(variableInputY())))+
            geom_point( alpha=0.6, color="#69b3a2") +
            xlab(variableInputX())+
            ylab(variableInputY())+
           ggtitle(paste0("The relationship between ",variableInputX(), " and ", variableInputY(), " in ", seasonInput()))
       ggplotly(scat)
    })
   
    
    output$dataset_summary <- renderTable({
        # ds <- datasetInput() %>%
        #     select(-c(Player, Team, Season, Pos))
        # summary(ds)
        
        # x<-summary(datasetInput()[,as.numeric(input$variableInput)])
        
        
    })
    
    output$dataset_rawdata <- renderDataTable({
        datasetInput()
    })
}
