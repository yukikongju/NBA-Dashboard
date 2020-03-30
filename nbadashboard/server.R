

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

advancedStats <-
  d_advanced_raw %>% select(-c(Player, Pos, Age, Team, Season)) %>% names()
regularStats <-
  d_regular_raw %>% select(-c(Player, Pos, Age, Team, Season)) %>% names()

# ------------------ Server -----------------

server <- function(input, output, session) {
  # ---------------- Leaderboard --------------
  
  leaderboardDatasetInput <- reactive({
    switch(
      input$leaderboard_dataset_input,
      "players" = d_season_combined,
      "teams" = d_team_regular_raw,
      "drafts" = d_draft
    )
  })
  
  leaderboardCharColumn <- reactive({
    switch(
      input$leaderboard_dataset_input,
      "players" = 'Player',
      "teams" = 'Team',
      "draft" = 'Player'
    )
  })
  
  leaderboardSeasonChoices <- reactive({
    levels(leaderboardDatasetInput()$Season)
  })
  
  leaderboardStatsChoices <- reactive({
    leaderboardDatasetInput() %>%
      select_if(is.numeric) %>%
      colnames()
  })
  
  output$leaderboard_season_choices <- renderUI({
    selectInput("leaderboard_seasonInput",
                "2. Select a Season",
                choices = leaderboardSeasonChoices())
  })
  
  output$leaderboard_stats_choices <- renderUI({
    selectInput("leaderboard_statsInput",
                "3. Select a Stats",
                choices = leaderboardStatsChoices())
  })
  
  leaderboardStatsChosen <- reactive({
    input$leaderboard_statsInput
  })
  
  leaderboardSeasonChosen <- reactive({
    input$leaderboard_seasonInput
  })
  
  output$leaderboard_table <- DT::renderDataTable({
    leaderboardDatasetInput() %>%
      filter(Season == leaderboardSeasonChosen()) %>%
      select(leaderboardCharColumn(), Team, leaderboardStatsChosen()) %>%
      dplyr::arrange(desc(get(leaderboardStatsChosen()))) %>%
      top_n(60)
    
  })
  
 
  
  ### todo: player swtich to team
  output$leaderboard_summary <- renderPrint({
    
  })
  
  leaderboardResidualDataset <- reactive({
    switch (input$leaderboard_dataset_input,
            "players" = d_season_residuals)
  })
  
  output$leaderboard_residuals <- renderPlot({
    
  })
  
  p_leaderboard_hist <- reactive({
    # mean <- mean(leaderboardDatasetInput()$leaderboardStatsChosen())
    
   leaderboardDatasetInput() %>% 
      filter(Season==leaderboardSeasonChosen()) %>% 
      ggplot(aes(x = get(leaderboardStatsChosen()))) +
      geom_histogram(
        aes(y = ..density..),
        bins = binsInput(),
        fill = "#69b3a2",
        color = "#e9ecef",
        alpha = 0.8
      ) +
      xlab(toString(leaderboardStatsChosen())) +
      ggtitle(paste0("Histogram of ", leaderboardStatsChosen(), " in ", leaderboardSeasonChosen()))
    #       stat_function(fun = dnorm, args = list(mean = mean(leaderboardDatasetInput()$get(leaderboardStatsChosen())),
    #                                          sd = sd(leaderboardDatasetInput()$get(leaderboardStatsChosen()))))

    
    
  })
  
  output$leaderboard_distribution <- renderPlot({
    p_leaderboard_hist()
     })
  
  
  
  # ------------ Player Evolution ------------------
  
  evolutionDatasetChosen <- reactive({
    switch (input$evolution_dataset_choices,
            "players" = d_season_combined,
            "teams" = d_team_regular_raw)
  })
  
   evolutionSeasonChoices <- reactive({
    levels(evolutionDatasetChosen()$Season)
  })
  
  output$evolution_season_choices <- renderUI({
    selectInput("evolutionSeasonInput",
                choices = evolutionSeasonChoices(),
                label = "3. Choose a season")
  })
  
  evolutionColumnBase <- reactive({
    switch (input$evolution_dataset_choices,
            "players" = 'Player',
            "teams" = 'Team')
    
  })
  
  evolutionIndividualChoices <- reactive({
    evolutionDatasetChosen() %>%
      select(evolutionColumnBase())
  })
  
  output$evolution_individual_choices <- renderUI({
    selectInput(
      "evolutionIndividualInput",
      choices = evolutionIndividualChoices(),
      label = paste("2. Choose an individual"),
      selected = evolutionIndividualChoices()
    )
    
  })
  
  evolutionStatsChoices <- reactive({
    evolutionDatasetChosen() %>%
      select_if(is.numeric) %>%
      names()
  })
  
  output$evolution_stats_choices <- renderUI({
    selectInput("evolutionStatsInput",
                choices = evolutionStatsChoices(),
                label = "4. Choose a stats")
  })
  
  individualChosen <- reactive({
    input$evolutionIndividualInput
  })
  
  output$stats_table_regular <- renderTable({
    ## switch player to team if team ds
    evolutionDatasetChosen() %>%
      filter(get(evolutionColumnBase()) == individualChosen()) %>%
      # select(Season, regularStats) %>%
      arrange(desc(Season)) %>%
      select(Season, everything())
  })
  
  output$stats_table_advanced <- renderTable({
    ## switch player to team if team ds
    evolutionDatasetChosen() %>%
      filter(get(evolutionColumnBase()) == individualChosen()) %>%
      # select(Season, advancedStats) %>%
      arrange(desc(Season)) %>%
      select(Season, everything())
  })
  
   evolutionStatsChosen <- reactive({
    input$evolutionStatsInput
  })
  
  evolutionSeasonChosen <- reactive({
    input$evolutionSeasonInput
  })
  
  output$evolution_plot_league_comparison <- renderPlot({
  individualRow <- evolutionDatasetChosen() %>% 
   filter(get(evolutionColumnBase())==individualChosen(),
          Season==evolutionSeasonChosen()) 
  
    evolutionDatasetChosen() %>% 
      filter(Season==evolutionSeasonChosen()) %>% 
      ggplot(aes("", y=get(evolutionStatsChosen())))+
      geom_boxplot()+
      coord_flip()+
      ggtitle(paste0("League ", evolutionStatsChosen(), " average in ", evolutionSeasonChosen()))+
      ylab(evolutionStatsChosen())+
      xlab("value")+
      geom_point(data=individualRow,aes(x="", y=as.numeric(get(evolutionStatsChosen()))),
                 color='red', size=3)+
      geom_text(data = individualRow, aes(label=individualChosen()), vjust=-1)
    
  })
  
  
  output$evolution_league_comparison_summary <- renderPrint({
    ds <- evolutionDatasetChosen() %>% 
      select(evolutionStatsChosen())
    summary(ds)
    
  })
  
  output$evolution_plot_across_years <- renderPlot({
      
  })
  
  
  # ------------- Comparing players ----------------
  
  comparisonDatasetChosen <- reactive({
    switch (input$comparison_dataset_input,
            "players" = d_season_combined,
            "teams" = d_team_regular_raw,
            "draft" = d_draft
      )})
  
  comparisonSeasonChoices <- reactive({
    levels(comparisonDatasetChosen()$Season)
    })
  
  output$comparison_season <- renderUI({
    selectInput("comparison_season_choices", label = "Choose a season",
                choices = comparisonSeasonChoices())
  })
  
  comparisonIndividualSelected <- reactive({
    switch (input$comparison_dataset_input,
      "players" = 'Player',
      "teams" = 'Team',
      "draft" = 'Player'
    )
  })
  
  comparisonIndividualChoices <- reactive({
    comparisonDatasetChosen() %>% 
      select(comparisonIndividualSelected())
    
  })
  
  output$comparison_player1 <- renderUI({
    selectInput("comparison_select_player1", label = "Choose a player", 
            choices = comparisonIndividualChoices())
  })
  
  output$comparison_player2 <- renderUI({
    selectInput("comparison_select_player2", label = "Choose a player", 
                choices = comparisonIndividualChoices())
  })
  
 
  output$comparison_barplot <- renderPlot({
    # d_season_combined %>% 
    #   filter(Player=="Steven Adams"| Player=="Stephen Curry", Season =="2018-2019") %>% 
    #   ggplot(aes(PTS, AST))+geom_point()
    
  })
  
  
  
  # ---------------- Screener ---------------
  
  screenerInput <- reactive({
    switch(
      input$screener_dataset_input,
      "players" = d_season_combined,
      "teams" = d_team_regular_raw,
      "drafts" = d_draft
    )
  })
  
  
  
  screenerColumnNames <- reactive({
    screenerInput() %>%
      select_if(is.numeric) %>%
      names()
  })
  
  screener_season_level <- reactive({
    levels(screenerInput()$Season)
  })
  
  output$screener_season <- renderUI({
    selectInput("screener_season", " 2. Select a season",
                choices = screener_season_level())
  })
  
  screenerSeasonInput <- reactive({
    input$screener_season
  })
  
  screenerInputValue1 <- reactive({
    input$screener_value1
  })
  screenerInputValue2 <- reactive({
    input$screener_value2
  })
  screenerInputValue3 <- reactive({
    input$screener_value3
  })
  screenerInputValue4 <- reactive({
    input$screener_value4
  })
  
  screenerInputVariable1 <- reactive({
    input$screener_v1
  })
  
  screenerInputVariable2 <- reactive({
    input$screener_v2
  })
  
  screenerInputVariable3 <- reactive({
    input$screener_v3
  })
  
  screenerInputVariable4 <- reactive({
    input$screener_v4
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
  
  output$screener_table <- DT::renderDataTable({
    screenerInput() %>%
      filter(
        Season == screenerSeasonInput(),
        get(screenerInputVariable1()) >= screenerInputValue1(),
        get(screenerInputVariable2()) >= screenerInputValue2(),
        get(screenerInputVariable3()) >= screenerInputValue3(),
        get(screenerInputVariable4()) >= screenerInputValue4()
      ) %>%
      select(-Season)
    
  })
  
  # ------------- View Dataset ---------------------
  
  datasetInput <- reactive({
    switch(
      input$select_dataset,
      "players" = d_season_combined,
      "teams" = d_team_regular_raw,
      "drafts" = d_draft
    )
  })
  
  numeric_columns <- reactive({
    datasetInput() %>%
      select_if(is.numeric) %>%
      names()
  })
  
  output$dataset_columns_x <- renderUI({
    selectInput("dataset_variable_x",
                "3. Select a variable in X-axis",
                choices = numeric_columns())
  })
  
  output$dataset_columns_y <- renderUI({
    selectInput(
      "dataset_variable_y",
      "4. Select a variable in Y-Axis to make the scatterplot",
      choices = numeric_columns()
    )
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
  
  p_histogram <- reactive({
    datasetInput() %>%
      filter(Season == seasonInput()) %>%
      ggplot(aes(x = get(variableInputX()))) +
      geom_histogram(
        bins = binsInput(),
        fill = "#69b3a2",
        color = "#e9ecef",
        alpha = 0.8
      ) +
      xlab(toString(variableInputX())) +
      ggtitle(paste0("Histogram of ", variableInputX(), " in ", seasonInput()))
  })
  
  output$dataset_histogram <- renderPlot({
    p_histogram()
  })
  
  leaderboardColumnBase <- reactive({
     switch(
       input$select_dataset,
      "players" = 'Player',
      "teams" = 'Team',
      "draft" = 'Player'
    )
    
  })
  
  ## todo: change player to team with scatterplot_text_label()
  p_scatterplot <- reactive({
    # individualName <- datasetInput() %>%
    #   select(leaderboardColumnBase())
    
    datasetInput() %>%
      filter(Season == seasonInput()) %>%
      ggplot(aes(
        x = get(variableInputX()),
        y = get(variableInputY())
      )) +
      geom_point(aes(text = get(leaderboardColumnBase())),
                 alpha = 0.6,
                 color = "#69b3a2") +
      geom_smooth(method = lm, se = TRUE) +
      xlab(variableInputX()) +
      ylab(variableInputY()) +
      ggtitle(
        paste0(
          "The relationship between ",
          variableInputX(),
          " and ",
          variableInputY(),
          " in ",
          seasonInput()
        )
      )
  })
  
  output$dataset_scatterplot <- renderPlotly({
    ggplotly(p_scatterplot()) %>%
      config(displayModeBar = FALSE)
    
  })
  
  dataset_scatterplot_lm <- reactive({
    datasetInput() %>%
      filter(Season == seasonInput()) %>%
      lm(get(variableInputY()) ~ get(variableInputX()), .)
  })
  
  output$dataset_scatterplot_summary <-  renderPrint({
    lm <-  dataset_scatterplot_lm()
    print(summary(lm))
  })
  
  
  output$dataset_summary <- renderPrint({
    ds <- datasetInput() %>%
      select(-c(Player, Team, Season, Pos))
    summary(ds)
    
    
  })
  
  output$dataset_download_hist <- downloadHandler(
    filename = function() {
      paste0(get(datasetInput()),
             "histogram",
             variableInputX(),
             sep = "_")
    },
    content = function(file) {
      if (input$dataset_export_img == "png") {
        png(file)
      } else{
        pdf(file)
      }
      p_histogram()
      dev.off()
    }
  )
  
  output$dataset_download_dataset <- downloadHandler(
    filename = function() {
      paste(get(datasetInput()))
    },
    content = function(file) {
      sep <- switch(input$dataset_export_table,
                    "csv" = ",",
                    "txt" = ";")
      write.table(datasetInput(), file, sep = sep)
    }
  )
  
  output$dataset_rawdata <- DT::renderDataTable({
    datasetInput() %>%
      filter(Season == seasonInput()) %>%
      select(-Season)
  })
}
