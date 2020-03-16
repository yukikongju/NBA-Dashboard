# NBA Dashboard

This dashboard shows NBA statistics tracked across the years and
can be used to visualize basic plots when going into data exploration. 

All data were scrape from https://www.basketball-reference.com/ using the rvest package.
Data Wrangling was handled with plyr and dplyr.
The dashboard was constructed using shiny and the plots were built using ggplot and plotly.

# Features

### Leaderboard

Allow the user to view the top players or teams based on a numerical statistic.

### Evolution

Allow the user to see a player's or team's stats across the season through a table and a graph. Compares that metrics to league average.

### Comparison

Allow the user to compare two players or teams for a particular season. 

### Screener

Allow the user to choose a dataset and find an observation that matches filters inputs.

![Screenshot](screenshots/screener.png)

### View Dataset

Allow the user to view and download a histogram, a scatterplot, a summary of the variables and raw observations of a dataset 

![Screenshot](screenshots/view_dataset.png)

