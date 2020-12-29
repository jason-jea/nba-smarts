#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(aws.s3)
library(everlaneR)
library(nbasmarts)

source("app_data_prep.R")

aws_creds <- get_aws_credentials()
active_players <-
    read.delim(
        text = rawToChar(
            get_object(
                object = "jj/nba/active_players.csv",
                bucket = "everlane-data",
                key = aws_creds["aws_access_key_id"],
                secret = aws_creds["aws_secret_access_key"]
            )
        ),
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
    )

player_stats <-
    read.delim(
        text = rawToChar(
            get_object(
                object = "jj/nba/player_stats.csv",
                bucket = "everlane-data",
                key = aws_creds["aws_access_key_id"],
                secret = aws_creds["aws_secret_access_key"]
            )
        ),
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA Smarts"),

    fluidRow(
        column(
            width = 2,
            selectInput('curr_year', 'Current Year', c(2020, 2019, 2018, 2017, 2016)),
            selectInput('prev_year', 'Previous Year', c(2019, 2018, 2017, 2016, 2015)),
            selectInput('position', 'Positions', unique(active_players$PLAYER_POSITION), selected = unique(active_players$PLAYER_POSITION), multiple = TRUE)
        ),
        column(
            width = 10,
            plotlyOutput(
                "yoy_scatter_plot",
                height = 500
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$yoy_scatter_plot <- renderPlotly({
        curr_year <- input$curr_year
        prev_year <- input$prev_year

        yoy_df <- create_yoy_df(player_stats, active_players, prev_year = input$prev_year, curr_year = input$curr_year)
        yoy_df <- yoy_df[yoy_df$PLAYER_POSITION %in% input$position,]
        yoy_df %>%
            ggplot(aes(x = prev_year, y = curr_year, label = name)) + geom_point() +
            theme_bw() +
            scale_x_continuous(name = prev_year) +
            scale_y_continuous(name = curr_year)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
