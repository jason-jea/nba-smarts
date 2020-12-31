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
library(purrr)
library(lubridate)
library(scales)
library(jsonlite)
library(httr)

source("data.R")

creds <- get_app_credentials("/Data/env")
active_players <-
    read.delim(
        text = rawToChar(
            get_object(
                object = "stats/active_players.csv",
                bucket = "nba-smarts",
                key = creds["aws_access_key_id"],
                secret = creds["aws_secret_access_key"]
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
                object = "stats/player_stats.csv",
                bucket = "nba-smarts",
                key = creds["aws_access_key_id"],
                secret = creds["aws_secret_access_key"]
            )
        ),
        sep = ",",
        header = TRUE,
        stringsAsFactors = FALSE
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$style("[type = 'number'] {font-size:80%;height:80%}"),
    tags$style(type = "text/css", "label.control-label { font-size: 80%;} .selectize-input { font-size: 80%; height: 80%} .selectize-dropdown { font-size: 80%; height: 80%}"),

    # Application title
    titlePanel("NBA Smarts"),

    fluidRow(
        column(
            width = 2,
            numericInput("pts_value", "Points", .5, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("reb_value", "Rebounds", 1, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("asst_value", "Assists", 1, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("trey_value", "3PM", .5, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("stl_value", "Steals", 2, min = 0, step = .1)
        )
    ),

    fluidRow(
        column(
            width = 2,
            numericInput("blk_value", "Blocks", 2, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("tov_value", "TOs", -1, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("td3_value", "TDs", 3, min = 0, step = .1)
        ),
        column(
            width = 2,
            numericInput("dd2_value", "DDs", 3, min = 0, step = .1)
        )
    ),

    br(),

    fluidRow(
        column(
            width = 2,
            selectInput('curr_year', 'Current Year', c(2020, 2019, 2018, 2017, 2016))
        ),
        column(
            width = 2,
            selectInput('prev_year', 'Previous Year', c(2019, 2018, 2017, 2016, 2015))
        ),
        column(
            width = 2,
            selectInput('position', 'Positions', unique(active_players$PLAYER_POSITION), selected = unique(active_players$PLAYER_POSITION), multiple = TRUE)
        )
    ),
    tabsetPanel(
        id = "visuals",
        tabPanel(
            "Season Totals",
            fluidRow(
                column(
                    width = 5,
                    plotlyOutput(
                        "yoy_scatter_plot",
                        height = 500
                    )
                ),
                column(
                    width = 3,
                    br(),
                    div(DT::dataTableOutput("curr_year_leaderboard"), style = "font-size:70%")
                ),
                column(
                    width = 3,
                    br(),
                    div(DT::dataTableOutput("prev_year_leaderboard"), style = "font-size:70%")
                )
            )
        ),
        tabPanel(
            "Player Comparisons",
            fluidRow(
                column(
                    width = 3,
                    uiOutput("player_select")
                ),
                column(
                    width = 2,
                    selectizeInput('season', 'Season(s)', choices = c(2020,2019, 2018, 2017, 2016, 2015), selected = 2020)
                ),
                column(
                    width = 2,
                    br(),
                    actionButton("get_player_data", "Get Player Data")
                )
            ),
            fluidRow(
                column(
                    width = 8,
                    plotlyOutput("player_ts", height = 500)
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    div(DT::dataTableOutput("player_game_logs"), style = "font-size:70%")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    fetch_point_values <- reactive({
        value_var_names <- names(input)[grepl("_value", names(input))]
        point_values <- map(value_var_names, function(x){
            input[[x]]
        })
        names(point_values) <- value_var_names

        return(point_values)
    })

    get_yoy_df <- reactive({
        value_var_names <- names(input)[grepl("_value", names(input))]
        point_values <- map(value_var_names, function(x){
            input[[x]]
        })
        names(point_values) <- value_var_names

        curr_year <- input$curr_year
        prev_year <- input$prev_year

        yoy_df <-
            create_yoy_df(
                player_stats,
                active_players,
                prev_year = input$prev_year,
                curr_year = input$curr_year,
                point_values = point_values
            )
        return(yoy_df[yoy_df$PLAYER_POSITION %in% input$position,])
    })

    output$yoy_scatter_plot <- renderPlotly({
        get_yoy_df() %>%
            ggplot(aes(x = prev_year, y = curr_year, label = name)) + geom_point() +
            theme_bw() +
            scale_x_continuous(name = input$prev_year, breaks = seq(0,60,10), limits = c(0,50)) +
            scale_y_continuous(name = input$curr_year, breaks = seq(0,60,10), limits = c(0,50))
    })

    output$curr_year_leaderboard <- DT::renderDataTable({
        curr_year_leaderboard <-
            get_yoy_df() %>%
                arrange(desc(coalesce(curr_year,0))) %>%
                select(-prev_year) %>%
                rename(!!input$curr_year := curr_year, pos = PLAYER_POSITION)

        return(DT::datatable(curr_year_leaderboard, options = list(pageLength = 10)))
    })

    output$prev_year_leaderboard <- DT::renderDataTable({
        prev_year_leaderboard <-
            get_yoy_df() %>%
                arrange(desc(coalesce(prev_year,0))) %>%
                select(-curr_year) %>%
                rename(!!input$prev_year := prev_year, pos = PLAYER_POSITION)

        return(DT::datatable(prev_year_leaderboard, options = list(pageLength = 10)))
    })

    get_player_choices <- reactive({
        unique(active_players[active_players$PLAYER_POSITION %in% input$position,]$DISPLAY_FIRST_LAST)
    })

    output$player_select <- renderUI({
        selectizeInput(
            "player_names",
            "Player(s)",
            choices = get_player_choices(),
            multiple = TRUE
        )
    })

    get_player_logs <- eventReactive(input$get_player_data, {
        ids <- active_players[active_players$DISPLAY_FIRST_LAST %in% input$player_names,"PERSON_ID"]
        print(ids)
        logs <-
            map_dfr(ids, function(x){
                print(paste0("getting log for player ", x))
                get_player_gamelogs(
                    PlayerID = x,
                    Season = paste0(input$season, "-", as.integer(substr(input$season, 3, 4)) + 1)
                )[,c(1,3,5,8:13,15:16,18:19,23:27,31,34:35)]
            })
        return(logs)
    })

    output$player_game_logs <- DT::renderDataTable({
        DT::datatable(get_player_logs(), options = list(pageLength = 10))
    })

    output$player_ts <- renderPlotly({
        if (length(input$player_names) == 0) {
            NULL
        }
        else {
            point_values <- fetch_point_values()

            get_player_logs() %>%
                calculate_fantasy_points(
                    df = .,
                    pts_value = point_values$pts_value,
                    reb_value = point_values$reb_value,
                    asst_value = point_values$asst_value,
                    trey_value = point_values$trey_value,
                    stl_value = point_values$stl_value,
                    blk_value = point_values$blk_value,
                    tov_value = point_values$tov_value,
                    td3_value = point_values$td3_value,
                    dd2_value = point_values$dd2_value
                ) %>%
                group_by(
                    week = floor_date(as.Date(GAME_DATE), "day"),
                    PLAYER_NAME
                ) %>%
                summarise(
                    ppg = sum(points)/n()
                ) %>%
                ungroup %>%
                ggplot(aes(x = week, y = ppg, colour = PLAYER_NAME)) + geom_line() + theme_bw()
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
