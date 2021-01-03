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
library(dbplyr)
library(RPostgreSQL)
library(RColorBrewer)
library(R6)
library(glue)

creds <- get_app_credentials("Data/env")
con <- establish_db_connection()

active_players_db <- tbl(con, in_schema("stats", "active_players"))
active_players <-
    active_players_db %>% collect()

player_stats_db <- tbl(con, in_schema("stats", "player_season_totals"))
player_stats <-
    player_stats_db %>% collect()

fantasy_teams_db <- tbl(con, in_schema("fantasy", "teams"))
fantasy_teams <-
    fantasy_teams_db %>%
    collect()

# Define UI for application that draws a histogram
ui <- navbarPage(
    "NBA Smarts",

    tabPanel(
        "Research",
        tags$style("[type = 'number'] {font-size:80%;height:80%}"),
        tags$style(type = "text/css", "label.control-label { font-size: 80%;} .selectize-input { font-size: 80%; height: 80%} .selectize-dropdown { font-size: 80%; height: 80%}"),
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
                selectInput('position', 'Positions', unique(active_players$player_position), selected = unique(active_players$player_position), multiple = TRUE)
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
                        width = 6,
                        plotlyOutput("player_ts", height = 400)
                    ),
                    column(
                        width = 6,
                        plotlyOutput("player_distribution", height = 400)
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
    ),
    tabPanel(
        "Team Comparisons",
        fluidRow(
            column(
                width = 2,
                selectizeInput("team_comp_teams", "Team", choices = fantasy_teams$name, selected = fantasy_teams$name, multiple = TRUE)
            )
        ),
        fluidRow(
            column(
                width = 6,
                plotlyOutput("team_comparison_density", height = 400)
            )
        )
    ),
    tabPanel(
        "League Management",
        fluidRow(
            column(
                width = 4,
                DT::dataTableOutput("list_fantasy_teams")
            ),
            column(
                width = 4,
                uiOutput("fantasy_team_select"),
                DT::dataTableOutput("selected_fantasy_team_players")
            ),
            column(
                width = 2,
                actionButton("add_player", "Add Player to Team"),
                selectizeInput('player_to_add', 'Player', choices = active_players$display_first_last, multiple = TRUE)
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
        return(yoy_df[yoy_df$player_position %in% input$position,])
    })

    output$yoy_scatter_plot <- renderPlotly({
        get_yoy_df() %>%
            ggplot(aes(x = prev_year, y = curr_year, label = name)) + geom_point() +
            theme_bw() +
            ggtitle("Year to year player avg point comparisons") +
            scale_x_continuous(name = input$prev_year, breaks = seq(0,60,10), limits = c(0,50)) +
            scale_y_continuous(name = input$curr_year, breaks = seq(0,60,10), limits = c(0,50))
    })

    output$curr_year_leaderboard <- DT::renderDataTable({
        curr_year_leaderboard <-
            get_yoy_df() %>%
                arrange(desc(coalesce(curr_year,0))) %>%
                select(-prev_year) %>%
                rename(!!input$curr_year := curr_year, pos = player_position)

        return(DT::datatable(curr_year_leaderboard, options = list(pageLength = 10)))
    })

    output$prev_year_leaderboard <- DT::renderDataTable({
        prev_year_leaderboard <-
            get_yoy_df() %>%
                arrange(desc(coalesce(prev_year,0))) %>%
                select(-curr_year) %>%
                rename(!!input$prev_year := prev_year, pos = player_position)

        return(DT::datatable(prev_year_leaderboard, options = list(pageLength = 10)))
    })

    get_player_choices <- reactive({
        unique(active_players[active_players$player_position %in% input$position,]$display_first_last)
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
        ids <- active_players[active_players$display_first_last %in% input$player_names,]$person_id
        season_str <- gen_season_year_str(input$season)

        con <- establish_db_connection()
        player_game_logs_db <- tbl(con, in_schema("stats", "player_game_logs"))

        logs <- player_game_logs_db %>%
            filter(player_id %in% ids & season_year == season_str) %>%
            collect
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
            p <-
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
                        week = floor_date(as.Date(game_date), "week"),
                        player_name
                    ) %>%
                    summarise(
                        ppg = sum(points)/n()
                    ) %>%
                    ungroup %>%
                    ggplot(aes(x = week, y = ppg, colour = player_name)) +
                    geom_line() +
                    theme_bw() +
                    ggtitle("Weekly ppg") +
                    scale_colour_brewer(palette = "Dark2")

            return(p)
        }
    })

    output$player_distribution <- renderPlotly({
        if (length(input$player_names) == 0) {
            NULL
        }
        else {
            point_values <- fetch_point_values()
            p <-
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
                ggplot(aes(x = points, fill = player_name)) +
                geom_density(alpha = .4) +
                theme_bw() +
                scale_fill_brewer(palette = "Dark2")

            return(p)
        }
    })

    output$list_fantasy_teams <- DT::renderDataTable({
        DT::datatable(fantasy_teams[,c("name", "owner_name")], options = list(pageLength = 12), selection = 'single')
    })

    get_selected_team_id <- reactive({
        row_num <- input$list_fantasy_teams_rows_selected
        selected_team_id <- fantasy_teams[row_num,]$id

        return(selected_team_id)
    })

    output$fantasy_team_select <- renderUI({
        if (length(get_selected_team_id()) == 0) {
            curr_name <- NULL
        } else {
            curr_name <- fantasy_teams[fantasy_teams$id == get_selected_team_id(),"name", drop = TRUE]
        }
        selectizeInput(
            "fantasy_team",
            "Team",
            choices = fantasy_teams$name,
            selected = curr_name
        )
    })

    output$selected_fantasy_team_players <- DT::renderDataTable({
        if (length(input$fantasy_team) > 0) {
            input$add_player

            selected_team_name <- input$fantasy_team
            t <- FantasyTeam$new(fantasy_teams[fantasy_teams$name == selected_team_name,]$id)
            players <- t$get_players()
            if (length(players) > 0 ) {
                players <- players[,c(3,11,15)]
                names(players) <- c("Name", "Team", "Position")

                DT::datatable(players, options = list(pageLength = 12))
            }
        }
    })

    observeEvent(input$add_player, {
        selected_team_name <- input$fantasy_team
        t <- FantasyTeam$new(fantasy_teams[fantasy_teams$name == selected_team_name,]$id)
        player_ids <- active_players[active_players$display_first_last %in% input$player_to_add,]$person_id

        withProgress(message = "Adding players", value = 0, {
            map(player_ids, function(id) {
                incProgress(1/length(player_ids), detail = selected_team_name)
                t$add_player(id)

                Sys.sleep(.1)
                }
            )
        })
    })

    output$team_comparison_density <- renderPlotly({
        con <- establish_db_connection()
        selected_team_names <- input$team_comp_teams

        if (month(Sys.Date()) < 9) {
            curr_season <- year(Sys.Date()) - 1
        } else {
            curr_season <- year(Sys.Date())
        }

        curr_season <- gen_season_year_str(curr_season)

        query <- glue_sql(
            "
            select
              name, player_name, game_date, pts, reb, ast, blk, stl, dd2, td3, fg3m, tov
            from fantasy.teams
            inner join stats.active_players on teams.id = active_players.fantasy_team_id
            inner join stats.player_game_logs on active_players.person_id = player_game_logs.player_id
            where name in ({names*}) and season_year = {curr_season}",
            names = selected_team_names,
            curr_season = curr_season,
            .con = con
        )
        logs <- dbGetQuery(con, query)

        p <- logs %>%
            calculate_fantasy_points %>%
            ggplot(aes(x = points, fill = name)) + geom_density(alpha = .4) + theme_bw() +
            scale_fill_brewer(name = "Team", palette = "Dark2") +
            ggtitle("Probability Density for Avg Points per Player per Game in 2020")

        return(p)
    })

    output$test <- renderTable({
        con <- establish_db_connection()
        selected_team_names <- input$team_comp_teams

        if (month(Sys.Date()) < 9) {
            curr_season <- year(Sys.Date()) - 1
        } else {
            curr_season <- year(Sys.Date())
        }

        curr_season <- gen_season_year_str(curr_season)

        query <- glue_sql(
            "
            select
              name, player_name, game_date, pts, reb, ast, blk, stl, dd2, td3, fg3m, tov
            from fantasy.teams
            inner join stats.active_players on teams.id = active_players.fantasy_team_id
            inner join stats.player_game_logs on active_players.person_id = player_game_logs.player_id
            where name in ({names*}) and season_year = {curr_season}",
            names = selected_team_names,
            curr_season = curr_season,
            .con = con
        )
        logs <- dbGetQuery(con, query)

        logs %>%
            calculate_fantasy_points
    })
}

# Run the application
shinyApp(ui = ui, server = server)
