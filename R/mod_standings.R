#' standings UI Function
#'
#' @description A shiny Module for displaying league standings.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_standings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "standings-container",
      uiOutput(ns("standings_tables"))
    )
  )
}

#' standings Server Functions
#'
#' @param db_conn Database connection
#' @param league_id League ID to get standings for
#' @param division_id Optional vector of division IDs to subset to
#' @noRd
mod_standings_server <- function(id, db_conn, league_id, division_id = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate standings data
    standings_data <- reactive({
      req(db_conn, league_id)

      # Get divisions for the league
      division_query <- "
        SELECT d.id, d.name, d.rank
        FROM division d
        WHERE d.league_id = $1
        ORDER BY d.rank
      "
      if (!is.null(division_id)) {
        division_query <- paste0(
          division_query,
          " AND d.id = ANY($2)"
        )
        divisions <- DBI::dbGetQuery(
          db_conn,
          division_query,
          list(league_id, division_id)
        )
      } else {
        divisions <- DBI::dbGetQuery(db_conn, division_query, list(league_id))
      }

      if (nrow(divisions) == 0) {
        return(NULL)
      }

      # Get all completed games for the divisions with team info
      games_query <- "
        SELECT 
          g.id as game_id,
          g.home_team_id,
          g.away_team_id,
          g.score_home,
          g.score_away,
          g.division_id,
          g.home_result,
          g.away_result,
          g.start_time,
          ht.name as home_team_name,
          ht.logo as home_team_logo,
          at.name as away_team_name,
          at.logo as away_team_logo
        FROM game g
        JOIN team ht ON g.home_team_id = ht.id
        JOIN team at ON g.away_team_id = at.id
        WHERE g.division_id IN ("

      # Create placeholders for division IDs
      division_placeholders <- paste0(
        "$",
        1:length(divisions$id),
        collapse = ", "
      )
      games_query <- paste0(
        games_query,
        division_placeholders,
        ")
          AND g.home_result IS NOT NULL 
          AND g.away_result IS NOT NULL
        ORDER BY g.start_time
      "
      )

      # Create parameter list with all division IDs
      query_params <- as.list(as.integer(divisions$id))

      games <- DBI::dbGetQuery(
        db_conn,
        games_query,
        query_params
      )

      # Convert integer64 columns to regular integers for proper R operations
      if (nrow(games) > 0) {
        games <- games |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::where(~ inherits(.x, "integer64")),
              .fns = as.integer
            )
          )
      }

      if (nrow(games) == 0) {
        # Return empty standings for divisions with no games
        return(list(
          divisions = divisions,
          standings = data.frame()
        ))
      }

      # Calculate team standings
      standings <- calculate_standings(games, divisions)

      list(
        divisions = divisions,
        standings = standings
      )
    })

    output$standings_tables <- renderUI({
      tryCatch(
        {
          data <- standings_data()
          if (is.null(data) || nrow(data$standings) == 0) {
            return(div(class = "no-standings", "No standings data available"))
          }

          # Create a GT table with division group headers
          gt_table <- tryCatch(
            {
              create_standings_table(data$standings, data$divisions)
            },
            error = function(e) {
              # Show error message if GT table creation fails
              div(
                class = "standings-error",
                p(paste("Error creating standings table:", e$message))
              )
            }
          )

          div(
            class = "division-standings",
            gt_table
          )
        },
        error = function(e) {
          div(
            class = "standings-error",
            h3("Error loading standings"),
            p(paste("Error details:", e$message)),
            p("Please check your database connection and try again.")
          )
        }
      )
    })
  })
}

## To be copied in the UI
# mod_standings_ui("standings_1")

## To be copied in the server
# mod_standings_server("standings_1", db_conn, league_id, division_id)
