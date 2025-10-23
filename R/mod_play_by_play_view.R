#' play_by_play_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param game_id The game to display events for.
#' @param db_conn The database connection.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_play_by_play_view_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("play_by_play_table"))
  )
}

#' play_by_play_view Server Functions
#'
#' @noRd
mod_play_by_play_view_server <- function(id, db_conn, game_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: Format ms clock to MM:SS
    format_clock <- function(ms) {
      mins <- ms %/% 60000
      secs <- (ms %% 60000) %/% 1000
      sprintf("%02d:%02d", mins, secs)
    }

    # Helper: Map event_type to label
    event_type_label <- function(type) {
      dplyr::case_when(
        type == "guy_play" ~ "Guy Play",
        type == "girl_play" ~ "Girl Play",
        type == "guy_touchdown" ~ "Guy Touchdown",
        type == "girl_touchdown" ~ "Girl Touchdown",
        type == "guy_touchdown_def" ~ "Defensive Guy Touchdown",
        type == "girl_touchdown_def" ~ "Defensive Girl Touchdown",
        type == "pat1_good" ~ "PAT 1pt Good",
        type == "pat2_good" ~ "PAT 2pt Good",
        type == "pat3_good" ~ "PAT 3pt Good",
        type == "pat1_miss" ~ "PAT 1pt Miss",
        type == "pat2_miss" ~ "PAT 2pt Miss",
        type == "pat3_miss" ~ "PAT 3pt Miss",
        type == "pat_def" ~ "Defensive PAT",
        type == "punt" ~ "Punt",
        type == "turnover" ~ "Turnover",
        type == "safety" ~ "Safety",
        type == "timeout_home" ~ "Timeout (Home)",
        type == "timeout_away" ~ "Timeout (Away)",
        TRUE ~ NA_character_
      )
    }

    # play-by-play data
    pbp_data <- reactive({
      invalidateLater(5000, session)
      events <- DBI::dbGetQuery(
        db_conn,
        "SELECT e.*, 
            g.home_team, g.away_team,
            th.team_name AS home_team_name,
            ta.team_name AS away_team_name
     FROM events e
     LEFT JOIN games g ON e.game_id = g.game_id
     LEFT JOIN teams th ON g.home_team = th.team_id
     LEFT JOIN teams ta ON g.away_team = ta.team_id
     WHERE e.game_id = $1
     ORDER BY e.event_id ASC",
        params = list(game_id)
      )
      if (nrow(events) == 0) {
        return(NULL)
      }

      # Team for event (possession or defense)
      events$team <- dplyr::case_when(
        # timeouts
        grepl("home", events$event_type) ~ events$home_team,
        grepl("away", events$event_type) ~ events$away_team,
        # defensive plays
        grepl("def|safety|turnover", events$event_type) &
          events$possession == "Home" ~
          events$away_team,
        grepl("def|safety|turnover", events$event_type) &
          events$possession == "Away" ~
          events$home_team,
        # remaining plays (offense)
        events$possession == "Home" ~ events$home_team,
        events$possession == "Away" ~ events$away_team,
        TRUE ~ NA_integer_
      )
      # Map team to team_name
      events$team_name <- dplyr::case_when(
        events$team == events$home_team ~ events$home_team_name,
        events$team == events$away_team ~ events$away_team_name,
        TRUE ~ NA_character_
      )
      # Event description
      events$desc <- event_type_label(events$event_type)
      events$clock_str <- format_clock(events$clock)
      events <- events[!is.na(events$desc), ]
      if (nrow(events) == 0) {
        return(NULL)
      }
      events
    })

    # Render play-by-play table
    output$play_by_play_table <- renderUI({
      events <- pbp_data()
      if (is.null(events)) {
        return(tags$em("No events yet."))
      }

      # Group by half
      halves <- unique(events$half)
      tagList(
        lapply(halves, function(hlf) {
          h_events <- events[events$half == hlf, ]
          tagList(
            tags$h4(paste(c("1st", "2nd")[hlf], "Half")),
            tags$table(
              style = "width:100%;",
              tags$tbody(
                lapply(seq_len(nrow(h_events)), function(i) {
                  ev <- h_events[i, ]
                  tags$tr(
                    tags$td(ev$team_name),
                    tags$td(ev$clock_str),
                    tags$td(ev$desc)
                  )
                })
              )
            )
          )
        })
      )
    })
  })
}

## To be copied in the UI
# mod_play_by_play_view_ui("play_by_play_view_1")

## To be copied in the server
# mod_play_by_play_view_server("play_by_play_view_1", db_conn, game_id)
