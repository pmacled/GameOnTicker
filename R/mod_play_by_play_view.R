#' play_by_play_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_play_by_play_view_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "play-by-play-container",
      uiOutput(ns("play_by_play_table"))
    )
  )
}

#' play_by_play_view Server Functions
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param game_data Reactive containing game data including events.
#'
#' @noRd
mod_play_by_play_view_server <- function(
  id,
  game_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: Format ms clock to MM:SS
    format_clock <- function(ms) {
      mins <- ms %/% 60000
      secs <- (ms %% 60000) %/% 1000
      sprintf("%02d:%02d", as.integer(mins), as.integer(secs))
    }

    # Helper: Map event type to label
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
        type == "turnover_on_downs" ~ "Turnover on Downs",
        type == "safety" ~ "Safety",
        type == "timeout_home" ~ "Timeout",
        type == "timeout_away" ~ "Timeout",
        type == "start_second_half" ~ "End of Half",
        type == "finalize_game" ~ "End of Game",
        TRUE ~ NA_character_
      )
    }

    # play-by-play data
    pbp_data <- reactive({
      req(game_data())

      events <- game_data()$events
      if (is.null(events) || nrow(events) == 0) {
        return(NULL)
      }

      # Team for event (possession or defense)
      events$team_name <- dplyr::case_when(
        # timeouts
        grepl("home", events$type) ~ events$home_team_name,
        grepl("away", events$type) ~ events$away_team_name,
        # defensive plays
        grepl("def|safety|turnover", events$type) &
          events$possession == "Home" ~
          events$away_team_name,
        grepl("def|safety|turnover", events$type) &
          events$possession == "Away" ~
          events$home_team_name,
        # remaining plays (offense)
        events$possession == "Home" ~ events$home_team_name,
        events$possession == "Away" ~ events$away_team_name,
        TRUE ~ NA_character_
      )
      # Event description
      events$desc <- event_type_label(events$type)
      events$clock_str <- format_clock(events$clock_ms)
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
        return(div(class = "no-events", "No events yet."))
      }

      # Sort halves descending (2nd, then 1st)
      halves <- sort(unique(events$half), decreasing = TRUE)
      tagList(
        lapply(halves, function(hlf) {
          h_events <- events[events$half == hlf, ]
          # Reverse events within half
          h_events <- h_events[rev(seq_len(nrow(h_events))), ]
          tagList(
            tags$h5(paste(c("1st", "2nd")[as.integer(hlf)], "Half")),
            tags$table(
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
# mod_play_by_play_view_server("play_by_play_view_1", game_data = game_data)
