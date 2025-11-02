#' ticker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ticker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "ticker-bar",
      # Top row: logos, team info, scores
      div(
        class = "ticker-top-row",
        div(class = "ticker-left-logo", uiOutput(ns("away_logo"))),
        div(
          class = "ticker-left-info",
          div(class = "team-name", textOutput(ns("away_name"))),
          div(class = "team-timeouts", uiOutput(ns("timeouts_away"))),
          div(class = "team-record", textOutput(ns("record_away")))
        ),
        div(class = "ticker-left-score", textOutput(ns("score_away"))),
        div(class = "ticker-center-spacer"),
        div(class = "ticker-right-score", textOutput(ns("score_home"))),
        div(
          class = "ticker-right-info",
          div(class = "team-name", textOutput(ns("home_name"))),
          div(class = "team-timeouts", uiOutput(ns("timeouts_home"))),
          div(class = "team-record", textOutput(ns("record_home")))
        ),
        div(class = "ticker-right-logo", uiOutput(ns("home_logo")))
      ),
      # Bottom row: clocks, half, down
      div(
        class = "ticker-bottom-row",
        div(
          class = "ticker-bottom-left",
          span(class = "game-half", textOutput(ns("half"))),
          span(class = "game-clock", textOutput(ns("game_clock")))
        ),
        div(
          class = "ticker-bottom-right",
          span(class = "play-clock", textOutput(ns("play_clock"))),
          span(
            class = "down-girl-plays-available",
            textOutput(ns("down_girl_plays_available"))
          )
        )
      )
    )
  )
}

#' ticker Server Functions
#'
#' @param game_data A reactive or static list with ticker fields
#' @noRd
mod_ticker_server <- function(id, game_data, game_clock, play_clock) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    get_val <- function(name) {
      if (is.reactive(game_data)) game_data()[[name]] else game_data[[name]]
    }

    output$away_logo <- renderUI({
      logo <- get_val("away_logo")
      if (!is.null(logo) && !is.na(logo) && nzchar(logo)) {
        logo_full <- ifelse(
          grepl("/", logo),
          logo,
          file.path(Sys.getenv("STORAGE_BASE_URL"), "images", "logos", logo)
        )
        tags$img(src = logo_full, class = "team-logo")
      } else {
        tags$div(class = "team-logo")
      }
    })
    output$home_logo <- renderUI({
      logo <- get_val("home_logo")
      if (!is.null(logo) && !is.na(logo) && nzchar(logo)) {
        logo_full <- ifelse(
          grepl("/", logo),
          logo,
          file.path(Sys.getenv("STORAGE_BASE_URL"), "images", "logos", logo)
        )
        tags$img(src = logo_full, class = "team-logo")
      } else {
        # Always reserve space, even if no logo
        tags$div(class = "team-logo")
      }
    })
    output$away_name <- renderText(get_val("away_name"))
    output$home_name <- renderText(get_val("home_name"))
    output$record_away <- renderText(get_val("record_away"))
    output$record_home <- renderText(get_val("record_home"))
    output$score_away <- renderText({
      if (isTRUE(get_val("possession") == "Away")) {
        paste(get_val("score_away"), "\u25C2")
      } else {
        get_val("score_away")
      }
    })
    output$score_home <- renderText({
      if (isTRUE(get_val("possession") == "Home")) {
        paste("\u25B8", get_val("score_home"))
      } else {
        get_val("score_home")
      }
    })
    output$half <- renderText(get_val("half"))
    output$down_girl_plays_available <- renderText(get_val(
      "down_girl_plays_available"
    ))
    output$game_clock <- renderText({
      if (is.reactive(game_clock)) game_clock() else game_clock
    })
    output$play_clock <- renderText({
      if (is.reactive(play_clock)) play_clock() else play_clock
    })

    output$timeouts_away <- renderUI({
      n_remaining <- get_val("timeouts_away")
      if (is.null(n_remaining) || length(n_remaining) == 0) {
        return(NULL)
      }
      lapply(1:3, function(i) {
        div(
          class = paste(
            "timeout-strip",
            if (i > n_remaining) "timeout-used" else "timeout-unused"
          )
        )
      })
    })
    output$timeouts_home <- renderUI({
      n_remaining <- get_val("timeouts_home")
      if (is.null(n_remaining) || length(n_remaining) == 0) {
        return(NULL)
      }
      lapply(1:3, function(i) {
        div(
          class = paste(
            "timeout-strip",
            if (i > n_remaining) "timeout-used" else "timeout-unused"
          )
        )
      })
    })
  })
}

## To be copied in the UI
# mod_ticker_ui("ticker_1")

## To be copied in the server
# mod_ticker_server("ticker_1", game_data)
