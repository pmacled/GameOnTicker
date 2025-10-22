#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      "Game On! Ticker (alpha)",
      tabPanel(
        "Referee Controls",
        mod_referee_controls_ui("referee_controls_1")
      ),
      tabPanel("Play-by-Play", mod_play_by_play_view_ui("play_by_play_view_1")),
      tabPanel("Data Controls", mod_data_controls_ui("data_controls_1"))
      # TODO next module should probably be the game viewer/ticker.
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GameOnTicker"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
