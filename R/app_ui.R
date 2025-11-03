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
    bslib::page_navbar(
      title = tagList(
        "Game On! Ticker (alpha)",
        bslib::input_dark_mode(id = "dark_mode", mode = "light")
      ),
      selected = "Standings",
      bslib::nav_panel(
        "Home",
        conditionalPanel(
          condition = "output.user_logged_in_global",
          mod_home_ui("home_1")
        ),
        conditionalPanel(
          condition = "!output.user_logged_in_global",
          div(
            class = "text-center mt-5",
            p(
              tags$a(
                "Login/Register",
                href = "#",
                class = "nav-link d-inline fw-bold text-primary",
                onclick = "$('#login_1-show_auth').click(); return false;"
              ),
              " to access your favorites and referee games."
            )
          )
        )
      ),
      bslib::nav_panel(
        "Standings",
        mod_standings_ui("standings_1")
      ),
      bslib::nav_panel(
        "Referee Simulator",
        mod_referee_controls_ui("referee_controls_sim")
      ),
      bslib::nav_item(
        login_status_ui("login_1")
      ),
      # TODO next module should probably be the game viewer/ticker.
      theme = bslib::bs_theme(preset = "shiny")
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "stylesheet", type = "text/css", href = "www/home.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/ticker.css"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "referee-controls.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/standings.css"
    )
  )
}
