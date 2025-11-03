#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  db_conn <- gameon_db_connect()
  session$onSessionEnded(function() {
    DBI::dbDisconnect(db_conn)
  })

  user_rv <- reactiveVal()

  # Global user login status for conditional panels
  output$user_logged_in_global <- reactive({
    !is.null(user_rv())
  })
  outputOptions(output, "user_logged_in_global", suspendWhenHidden = FALSE)

  mod_login_server("login_1", db_conn, user_rv)

  mod_home_server("home_1", db_conn, user_rv)

  mod_standings_server(
    "standings_1",
    db_conn = db_conn,
    league_id = 1
  )

  mod_referee_controls_server(
    "referee_controls_sim",
    db_conn,
    game_id = NA,
    user_rv = user_rv
  )
}
