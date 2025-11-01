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

  observeEvent(user_rv(), {
    req(user_rv())
    showNotification(
      paste("Welcome,", user_rv()),
      type = "message"
    )
  })

  mod_login_server("login_1", db_conn, user_rv)

  mod_referee_controls_server("referee_controls_1", db_conn, game_id = 170)
  mod_play_by_play_view_server("play_by_play_view_1", db_conn, game_id = 170)
}
