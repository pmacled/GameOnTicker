#' data_controls UI Function
#'
#' @description A shiny Module for manual DB table editing.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput actionButton uiOutput
mod_data_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Manual Data Table Editor"),
    selectInput(ns("table_select"), "Choose table to edit", choices = NULL),
    rhandsontable::rHandsontableOutput(ns("table_editor")),
    actionButton(ns("save_changes"), "Save Changes")
  )
}

#' data_controls Server Function
#'
#' @param db_conn A DBI connection to the database.
#' @noRd
mod_data_controls_server <- function(id, db_conn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tables <- reactive({
      DBI::dbListTables(db_conn)
    })

    observe({
      updateSelectInput(session, "table_select", choices = tables())
    })

    table_data <- reactiveVal(NULL)
    observe({
      req(input$table_select)
      df <- DBI::dbReadTable(db_conn, input$table_select)
      # Convert POSIXct columns to character for editing
      df <- df |>
        dplyr::mutate(dplyr::across(
          dplyr::where(~ inherits(.x, "POSIXct")),
          as.character
        ))
      table_data(df)
    }) |>
      bindEvent(input$save_changes, input$table_select)

    output$table_editor <- rhandsontable::renderRHandsontable({
      req(table_data())
      rhandsontable::rhandsontable(table_data(), rowHeaders = NULL) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = FALSE
        )
    })

    edited_data <- reactive({
      req(input$table_editor)
      df <- rhandsontable::hot_to_r(input$table_editor)
      # Convert character columns back to POSIXct if needed
      orig <- DBI::dbReadTable(db_conn, input$table_select)
      posix_cols <- names(orig)[vapply(orig, inherits, logical(1), "POSIXct")]
      for (col in posix_cols) {
        if (col %in% names(df)) {
          # Only convert if not already POSIXct
          if (!inherits(df[[col]], "POSIXct")) {
            df[[col]] <- as.POSIXct(df[[col]], tz = attr(orig[[col]], "tzone"))
          }
        }
      }
      df
    })

    observeEvent(input$save_changes, {
      req(input$table_select, edited_data())
      DBI::dbWriteTable(
        db_conn,
        input$table_select,
        edited_data(),
        overwrite = TRUE
      )
      showNotification("Table updated.", type = "message")
      table_data(DBI::dbReadTable(db_conn, input$table_select))
    })
  })
}

## To be copied in the UI
# mod_data_controls_ui("data_controls_1")

## To be copied in the server
# mod_data_controls_server("data_controls_1", db_conn)
