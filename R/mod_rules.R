#' rules UI Function
#'
#' @description A shiny Module for displaying rules documentation with an AI assistant.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rules_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "rules-container",
      div(
        class = "rules-header",
        actionButton(
          ns("ask_ai"),
          "Ask AI",
          class = "btn btn-primary btn-sm",
          icon = icon("robot")
        )
      ),
      div(
        class = "rules-content",
        uiOutput(ns("rules_markdown"))
      )
    )
  )
}

#' rules Server Functions
#'
#' @param id Module id
#' @param rules_content The complete rules/documentation content as a character string
#' @param rules_type A description of what type of rules these are (e.g., "co-ed touch football rules")
#'
#' @noRd
mod_rules_server <- function(
  id,
  rules_content,
  rules_type = "rules"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the markdown content
    output$rules_markdown <- renderUI({
      if (is.null(rules_content) || nchar(rules_content) == 0) {
        div(
          class = "alert alert-warning",
          "Rules content is not available."
        )
      } else {
        # Convert markdown to HTML
        html_content <- markdown::markdownToHTML(
          text = rules_content,
          fragment.only = TRUE
        )
        HTML(html_content)
      }
    })

    # Handle Ask AI button click
    observeEvent(input$ask_ai, {
      showModal(modalDialog(
        title = "Rules Assistant",
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        mod_rules_assistant_ui(
          ns("assistant"),
          title = "Rules Assistant",
          description = "Ask me anything about the rules.",
          quick_questions = list(
            "What are the female involvement requirements?",
            "How does scoring work?",
            "What are the timeout rules?",
            "How does overtime work?",
            "What are the common penalties?"
          )
        )
      ))
    })

    # Initialize the rules assistant module
    mod_rules_assistant_server(
      "assistant",
      rules_content = rules_content,
      rules_type = rules_type
    )
  })
}

## To be copied in the UI
# mod_rules_ui("rules_1")

## To be copied in the server
# mod_rules_server("rules_1",
#                  rules_content = rules_markdown_content,
#                  rules_type = "co-ed touch football rules")
