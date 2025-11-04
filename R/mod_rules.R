#' rules UI Function
#'
#' @description A shiny Module for providing an AI chat interface to answer
#' questions about rules or documentation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param title The title to display at the top of the module
#' @param description The description text to show below the title
#' @param quick_questions Optional list of quick questions to display as buttons
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rules_ui <- function(
  id,
  title = "Rules Assistant",
  description = "Ask me anything about the rules.",
  quick_questions = NULL
) {
  ns <- NS(id)

  # Create initial messages with quick questions if provided
  initial_messages <- paste0(
    "**",
    title,
    "**\n\n",
    description
  )

  # Add quick questions as suggestions if provided
  if (!is.null(quick_questions) && length(quick_questions) > 0) {
    suggestions <- paste(
      sapply(quick_questions, function(q) {
        paste0("* <span class=\"suggestion submit\">", q, "</span>")
      }),
      collapse = "\n"
    )
    initial_messages <- paste0(
      initial_messages,
      "\n\nHere are some quick questions to get you started:\n\n",
      suggestions
    )
  }

  tagList(
    div(
      class = "rules-container",
      shinychat::chat_ui(
        id = ns("chat"),
        messages = initial_messages,
        fillable_mobile = TRUE
      )
    )
  )
}

#' rules Server Functions
#'
#' @param id Module id
#' @param rules_content The complete rules/documentation content as a character string
#' @param rules_type A description of what type of rules these are (e.g., "co-ed touch football rules")
#' @param model The Claude model to use (default: "claude-haiku-4-5-20251001")
#'
#' @noRd
mod_rules_server <- function(
  id,
  rules_content,
  rules_type = "rules",
  model = "claude-haiku-4-5-20251001"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create the system prompt
    system_prompt <- paste0(
      "You are a helpful assistant that answers questions about ",
      rules_type,
      ". ",
      "You have access to the complete documentation provided below. When answering questions:\n\n",
      "1. Always quote the relevant sections directly and in full\n",
      "2. When quoting, use markdown formatting with > blockquotes\n",
      "3. Provide specific section headings when available\n",
      "4. Be comprehensive - quote entire sections when they are relevant\n",
      "5. If a question is unclear, ask for clarification\n",
      "6. If a question is not covered by the documentation, say so clearly\n",
      "7. Use a friendly, helpful tone but still be concise\n",
      "8. Format your responses clearly with headers and bullet points when appropriate\n\n",
      "Example response format:\n",
      "## Section: [Section Name]\n\n",
      "> [Full quoted text here]\n\n",
      "This means that [explanation].\n\n",
      "Here is the complete documentation:\n\n",
      rules_content
    )

    # Create the chat object
    chat <- ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model = model
    )

    # Set up shinychat reactive listener
    observeEvent(input$chat_user_input, {
      stream <- chat$stream_async(input$chat_user_input)
      shinychat::chat_append("chat", stream, session = session)
    })
  })
}

## To be copied in the UI
# mod_rules_ui("rules_1",
#              title = "Touch Football Rules Assistant",
#              description = "Ask me anything about the co-ed touch football rules...",
#              quick_questions = list(
#                "What are the female involvement requirements?",
#                "How does scoring work?",
#                "What are the timeout rules?"
#              ))

## To be copied in the server
# mod_rules_server("rules_1",
#                  rules_content = rules_markdown_content,
#                  rules_type = "co-ed touch football rules")
