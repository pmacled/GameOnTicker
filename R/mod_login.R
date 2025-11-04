#' login/register UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_auth"), "Register/Login", style = "width:180px;"),
    uiOutput(ns("auth_modal"))
  )
}

#' login status UI (for navbar)
#'
#' @noRd
login_status_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("login_status"))
}

#' login/register Server Functions
#'
#' @noRd
mod_login_server <- function(id, db_conn, user_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check for stored login on module initialization
    observe({
      session$sendCustomMessage("getStoredUser", "")
    })

    # Handle stored user data from localStorage
    observeEvent(input$storedUser, {
      if (!is.null(input$storedUser) && input$storedUser != "") {
        tryCatch(
          {
            stored_data <- jsonlite::fromJSON(input$storedUser)
            if (!is.null(stored_data$user_id)) {
              # Verify user still exists in database
              user <- DBI::dbGetQuery(
                db_conn,
                "SELECT * FROM public.user WHERE id = $1",
                params = list(as.integer(stored_data$user_id))
              )
              if (nrow(user) == 1) {
                user_rv(user)
              } else {
                # User no longer exists, clear storage
                session$sendCustomMessage("clearStoredUser", "")
              }
            }
          },
          error = function(e) {
            # Invalid stored data, clear it
            session$sendCustomMessage("clearStoredUser", "")
          }
        )
      }
    })

    output$login_status <- renderUI({
      if (is.null(user_rv())) {
        actionLink(ns("show_auth"), "Login/Register", class = "nav-link")
      } else {
        actionLink(
          ns("sign_out"),
          sprintf("Sign Out (%s)", user_rv()$username),
          class = "nav-link"
        )
      }
    })

    observeEvent(input$show_auth, {
      # Clear any previous status messages when opening the modal
      output$auth_status <- renderText("")

      showModal(modalDialog(
        shinyWidgets::radioGroupButtons(
          inputId = ns("auth_mode"),
          label = NULL,
          choices = c("Login", "Register"),
          selected = "Login",
          justified = TRUE,
          size = "sm"
        ),
        textInput(ns("auth_username"), "Username", width = "150px"),
        passwordInput(ns("auth_password"), "Password", width = "150px"),
        checkboxInput(ns("remember_me"), "Keep me signed in", value = TRUE),
        actionButton(
          ns("auth_submit"),
          "Submit",
          style = "font-size:12px; padding:2px 10px;"
        ),
        textOutput(ns("auth_status")),
        size = "s",
        easyClose = TRUE,
        style = "font-size:13px;",
        footer = actionButton(
          ns("dismiss_modal"),
          "Dismiss",
          style = "font-size:12px; padding:2px 10px;"
        )
      ))
    })

    observeEvent(input$auth_submit, {
      req(input$auth_username, input$auth_password)
      mode <- input$auth_mode
      if (mode == "Register") {
        # Registration logic
        exists <- DBI::dbGetQuery(
          db_conn,
          "SELECT COUNT(*) FROM public.user WHERE LOWER(username) = LOWER($1)",
          params = list(input$auth_username)
        )[[1]] >
          0
        if (exists) {
          output$auth_status <- renderText("Username already taken.")
        } else {
          hash <- digest::sha1(input$auth_password)
          DBI::dbExecute(
            db_conn,
            "INSERT INTO public.user (username, password_hash) VALUES ($1, $2)",
            params = list(input$auth_username, hash)
          )
          output$auth_status <- renderText("Account created.")
        }
      } else {
        # Login logic
        hash <- digest::sha1(input$auth_password)
        user <- DBI::dbGetQuery(
          db_conn,
          "SELECT * FROM public.user WHERE LOWER(username) = LOWER($1) AND password_hash = $2",
          params = list(input$auth_username, hash)
        )
        if (nrow(user) == 1) {
          user_rv(user)
          # Store user info in localStorage for persistent login (only if remember_me is checked)
          if (isTRUE(input$remember_me)) {
            user_data <- list(
              user_id = user$id[1],
              username = user$username[1]
            )
            json_data <- jsonlite::toJSON(user_data)
            session$sendCustomMessage("storeUser", json_data)
          }
          output$auth_status <- renderText("Login successful.")
          removeModal()
        } else {
          output$auth_status <- renderText("Invalid credentials.")
        }
      }
    })

    observeEvent(input$dismiss_modal, {
      removeModal()
    })

    observeEvent(input$sign_out, {
      user_rv(NULL)
      # Clear stored user data from localStorage
      session$sendCustomMessage("clearStoredUser", "")
      # Clear any status messages when signing out
      output$auth_status <- renderText("")
    })
  })
}

## To be copied in the UI
# mod_login_ui("login_1")

## To be copied in the server
# mod_login_server("login_1")
