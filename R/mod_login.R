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
        # Clean up expired cookies during auto-login attempts
        tryCatch(
          {
            cleanup_expired_cookies(db_conn)
          },
          error = function(e) {
            # Continue with login validation even if cleanup fails
            warning("Cookie cleanup failed during auto-login: ", e$message)
          }
        )

        tryCatch(
          {
            stored_data <- jsonlite::fromJSON(input$storedUser)

            # Validate using cookie-based authentication
            if (
              !is.null(stored_data$cookie) &&
                !is.null(stored_data$user_id) &&
                !is.null(stored_data$username)
            ) {
              user <- validate_login_cookie(
                db_conn,
                stored_data$user_id,
                stored_data$username,
                stored_data$cookie
              )

              if (!is.null(user)) {
                user_rv(user)
              } else {
                # Invalid cookie, clear storage
                session$sendCustomMessage("clearStoredUser", "")
              }
            } else {
              # Invalid or incomplete data format, clear storage
              session$sendCustomMessage("clearStoredUser", "")
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

          # Get the newly created user
          user <- DBI::dbGetQuery(
            db_conn,
            "SELECT * FROM public.user WHERE LOWER(username) = LOWER($1)",
            params = list(input$auth_username)
          )

          if (nrow(user) == 1) {
            user_rv(user)

            # Create login cookie if remember_me is checked
            if (isTRUE(input$remember_me)) {
              ip_address <- session$request$HTTP_X_FORWARDED_FOR %||%
                session$request$HTTP_X_REAL_IP %||%
                session$request$REMOTE_ADDR %||%
                session$clientData$url_hostname %||%
                "unknown"

              cookie_token <- create_login_cookie(
                db_conn,
                user$id[1],
                expires_days = 30,
                ip_address = ip_address
              )

              if (!is.null(cookie_token)) {
                user_data <- list(
                  user_id = user$id[1],
                  username = user$username[1],
                  cookie = cookie_token
                )
                json_data <- jsonlite::toJSON(user_data)
                session$sendCustomMessage("storeUser", json_data)
              }
            }

            output$auth_status <- renderText("Account created and logged in.")
            removeModal()
          } else {
            output$auth_status <- renderText(
              "Account created but login failed."
            )
          }
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
            ip_address <- session$request$HTTP_X_FORWARDED_FOR %||%
              session$request$HTTP_X_REAL_IP %||%
              session$request$REMOTE_ADDR %||%
              session$clientData$url_hostname %||%
              "unknown"

            cookie_token <- create_login_cookie(
              db_conn,
              user$id[1],
              expires_days = 30,
              ip_address = ip_address
            )

            if (!is.null(cookie_token)) {
              user_data <- list(
                user_id = user$id[1],
                username = user$username[1],
                cookie = cookie_token
              )
              json_data <- jsonlite::toJSON(user_data)
              session$sendCustomMessage("storeUser", json_data)
            }
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
      current_user <- user_rv()

      # Get stored cookie data to invalidate it
      session$sendCustomMessage("getStoredUser", "")

      # Invalidate cookies based on available data
      if (!is.null(input$storedUser) && input$storedUser != "") {
        tryCatch(
          {
            stored_data <- jsonlite::fromJSON(input$storedUser)
            if (!is.null(stored_data$cookie)) {
              # Invalidate the specific cookie
              invalidate_login_cookie(db_conn, stored_data$cookie)
            } else if (!is.null(current_user) && !is.null(current_user$id)) {
              # Fallback: invalidate all cookies for this user
              invalidate_user_cookies(db_conn, current_user$id)
            }
          },
          error = function(e) {
            # If we can't get cookie info, invalidate all user cookies as fallback
            if (!is.null(current_user) && !is.null(current_user$id)) {
              invalidate_user_cookies(db_conn, current_user$id)
            }
          }
        )
      } else if (!is.null(current_user) && !is.null(current_user$id)) {
        # No stored data, but we have current user - invalidate all their cookies
        invalidate_user_cookies(db_conn, current_user$id)
      }

      # Clear user state and localStorage
      user_rv(NULL)
      session$sendCustomMessage("clearStoredUser", "")
      output$auth_status <- renderText("")
    })
  })
}

## To be copied in the UI
# mod_login_ui("login_1")

## To be copied in the server
# mod_login_server("login_1")
