#' schedule UI Function
#'
#' @description A shiny Module for displaying game schedule with filters.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "schedule-container",
      # Filter button and active filters display
      div(
        class = "schedule-filter-controls mb-3",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "filter-button-container",
            actionButton(
              ns("show_filters"),
              "Filters",
              icon = icon("filter"),
              class = "btn btn-outline-primary"
            )
          ),
          div(
            class = "active-filters-summary",
            uiOutput(ns("active_filters_display"))
          )
        )
      ),
      # Schedule display
      div(
        class = "schedule-display",
        uiOutput(ns("schedule_table"))
      )
    )
  )
}

#' schedule Server Functions
#'
#' @param db_conn Database connection
#' @param league_id League ID to get schedule for (can be reactive)
#' @param default_teams Optional reactive vector of team IDs to pre-filter by default
#' @noRd
mod_schedule_server <- function(
  id,
  db_conn,
  league_id,
  default_teams = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ensure default_teams is reactive
    default_teams_reactive <- if (is.reactive(default_teams)) {
      default_teams
    } else {
      reactive(default_teams)
    }

    # Reactive filter state
    filter_state <- reactiveValues(
      divisions = NULL,
      teams = NULL, # Will be set by observe below
      dates = NULL
    )

    # Set initial filter state based on default_teams
    observe({
      teams <- default_teams_reactive()
      filter_state$teams <- teams
    })

    # Temporary filter state for modal
    temp_filter_state <- reactiveValues(
      divisions = NULL,
      teams = NULL, # Will be set by observe below
      dates = NULL
    )

    # Set initial temp filter state based on default_teams
    observe({
      teams <- default_teams_reactive()
      temp_filter_state$teams <- teams
    })

    # Search terms for filtering options in modal
    search_terms <- reactiveValues(
      divisions = "",
      teams = "",
      dates = ""
    )

    # Modal handlers
    observeEvent(input$show_filters, {
      # Copy current filters to temp state
      temp_filter_state$divisions <- filter_state$divisions
      temp_filter_state$teams <- filter_state$teams
      temp_filter_state$dates <- filter_state$dates

      # Reset search terms
      search_terms$divisions <- ""
      search_terms$teams <- ""
      search_terms$dates <- ""

      # Show modal using standard Shiny pattern
      showModal(modalDialog(
        title = "Filter Games",
        size = "m",
        easyClose = TRUE,
        fluidRow(
          column(
            12,
            h6("Division"),
            uiOutput(ns("division_filter_options"))
          )
        ),
        hr(),
        fluidRow(
          column(
            12,
            h6("Team"),
            uiOutput(ns("team_filter_options"))
          )
        ),
        hr(),
        fluidRow(
          column(
            12,
            h6("Date"),
            uiOutput(ns("date_filter_options"))
          )
        ),
        footer = div(
          class = "d-flex justify-content-between w-100",
          actionButton(
            ns("clear_filters"),
            "Clear All",
            class = "btn btn-outline-secondary btn-sm"
          ),
          div(
            tags$button(
              type = "button",
              class = "btn btn-secondary btn-sm me-2",
              `data-bs-dismiss` = "modal",
              "Cancel"
            ),
            actionButton(
              ns("apply_filters"),
              "Apply Filters",
              class = "btn btn-primary btn-sm"
            )
          )
        )
      ))
    })

    observeEvent(input$apply_filters, {
      # Apply temp filters to actual filter state
      filter_state$divisions <- temp_filter_state$divisions
      filter_state$teams <- temp_filter_state$teams
      filter_state$dates <- temp_filter_state$dates

      # Hide modal
      removeModal()
    })

    observeEvent(input$clear_filters, {
      temp_filter_state$divisions <- NULL
      temp_filter_state$teams <- NULL
      temp_filter_state$dates <- NULL

      # Clear search terms
      search_terms$divisions <- ""
      search_terms$teams <- ""
      search_terms$dates <- ""

      # Update the checkbox inputs
      updateCheckboxGroupInput(
        session,
        "temp_divisions",
        selected = character(0)
      )
      updateCheckboxGroupInput(session, "temp_teams", selected = character(0))
      updateCheckboxGroupInput(session, "temp_dates", selected = character(0))

      # Update search inputs
      updateTextInput(session, "division_search", value = "")
      updateTextInput(session, "team_search", value = "")
      updateTextInput(session, "date_search", value = "")
    })

    # Auto-refresh timer
    timer <- reactiveTimer(60000)

    # Get all schedule data (refreshed periodically)
    all_schedule_data <- reactive({
      # Invalidate when timer ticks
      timer()

      req(db_conn)

      # Handle both reactive and non-reactive league_id
      current_league_id <- if (is.reactive(league_id)) {
        req(league_id())
        league_id()
      } else {
        req(league_id)
        league_id
      }

      query <- "
        SELECT 
          g.id as game_id,
          g.start_time,
          g.game_type,
          g.score_home,
          g.score_away,
          g.home_result,
          g.away_result,
          g.home_team_id,
          g.away_team_id,
          g.division_id,
          ht.name as home_team,
          at.name as away_team,
          ht.logo as home_logo,
          at.logo as away_logo,
          d.name as division_name,
          l.name as location_name
        FROM game g
        JOIN division d ON g.division_id = d.id
        JOIN team ht ON g.home_team_id = ht.id
        JOIN team at ON g.away_team_id = at.id
        LEFT JOIN location l ON g.location_id = l.id
        WHERE d.league_id = $1
        ORDER BY DATE(g.start_time) DESC, g.start_time, d.rank, g.id
        "

      tryCatch(
        {
          games <- DBI::dbGetQuery(db_conn, query, list(current_league_id))

          if (nrow(games) > 0) {
            games <- games |>
              dplyr::mutate(
                dplyr::across(
                  .cols = dplyr::where(~ inherits(.x, "integer64")),
                  .fns = as.integer
                ),
                start_time = as.POSIXct(start_time),
                # Convert to EDT timezone
                start_time_edt = format(
                  as.POSIXct(start_time, tz = "UTC"),
                  tz = "America/New_York",
                  usetz = TRUE
                ),
                game_date = as.Date(start_time),
                game_time = paste(
                  format(
                    as.POSIXct(start_time, tz = "UTC"),
                    "%a, %b %d",
                    tz = "America/New_York"
                  ),
                  gsub(
                    "^0",
                    "",
                    format(
                      as.POSIXct(start_time, tz = "UTC"),
                      "%I:%M %p",
                      tz = "America/New_York"
                    )
                  )
                )
              )
          }

          games
        },
        error = function(e) {
          showNotification(
            paste("Error loading schedule:", e$message),
            type = "error"
          )
          data.frame()
        }
      )
    })

    # Get available filter choices based on current data and temp filter selections
    available_divisions <- reactive({
      games <- all_schedule_data()
      if (nrow(games) == 0) {
        return(data.frame(id = numeric(0), name = character(0)))
      }

      # Filter based on temp filter selections for modal
      filtered_games <- games

      if (
        !is.null(temp_filter_state$teams) && length(temp_filter_state$teams) > 0
      ) {
        team_ids <- as.integer(temp_filter_state$teams)
        filtered_games <- filtered_games[
          filtered_games$home_team_id %in%
            team_ids |
            filtered_games$away_team_id %in% team_ids,
        ]
      }

      if (
        !is.null(temp_filter_state$dates) && length(temp_filter_state$dates) > 0
      ) {
        filtered_games <- filtered_games[
          as.character(filtered_games$game_date) %in% temp_filter_state$dates,
        ]
      }

      unique(filtered_games[, c("division_id", "division_name")])[
        order(
          unique(filtered_games[, c(
            "division_id",
            "division_name"
          )])$division_name
        ),
      ]
    })

    available_teams <- reactive({
      games <- all_schedule_data()
      if (nrow(games) == 0) {
        return(data.frame(id = numeric(0), name = character(0)))
      }

      # Filter based on temp filter selections for modal
      filtered_games <- games

      if (
        !is.null(temp_filter_state$divisions) &&
          length(temp_filter_state$divisions) > 0
      ) {
        division_ids <- as.integer(temp_filter_state$divisions)
        filtered_games <- filtered_games[
          filtered_games$division_id %in% division_ids,
        ]
      }

      if (
        !is.null(temp_filter_state$dates) && length(temp_filter_state$dates) > 0
      ) {
        filtered_games <- filtered_games[
          as.character(filtered_games$game_date) %in% temp_filter_state$dates,
        ]
      }

      # Get all teams from home and away
      home_teams <- unique(filtered_games[, c("home_team_id", "home_team")])
      away_teams <- unique(filtered_games[, c("away_team_id", "away_team")])

      names(home_teams) <- c("id", "name")
      names(away_teams) <- c("id", "name")

      all_teams <- rbind(home_teams, away_teams)
      unique(all_teams)[order(unique(all_teams)$name), ]
    })

    available_dates <- reactive({
      games <- all_schedule_data()
      if (nrow(games) == 0) {
        return(data.frame(game_date = character(0)))
      }

      # Filter based on temp filter selections for modal
      filtered_games <- games

      if (
        !is.null(temp_filter_state$divisions) &&
          length(temp_filter_state$divisions) > 0
      ) {
        division_ids <- as.integer(temp_filter_state$divisions)
        filtered_games <- filtered_games[
          filtered_games$division_id %in% division_ids,
        ]
      }

      if (
        !is.null(temp_filter_state$teams) && length(temp_filter_state$teams) > 0
      ) {
        team_ids <- as.integer(temp_filter_state$teams)
        filtered_games <- filtered_games[
          filtered_games$home_team_id %in%
            team_ids |
            filtered_games$away_team_id %in% team_ids,
        ]
      }

      unique_dates <- unique(filtered_games$game_date)
      data.frame(game_date = sort(unique_dates, decreasing = TRUE))
    })

    # Render filter options for modal
    output$division_filter_options <- renderUI({
      divisions <- available_divisions()

      if (nrow(divisions) == 0) {
        return(p("No divisions available", class = "text-muted"))
      }

      # Filter divisions based on search term
      search_term <- search_terms$divisions
      if (!is.null(search_term) && nzchar(search_term)) {
        divisions <- divisions[
          grepl(search_term, divisions$division_name, ignore.case = TRUE),
        ]
      }

      div(
        # Search input
        textInput(
          ns("division_search"),
          label = NULL,
          placeholder = "Search divisions...",
          value = search_terms$divisions
        ),
        # Filtered checkbox options
        div(
          class = "filter-option-group",
          if (nrow(divisions) > 0) {
            checkboxGroupInput(
              ns("temp_divisions"),
              label = NULL,
              choices = setNames(
                divisions$division_id,
                divisions$division_name
              ),
              selected = temp_filter_state$divisions,
              inline = FALSE
            )
          } else {
            p("No divisions match your search", class = "text-muted small")
          }
        )
      )
    })

    output$team_filter_options <- renderUI({
      teams <- available_teams()

      if (nrow(teams) == 0) {
        return(p("No teams available", class = "text-muted"))
      }

      # Filter teams based on search term
      search_term <- search_terms$teams
      if (!is.null(search_term) && nzchar(search_term)) {
        teams <- teams[grepl(search_term, teams$name, ignore.case = TRUE), ]
      }

      div(
        # Search input
        textInput(
          ns("team_search"),
          label = NULL,
          placeholder = "Search teams...",
          value = search_terms$teams
        ),
        # Filtered checkbox options
        div(
          class = "filter-option-group",
          if (nrow(teams) > 0) {
            checkboxGroupInput(
              ns("temp_teams"),
              label = NULL,
              choices = setNames(teams$id, teams$name),
              selected = temp_filter_state$teams,
              inline = FALSE
            )
          } else {
            p("No teams match your search", class = "text-muted small")
          }
        )
      )
    })

    output$date_filter_options <- renderUI({
      dates <- available_dates()

      if (nrow(dates) == 0) {
        return(p("No dates available", class = "text-muted"))
      }

      date_choices <- setNames(
        as.character(dates$game_date),
        format(dates$game_date, "%B %d, %Y")
      )

      # Filter dates based on search term
      search_term <- search_terms$dates
      if (!is.null(search_term) && nzchar(search_term)) {
        matching_dates <- grepl(
          search_term,
          names(date_choices),
          ignore.case = TRUE
        )
        date_choices <- date_choices[matching_dates]
      }

      div(
        # Search input
        textInput(
          ns("date_search"),
          label = NULL,
          placeholder = "Search dates...",
          value = search_terms$dates
        ),
        # Filtered checkbox options
        div(
          class = "filter-option-group",
          if (length(date_choices) > 0) {
            checkboxGroupInput(
              ns("temp_dates"),
              label = NULL,
              choices = date_choices,
              selected = temp_filter_state$dates,
              inline = FALSE
            )
          } else {
            p("No dates match your search", class = "text-muted small")
          }
        )
      )
    })

    # Update temp filter state when checkboxes change
    observeEvent(
      input$temp_divisions,
      {
        temp_filter_state$divisions <- input$temp_divisions
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$temp_teams,
      {
        temp_filter_state$teams <- input$temp_teams
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$temp_dates,
      {
        temp_filter_state$dates <- input$temp_dates
      },
      ignoreNULL = FALSE
    )

    # Update search terms when users type in search inputs
    observeEvent(
      input$division_search,
      {
        search_terms$divisions <- input$division_search
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    observeEvent(
      input$team_search,
      {
        search_terms$teams <- input$team_search
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    observeEvent(
      input$date_search,
      {
        search_terms$dates <- input$date_search
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Render active filters summary
    output$active_filters_display <- renderUI({
      active_filters <- list()

      if (
        !is.null(filter_state$divisions) && length(filter_state$divisions) > 0
      ) {
        games <- all_schedule_data()
        if (nrow(games) > 0) {
          div_names <- unique(games[
            games$division_id %in% filter_state$divisions,
            "division_name"
          ])
          active_filters$divisions <- paste(
            "Divisions:",
            paste(div_names, collapse = ", ")
          )
        }
      }

      if (!is.null(filter_state$teams) && length(filter_state$teams) > 0) {
        games <- all_schedule_data()
        if (nrow(games) > 0) {
          home_teams <- games[
            games$home_team_id %in% filter_state$teams,
            c("home_team_id", "home_team")
          ]
          away_teams <- games[
            games$away_team_id %in% filter_state$teams,
            c("away_team_id", "away_team")
          ]
          names(home_teams) <- c("id", "name")
          names(away_teams) <- c("id", "name")
          all_teams <- unique(rbind(home_teams, away_teams))
          team_names <- all_teams$name[all_teams$id %in% filter_state$teams]
          active_filters$teams <- paste(
            "Teams:",
            paste(team_names, collapse = ", ")
          )
        }
      }

      if (!is.null(filter_state$dates) && length(filter_state$dates) > 0) {
        date_labels <- format(as.Date(filter_state$dates), "%b %d")
        active_filters$dates <- paste(
          "Dates:",
          paste(date_labels, collapse = ", ")
        )
      }

      if (length(active_filters) == 0) {
        return(span("All games", class = "text-muted small"))
      }

      div(
        class = "active-filters small",
        lapply(active_filters, function(filter) {
          span(filter, class = "badge bg-secondary me-1")
        })
      )
    })

    # Auto-refresh timer
    timer <- reactiveTimer(60000)

    # Get filtered schedule data with auto-refresh
    schedule_data <- reactive({
      games <- all_schedule_data()

      if (nrow(games) == 0) {
        return(games)
      }

      # Apply filters on the R side using filter_state
      filtered_games <- games

      # Apply division filter
      if (
        !is.null(filter_state$divisions) && length(filter_state$divisions) > 0
      ) {
        division_ids <- as.integer(filter_state$divisions)
        filtered_games <- filtered_games[
          filtered_games$division_id %in% division_ids,
        ]
      }

      # Apply team filter
      if (!is.null(filter_state$teams) && length(filter_state$teams) > 0) {
        team_ids <- as.integer(filter_state$teams)
        filtered_games <- filtered_games[
          filtered_games$home_team_id %in%
            team_ids |
            filtered_games$away_team_id %in% team_ids,
        ]
      }

      # Apply date filter
      if (!is.null(filter_state$dates) && length(filter_state$dates) > 0) {
        filtered_games <- filtered_games[
          as.character(filtered_games$game_date) %in% filter_state$dates,
        ]
      }

      filtered_games
    })

    # Render schedule table
    output$schedule_table <- renderUI({
      games <- schedule_data()

      if (nrow(games) == 0) {
        return(
          div(
            class = "no-games text-center mt-4",
            h4("No games found"),
            p("Try adjusting your filters or check back later.")
          )
        )
      }

      # Group games by date
      games_by_date <- split(games, games$game_date)

      # Sort dates in descending order (latest first)
      sorted_dates <- sort(names(games_by_date), decreasing = TRUE)

      # Create tables for each date in descending order
      date_tables <- lapply(sorted_dates, function(date) {
        date_games <- games_by_date[[date]]
        formatted_date <- format(as.Date(date), "%A, %B %d, %Y")

        # Create game rows with ticker-style display
        game_rows <- lapply(seq_len(nrow(date_games)), function(i) {
          game <- date_games[i, ]

          # Create ticker data for this game
          ticker_data <- list(
            away_name = game$away_team,
            home_name = game$home_team,
            away_logo = game$away_logo,
            home_logo = game$home_logo,
            score_away = if (!is.na(game$score_away)) game$score_away else 0,
            score_home = if (!is.na(game$score_home)) game$score_home else 0,
            possession = determine_possession(game),
            half = if (!is.na(game$home_result) && !is.na(game$away_result)) {
              "Final"
            } else {
              ""
            },
            down_girl_plays_available = "",
            timeouts_away = 3,
            timeouts_home = 3,
            record_away = "",
            record_home = ""
          )

          # Determine game status and times
          status_info <- determine_game_status(game)

          div(
            class = "schedule-game-ticker mb-3",
            # Game header with time and location
            div(
              class = "game-header p-2 bg-light border-bottom",
              # First row: Start time and location
              div(
                class = "d-flex justify-content-between align-items-center mb-1",
                div(
                  class = "game-start-time text-muted small",
                  game$game_time
                ),
                div(
                  class = "game-location text-muted small",
                  if (
                    !is.null(game$location_name) && !is.na(game$location_name)
                  ) {
                    game$location_name
                  } else {
                    ""
                  }
                )
              ),
              # Second row: Status and division
              div(
                class = "d-flex justify-content-between align-items-center",
                div(
                  class = "game-status fw-bold text-body",
                  status_info$display_time
                ),
                div(
                  class = "game-division text-muted small",
                  game$division_name
                )
              )
            ),
            # Ticker display
            create_schedule_ticker(ticker_data, status_info)
          )
        })

        div(
          class = "date-section mb-4",
          h5(class = "date-header mb-3", formatted_date),
          div(class = "games", game_rows)
        )
      })

      div(class = "schedule-content", date_tables)
    })
  })
}

#' Create team display with logo
#'
#' @param team_name Team name
#' @param team_logo Team logo filename
#' @return HTML for team display
#'
#' @noRd
create_team_display <- function(team_name, team_logo) {
  logo_html <- if (
    !is.null(team_logo) && !is.na(team_logo) && nzchar(team_logo)
  ) {
    logo_url <- ifelse(
      grepl("/", team_logo),
      team_logo,
      file.path(
        Sys.getenv("STORAGE_BASE_URL"),
        "images",
        "logos",
        team_logo
      )
    )
    paste0(
      '<img src="',
      logo_url,
      '" style="height: 1.5em; width: 1.5em; object-fit: contain; margin-right: 0.5em;">'
    )
  } else {
    ""
  }

  HTML(paste0(
    '<div class="team-display d-flex align-items-center">',
    logo_html,
    '<span>',
    team_name,
    '</span>',
    "</div>"
  ))
}

#' Create ticker-style display for a game
#'
#' @param ticker_data List with ticker display data
#' @param status_info List with game status information
#' @return HTML for ticker display
#'
#' @noRd
create_schedule_ticker <- function(ticker_data, status_info) {
  # Create team logo HTML
  away_logo_html <- create_logo_html(ticker_data$away_logo, "team-logo")
  home_logo_html <- create_logo_html(ticker_data$home_logo, "team-logo")

  # Add possession indicators to scores
  away_score_display <- if (ticker_data$possession == "Away") {
    paste(ticker_data$score_away, "\u25C2")
  } else {
    as.character(ticker_data$score_away)
  }

  home_score_display <- if (ticker_data$possession == "Home") {
    paste("\u25B8", ticker_data$score_home)
  } else {
    as.character(ticker_data$score_home)
  }

  div(
    class = "schedule-ticker-bar border rounded",
    # Top row: logos, team info, scores
    div(
      class = "ticker-top-row p-2",
      div(class = "ticker-left-logo", HTML(away_logo_html)),
      div(
        class = "ticker-left-info",
        div(class = "team-name fw-bold", ticker_data$away_name),
        div(class = "team-record text-muted small", ticker_data$record_away)
      ),
      div(class = "ticker-left-score fs-4 fw-bold", away_score_display),
      div(class = "ticker-center-spacer"),
      div(class = "ticker-right-score fs-4 fw-bold", home_score_display),
      div(
        class = "ticker-right-info text-end",
        div(class = "team-name fw-bold", ticker_data$home_name),
        div(class = "team-record text-muted small", ticker_data$record_home)
      ),
      div(class = "ticker-right-logo", HTML(home_logo_html))
    ),
    # Bottom row: game status
    if (status_info$show_status && !status_info$is_completed) {
      div(
        class = "ticker-bottom-row p-2 border-top bg-light",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "game-status",
            if (ticker_data$half != "") {
              span(class = "badge bg-secondary", ticker_data$half)
            }
          ),
          if (status_info$is_live) {
            div(
              class = "live-indicator",
              span(class = "badge bg-danger", "LIVE"),
              span(class = "text-muted small ms-2", "Updates every 10s")
            )
          }
        )
      )
    }
  )
}

#' Create logo HTML
#'
#' @param logo Logo filename or URL
#' @param css_class CSS class for the logo
#' @return HTML string
#'
#' @noRd
create_logo_html <- function(logo, css_class = "team-logo") {
  if (!is.null(logo) && !is.na(logo) && nzchar(logo)) {
    logo_url <- ifelse(
      grepl("/", logo),
      logo,
      file.path(
        Sys.getenv("STORAGE_BASE_URL"),
        "images",
        "logos",
        logo
      )
    )
    paste0('<img src="', logo_url, '" class="', css_class, '">')
  } else {
    paste0('<div class="', css_class, '"></div>')
  }
}

#' Determine game possession based on game data
#'
#' @param game Game data row
#' @return String indicating possession ("Home", "Away", or "")
#'
#' @noRd
determine_possession <- function(game) {
  # For completed games, no possession
  if (!is.na(game$home_result) && !is.na(game$away_result)) {
    return("")
  }

  # For scheduled games, randomly assign or use some logic
  # In a real implementation, this would come from the game state
  return("")
}

#' Determine game status and display information
#'
#' @param game Game data row
#' @return List with status information
#'
#' @noRd
determine_game_status <- function(game) {
  current_time <- Sys.time()
  game_start <- as.POSIXct(game$start_time)

  is_completed <- !is.na(game$home_result) && !is.na(game$away_result)
  is_live <- !is_completed &&
    current_time >= game_start &&
    current_time <= (game_start + 3600) # Assume 1 hour max game time

  if (is_completed) {
    display_time <- "Final"
  } else if (is_live) {
    # Calculate elapsed time
    elapsed_mins <- as.numeric(difftime(
      current_time,
      game_start,
      units = "mins"
    ))
    display_time <- paste0("Live - ", floor(elapsed_mins), " min")
  } else if (current_time < game_start) {
    display_time <- format(game_start, "%I:%M %p")
  } else {
    display_time <- "Completed"
  }

  list(
    display_time = display_time,
    is_live = is_live,
    is_completed = is_completed,
    show_status = TRUE
  )
}

## To be copied in the UI
# mod_schedule_ui("schedule_1")

## To be copied in the server
# mod_schedule_server("schedule_1", db_conn, league_id)
