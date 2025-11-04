#' home UI Function
#'
#' @description A shiny Module for the Home panel showing upcoming ref games
#' and favorited team games.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "home-container",

      # Manage favorites modal
      uiOutput(ns("favorites_modal")),

      # Upcoming ref games section
      uiOutput(ns("ref_games_section")),

      # Manage favorites button (always visible for logged in users)
      conditionalPanel(
        condition = "output.user_logged_in",
        ns = ns,
        div(
          class = "text-center mb-4",
          actionButton(
            ns("manage_favorites"),
            "Manage Favorites",
            class = "btn-outline-primary",
            icon = icon("heart")
          )
        )
      ),

      # Favorited teams section
      uiOutput(ns("favorite_teams_section")),

      # Standings section for favorite team divisions
      uiOutput(ns("favorite_standings_section")),

      # Schedule section for favorite teams
      uiOutput(ns("favorite_schedule_section"))
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, db_conn, user_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive trigger for when favorites are saved
    favorites_trigger <- reactiveVal(0)

    # Output to track if user is logged in (for conditionalPanel)
    output$user_logged_in <- reactive({
      !is.null(user_rv())
    })
    outputOptions(output, "user_logged_in", suspendWhenHidden = FALSE)

    # Reactive for upcoming ref games
    upcoming_ref_games <- reactive({
      req(user_rv())
      user_id <- as.integer(user_rv()$id)

      query <- "
        SELECT g.id, g.start_time, g.score_home, g.score_away,
               ht.name as home_team, at.name as away_team,
               l.name as location_name
        FROM public.game g
        JOIN public.game_referee gr ON g.id = gr.game_id
        LEFT JOIN public.team ht ON g.home_team_id = ht.id
        LEFT JOIN public.team at ON g.away_team_id = at.id
        LEFT JOIN public.location l ON g.location_id = l.id
        WHERE gr.user_id = $1 
          AND g.start_time >= NOW()
        ORDER BY g.start_time
        LIMIT 5
      "

      DBI::dbGetQuery(db_conn, query, params = list(user_id))
    })

    # Reactive to check if user has ever been a referee
    user_has_referee_history <- reactive({
      req(user_rv())
      user_id <- as.integer(user_rv()$id)

      query <- "
        SELECT COUNT(*) as referee_count
        FROM public.game_referee
        WHERE user_id = $1
      "

      result <- DBI::dbGetQuery(db_conn, query, params = list(user_id))
      as.integer(result$referee_count) > 0
    })

    # Reactive for upcoming favorite team games
    upcoming_favorite_games <- reactive({
      req(user_rv())
      user_id <- as.integer(user_rv()$id)

      query <- "
        SELECT g.id, g.start_time, g.score_home, g.score_away,
               ht.name as home_team, at.name as away_team,
               l.name as location_name,
               CASE 
                 WHEN g.home_team_id = uft.team_id THEN 'home'
                 WHEN g.away_team_id = uft.team_id THEN 'away'
               END as favorite_side
        FROM public.game g
        JOIN public.user_favorite_team uft ON (g.home_team_id = uft.team_id OR g.away_team_id = uft.team_id)
        LEFT JOIN public.team ht ON g.home_team_id = ht.id
        LEFT JOIN public.team at ON g.away_team_id = at.id
        LEFT JOIN public.location l ON g.location_id = l.id
        WHERE uft.user_id = $1 
          AND g.start_time >= NOW()
        ORDER BY g.start_time
        LIMIT 10
      "

      DBI::dbGetQuery(db_conn, query, params = list(user_id))
    })

    # Reactive for all teams (for favorites management)
    all_teams <- reactive({
      DBI::dbGetQuery(db_conn, "SELECT id, name FROM public.team ORDER BY name")
    })

    # Reactive for user's current favorite teams
    user_favorites <- reactive({
      req(user_rv())
      # Force re-query when manage_favorites button is clicked or favorites are saved
      input$manage_favorites
      favorites_trigger()

      user_id <- as.integer(user_rv()$id)

      query <- "
        SELECT t.id, t.name
        FROM public.team t
        JOIN public.user_favorite_team uft ON t.id = uft.team_id
        WHERE uft.user_id = $1
        ORDER BY t.name
      "

      DBI::dbGetQuery(db_conn, query, params = list(user_id))
    })

    # Reactive for divisions containing favorite teams
    favorite_team_divisions <- reactive({
      req(user_rv())
      favorites <- user_favorites()

      if (nrow(favorites) == 0) {
        return(data.frame())
      }

      # Get unique divisions for favorite teams
      team_ids <- paste0("$", 1:nrow(favorites), collapse = ", ")
      query <- paste0(
        "
        SELECT DISTINCT d.id, d.name, d.rank, d.league_id
        FROM public.division d
        JOIN public.team_registration tr ON d.id = tr.division_id
        WHERE tr.team_id IN (",
        team_ids,
        ")
        ORDER BY d.league_id, d.rank
      "
      )

      DBI::dbGetQuery(
        db_conn,
        query,
        params = as.list(as.integer(favorites$id))
      )
    })

    # Reactive for standings data for favorite team divisions
    favorite_standings_data <- reactive({
      divisions <- favorite_team_divisions()

      if (nrow(divisions) == 0) {
        return(NULL)
      }

      # Get all completed games for these divisions with team info
      division_placeholders <- paste0("$", 1:nrow(divisions), collapse = ", ")
      games_query <- paste0(
        "
        SELECT 
          g.id as game_id,
          g.home_team_id,
          g.away_team_id,
          g.score_home,
          g.score_away,
          g.division_id,
          g.home_result,
          g.away_result,
          g.start_time,
          ht.name as home_team_name,
          ht.logo as home_team_logo,
          at.name as away_team_name,
          at.logo as away_team_logo
        FROM public.game g
        JOIN public.team ht ON g.home_team_id = ht.id
        JOIN public.team at ON g.away_team_id = at.id
        WHERE g.division_id IN (",
        division_placeholders,
        ")
          AND g.home_result IS NOT NULL 
          AND g.away_result IS NOT NULL
        ORDER BY g.start_time
      "
      )

      games <- DBI::dbGetQuery(
        db_conn,
        games_query,
        params = as.list(as.integer(divisions$id))
      )

      # Convert integer64 columns to regular integers for proper R operations
      if (nrow(games) > 0) {
        games <- games |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::where(~ inherits(.x, "integer64")),
              .fns = as.integer
            )
          )
      }

      if (nrow(games) == 0) {
        return(list(
          divisions = divisions,
          standings = data.frame()
        ))
      }

      # Calculate team standings using the same function as the standings module
      standings <- calculate_standings(games, divisions)

      list(
        divisions = divisions,
        standings = standings
      )
    })

    # Render ref games section
    output$ref_games_section <- renderUI({
      if (is.null(user_rv())) {
        return(NULL)
      }

      ref_games <- upcoming_ref_games()
      has_referee_history <- user_has_referee_history()

      # Only show referee section if user has referee history
      if (!has_referee_history) {
        return(NULL)
      }

      if (nrow(ref_games) == 0) {
        return(div(
          class = "mb-4 text-center",
          div(
            class = "card border-light",
            div(
              class = "card-body",
              h5("No Upcoming Referee Games", class = "card-title text-muted")
            )
          )
        ))
      }

      div(
        class = "mb-4",
        h4("Your Upcoming Referee Games", class = "text-primary"),
        div(
          class = "row",
          lapply(1:nrow(ref_games), function(i) {
            game <- ref_games[i, ]
            start_time <- as.POSIXct(game$start_time)

            div(
              class = "col-md-6 col-lg-4 mb-3",
              div(
                class = "card border-primary",
                div(
                  class = "card-body",
                  h6(
                    paste(game$home_team, "vs", game$away_team),
                    class = "card-title"
                  ),
                  p(
                    format(start_time, "%b %d, %Y at %I:%M %p"),
                    class = "card-text text-muted"
                  ),
                  if (!is.na(game$location_name)) {
                    p(game$location_name, class = "card-text small")
                  }
                )
              )
            )
          })
        )
      )
    })

    # Render favorite teams section
    output$favorite_teams_section <- renderUI({
      if (is.null(user_rv())) {
        return(NULL)
      }

      fav_games <- upcoming_favorite_games()

      if (nrow(fav_games) == 0) {
        return(div(
          class = "mb-4 text-center",
          div(
            class = "card border-light",
            div(
              class = "card-body",
              h5(
                "No Upcoming Games for Favorite Teams",
                class = "card-title text-muted"
              )
            )
          )
        ))
      }

      div(
        class = "mb-4",
        h4("Upcoming Games for Your Favorite Teams", class = "text-success"),
        div(
          class = "row",
          lapply(1:nrow(fav_games), function(i) {
            game <- fav_games[i, ]
            start_time <- as.POSIXct(game$start_time)

            # Highlight the favorite team
            if (game$favorite_side == "home") {
              team_display <- tags$strong(game$home_team)
              vs_display <- paste("vs", game$away_team)
            } else {
              team_display <- paste(game$home_team, "vs")
              vs_display <- tags$strong(game$away_team)
            }

            div(
              class = "col-md-6 col-lg-4 mb-3",
              div(
                class = "card border-success",
                div(
                  class = "card-body",
                  h6(
                    tagList(team_display, " ", vs_display),
                    class = "card-title"
                  ),
                  p(
                    format(start_time, "%b %d, %Y at %I:%M %p"),
                    class = "card-text text-muted"
                  ),
                  if (!is.na(game$location_name)) {
                    p(game$location_name, class = "card-text small")
                  }
                )
              )
            )
          })
        )
      )
    })

    # Render schedule section for favorite teams
    output$favorite_schedule_section <- renderUI({
      if (is.null(user_rv())) {
        return(NULL)
      }

      favorites <- user_favorites()

      if (nrow(favorites) == 0) {
        return(NULL)
      }

      div(
        class = "mb-4",
        h4("Favorite Team Schedule", class = "text-info"),
        div(
          class = "schedule-wrapper",
          mod_schedule_ui(ns("favorite_schedule"))
        )
      )
    })

    # Initialize schedule module server for favorite teams
    # Get the league_id - you may need to adjust this based on your app's league structure
    league_id <- reactive({
      league_query <- "SELECT id FROM public.league LIMIT 1"
      league_result <- DBI::dbGetQuery(db_conn, league_query)

      if (nrow(league_result) > 0) {
        as.integer(league_result$id[1])
      } else {
        NULL
      }
    })

    # Get favorite team IDs as reactive
    favorite_team_ids <- reactive({
      req(user_rv())
      favorites <- user_favorites()

      if (nrow(favorites) > 0) {
        as.integer(favorites$id)
      } else {
        NULL
      }
    })

    # Call the schedule module server once with reactive inputs
    mod_schedule_server(
      "favorite_schedule",
      db_conn,
      league_id,
      default_teams = favorite_team_ids
    )

    # Render standings section for favorite team divisions
    output$favorite_standings_section <- renderUI({
      if (is.null(user_rv())) {
        return(NULL)
      }

      standings_data <- favorite_standings_data()

      if (is.null(standings_data) || nrow(standings_data$standings) == 0) {
        return(NULL)
      }

      tryCatch(
        {
          # Create a GT table with division group headers
          gt_table <- create_standings_table(
            standings_data$standings,
            standings_data$divisions
          )

          div(
            class = "mb-4",
            h4("Favorite Team Standings", class = "text-info"),
            div(
              class = "standings-wrapper",
              gt_table
            )
          )
        },
        error = function(e) {
          div(
            class = "mb-4",
            h4("Standings for Your Favorite Teams", class = "text-info"),
            div(
              class = "alert alert-warning",
              "Unable to load standings at this time."
            )
          )
        }
      )
    })

    # Handle manage favorites modal
    observeEvent(input$manage_favorites, {
      favorites <- user_favorites()
      teams <- all_teams()

      showModal(modalDialog(
        title = tagList(icon("heart"), " Manage Your Favorite Teams"),
        size = "m",

        div(
          # Multi-select input for teams
          selectInput(
            ns("team_selection"),
            label = NULL,
            choices = setNames(teams$id, teams$name),
            selected = favorites$id,
            multiple = TRUE,
            width = "100%",
            selectize = TRUE
          )
        ),

        footer = tagList(
          actionButton(
            ns("save_favorites"),
            "Save Changes",
            class = "btn-primary"
          ),
          modalButton("Cancel")
        )
      ))
    })

    # Handle saving favorites
    observeEvent(input$save_favorites, {
      req(user_rv())
      user_id <- as.integer(user_rv()$id)
      selected_teams <- input$team_selection

      # Remove all current favorites
      DBI::dbExecute(
        db_conn,
        "DELETE FROM public.user_favorite_team WHERE user_id = $1",
        params = list(user_id)
      )

      # Add all selected favorites
      if (length(selected_teams) > 0) {
        for (team_id in selected_teams) {
          DBI::dbExecute(
            db_conn,
            "INSERT INTO public.user_favorite_team (user_id, team_id) VALUES ($1, $2)",
            params = list(user_id, as.integer(team_id))
          )
        }
      }

      # Trigger reactive updates
      favorites_trigger(favorites_trigger() + 1)

      removeModal()
      showNotification("Favorite teams updated successfully!", type = "message")
    })

    # Render favorites modal (empty by default, populated by observeEvent)
    output$favorites_modal <- renderUI({
      NULL
    })
  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
