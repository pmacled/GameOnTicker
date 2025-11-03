#' standings UI Function
#'
#' @description A shiny Module for displaying league standings.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_standings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "standings-container",
      uiOutput(ns("standings_tables"))
    )
  )
}

#' standings Server Functions
#'
#' @param db_conn Database connection
#' @param league_id League ID to get standings for
#' @param division_id Optional vector of division IDs to subset to
#' @noRd
mod_standings_server <- function(id, db_conn, league_id, division_id = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate standings data
    standings_data <- reactive({
      req(db_conn, league_id)

      # Get divisions for the league
      division_query <- "
        SELECT d.id, d.name, d.rank
        FROM division d
        WHERE d.league_id = $1
        ORDER BY d.rank
      "
      if (!is.null(division_id)) {
        division_query <- paste0(
          division_query,
          " AND d.id = ANY($2)"
        )
        divisions <- DBI::dbGetQuery(
          db_conn,
          division_query,
          list(league_id, division_id)
        )
      } else {
        divisions <- DBI::dbGetQuery(db_conn, division_query, list(league_id))
      }

      if (nrow(divisions) == 0) {
        return(NULL)
      }

      # Get all completed games for the divisions with team info
      games_query <- "
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
        FROM game g
        JOIN team ht ON g.home_team_id = ht.id
        JOIN team at ON g.away_team_id = at.id
        WHERE g.division_id IN ("

      # Create placeholders for division IDs
      division_placeholders <- paste0(
        "$",
        1:length(divisions$id),
        collapse = ", "
      )
      games_query <- paste0(
        games_query,
        division_placeholders,
        ")
          AND g.home_result IS NOT NULL 
          AND g.away_result IS NOT NULL
        ORDER BY g.start_time
      "
      )

      # Create parameter list with all division IDs
      query_params <- as.list(as.integer(divisions$id))

      games <- DBI::dbGetQuery(
        db_conn,
        games_query,
        query_params
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
        # Return empty standings for divisions with no games
        return(list(
          divisions = divisions,
          standings = data.frame()
        ))
      }

      # Calculate team standings
      standings <- calculate_standings(games, divisions)

      list(
        divisions = divisions,
        standings = standings
      )
    })

    output$standings_tables <- renderUI({
      tryCatch(
        {
          data <- standings_data()
          if (is.null(data) || nrow(data$standings) == 0) {
            return(div(class = "no-standings", "No standings data available"))
          }

          # Create a GT table with division group headers
          gt_table <- tryCatch(
            {
              create_standings_table(data$standings, data$divisions)
            },
            error = function(e) {
              # Show error message if GT table creation fails
              div(
                class = "standings-error",
                p(paste("Error creating standings table:", e$message))
              )
            }
          )

          div(
            class = "division-standings",
            gt_table
          )
        },
        error = function(e) {
          div(
            class = "standings-error",
            h3("Error loading standings"),
            p(paste("Error details:", e$message)),
            p("Please check your database connection and try again.")
          )
        }
      )
    })
  })
}

#' Calculate team standings from games data
#'
#' @param games Data frame of completed games
#' @param divisions Data frame of divisions
#' @return Data frame with team standings
#'
#' @noRd
calculate_standings <- function(games, divisions) {
  if (nrow(games) == 0) {
    return(data.frame())
  }

  # Get all teams that played games
  all_teams <- unique(c(
    paste(
      games$home_team_id,
      games$home_team_name,
      games$home_team_logo,
      games$division_id,
      sep = "|"
    ),
    paste(
      games$away_team_id,
      games$away_team_name,
      games$away_team_logo,
      games$division_id,
      sep = "|"
    )
  ))

  team_info <- do.call(
    rbind,
    lapply(all_teams, function(x) {
      parts <- strsplit(x, "\\|")[[1]]
      data.frame(
        team_id = as.integer(parts[1]),
        team_name = parts[2],
        team_logo = ifelse(is.na(parts[3]) || parts[3] == "NA", "", parts[3]),
        division_id = as.integer(parts[4]),
        stringsAsFactors = FALSE
      )
    })
  )

  # Calculate basic stats for each team
  standings <- do.call(
    rbind,
    lapply(seq_len(nrow(team_info)), function(i) {
      team_id <- team_info$team_id[i]
      team_name <- team_info$team_name[i]
      team_logo <- team_info$team_logo[i]
      div_id <- team_info$division_id[i]

      # Get games for this team (ordered by game_id for chronological order)
      home_games <- games[
        games$home_team_id == team_id & games$division_id == div_id,
      ]
      away_games <- games[
        games$away_team_id == team_id & games$division_id == div_id,
      ]

      # Calculate stats
      games_played <- nrow(home_games) + nrow(away_games)

      # Use existing win/loss/tie results from game table
      home_wins <- sum(home_games$home_result == "W", na.rm = TRUE)
      home_losses <- sum(home_games$home_result == "L", na.rm = TRUE)
      home_ties <- sum(home_games$home_result == "T", na.rm = TRUE)

      away_wins <- sum(away_games$away_result == "W", na.rm = TRUE)
      away_losses <- sum(away_games$away_result == "L", na.rm = TRUE)
      away_ties <- sum(away_games$away_result == "T", na.rm = TRUE)

      wins <- home_wins + away_wins
      losses <- home_losses + away_losses
      ties <- home_ties + away_ties

      # Points for/against
      points_for <- sum(home_games$score_home, na.rm = TRUE) +
        sum(away_games$score_away, na.rm = TRUE)
      points_against <- sum(home_games$score_away, na.rm = TRUE) +
        sum(away_games$score_home, na.rm = TRUE)
      point_diff <- points_for - points_against
      plus_minus <- if (games_played > 0) {
        round(point_diff / games_played, 1)
      } else {
        0
      }

      # Win percentage (ties count as 0.5 wins)
      win_pct <- if (games_played > 0) (wins + 0.5 * ties) / games_played else 0

      # Calculate current streak
      streak <- if (games_played > 0) {
        # Combine and sort all games by start_time (chronological order)
        all_team_games <- rbind(
          data.frame(
            start_time = home_games$start_time,
            result = home_games$home_result,
            stringsAsFactors = FALSE
          ),
          data.frame(
            start_time = away_games$start_time,
            result = away_games$away_result,
            stringsAsFactors = FALSE
          )
        )

        if (nrow(all_team_games) > 0) {
          # Sort by start_time (chronological order)
          all_team_games <- all_team_games[order(all_team_games$start_time), ]

          # Get the results in order
          results <- all_team_games$result

          # Use rle to find runs of consecutive results
          if (length(results) > 0) {
            rle_results <- rle(results)
            # Get the last (most recent) streak
            last_streak_length <- rle_results$lengths[length(
              rle_results$lengths
            )]
            last_streak_type <- rle_results$values[length(rle_results$values)]
            paste0(last_streak_type, last_streak_length)
          } else {
            ""
          }
        } else {
          ""
        }
      } else {
        ""
      }

      data.frame(
        team_id = team_id,
        team_name = team_name,
        team_logo = team_logo,
        division_id = div_id,
        games_played = games_played,
        wins = wins,
        losses = losses,
        ties = ties,
        win_pct = win_pct,
        points_for = points_for,
        points_against = points_against,
        point_diff = point_diff,
        plus_minus = plus_minus,
        streak = streak,
        stringsAsFactors = FALSE
      )
    })
  )

  # Calculate head-to-head records for tie-breaking
  standings <- calculate_head_to_head(standings, games)

  # Sort standings within each division
  standings_list <- split(standings, standings$division_id)
  sorted_standings <- do.call(
    rbind,
    lapply(standings_list, function(div_standings) {
      # Sort by win percentage, then head-to-head, then point differential
      div_standings[
        order(
          -div_standings$win_pct,
          -div_standings$h2h_advantage,
          -div_standings$point_diff
        ),
      ]
    })
  )

  rownames(sorted_standings) <- NULL
  sorted_standings
}

#' Calculate head-to-head advantages for tie-breaking
#'
#' @param standings Current standings data frame
#' @param games Games data frame
#' @return Standings with h2h_advantage column added
#'
#' @noRd
calculate_head_to_head <- function(standings, games) {
  standings$h2h_advantage <- 0

  # For each division, calculate head-to-head records
  for (div_id in unique(standings$division_id)) {
    div_standings <- standings[standings$division_id == div_id, ]
    div_games <- games[games$division_id == div_id, ]

    if (nrow(div_standings) < 2) {
      next
    }

    # Group teams by win percentage to find ties
    win_pct_groups <- split(div_standings, div_standings$win_pct)

    for (group in win_pct_groups) {
      if (nrow(group) < 2) {
        next
      } # No tie to break

      # Calculate head-to-head for teams in this tie
      for (i in seq_len(nrow(group))) {
        team_id <- group$team_id[i]
        opponents <- group$team_id[group$team_id != team_id]

        h2h_wins <- 0
        h2h_losses <- 0

        for (opp_id in opponents) {
          # Games where team_id was home vs opp_id
          home_vs_opp <- div_games[
            div_games$home_team_id == team_id &
              div_games$away_team_id == opp_id,
          ]
          # Games where team_id was away vs opp_id
          away_vs_opp <- div_games[
            div_games$away_team_id == team_id &
              div_games$home_team_id == opp_id,
          ]

          # Count wins/losses using result fields
          h2h_wins <- h2h_wins +
            sum(home_vs_opp$home_result == "W", na.rm = TRUE) +
            sum(away_vs_opp$away_result == "W", na.rm = TRUE)

          h2h_losses <- h2h_losses +
            sum(home_vs_opp$home_result == "L", na.rm = TRUE) +
            sum(away_vs_opp$away_result == "L", na.rm = TRUE)
        }

        # Calculate head-to-head advantage (win pct difference from 0.5)
        h2h_games <- h2h_wins + h2h_losses
        h2h_pct <- if (h2h_games > 0) h2h_wins / h2h_games else 0.5
        standings$h2h_advantage[
          standings$team_id == team_id & standings$division_id == div_id
        ] <-
          h2h_pct - 0.5
      }
    }
  }

  standings
}

#' Create a GT table for all division standings
#'
#' @param standings Standings data for all divisions
#' @param divisions Division information
#' @return GT table object
#'
#' @noRd
create_standings_table <- function(standings, divisions) {
  # Add division names to standings
  standings <- standings |>
    dplyr::left_join(
      divisions |> dplyr::select(id, division_name = name, rank),
      by = c("division_id" = "id")
    ) |>
    dplyr::arrange(rank, -win_pct, -h2h_advantage, -point_diff)

  # Add rank within division
  standings <- standings |>
    dplyr::group_by(division_id) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup()

  # Logo column
  standings$logo_display <- sapply(
    seq_len(nrow(standings)),
    function(i) {
      team_logo <- standings$team_logo[i]

      if (!is.null(team_logo) && !is.na(team_logo) && nzchar(team_logo)) {
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
          '" style="height: 1.2em; width: 1.2em; object-fit: contain;">'
        )
      } else {
        # Empty div that preserves space
        '<div style="height: 1.2em; width: 1.2em; display: inline-block;"></div>'
      }
    }
  )

  gt_table <- standings |>
    dplyr::select(
      division_name,
      logo_display,
      team_name,
      wins,
      losses,
      ties,
      win_pct,
      points_for,
      points_against,
      plus_minus,
      streak
    ) |>
    gt::gt(groupname_col = "division_name") |>
    gt::cols_label(
      logo_display = "",
      team_name = "",
      wins = "W",
      losses = "L",
      ties = "T",
      win_pct = "PCT",
      points_for = "PF",
      points_against = "PA",
      plus_minus = "+/-",
      streak = "STRK"
    ) |>
    gt::fmt_number(
      columns = win_pct,
      decimals = 3,
      drop_trailing_zeros = FALSE,
      pattern = "{x}"
    ) |>
    gt::fmt(
      columns = win_pct,
      fns = function(x) {
        formatted <- sprintf("%.3f", x)
        # Remove leading zero for values less than 1
        gsub("^0\\.", ".", formatted)
      }
    ) |>
    gt::fmt_markdown(columns = logo_display) |>
    gt::cols_width(
      logo_display ~ px(35)
    ) |>
    gt::cols_align(
      align = "center",
      columns = logo_display
    ) |>
    gt::data_color(
      columns = plus_minus,
      fn = scales::col_numeric(
        palette = c("#ff4444", "#ffffff", "#44ff44"),
        domain = c(
          -1 * max(abs(standings$plus_minus)),
          max(abs(standings$plus_minus))
        )
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_options(
      table.font.size = "14px",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold"
    )

  # Convert to HTML for Shiny
  gt::as_raw_html(gt_table)
}

## To be copied in the UI
# mod_standings_ui("standings_1")

## To be copied in the server
# mod_standings_server("standings_1", db_conn, league_id, division_id)
