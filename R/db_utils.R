gameon_db_connect <- function(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode = "require"
) {
  DBI::dbConnect(
    drv = drv,
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = sslmode
  )
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
        palette = c("#b91c1c", "#ececec54", "#059669"),
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
    ) |>
    gt::opt_css(
      css = "
      .gt_table {
        background-color: transparent !important;
        color: var(--bs-body-color, currentColor) !important;
      }
      .gt_col_heading {
        background-color: transparent !important;
        color: var(--bs-body-color, currentColor) !important;
      }
      .gt_group_heading {
        background-color: transparent !important;
        color: var(--bs-body-color, currentColor) !important;
      }
      /* Only make rows transparent if they don't have inline background styles */
      .gt_row:not([style*='background-color']) {
        background-color: transparent !important;
      }
      /* Apply theme color to table cells without data colors */
      .gt_table td:not([style*='background-color']) {
        color: var(--bs-body-color, currentColor) !important;
      }
      "
    )

  # Convert to HTML for Shiny
  gt::as_raw_html(gt_table)
}
