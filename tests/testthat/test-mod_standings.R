test_that("calculate_standings works with empty games", {
  games <- data.frame()
  divisions <- data.frame(id = 1, name = "Division 1", rank = 1)

  result <- calculate_standings(games, divisions)
  expect_equal(nrow(result), 0)
})

test_that("calculate_standings works with sample data", {
  games <- data.frame(
    game_id = 1:3,
    home_team_id = c(1, 2, 3),
    away_team_id = c(2, 3, 1),
    score_home = c(14, 21, 7),
    score_away = c(7, 14, 21),
    division_id = c(1, 1, 1),
    home_result = c("W", "W", "L"),
    away_result = c("L", "L", "W"),
    home_team_name = c("Team A", "Team B", "Team C"),
    away_team_name = c("Team B", "Team C", "Team A"),
    stringsAsFactors = FALSE
  )

  divisions <- data.frame(
    id = 1,
    name = "Division 1",
    rank = 1,
    stringsAsFactors = FALSE
  )

  result <- calculate_standings(games, divisions)

  expect_equal(nrow(result), 3)
  expect_true(all(
    c(
      "team_id",
      "team_name",
      "wins",
      "losses",
      "ties",
      "win_pct",
      "plus_minus"
    ) %in%
      names(result)
  ))
  expect_equal(sum(result$wins), 3) # Total wins should equal total games
  expect_equal(sum(result$losses), 3) # Total losses should equal total games
})

test_that("calculate_head_to_head works", {
  standings <- data.frame(
    team_id = c(1, 2),
    team_name = c("Team A", "Team B"),
    division_id = c(1, 1),
    games_played = c(1, 1),
    wins = c(1, 0),
    losses = c(0, 1),
    ties = c(0, 0),
    win_pct = c(1.0, 0.0),
    points_scored = c(14, 7),
    points_against = c(7, 14),
    point_diff = c(7, -7),
    stringsAsFactors = FALSE
  )

  games <- data.frame(
    game_id = 1,
    home_team_id = 1,
    away_team_id = 2,
    score_home = 14,
    score_away = 7,
    division_id = 1,
    home_result = "W",
    away_result = "L",
    stringsAsFactors = FALSE
  )

  result <- calculate_head_to_head(standings, games)

  expect_true("h2h_advantage" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("standings UI function returns valid HTML", {
  ui_result <- mod_standings_ui("test")

  expect_s3_class(ui_result, "shiny.tag.list")
})

test_that("create_standings_table works", {
  standings <- data.frame(
    team_name = c("Team A", "Team B"),
    games_played = c(1, 1),
    wins = c(1, 0),
    losses = c(0, 1),
    ties = c(0, 0),
    win_pct = c(1.0, 0.0),
    points_scored = c(14, 7),
    points_against = c(7, 14),
    point_diff = c(7, -7),
    plus_minus = c(7.0, -7.0),
    stringsAsFactors = FALSE
  )

  result <- create_standings_table(standings, "Test Division")

  expect_type(result, "character")
  expect_true(grepl("table", result, ignore.case = TRUE))
})
