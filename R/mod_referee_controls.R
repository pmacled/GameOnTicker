#' referee_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param game_id The game to display events for.
#' @param db_conn The database connection.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_referee_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Referee Controls"),
    div(
      style = "display: flex; flex-direction: column; gap: 8px; align-items: center;",
      mod_ticker_ui(ns("ticker")),
      actionButton(ns("undo_event"), "Undo Last Event"),
      div(
        style = "display: flex; gap: 8px;",
        actionButton(ns("start_pause"), "Start Clock", style = "width:180px;"),
        actionButton(ns("edit_clock"), "Edit Clock", style = "width:180px;")
      ),
      div(
        style = "display: flex; gap: 8px;",
        actionButton(
          ns("start_pause_play_clock"),
          "Start Play Clock",
          style = "width:180px;"
        ),
        actionButton(
          ns("reset_play_clock"),
          "Reset Play Clock",
          style = "width:180px;"
        )
      ),
      uiOutput(ns("play_by_play_ui"))
    )
  )
}

#' referee_controls Server Functions
#'
#' @noRd
mod_referee_controls_server <- function(id, db_conn, game_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # functions ----

    get_event_points <- function(event_type) {
      switch(
        event_type,
        guy_touchdown = 6,
        guy_touchdown_def = 6,
        girl_touchdown = 8,
        girl_touchdown_def = 8,
        pat1_good = 1,
        pat2_good = 2,
        pat3_good = 3,
        pat_def = 2,
        safety = 2,
        0
      )
    }

    get_scoring_team <- function(event_type) {
      offense <- possession_rv()
      defense <- setdiff(c("Home", "Away"), offense)
      if (
        event_type %in%
          c("guy_touchdown_def", "girl_touchdown_def", "pat_def", "safety")
      ) {
        defense
      } else if (get_event_points(event_type) > 0) {
        offense
      } else {
        NA_character_
      }
    }

    # function to handle recording of event to database.
    # events are expected to be recorded prior to points scored, timeout used, etc.
    record_event <- function(event_type) {
      DBI::dbExecute(
        db_conn,
        "INSERT INTO football_event (game_id, half, clock_ms, down, girl_plays, possession, score_home, score_away, timeouts_home, timeouts_away, type, points, scored_by)
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)",
        params = list(
          game_id,
          switch(
            timer_mode(),
            first_half = 1,
            halftime = 1,
            second_half = 2,
            ended = 2,
            final = 2
          ),
          switch(
            timer_mode(),
            halftime = 0,
            ended = 0,
            final = 0,
            clock_ms_rv()
          ),
          down_rv(),
          girl_plays_rv(),
          possession_rv(),
          score_home_rv(),
          score_away_rv(),
          timeouts_home_rv(),
          timeouts_away_rv(),
          event_type,
          get_event_points(event_type),
          get_scoring_team(event_type)
        )
      )
    }

    stop_clock <- function(record = TRUE) {
      if (record && timer_mode() %in% c("first_half", "second_half")) {
        record_event("stop_clock")
      }
      clock_running_rv(FALSE)
    }

    start_clock <- function(record = TRUE) {
      if (record && timer_mode() %in% c("first_half", "second_half")) {
        record_event("start_clock")
      }
      clock_running_rv(TRUE)
    }

    stop_play_clock <- function() {
      play_clock_running_rv(FALSE)
    }

    # Helper to reset play clock, optionally stopped/started
    reset_play_clock <- function(stop_play_clock = NULL) {
      if (is.logical(stop_play_clock)) {
        play_clock_running_rv(!stop_play_clock)
      }
      play_clock_ms_rv(30 * 1000)
    }

    reset_downs <- function() {
      down_rv(1)
      girl_plays_rv(0)
    }

    change_possession <- function() {
      possession_rv(setdiff(c("Home", "Away"), possession_rv()))
      reset_downs()
    }

    turnover <- function(record = TRUE) {
      if (record) {
        record_event("turnover")
      }
      change_possession()
    }

    turnover_on_downs <- function(record = TRUE) {
      if (record) {
        record_event("turnover_on_downs")
      }
      change_possession()
      showModal(modalDialog(
        title = "Turnover",
        "Turnover on Downs!"
      ))
    }

    play <- function(girl = FALSE, record = TRUE) {
      if (girl) {
        turnover_lgl <-
          # 5th and 2. regular girl play is a turnover.
          (down_rv() == 5 & girl_plays_rv() < 2) ||
          # 6th down.
          (down_rv() >= 6)
        girl_plays_rv(girl_plays_rv() + 1)
      } else {
        turnover_lgl <-
          # 5th and 2 or 5th and 1. regular guy play is a turnover.
          (down_rv() == 5 & girl_plays_rv() < 2) ||
          # 6th down.
          (down_rv() >= 6)
      }
      if (turnover_lgl) {
        turnover_on_downs(record = record)
      } else {
        down_rv(down_rv() + 1)
      }
    }
    guy_play <- function(record = TRUE) {
      if (record) {
        record_event("guy_play")
      }
      play(record = record)
    }
    girl_play <- function(record = TRUE) {
      if (record) {
        record_event("girl_play")
      }
      play(girl = TRUE, record = record)
    }

    # Helper for touchdowns
    # Updates points, potentially updates possession, and updates down to PAT
    touchdown <- function(girl = FALSE, defense = FALSE, record = TRUE) {
      points <- ifelse(girl, 8, 6)

      # 5th and 2. must be a girl touchdown.
      turnover_lgl <- !defense &&
        !girl &&
        (down_rv() == 5 & girl_plays_rv() < 2)

      event <- dplyr::case_when(
        turnover_lgl ~ "guy_play",
        !girl & !defense ~ "guy_touchdown",
        girl & !defense ~ "girl_touchdown",
        !girl & defense ~ "guy_touchdown_def",
        girl & defense ~ "girl_touchdown_def"
      )
      if (record) {
        record_event(event)
      }

      if (!defense) {
        # offense scores
        if (turnover_lgl) {
          turnover_on_downs(record = record)
        } else {
          if (possession_rv() == "Home") {
            score_home_rv(score_home_rv() + points)
          } else {
            score_away_rv(score_away_rv() + points)
          }
        }
      } else {
        # defense scores
        if (possession_rv() == "Home") {
          score_away_rv(score_away_rv() + points)
        } else {
          score_home_rv(score_home_rv() + points)
        }
        change_possession()
      }
      # update down to NA to indicate a PAT
      down_rv(NA_real_)
    }

    guy_touchdown <- function(record = TRUE) {
      touchdown(record = record)
    }

    girl_touchdown <- function(record = TRUE) {
      touchdown(girl = TRUE, record = record)
    }

    guy_touchdown_def <- function(record = TRUE) {
      touchdown(defense = TRUE, record = record)
    }

    girl_touchdown_def <- function(record = TRUE) {
      touchdown(girl = TRUE, defense = TRUE, record = record)
    }

    # Helper for PATs
    pat <- function(points, defense = FALSE) {
      if (defense) {
        points <- 2
        if (possession_rv() == "Home") {
          score_away_rv(score_away_rv() + points)
        } else {
          score_home_rv(score_home_rv() + points)
        }
      } else {
        if (possession_rv() == "Home") {
          score_home_rv(score_home_rv() + points)
        } else {
          score_away_rv(score_away_rv() + points)
        }
      }
      change_possession()
    }

    pat1_good <- function(record = TRUE) {
      if (record) {
        record_event("pat1_good")
      }
      pat(points = 1)
    }
    pat1_miss <- function(record = TRUE) {
      if (record) {
        record_event("pat1_miss")
      }
      pat(points = 0)
    }
    pat2_good <- function(record = TRUE) {
      if (record) {
        record_event("pat2_good")
      }
      pat(points = 2)
    }
    pat2_miss <- function(record = TRUE) {
      if (record) {
        record_event("pat2_miss")
      }
      pat(points = 0)
    }
    pat3_good <- function(record = TRUE) {
      if (record) {
        record_event("pat3_good")
      }
      pat(points = 3)
    }
    pat3_miss <- function(record = TRUE) {
      if (record) {
        record_event("pat3_miss")
      }
      pat(points = 0)
    }

    pat_def <- function(record = TRUE) {
      if (record) {
        record_event("pat_def")
      }
      pat(points = 2, defense = TRUE)
    }

    # Helper for safeties. Updates points and updates possession.
    safety <- function(record = TRUE) {
      if (record) {
        record_event("safety")
      }
      points <- 2
      if (possession_rv() == "Home") {
        score_away_rv(score_away_rv() + points)
      } else {
        score_home_rv(score_home_rv() + points)
      }
      change_possession()
    }

    punt <- function(record = TRUE) {
      if (record) {
        record_event("punt")
      }
      change_possession()
    }

    # Helper for timeouts
    timeout <- function(team, record = TRUE) {
      # assumption is that there the team has timeouts remaining since the
      # button gets disabled once a team has used all of their timeouts
      if (record) {
        record_event(paste0("timeout_", tolower(team)))
      }
      stop_clock(record = FALSE)
      reset_play_clock(stop_play_clock = TRUE)
      if (team == "Home") {
        timeouts_home_rv(timeouts_home_rv() - 1)
        timeouts_remaining <- timeouts_home_rv()
      } else if (team == "Away") {
        timeouts_away_rv(timeouts_away_rv() - 1)
        timeouts_remaining <- timeouts_away_rv()
      }
      showModal(modalDialog(
        title = "Timeout",
        sprintf(
          "%s team called a timeout. %d remaining.",
          team,
          as.integer(timeouts_remaining)
        )
      ))
    }
    timeout_home <- function(record = TRUE) {
      timeout("Home", record = record)
    }
    timeout_away <- function(record = TRUE) {
      timeout("Away", record = record)
    }

    # Helper to start halftime
    halftime <- function(record = TRUE) {
      # recording clock_expired because a score can still be recorded before true halftime
      if (record) {
        record_event("first_half_clock_expired")
      }
      timer_mode("halftime")
      clock_ms_rv(5 * 60 * 1000) # 5 minutes for halftime
      showModal(modalDialog(
        title = "Halftime",
        "Halftime started. Start second half when ready."
      ))
    }

    # Helper to start the second half
    start_second_half <- function(record = TRUE) {
      if (record) {
        record_event("start_second_half")
      }
      clock_running_rv(FALSE)
      timer_mode("second_half")
      clock_ms_rv(25 * 60 * 1000)
      change_possession()
    }

    # Helper to end a game
    end_game <- function(record = TRUE) {
      if (record) {
        record_event("second_half_clock_expired")
      }
      timer_mode("ended")
      showModal(modalDialog(
        title = "End of Game",
        "Game ended. Finalize game when ready."
      ))
    }

    # Helper to finalize a game
    finalize_game <- function(record = TRUE) {
      if (record) {
        record_event("finalize_game")
      }
      timer_mode("final")
      showModal(modalDialog(
        title = "Game Finalized",
        "Game finalized."
      ))
    }

    # initialization ----

    # validate that the game_id is in the games table
    game <- DBI::dbGetQuery(
      db_conn,
      "SELECT * FROM game WHERE id = $1 LIMIT 1",
      params = list(game_id)
    )
    if (nrow(game) == 0) {
      showModal(modalDialog(
        title = "Error",
        sprintf("Game ID %s not found in games table.", game_id)
      ))
      return(NULL)
    }

    # collect all game and team information
    game_info <- DBI::dbGetQuery(
      db_conn,
      "
  SELECT
    g.id AS game_id,
    g.home_team_id,
    home_team.name AS home_name,
    home_team.logo AS home_logo,
    g.away_team_id,
    away_team.name AS away_name,
    away_team.logo AS away_logo,
    g.division_id,
    d.name AS division_name,
    d.league_id,
    l.name AS league_name,
    l.season
  FROM game g
    LEFT JOIN team home_team ON g.home_team_id = home_team.id
    LEFT JOIN team away_team ON g.away_team_id = away_team.id
    LEFT JOIN division d ON g.division_id = d.id
    LEFT JOIN league l ON d.league_id = l.id
  WHERE g.id = $1
  LIMIT 1
  ",
      params = list(game_id)
    )

    # one row per team with win/loss/tie counts for games in this
    # division/season played up to this game.
    team_records <- DBI::dbGetQuery(
      db_conn,
      "
  SELECT
    t.id AS team_id,
    t.name AS team_name,
    SUM(CASE
      WHEN ((g.home_team_id = t.id AND g.score_home > g.score_away) OR
            (g.away_team_id = t.id AND g.score_away > g.score_home))
      THEN 1 ELSE 0 END) AS wins,
    SUM(CASE
      WHEN ((g.home_team_id = t.id AND g.score_home < g.score_away) OR
            (g.away_team_id = t.id AND g.score_away < g.score_home))
      THEN 1 ELSE 0 END) AS losses,
    SUM(CASE
      WHEN (g.score_home = g.score_away)
      THEN 1 ELSE 0 END) AS ties
  FROM team t
    JOIN game g ON (g.home_team_id = t.id OR g.away_team_id = t.id)
  WHERE g.division_id = $1
    AND g.start_time < NOW()
    AND (t.id = $2 OR t.id = $3)
  GROUP BY t.id, t.name
  ",
      params = list(
        game_info$division_id,
        game_info$home_team_id,
        game_info$away_team_id
      )
    )

    # reactive values
    timer_mode <- reactiveVal("first_half") # "first_half", "halftime", "second_half", "ended", "final"
    clock_running_rv <- reactiveVal(FALSE)
    clock_ms_rv <- reactiveVal(25 * 60 * 1000) # 25 minutes in ms
    timer <- reactiveTimer(100, session) # triggers every 100 ms

    play_clock_ms_rv <- reactiveVal(30 * 1000)
    play_clock_running_rv <- reactiveVal(FALSE)
    play_timer <- reactiveTimer(100, session)

    down_rv <- reactiveVal(1)
    girl_plays_rv <- reactiveVal(0)

    score_home_rv <- reactiveVal(0)
    score_away_rv <- reactiveVal(0)

    possession_rv <- reactiveVal("Home")

    timeouts_home_rv <- reactiveVal(3)
    timeouts_away_rv <- reactiveVal(3)

    # a reactive value that will be observed.
    # intended for replaying the last event when reloading the app.
    call_event_rv <- reactiveVal(FALSE)

    # Initialize game based on most recent logged record for the game_id in the events table
    last_event_init <- DBI::dbGetQuery(
      db_conn,
      "SELECT * FROM football_event WHERE game_id = $1 ORDER BY id DESC LIMIT 1",
      params = list(game_id)
    )

    if (nrow(last_event_init) == 1) {
      if (last_event_init$half == 1) {
        timer_mode("first_half")
      } else if (last_event_init$half == 2) {
        timer_mode("second_half")
      }

      clock_ms_rv(last_event_init$clock_ms)

      # first set to last state recorded, then re-play the last
      # event without recording it.
      down_rv(last_event_init$down)
      girl_plays_rv(last_event_init$girl_plays)

      score_home_rv(last_event_init$score_home)
      score_away_rv(last_event_init$score_away)

      possession_rv(last_event_init$possession)

      timeouts_home_rv(last_event_init$timeouts_home)
      timeouts_away_rv(last_event_init$timeouts_away)

      # replay the event without recording
      call_event_rv(last_event_init$type)
    }

    ticker_game_data <- reactive({
      half_label <- switch(
        timer_mode(),
        "first_half" = "1st Half",
        "halftime" = "Halftime",
        "second_half" = "2nd Half",
        "ended" = "Game Ended",
        "final" = "Final"
      )

      down_names <- c("1st", "2nd", "3rd", "4th", "5th", "6th")
      down_girl_plays_available <-
        if (is.na(down_rv())) {
          # NA down means a touchdown was just scored
          "PAT"
        } else {
          sprintf(
            "%s and %d",
            down_names[as.integer(down_rv())],
            2 - min(as.integer(girl_plays_rv()), 2)
          )
        }

      format_record <- function(wins, losses, ties) {
        if (ties > 0) {
          sprintf(
            "%d-%d-%d",
            as.integer(wins),
            as.integer(losses),
            as.integer(ties)
          )
        } else {
          sprintf("%d-%d", as.integer(wins), as.integer(losses))
        }
      }

      record_home <- with(
        team_records[
          team_records$team_id == game_info$home_team_id,
        ],
        format_record(wins, losses, ties)
      )
      record_away <- with(
        team_records[
          team_records$team_id == game_info$away_team_id,
        ],
        format_record(wins, losses, ties)
      )

      list(
        home_logo = game_info$home_logo,
        away_logo = game_info$away_logo,
        home_name = game_info$home_name,
        away_name = game_info$away_name,
        record_home = record_home,
        record_away = record_away,
        timeouts_home = as.integer(timeouts_home_rv()),
        timeouts_away = as.integer(timeouts_away_rv()),
        score_home = as.integer(score_home_rv()),
        score_away = as.integer(score_away_rv()),
        possession = possession_rv(),
        half = half_label,
        down_girl_plays_available = down_girl_plays_available
      )
    })

    ticker_game_clock <- reactive({
      mins <- clock_ms_rv() %/% 60000
      secs <- clock_ms_rv() %% 60000 %/% 1000
      sprintf(
        "%02d:%02d",
        as.integer(mins),
        as.integer(secs)
      )
    })

    ticker_play_clock <- reactive({
      secs <- play_clock_ms_rv() %/% 1000
      sprintf("%02d", as.integer(secs))
    })

    mod_ticker_server(
      "ticker",
      game_data = ticker_game_data,
      game_clock = ticker_game_clock,
      play_clock = ticker_play_clock
    )

    # observers ----

    observe({
      if (is.character(call_event_rv())) {
        if (call_event_rv() == "first_half_clock_expired") {
          halftime(record = FALSE)
        } else if (call_event_rv() == "second_half_clock_expired") {
          end_game(record = FALSE)
        } else if (
          exists(call_event_rv()) && !call_event_rv() %in% c("start_clock")
        ) {
          event_fn <- get(call_event_rv())
          if (is.function(event_fn)) {
            do.call(event_fn, args = list(record = FALSE))
          }
        }
        call_event_rv(FALSE)
      }
    })

    # game clock
    observe({
      if (clock_running_rv()) {
        timer()
        isolate({
          if (clock_ms_rv() > 0) {
            clock_ms_rv(clock_ms_rv() - 100)
          }
        })
      }
    })

    observe({
      # When clock reaches zero
      if (clock_ms_rv() <= 0 && clock_running_rv()) {
        stop_clock(record = FALSE)
        reset_play_clock(stop_play_clock = TRUE)
        if (timer_mode() == "first_half") {
          halftime()
        } else if (timer_mode() == "halftime") {
          # Wait for referee to start second half
        } else if (timer_mode() == "second_half") {
          end_game()
        }
      }
    })

    # disable/enable timeouts
    observe(
      if (timeouts_home_rv() <= 0) {
        updateActionButton(
          session,
          "timeout_home",
          disabled = TRUE
        )
      } else if (timeouts_home_rv() > 0) {
        updateActionButton(
          session,
          "timeout_home",
          disabled = FALSE
        )
      }
    )
    observe(
      if (timeouts_away_rv() <= 0) {
        updateActionButton(
          session,
          "timeout_away",
          disabled = TRUE
        )
      } else if (timeouts_away_rv() > 0) {
        updateActionButton(
          session,
          "timeout_away",
          disabled = FALSE
        )
      }
    )

    observeEvent(input$start_second_half, {
      start_second_half()
    })

    observeEvent(input$finalize_game, {
      finalize_game()
    })

    observeEvent(input$start_pause, {
      if (clock_running_rv()) {
        stop_clock()
      } else {
        start_clock()
      }
      updateActionButton(
        session,
        "start_pause",
        label = if (clock_running_rv()) "Pause Clock" else "Start Clock"
      )
    })

    observe({
      updateActionButton(
        session,
        "start_pause",
        label = if (clock_running_rv()) "Pause Clock" else "Start Clock"
      )
    })

    observe({
      updateActionButton(
        session,
        "edit_clock",
        disabled = clock_running_rv()
      )
    })

    observeEvent(input$edit_clock, {
      showModal(modalDialog(
        numericInput(
          ns("new_mins"),
          "Set Minutes",
          value = clock_ms_rv() %/% 60000,
          min = 0,
          max = 25
        ),
        numericInput(
          ns("new_secs"),
          "Set Seconds",
          value = clock_ms_rv() %% 60000 %/% 1000,
          min = 0,
          max = 59
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_edit"), "Set Time")
        )
      ))
    })

    observeEvent(input$confirm_edit, {
      new_mins <- dplyr::coalesce(input$new_mins, 0)
      new_secs <- dplyr::coalesce(input$new_secs, 0)
      new_secs_total <- new_mins * 60 + new_secs
      if (!(new_secs_total >= 0 && new_secs_total <= 25 * 60)) {
        showNotification("Set a timer up to 25 minutes long.", type = "error")
      } else {
        record_event("edit_clock")
        clock_ms_rv(new_secs_total * 1000)
        removeModal()
      }
    })

    # Play clock logic
    observe({
      if (play_clock_running_rv()) {
        play_timer()
        isolate({
          if (play_clock_ms_rv() > 0) {
            play_clock_ms_rv(play_clock_ms_rv() - 100)
          }
        })
      }
    })

    observe({
      # When play clock reaches zero
      if (play_clock_ms_rv() <= 0 && play_clock_running_rv()) {
        # intentionally not stopping the play clock with play_clock_running_rv
        # so resetting the play clock immediately runs the play clock
        play_clock_ms_rv(0)
      }
    })

    # Button logic
    observe({
      label <- if (play_clock_running_rv()) {
        "Pause Play Clock"
      } else {
        "Start Play Clock"
      }
      updateActionButton(session, "start_pause_play_clock", label = label)
    })

    observeEvent(input$start_pause_play_clock, {
      play_clock_running_rv(!play_clock_running_rv())
    })

    observeEvent(input$reset_play_clock, {
      reset_play_clock()
    })

    observeEvent(input$timeout_home, {
      timeout_home()
    })
    observeEvent(input$timeout_away, {
      timeout_away()
    })

    observeEvent(input$guy_play, {
      guy_play()
    })
    observeEvent(input$girl_play, {
      girl_play()
    })
    observeEvent(input$guy_td, {
      guy_touchdown()
    })
    observeEvent(input$girl_td, {
      girl_touchdown()
    })
    observeEvent(input$punt, {
      punt()
    })

    observeEvent(input$turnover, {
      turnover()
    })
    observeEvent(input$safety, {
      safety()
    })
    observeEvent(input$guy_td_def, {
      guy_touchdown_def()
    })
    observeEvent(input$girl_td_def, {
      girl_touchdown_def()
    })

    observeEvent(input$pat1_good, {
      pat1_good()
    })
    observeEvent(input$pat1_miss, {
      pat1_miss()
    })
    observeEvent(input$pat2_good, {
      pat2_good()
    })
    observeEvent(input$pat2_miss, {
      pat2_miss()
    })
    observeEvent(input$pat3_good, {
      pat3_good()
    })
    observeEvent(input$pat3_miss, {
      pat3_miss()
    })
    observeEvent(input$pat_def, {
      pat_def()
    })

    observeEvent(input$undo_event, {
      # the event that will be deleted and the state we are restoring to
      last_event <- DBI::dbGetQuery(
        db_conn,
        "SELECT * FROM football_event WHERE game_id = $1 ORDER BY id DESC LIMIT 1",
        params = list(game_id)
      )
      # Delete last event from database
      DBI::dbExecute(
        db_conn,
        "DELETE FROM football_event WHERE id = (
    SELECT id FROM football_event WHERE game_id = $1 ORDER BY id DESC LIMIT 1
  )",
        params = list(game_id)
      )
      if (nrow(last_event) == 1) {
        # Restore state from previous event (not including game clock)
        down_rv(last_event$down)
        possession_rv(last_event$possession)
        score_home_rv(last_event$score_home)
        score_away_rv(last_event$score_away)
        girl_plays_rv(last_event$girl_plays)
        timeouts_home_rv(last_event$timeouts_home)
        timeouts_away_rv(last_event$timeouts_away)
      } else {
        # No events left, reset to initial state
        down_rv(1)
        possession_rv("Home")
        score_home_rv(0)
        score_away_rv(0)
        girl_plays_rv(0)
        timeouts_home_rv(3)
        timeouts_away_rv(3)
      }
    })

    # renderers ----
    # UI output for play-by-play events
    output$play_by_play_ui <- renderUI({
      halftime_button <- actionButton(
        ns("start_second_half"),
        "Start Second Half"
      )

      finalize_game_button <- actionButton(
        ns("finalize_game"),
        "Finalize Game"
      )

      tagList(
        div(
          style = "display: flex; flex-direction: column; gap: 8px; align-items: center;",
          # call timeouts
          div(
            style = "display: flex; gap: 8px;",
            actionButton(
              ns("timeout_home"),
              "Home Timeout",
              style = "width:180px;"
            ),
            actionButton(
              ns("timeout_away"),
              "Away Timeout",
              style = "width:180px;"
            )
          ),
          if (timer_mode() == "final") {
            NULL
          } else if (is.na(down_rv())) {
            # PAT options
            div(
              style = "display: flex; flex-direction: column; gap: 8px; align-items: center;",
              if (timer_mode() == "halftime") halftime_button,
              if (timer_mode() == "ended") finalize_game_button,
              div(tags$strong("Offense")),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("pat1_good"),
                  "1-pt Good",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("pat1_miss"),
                  "1-pt Miss",
                  style = "width:180px;"
                )
              ),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("pat2_good"),
                  "2-pt Good",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("pat2_miss"),
                  "2-pt Miss",
                  style = "width:180px;"
                )
              ),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("pat3_good"),
                  "3-pt Good",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("pat3_miss"),
                  "3-pt Miss",
                  style = "width:180px;"
                )
              ),
              div(tags$strong("Defense")),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("pat_def"),
                  "Defensive Return",
                  style = "width:180px;"
                )
              )
            )
          } else {
            # Regular play options
            div(
              style = "display: flex; flex-direction: column; gap: 8px; align-items: center;",
              if (timer_mode() == "halftime") halftime_button,
              if (timer_mode() == "ended") finalize_game_button,
              div(tags$strong("Offense")),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("guy_play"),
                  "Guy Play",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("girl_play"),
                  "Girl Play",
                  style = "width:180px;"
                )
              ),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("guy_td"),
                  "Guy TD",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("girl_td"),
                  "Girl TD",
                  style = "width:180px;"
                )
              ),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("punt"),
                  "Punt",
                  disabled = !(down_rv() < 6 && girl_plays_rv() > 0),
                  style = "width:180px;"
                )
              ),
              div(tags$strong("Defense")),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("turnover"),
                  "Turnover",
                  style = "width:180px;"
                ),
                actionButton(ns("safety"), "Safety", style = "width:180px;")
              ),
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  ns("guy_td_def"),
                  "Guy TD (Def)",
                  style = "width:180px;"
                ),
                actionButton(
                  ns("girl_td_def"),
                  "Girl TD (Def)",
                  style = "width:180px;"
                )
              )
            )
          }
        )
      )
    })
  })
}

## To be copied in the UI
# mod_referee_controls_ui("referee_controls_1")

## To be copied in the server
# mod_referee_controls_server("referee_controls_1", db_conn, game_id)
