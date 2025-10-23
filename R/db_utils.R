gameon_db_connect <- function(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
) {
  DBI::dbConnect(
    drv = drv,
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password
  )
}

gameon_db_init <- function(db_conn) {
  # users (primarily for administrators and referees)
  DBI::dbExecute(
    db_conn,
    "CREATE SEQUENCE IF NOT EXISTS seq_user_id START 1"
  )
  DBI::dbExecute(
    db_conn,
    "CREATE TABLE IF NOT EXISTS users (
    user_id INTEGER PRIMARY KEY DEFAULT nextval('seq_user_id'),
    username TEXT,
    password_hash TEXT
  )"
  )

  # teams
  DBI::dbExecute(
    db_conn,
    "CREATE SEQUENCE IF NOT EXISTS seq_team_id START 1"
  )
  DBI::dbExecute(
    db_conn,
    "CREATE TABLE IF NOT EXISTS teams (
      team_id INTEGER PRIMARY KEY DEFAULT nextval('seq_team_id'),
      team_name TEXT NOT NULL,
      division INTEGER NOT NULL,
      logo_url TEXT
    )"
  )

  # games
  DBI::dbExecute(
    db_conn,
    "CREATE SEQUENCE IF NOT EXISTS seq_game_id START 1"
  )
  DBI::dbExecute(
    db_conn,
    "CREATE TABLE IF NOT EXISTS games (
      game_id INTEGER PRIMARY KEY DEFAULT nextval('seq_game_id'),
      home_team INTEGER REFERENCES teams(team_id),
      away_team INTEGER REFERENCES teams(team_id),
      start_time TIMESTAMP,
      game_type TEXT,
      score_home INTEGER,
      score_away INTEGER
    )"
  )
  # events
  DBI::dbExecute(
    db_conn,
    "CREATE SEQUENCE IF NOT EXISTS seq_event_id START 1"
  )
  DBI::dbExecute(
    db_conn,
    "CREATE TABLE IF NOT EXISTS events (
    event_id INTEGER PRIMARY KEY DEFAULT nextval('seq_event_id'),
    game_id INTEGER,
    half INTEGER,
    clock INTEGER,
    down INTEGER,
    girl_plays INTEGER,
    possession TEXT,
    score_home INTEGER,
    score_away INTEGER,
    timeouts_home INTEGER,
    timeouts_away INTEGER,
    event_type TEXT,
    event_points INTEGER,
    scored_by TEXT,
    event_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )"
  )
}

make_example_data <- function() {
  db_conn <- gameon_db_connect()

  # add some teams
  DBI::dbExecute(
    db_conn,
    "INSERT INTO teams (team_name, division) VALUES ($1, $2)",
    params = list("Trophy Wives", 4)
  )
  DBI::dbExecute(
    db_conn,
    "INSERT INTO teams (team_name, division) VALUES ($1, $2)",
    params = list("Any Given Saturday", 4)
  )
  DBI::dbExecute(
    db_conn,
    "INSERT INTO teams (team_name, division) VALUES ($1, $2)",
    params = list("boozin and losin", 4)
  )
  # add some games
  DBI::dbExecute(
    db_conn,
    "INSERT INTO games (home_team, away_team, start_time, game_type)
   VALUES ($1, $2, $3, $4)",
    params = list(
      1, # home_team (team_id)
      2, # away_team (team_id)
      as.POSIXct("2025-10-25 09:00:00", tz = "UTC"), # start_time
      "regular" # game_type
    )
  )
  DBI::dbExecute(
    db_conn,
    "INSERT INTO games (home_team, away_team, start_time, game_type)
   VALUES ($1, $2, $3, $4)",
    params = list(
      1, # home_team (team_id)
      3, # away_team (team_id)
      as.POSIXct("2025-11-01 10:00:00", tz = "UTC"), # start_time
      "regular" # game_type
    )
  )
}
