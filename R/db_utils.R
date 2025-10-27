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
