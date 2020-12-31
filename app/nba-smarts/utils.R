
create_db_conn <- function(creds) {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(
    drv,
    dbname = creds["proddb_dbname"],
    host = creds["proddb_host"],
    port = creds["proddb_port"],
    user = creds["proddb_user"],
    password = creds["proddb_password"]
  )
}
