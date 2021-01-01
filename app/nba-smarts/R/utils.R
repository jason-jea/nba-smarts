get_app_credentials <- function(file_path) {
  # reads keys from credentials file in the ~/.aws/ folder (requires user to have aws cli installed)
  creds_file =
    tryCatch({
      file(file_path, "r")
    }, warning = function(w) {
      file(paste0("./Data/",'env'), "r")
    }, error = function(e) {
      file(paste0("./Data/",'env'), "r")
    })
  # initialize empty list to store aws credentials
  creds <- list()

  i = 0
  # read in list of credentials
  while (TRUE) {
    # loop through lines in credentials files
    line = readLines(creds_file, n = 1, warn = FALSE)
    if ( (length(line) == 0 )) {
      break # break loop if line is empty
    }
    line <- gsub("\n", "", gsub(" ", "", line)) # basic scrubbing of newline character and empty spaces

    # create key-value element in list for credential in current line
    # key is character before '=', value is character after '='
    creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
    i = i+1
  }
  close(creds_file)
  return(creds)
}

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
