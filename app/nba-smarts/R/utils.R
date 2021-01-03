get_app_credentials <- function(file_path = "./Data/env") {
  # reads keys from credentials file in the ~/.aws/ folder (requires user to have aws cli installed)
  creds_file =
    tryCatch({
      file(file_path, "r")
    }, warning = function(w) {
      file(paste0("./app/nba-smarts/Data/",'env'), "r")
    }, error = function(e) {
      file(paste0("./app/nba-smarts/Data/",'env'), "r")
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

db_upsert <- function(con, target_tbl, source_tbl, keys) {

  target_tbl_ref <- paste0(target_tbl, collapse=".")
  if (length(target_tbl) == 1) {
    target_tbl_name <- target_tbl
    target_tbl_schema_name <- ""
  } else {
    target_tbl_name <- target_tbl[2]
    target_tbl_schema_name <- target_tbl[1]
  }

  if (class(source_tbl) == "data.frame") {
    cat("staging merge ids from dataframe...")
    tryCatch({
      dbGetQuery(
        con,
        paste0(
          "drop table if exists upsert_ids; create local temp table upsert_ids (",
          paste(
            keys,
            map_chr(
              keys,
              function(x) {
                c <- class(source_tbl[,x])
                if (c == "character") {
                  c <- paste0("varchar(", length(source_tbl[,x]) + 10, ")")
                }
                return(c)
              }
            ),
            collapse = ",\n"
          ),
          ");"
        )
      )
      },
      error = function(e) {
        stop(e$message)
      },
      warning = function(w) {
        stop(w$message)
      }
    )

    dbWriteTable(con, "upsert_ids", source_tbl[, keys, drop = FALSE], row.names=FALSE, append=TRUE)
    cat("ids from dataframe staged!")
    source_tbl_ref <- "upsert_ids"
    source_tbl_name <- "upsert_ids"
    source_tbl_schema_name <- ""

  } else {
    source_tbl_ref <- paste0(source_tbl, collapse = ".")
    if (length(source_tbl) == 1) {
      source_tbl_name <- source_tbl
      source_tbl_schema_name <- ""
    } else {
      source_tbl_name <- source_tbl[2]
      source_tbl_schema_name <- source_tbl[1]
    }
  }

  cat("Deleting from target table...")
  tryCatch({
    dbGetQuery(
      con,
      paste0(
        "delete from ",
        target_tbl_ref,
        "\nusing ",
        source_tbl_ref,
        " where ",
        paste0(target_tbl_name, ".", keys, "=", source_tbl_name, ".", keys, collapse = " and "),
        ";"
      )
    )
    },
    error = function(e) {
      stop(e$message)
    },
    warning = function(w) {
      stop(w$message)
    }
  )
  cat("Existing rows deleted from target table!")

  cat("Inserting new and updated rows into target table...")
  if (class(source_tbl) == "data.frame") {
    dbWriteTable(con, target_tbl, source_tbl, row.names=FALSE, append=TRUE)
  } else {
    tryCatch({
      dbGetQuery(
        con,
        paste0("insert into ", target_tbl_ref, " select * from ", source_tbl_ref)
      )
    },
    error = function(e) {
      stop(e$message)
    },
    warning = function(w) {
      stop(w$message)
    }
    )
  }
  cat("Upsert finished.")
}

db_IsValid <- function(dbObj) {
  isValid <- tryCatch(
    dbSendQuery(dbObj, "select TRUE"),
    error = function(e) {
      e <- NULL
      return(e)
    },
    warning = function(w) {
      w <- NULL
      return(w)
    }
  )

  !is.null(isValid)
}

establish_db_connection <- function() {
  if (!exists("creds")) {
    creds <- get_app_credentials()
  }

  if (!exists("con") | !db_IsValid(con)) {
    lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {
      results <- dbListResults(x)
      if (length(results) > 0) {
        dbClearResult(results[[1]])
      }
      dbDisconnect(conn = x)
    })
    con <- create_db_conn(creds)
  } else {
    results <- dbListResults(con)
    if (length(results) > 0) {
      dbClearResult(results[[1]])
    }
  }

  return(con)
}
