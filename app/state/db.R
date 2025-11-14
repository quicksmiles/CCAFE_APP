box::use(
    glue[glue_sql]
)
# =============================================================================
# === database state management
# =============================================================================

stringify <- function(x) {
    paste(utils::capture.output(utils::str(x)), collapse = "\n")
}

#' Get a connection to the database
#' 
#' Creates the database file if it does not already exist.
#' 
#' @param readonly Logical indicating if the connection should be non-writeable (default FALSE)
#' @return A database connection object
#' @export
getDBCon <- function(readonly = FALSE, db_path = NULL) {
    # get a connection to the database file
    db_path <- ifelse(!is.null(db_path), db_path, Sys.getenv(
        "DATABASE_PATH",
        unset=file.path(Sys.getenv("APP_ROOT", unset="."), "app_state", "ccafe.sqlite")
    ))

    # make the folders if they don't exist...
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

    message("Connecting to database at: ", db_path)

    # ...and connect
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path, extended_types = TRUE)

    return(con)
}

#' Create schema in the new database
#' @export
makeTables <- function(con) {
    # create the necessary tables in the database
    # (these are three separate statements to avoid RSQLite dropping anything
    # after the semicolon, a long-standing issue w/RSQLite:
    # https://github.com/r-dbi/RSQLite/issues/313)
    DBI::dbExecute(con, "
        -- stores job results prior to retrieval by the user
        CREATE TABLE IF NOT EXISTS job_results (
            id TEXT PRIMARY KEY,
            email TEXT,
            succeeded BOOLEAN NOT NULL,
            results BLOB,
            error_message TEXT,
            created_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            retrieved_on TIMESTAMP
        );"
    )
    DBI::dbExecute(con, "
        -- caches variant responses from gnomAD
        CREATE TABLE IF NOT EXISTS variants (
            chrom   TEXT    NOT NULL,
            pos     INTEGER NOT NULL,
            ref     TEXT    NOT NULL,
            alt     TEXT    NOT NULL,
            genome  TEXT,
            created_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            CONSTRAINT pk_variants PRIMARY KEY (chrom, pos, ref, alt)
            -- CONSTRAINT genome_is_json CHECK (genome IS NULL OR json_valid(genome))
        ) WITHOUT ROWID;"
    )
    DBI::dbExecute(con, "
        -- index chrom, pos for fast cache checks
        CREATE INDEX IF NOT EXISTS variants_chr_pos_idx ON variants(chrom, pos);
    ")
}

#' Close a db connection
#' 
#' @export
closeCon <- function(con) {
    DBI::dbDisconnect(con, shutdown = TRUE)
}


# =============================================================================
# === variant caching from gnomad responses
# =============================================================================

#' Retrieve a cached variant from the database
#' @export
getCachedVariant <- function(chrom, pos, ref, alt, con = NULL) {
    if (is.null(con)) {
        con <- getDBCon(readonly = TRUE)
        on.exit(closeCon(con))
    }

    result <- DBI::dbGetQuery(con, "
        SELECT genome FROM variants
        WHERE chrom = ? AND pos = ? AND ref = ? AND alt = ?;
    ", params = list(chrom, pos, ref, alt))

    if (nrow(result) == 0) {
        return(NULL)
    }

    if (is.na(result$genome[[1]]) || is.null(result$genome[[1]])) {
        return(NULL)
    }

    return(jsonlite::fromJSON(result$genome[[1]]))
}

#' Retrieve a dataframe of cached variants from the database
#' that match an input dataframe with columns chrom, pos, ref, alt
#' @export
getCachedVariantsDF <- function(uploaded_data, con = NULL) {
    if (is.null(con)) {
        con <- getDBCon(readonly = TRUE)
        on.exit(closeCon(con))
    }

    vals <- glue_sql("({chrom}, {pos}, {ref}, {alt})",
                    .con = con, .x = uploaded_data)

    sql <- glue_sql("
    SELECT v.chrom, v.pos, v.ref, v.alt, v.genome
    FROM variants v
    JOIN (VALUES {vals*}) AS u(chrom, pos, ref, alt)
        ON v.chrom = u.chrom
    AND v.pos   = u.pos
    AND v.ref   = u.ref
    AND v.alt   = u.alt
    ", .con = con)

    res <- DBI::dbGetQuery(con, sql)

    return(res)
}


#' Cache a variant response into the database
#' @export
cacheVariant <- function(chrom, pos, ref, alt, genome_data, con = NULL) {
    if (is.null(con)) {
        con <- getDBCon(readonly = FALSE)
        on.exit(closeCon(con))
    }

    if (is.null(genome_data) || is.na(genome_data)) {
        genome_json <- NA
    } else {
        genome_json <- jsonlite::toJSON(genome_data, auto_unbox = TRUE)
    }

    DBI::dbExecute(con, "
        INSERT OR REPLACE INTO variants (chrom, pos, ref, alt, genome)
        VALUES (?, ?, ?, ?, ?);
    ", params = list(chrom, pos, ref, alt, genome_json))
}

# =============================================================================
# === job management
# =============================================================================

#' Add a job result to the database
#' 
#' @param user_email The email of the user associated with the job result
#' @param results The results data frame to store
#' @param con A connection object; if NULL, creates a new connection and closes it after use
#' @return The UUID of the stored job result, for inclusion in emails
#' @export
addJobResult <- function(user_email, succeeded, results = NA, error_message = NA, con = NULL) {
    if (is.null(con)) {
        con <- getDBCon(readonly = FALSE)
        on.exit(closeCon(con))
    }

    # ensure the job tables exist (this will typically be a no-op)
    makeTables(con)

    job_result_id <- uuid::UUIDgenerate()
    
    if (!is.null(results) && !is.na(results)) {
        results_blob <- blob::blob(serialize(results, NULL))
    } else {
        results_blob <- NA
    }

    message("Storing job result with ID: ", job_result_id)

    DBI::dbExecute(con, "
        INSERT INTO job_results (id, email, succeeded, results, error_message) VALUES (?, ?, ?, ?, ?);
    ", params = list(job_result_id, user_email, succeeded, results_blob, error_message))

    return(job_result_id)
}

#' Retrieve a job result from the database
#' 
#' @param job_code The UUID of the job result to retrieve
#' @param con A database connection object; if NULL, creates a new connection and closes it after use
#' @return The deserialized results data frame, or NULL if not found
#' @export
getJobResult <- function(job_code, con = NULL) {
    close_on_exit <- FALSE
    if (is.null(con)) {
        con <- getDBCon(readonly = TRUE)
        close_on_exit <- TRUE
    }

    # Query the job_results table
    result <- DBI::dbGetQuery(con, "
        SELECT results, retrieved_on FROM job_results WHERE id = ?;
    ", params = list(job_code))

    if (close_on_exit) {
        closeCon(con)
    }

    if (nrow(result) == 0) {
        return(NULL)
    }

    # Mark as retrieved if this is the first time
    if (is.na(result$retrieved_on[1])) {
        con_write <- getDBCon(readonly = FALSE)
        DBI::dbExecute(con_write, "
            UPDATE job_results SET retrieved_on = CURRENT_TIMESTAMP WHERE id = ?;
        ", params = list(job_code))
        closeCon(con_write)
    }

    # Deserialize and return the results
    return(unserialize(result$results[[1]]))
}
