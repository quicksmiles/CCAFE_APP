# ghql is a library for making graphql queries
# intro to ghql:
# https://docs.ropensci.org/ghql/articles/ghql.html
box::use(
  ghql[...],
  tidyverse[...],
  ratelimitr[...],
  dplyr[...],
)
# ratelimitr is a library for rate limiting function calls, e.g. to 10 per
# second
# intro to ratelimitr:
# https://cran.r-project.org/web/packages/ratelimitr/vignettes/introduction.html

box::use(
  app/state/db[getDBCon, closeCon, makeTables, getCachedVariant, cacheVariant],
  app/utils/debug[print_structure],
)

#df <- read.delim(file = "/Users/hugolemus/Downloads/pheno_295_EUR.txt.gz", header = TRUE, sep = "\t")
gnomad_api_url <- Sys.getenv("GNOMAD_API_URL", unset="https://gnomad.broadinstitute.org/api")

USE_VARIANT_CACHE <- Sys.getenv("USE_VARIANT_CACHE", unset="FALSE") == "TRUE"
WRITE_TO_VARIANT_CACHE <- Sys.getenv("WRITE_TO_VARIANT_CACHE", unset="TRUE") == "TRUE"

# Function to read input file and generate query ranges
generate_ranges <- function(data, range_size = 100000) {
  message("Generating query ranges for dataset with ", nrow(data), " rows.")

  # if the input has zero rows, just return an empty dataframe
  if (nrow(data) == 0) {
    return(data.frame(
      chrom = character(0),
      start = integer(0),
      stop = integer(0)
    ))
  }

  data %>%
    group_by(chrom) %>%
    filter(any(!is.na(pos))) %>%
    mutate(
      min_pos = min(pos),
      max_pos = max(pos),
      bin     = (pos - min_pos) %/% range_size,
      start   = min_pos + bin * range_size,
      stop    = pmin(start + range_size - 1L, max_pos)
    ) %>%
    distinct(chrom, start, stop) %>%
    arrange(chrom, start) %>%
    ungroup()
}

do_query <- function(uploaded_data) {
  # build a connection object to consume gnomad's graphql API
  con <- GraphqlClient$new(
    url = gnomad_api_url
  )

  # build a template for the query with params that we'll provide
  # to the execute_query function
  qry <- Query$new()

  # you can visit https://gnomad.broadinstitute.org/api to see the available
  # entities in gnomad. on that site, click the 'Docs' button in the top-right,
  # then click 'Query' in the resulting sidebar, and you'll see the available
  # queryable entities. you can also try out graphql queries against the API on
  # that site.

  qry$query(
    'VariantsInRange',
    'query($chrom: String!, $start: Int!, $stop: Int!) {
      region(chrom: $chrom, start: $start, stop: $stop, reference_genome: GRCh38) {
        chrom, start, stop, reference_genome,

        variants(dataset: gnomad_r4) {
          chrom
          pos
          ref
          alt
          genome {
            populations {
              id
              ac
              an
              }
            }
          }
        }
      }'
  )

  # create a function to invoke the graphql query.
  # throttle the function to 10 requests per 60 seconds
  # per stated limits at https://gnomad.broadinstitute.org/data#api
  exec_query <- limit_rate(
    function(args) {
      
      tryCatch({
        x <- con$exec(qry$queries$VariantsInRange, args)
        return(jsonlite::fromJSON(x))
      }, error = function(e) {
        message("Error: ", e$message)
        # also print the body of the error response
        message("Response: ", e$response)
        
        return(NULL)
      })
      # x <- con$exec(qry$queries$VariantsInRange, args)
      # return(jsonlite::fromJSON(x))
    },
    rate(n = 10, period = 60)
  )

  # get a handle to the db so we can pull cached variants
  dbCon <- getDBCon(readonly = TRUE)
  on.exit(closeCon(dbCon))

  # ensure the tables we need exist
  makeTables(dbCon)

  # this list will ultimately store all of the query results
  results <- list()

  if (USE_VARIANT_CACHE) {
    message("USE_VARIANT_CACHE=TRUE, using variant cache for queries.")

    # iterate through uploaded_data to find which variants are cached
    # and add them to results
    variants_cached <- uploaded_data %>%
      rowwise() %>%
      mutate(
        genome_populations = list(
          getCachedVariant(chrom, pos, ref, alt, con = dbCon)
        )
      ) %>%
      # filter(!is.null(genome_populations)) %>%
      ungroup() %>%
      arrange(chrom, pos)

    assign("variants_cached_df", variants_cached, envir = .GlobalEnv)

    # if there are any, put these cached variants into results
    if (nrow(variants_cached) > 0) {
      message("Found ", nrow(variants_cached), " cached variants.")

      for (i in seq_len(nrow(variants_cached))) {
        # 1) ensure populations is a plain data.frame
        pop_df <- as.data.frame(variants_cached$genome_populations[[i]]$populations,
                                stringsAsFactors = FALSE)
        rownames(pop_df) <- NULL

        # 2) make a 1x1 data.frame whose single column is 'populations'
        genome_df <- data.frame(
          populations = I(list(pop_df)),
          stringsAsFactors = FALSE
        )

        # 3) variants is a data.frame with a 'genome' column that is itself a 1x1 df
        variants_df <- data.frame(
          chrom = as.character(variants_cached$chrom[i]),
          pos   = as.integer(variants_cached$pos[i]),
          ref   = as.character(variants_cached$ref[i]),
          alt   = as.character(variants_cached$alt[i]),
          genome = I(genome_df),
          stringsAsFactors = FALSE
        )

        results[[length(results) + 1]] <- list(
          data = list(
            region = list(
              variants = variants_df
            )
          )
        )
      }
    }

    # create a version of uploaded_data that uses results
    # to exclude variants that are already cached
    uploaded_data_sans_cached <- uploaded_data %>%
      anti_join(
        variants_cached %>%
          select(chrom, pos, ref, alt),
        by = c("chrom", "pos", "ref", "alt")
      )

    # generate ranges for the remaining variants we need to query for, since
    # they're apparently not in the cache
    ranges <- generate_ranges(uploaded_data_sans_cached)
  }
  else {
    message("USE_VARIANT_CACHE=FALSE, Not using variant cache for queries.")

    # generate the full set of ranges, since we have no cache to remove any of them
    ranges <- generate_ranges(uploaded_data)
  }

  # Query gnomAD API for each range and store the results
  full_query_runtime <- system.time({
    if (nrow(ranges) > 0) {
      for (i in seq_len(nrow(ranges))) {
        single_query_runtime <- system.time({
          message("Querying range ", i, " of ", nrow(ranges))
          args <- list(
            chrom = as.character(ranges$chrom[i]),
            start =  as.integer(ranges$start[i]),
            stop = as.integer(ranges$stop[i])
          )
          message("Args: ", paste(names(args), args, sep="=", collapse=", "))
          
          result <- exec_query(args)

          # remove the chrom, start, stop, reference_genome fields from result$data$region
          for (field in c("chrom", "start", "stop", "reference_genome")) {
            result$data$region[[field]] <- NULL
          }

          if (!is.null(result) && !is.null(result$data$region$variants)) {
            # extract dataframe
            variants_df <- result$data$region$variants

            # filter it to just variants in uploaded_data
            # (note that this solves the memory issue we were having before, where 
            # all the variants in large ranges were being retained -- consuming
            # large amounts of memory -- which are ultimately dropped near the
            # end of the merge process.)
            filtered_variants <- dplyr::semi_join(
              variants_df,
              uploaded_data,
              by = c("chrom", "pos")
            )

            # print out the contents of the filtered_variants dataframe
            message("Filtered variants for range ", i, ":")

            message(paste(utils::capture.output(print(filtered_variants)), collapse = "\n"))

            if (WRITE_TO_VARIANT_CACHE) {
              message("WRITE_TO_VARIANT_CACHE=TRUE, writing queried variants to cache.")

              # insert each variant into the cache
              for (j in seq_len(nrow(filtered_variants))) {
                # check if filtered_variants$genome[j] is not NULL; if it is, skip caching
                # TODO: this still needs to be tested on the 500-variant sample file
                if (is.null(filtered_variants$populations[[j]])) {
                  message("Variant at ", filtered_variants$chrom[j], ":", filtered_variants$pos[j],
                          " has no population data; writing NULL to cache.")
                  
                  genome_data_val <- NA
                }
                else {
                  genome_data_val <- list(
                    populations = filtered_variants$populations[[j]]
                  )
                }

                cacheVariant(
                  chrom = filtered_variants$chrom[j],
                  pos = filtered_variants$pos[j],
                  ref = filtered_variants$ref[j],
                  alt = filtered_variants$alt[j],
                  genome_data = genome_data_val,
                  con = dbCon
                )
              }
            } else {
              message("WRITE_TO_VARIANT_CACHE=FALSE, not writing queried variants to cache.")
            }

            # reassign filtered variants to result's field
            result$data$region$variants <- filtered_variants

            results[[i]] <- result
          }
        })

        # emit this range's timing
        message("Single query runtime for range ", i, ": ", single_query_runtime[3], " seconds")
      }
    }
    else {
      message("All variants were found in the cache; no gnomad queries needed.")
    }
  })

  # Display system run time for query 
  message("Full query runtime: ", full_query_runtime[3], " seconds")

  # print_structure(results, name = "final_results_list")

  assign(
    paste0("final_results_list__cached_", as.character(USE_VARIANT_CACHE)),
    results, envir = .GlobalEnv
  )

  # Combine all variant query results from all generated range values
  query_results <- bind_rows(lapply(results, function(res){
    if (!is.null(res$data$region$variants)) {
      variants <- as.data.frame(res$data$region$variants)
      #return(variants)
    } else {
      NULL
    }
  }))

  # Start merging process here
  # Here we can apply a function where from the query we only extract the variants in the uploaded file before further processing
}
