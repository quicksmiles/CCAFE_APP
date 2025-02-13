# ghql is a library for making graphql queries
# intro to ghql:
# https://docs.ropensci.org/ghql/articles/ghql.html
library(ghql)
library(tidyverse)

# ratelimitr is a library for rate limiting function calls, e.g. to 10 per
# second
# intro to ratelimitr:
# https://cran.r-project.org/web/packages/ratelimitr/vignettes/introduction.html
library(ratelimitr)
#df <- read.delim(file = "/Users/hugolemus/Downloads/pheno_295_EUR.txt.gz", header = TRUE, sep = "\t")
gnomad_api_url <- Sys.getenv("GNOMAD_API_URL", unset="https://gnomad.broadinstitute.org/api")

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

      variants(dataset: gnomad_r4_non_ukb) {
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

# Function to read input file and generate query ranges
generate_ranges <- function(file_path, range_size = 100000) {
  range_size = 100000
  data <- read.delim(file_path, header = TRUE, sep = "\t")
  #data <- as.data.table(data)
  # Extract unique chromosome and position ranges
  ranges <- data %>%
    group_by(chrom) %>%
    summarise(
      min_pos = min(pos),
      max_pos = max(pos),
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    do({
      chrom <- .$chrom
      start_positions <- seq(.$min_pos, .$max_pos, by = range_size)
      data.frame(
        chrom = chrom,
        start = start_positions,
        stop = pmin(start_positions + range_size - 1, .$max_pos)
      )
    }) %>%
    ungroup()
  
  # ranges <- as.data.table(ranges)
  # 
  # setkey(ranges, chrom, start, stop)
  # ranges <- ranges[
  #   data, 
  #   on = .(chrom, start <= pos, stop >= pos), 
  #   nomatch = 0
  # ]
  # Iterate over each row of the ranges and check that 
  # data$pos values fall between the start/stop ranges
  # If TRUE keeps range start/stop values otherwise discards
  ranges$keep <- sapply(seq_len(nrow(ranges)), function(i) {
    any(data$chrom == ranges$chrom[i] & data$pos >= ranges$start[i] & data$pos <= ranges$stop[i])
  })

  # Filter the ranges dataframe
  ranges <- ranges[ranges$keep, ]
  ranges$keep <- NULL  # Drop the helper column
  
  return(ranges)
}

# Read the provided file and generate ranges
file_path <- "../CCAFE/uploaded_user_file.text.gz"  # Update this with the correct path
ranges <- generate_ranges(file_path)

# Query gnomAD API for each range and store the results
full_query_runtime <- system.time({
  results <- list()
  for (i in 1:nrow(ranges)) {
    message("Querying range ", i, " of ", nrow(ranges))
    args <- list(
      chrom = as.character(ranges$chrom[i]),
      start =  as.integer(ranges$start[i]),
      stop = as.integer(ranges$stop[i])
    )
    
    result <- exec_query(args)
    if (!is.null(result)) {
      results[[i]] <- result
    }
  }
})
# Display system run time for query 
print(full_query_runtime)

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