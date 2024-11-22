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

      variants(dataset: gnomad_r4) {
        chrom
        pos
        ref
        alt
        joint {
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
  function(args = list(chrom = "17", start = 49210411, stop = 49210412)) {
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

gnomAD_data <- exec_query(list(chrom = "17", start = 49210411, stop = 49210412))
joint_data <- gnomAD_data$data$region$variants$joint

print(
  as.data.frame(gnomAD_data)
)