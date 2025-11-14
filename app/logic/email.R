box::use(
  httr[POST, authenticate, status_code],
  data.table[fwrite],
  app/state/db[addJobResult],
)

#' Save results and return a job result ID
#' @param results The results data frame to save
#' @param user_email The email of the user associated with the results
#' @param succeeded Boolean indicating if the job succeeded
#' @return The UUID of the stored job result
#' @export
save_results <- function(results, user_email, succeeded) {
  # adds the results to the database and
  # returns the UUID for the stored results
  job_result_id <- addJobResult(
    user_email = user_email,
    succeeded = succeeded,
    results = ifelse(succeeded, results, NA),
    error_message = ifelse(succeeded, NA, as.character(results))
  )

  # save the results to a folder named job_exports under the current working directory
  output_dir <- "job_exports/"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  # save the results as a gzipped tsv file
  output_path <- file.path(output_dir, paste0(job_result_id, ".tsv.gz"))
  fwrite(results, output_path, sep = "\t", compress = "gzip")

  return(job_result_id)
}

#' Generic function to send an email via Mailgun
#' @param email Recipient email address
#' @param subject Email subject
#' @param text Email body text
#' @return TRUE if email sent successfully, FALSE otherwise
#' @export
send_email <- function(email, subject, text) {
  mailgun_api_key <- Sys.getenv("MAILGUN_API_KEY")
  domain <- Sys.getenv("MAILGUN_DOMAIN")

  res <- POST(
    url = paste0("https://api.mailgun.net/v3/", domain, "/messages"),
    authenticate("api", mailgun_api_key),
    body = list(
      from = Sys.getenv("MAILGUN_FROM"),
      to = email,
      subject = subject,
      text = text
      # attachment = httr::upload_file(file_path)
    )
  )
  
  if (status_code(res) == 200) {
    # message(paste("Email sent to:", email))
    return(TRUE)
  } else {
    message("Email sending failed.")
    return(FALSE)
  }
}

#' Send results email with link to retrieve results
#' @param user_email The recipient's email address
#' @param job_result_id The UUID of the job result
#' @return the result of send_email()
#' @export
send_results_email <- function(user_email, job_result_id) {
  # app_base_url <- Sys.getenv("APP_BASE_URL", unset="https://hendrickslab.cu-dbmi.dev/ccafe")

  # construct a URL for the job results file
  results_url <- paste0(
    Sys.getenv("APP_BASE_URL", unset="https://hendrickslab.cu-dbmi.dev/ccafe"), "/job_exports/", job_result_id, ".tsv.gz"
  )
  
  send_email(
    user_email, subject = "Your Case Control AF Results",
    text = stringr::str_glue(
      "Your CCAFE App results are available at the following link:\n\n",
      # "{Sys.getenv('APP_BASE_URL', unset='https://hendrickslab.cu-dbmi.dev/ccafe')}/retrieve_results/{job_result_id}\n\n",
      "{results_url}\n\n",
      "Thank you for using CCAFE!"
  ))
}

#' Send job failure notification email
#' @param user_email The recipient's email address
#' @param error_message The error message to include in the email
#' @return the result of send_email()
#' @export
send_failure_email <- function(user_email, error_message) {
  send_email(
    user_email, subject = "CCAFE App Job Failed",
    text = stringr::str_glue(
      "Unfortunately, your CCAFE App job has failed with the following error:\n\n",
      "{error_message}\n\n",
      "Please try again or contact us for support."
  ))
}
