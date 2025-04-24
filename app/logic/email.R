box::use(
  httr[POST, authenticate, status_code],
  data.table[fwrite],
)

save_results <- function(results, user_email) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path <- paste0("../CCAFE/results_", timestamp, ".text.gz") # change file path as needed
  
  fwrite(results, file_path, sep = "\t", compress = "gzip")
  
  # Store metadata (email, file path) for later retrieval
  metadata <- data.frame(email = user_email, file = file_path, timestamp = Sys.time())
  utils::write.csv(metadata, "../CCAFE/results_metadata.csv", row.names = FALSE, append = TRUE) #change file path as needed
}

send_email <- function(email, file_path) {

  mailgun_api_key <- Sys.getenv("MAILGUN_API_KEY")
  domain <- Sys.getenv("MAILGUN_DOMAIN")

  
  res <- POST(
    url = paste0("https://api.mailgun.net/v3/", domain, "/messages"),
    authenticate("api", mailgun_api_key),
    body = list(
      from = Sys.getenv("MAILGUN_FROM"),
      to = email,
      subject = "Your Case Control AF Results",
      text = "Attached are your CCAFE App results.",
      attachment = httr::upload_file(file_path)
    )
  )
  
  if (status_code(res) == 200) {
    print(paste("Email sent to:", email))
    return(TRUE)
  } else {
    print("Email sending failed.")
    return(FALSE)
  }
}
