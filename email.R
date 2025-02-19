library(httr)

send_email <- function(email, file_path) {
  mailgun_api_key <- Sys.getenv("MAILGUN_API_KEY")
  domain <- Sys.getenv("MAILGUN_DOMAIN")
  
  res <- POST(
    url = paste0("https://api.mailgun.net/v3/", domain, "/messages"),
    authenticate("api", mailgun_api_key),
    body = list(
      from = Sys.getenv("MAILGUN_FROM"),
      to = email,
      subject = "Your CaseControl_SE Results",
      text = "Attached are your results.",
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

# Read stored metadata
metadata_file <- "../CCAFE/results_metadata.csv"
metadata <- read.csv(metadata_file)

# Only run when there are entries in metadata
if (nrow(metadata) > 0) {
  # Track entries to remove
  entries_to_keep <- metadata
  
  for (i in seq_len(nrow(metadata))) {
    email <- metadata$email[i]
    file_path <- metadata$file[i]
    
    if (send_email(email, file_path)) {
      # Delete the file after successful email
      file.remove(file_path)
      print(paste("Deleted file:", file_path))
      
      # Remove entry from metadata
      entries_to_keep <- entries_to_keep[-i, ]
    }
  }
  
  # Update metadata file (overwrite with remaining entries)
  write.csv(entries_to_keep, metadata_file, row.names = FALSE)
}
