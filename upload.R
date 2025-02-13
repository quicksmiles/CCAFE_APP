#
# This file provides functions utilized by file_upload.R that aims
# to facilitate the read and upload of user files into CCAFE App.
#
# Find out more about CCAFE methods and their use here:
#
#       https://wolffha.github.io/CCAFE_documentation/
#


extract_info_fields <- function(vcf, elements) {
  # Initialize a list to store extracted columns
  info_data <- list()
  
  # Loop through each element to extract from INFO
  for (element in elements) {
    # Use regex to match the pattern for each element
    info_data[[element]] <- vcfR::extract.info(vcf, element = element)
  }
  
  # Combine extracted columns into a data frame
  info_df <- as.data.frame(do.call(cbind, info_data))
  colnames(info_df) <- elements
  
  info_df
}

# Read the VCF file in chunks
vcf_upload <- function(vcf_file) {
  # Initialize the progress indicator
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Processing VCF File", value = 0)
  
  tryCatch({
    file_path <- vcf_file$datapath
    
    # Using vcfR for flexible reading
    progress$inc(0.2, detail = "Reading VCF file")
    vcf <- read.vcfR(file_path, verbose = TRUE)
    
    # Extract INFO fields and basic data
    progress$inc(0.4, detail = "Parsing INFO fields")
    info_fields <- c("AC", "AN", "AF")
    info <- (extract_info_fields(vcf = vcf, elements = info_fields))
    
    # Select relevant columns to display
    progress$inc(0.3, detail = "Data Loaded Successfully")
    vcf_data <- cbind(vcf@fix[, c("CHROM", "POS", "REF", "ALT", "QUAL", "FILTER")], info)
    colnames(vcf_data) <- c("chrom", "pos", "ref", "alt", "qual", "filter", "ac", "an", "af")
    vcf_data <- vcf_data[complete.cases(vcf_data), ]
    vcf_dt <- as.data.frame(vcf_data)
    vcf_dt
    
  }, error = function(e) {
    showNotification(paste("Error loading VCF:", e$message), type = "error")
    NULL
  })
}

# Read the text file in chunks
text_upload <- function(text_file) {
  # Initialize the progress indicator
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Processing File", value = 0)
  
  tryCatch({
    file_path <- text_file$datapath
    
    # Using fread to read the user uploaded text file
    progress$inc(0.2, detail = "Reading VCF file")
    
    # Read the text file using fread to unzip first
    # Filter and alter columns
    text_data <- fread(cmd = paste("gzip -dc", file_path), header = TRUE, sep = "\t")
    # Rename the first 4 columns of the file as chrom, pos, ref, alt
    colnames(text_data)[1:4] <- c("chrom", "pos", "ref", "alt")
    # Alter the row values in the chrom column if they include "chr"
    # remove "chr" from string so only the number value remains
    text_data$chrom <- ifelse(grepl("^chr", text_data$chrom), gsub("^chr", "", text_data$chrom), text_data$chrom) 
    # Convert processed uploaded file data into a data.table structure
    progress$inc(0.4, detail = "Parsing necessary fields")
    text_data <- text_data[complete.cases(text_data), ]
    text_dt <- as.data.frame(text_data)
    # 
    progress$inc(0.3, detail = "Data Loaded Successfully")
    text_dt
    
  }, error = function(e) {
    showNotification(paste("Error loading VCF:", e$message), type = "error")
    NULL
  })
}


upload_file <- function(file, file_name, file_path) {
  ext <- tools::file_ext(file_name)
  switch(ext,
         "bgz" = return(vcf_upload(file)),
         "gz" = ifelse(grepl("\\.txt\\.gz$", file_name) || grepl("\\.text\\.gz$", file_name),
                       return(text_upload(file)),
                       validate("Invalid file: Please upload a .txt or .text gzip file")),
         validate("Invalid file: Please upload a a .txt or .text gzip file")
  )
}


save_file <- function(file_data, output_dir = "/tmp/CCAFE/", output_name = "uploaded_user_file.text.gz") {
  # ensure the output folder exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  new_path <- file.path(output_dir, output_name)
  fwrite(file_data, new_path, sep = "\t", compress = "gzip")
  return(new_path)
}