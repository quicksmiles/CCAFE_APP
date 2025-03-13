source("app/utils/merge.R", local = TRUE)
source("app/utils/email.R", local = TRUE)

# To install this package from GitHub:
#   
#   if(!require(devtools, quietly = TRUE)) {
#     install.packages("devtools")
#   }
# 
# devtools::install_github("https://github.com/wolffha/CCAFE")
library(CCAFE)

handle_se <- function(selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
                         OR_colname_se, SE_colname_se, chromosome_colname, position_colname) {
  # Call merge.R final_results function to obtain gnomAD population data
  super_population_results <- do_merge(uploaded_data, selected_population)
  selected_MAF_col <- paste0("MAF_", selected_population)
  # Create the correction data dataframe required as an argument for CC_SE function
  corr_data <- data.frame(CHR = as.character(super_population_results$chrom),
                          POS = super_population_results$pos,
                          # Use the user selected population to correct for bias
                          proxy_MAF = super_population_results[[selected_MAF_col]]
  )

  # Perform CaseControl_SE operation
  results_se <- CaseControl_SE(data = uploaded_data,
                               N_case = N_case_se,
                               N_control = N_control_se,
                               OR_colname = OR_colname_se,
                               SE_colname = SE_colname_se,
                               chromosome_colname = chromosome_colname,
                               position_colname = position_colname,
                               do_correction = TRUE,
                               correction_data = corr_data,
                               sex_chromosomes = FALSE,
                               remove_sex_chromosomes = TRUE
  )
  
  save_results(results_se, user_email)
  print("Saved SE results")
  
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
  
  return(results_se)
  print("ControlCase_SE method was executed successfully!")
}