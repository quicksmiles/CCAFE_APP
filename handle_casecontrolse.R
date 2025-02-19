source("merge.R", local = TRUE)

save_results <- function(results, user_email) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path <- paste0("../CCAFE/results_", timestamp, ".text.gz") # change file path as needed
  
  fwrite(results, file_path, sep = "\t", compress = "gzip")
  
  # Store metadata (email, file path) for later retrieval
  metadata <- data.frame(email = user_email, file = file_path, timestamp = Sys.time())
  write.csv(metadata, "../CCAFE/results_metadata.csv", row.names = FALSE, append = TRUE) #change file path as needed
}

handle_cc_se <- function(selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
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
  
  print("Saved SE results")
  save_results(results_se, user_email)
  
  return(results_se)
  print("ControlCase_SE method was executed successfully!")
}