box::use(
  CCAFE[...],
)

box::use(
  app/logic/merge[...],
  app/logic/email[...],
)

# To install this package from GitHub:
#   
#   if(!require(devtools, quietly = TRUE)) {
#     install.packages("devtools")
#   }
# 
# devtools::install_github("https://github.com/wolffha/CCAFE")

handle_se <- function(selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
                         OR_colname_se, SE_colname_se, chromosome_colname, position_colname) {

  # indicate that we've started the bg process
  message("Starting ControlCase_SE method...")
  
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
  casecontrol_result <- tryCatch({
    data <- CaseControl_SE(data = uploaded_data,
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

    message("CaseControl_SE completed successfully.")

    # save the job results to the db, giving us a UUID to email to the user
    job_result_id <- save_results(data, user_email, TRUE)

    message("Job results saved with ID: ", job_result_id)

    # finally, send the success email with an embedded link to the results
    send_results_email(user_email, job_result_id)

    message("Results email sent to: ", user_email)

    TRUE  # return TRUE on success
  }, error = function(e) {
    message("Error in CaseControl_SE: ", e$message)

    # get the traceback as a string and write that out
    tb <- utils::capture.output(traceback())
    message("Traceback:\n", paste(tb, collapse = "\n"))

    # send failure email
    send_failure_email(user_email, e$message)

    message("Failure email sent to: ", user_email)

    FALSE  # return FALSE in case of error
  })

  return(casecontrol_result)
}