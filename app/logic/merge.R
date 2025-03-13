#
# This file is a starting point toward functions that will serve to merge
# a users compressed file data with gnomADs GRCh38 version 4.1.0 population specific data. 
# This will achieve our overall goal of applying CCAFE methods to a users data.
#
# Further research, development, and code readability is required.
#
library(data.table)
library(dplyr)
library(purrr)
# Link query.R file and its functions to be executed once merge.R is run 
source("app/utils/query.R", local = TRUE)

do_merge <- function(uploaded_data, user_selected_population) {
  # call query method
  query_results <- do_query(uploaded_data)

  # merge user data and gnomAD data by chromosome, position combination values
  combined_results <- merge(uploaded_data, query_results, by = c("chrom", "pos"))
  combined_results <- combined_results %>% filter(map_lgl(combined_results$genome$populations, ~ !is.null(.)))

  # This creates a data frame for each row in `variants` with extracted gnomAD population specific data
  population_results <- lapply(combined_results$genome, function(populations) {
    # Combine all population data frames into one
    combined_pop <- do.call(rbind, populations)
    
    
    # Reshape "ac" and "an" into separate columns for each "id"
    reshaped_pop <- do.call(cbind, lapply(split(combined_pop, combined_pop$id), function(df) {
      # Create columns like "amr_ac" and "amr_an"
      data.frame(
        ac = setNames(df$ac, paste(df$id[1], "ac")),
        an = setNames(df$an, paste(df$id[1], "an"))
      )
    }))
    colnames(reshaped_pop) <- gsub("\\.", "_", colnames(reshaped_pop))
    return(reshaped_pop)
    
  })

  # Iterate over each reshaped population data frame in the list
  af_population_results <- lapply(population_results, function(af_populations) {
      # Identify unique population abbreviations based on column names
      populations <- population_results$populations
      pop_ids <- unique(sub("_.*", "", colnames(populations)))
      # Iterate over population IDs to calculate allele frequencies
      for (pop in pop_ids) {
        ac_col <- paste0(pop, "_ac")
        an_col <- paste0(pop, "_an")
        af_col <- paste0("AF_", pop)
        
        # Check if both ac and an columns exist for the specified population
        if (ac_col %in% colnames(populations) && an_col %in% colnames(populations)) {
          # add calculated allele frequency values to existing population_results dataframe
          af_populations[[af_col]] <- populations[[ac_col]] / populations[[an_col]]
        } else {
          af_populations[["AF_total"]] <- populations[["ac"]] / populations[["an"]]
        }
      }
      return(af_populations)
  })

  maf_population_results <- lapply(af_population_results, function(maf_populations) {
    af_populations <- af_population_results$populations
    matches <- grepl("^AF", colnames(af_populations))
    af_ids <- sub("*AF_", "", colnames(af_populations[matches]))
    
    for(af in af_ids) {
      af_col <- paste0("AF_", af)
      maf_col <- paste0("MAF_", af)
      is_maf_col <- paste0("isMAF_", af)
      maf_ref_col <- paste0(af , "MAF_ref")
      maf_alt_col <- paste0(af, "MAF_alt")
      
      # Check allele frequencies in af_population_results
      # if population af greater than 0.5 it is not the minor allele frequency 
      maf_populations[[maf_col]] <- ifelse(af_populations[[af_col]] > 0.5, 
                                          1 - af_populations[[af_col]], 
                                          af_populations[[af_col]])
      maf_populations[[is_maf_col]] <- af_populations[[af_col]] <= 0.5
      maf_populations[[maf_ref_col]] <- ifelse(af_populations[[af_col]] <= 0.5, 
                                              combined_results$ref.y, combined_results$alt.y)
      maf_populations[[maf_alt_col]] <- ifelse(af_populations[[af_col]] > 0.5, 
                                              combined_results$ref.y, combined_results$alt.y)
    }
    return(maf_populations)
  })

  pre_merged_results <- cbind(combined_results, maf_population_results$populations)

  final_merge <- inner_join(uploaded_data, pre_merged_results, by = c("chrom", "pos"), relationship = "many-to-many")

  # merge on correct ref and alt allele
  final_merge <- final_merge[
   (final_merge$ref.x == final_merge$alt.y & final_merge$ref.x == final_merge$ref.y) |
   (final_merge$ref.x == final_merge$ref.y & final_merge$alt.x == final_merge$alt.y),
  ]
  
  #
  start_col <- which(names(final_merge) == "alt")
  end_col <- which(names(final_merge) == "afr_ac")
  
  # Remove all columns in between start and end columns (excluding start and end)
  final_merge <- final_merge[ , -((start_col + 1):(end_col - 1)), drop = FALSE]
  
  maf_column <- paste0("MAF_", user_selected_population)
  # print(af_column)
  final_merge <- final_merge[final_merge[[maf_column]] > 0.01, ]
  return(final_merge)
}