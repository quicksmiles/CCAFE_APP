#
# This file is a starting point toward functions that will serve to merge
# a users gzipped file data with gnomADs gh38 version 4.1.0 population specific data. 
# This will achieve our overall goal of applying CCAFE methods to a users data.
#
# Further research, development, and code readability is required.
#
library(data.table)
# CaseControlAF methods will be applied to the 'joined' dataset 
# after successful query and merge functionality 

# To install this package from GitHub:
#   
#   if(!require(devtools, quietly = TRUE)) {
#     install.packages("devtools")
#   }
# 
# devtools::install_github("https://github.com/wolffha/CCAFE")
library(CCAFE)
# Link query.R file and its functions to be executed once merge.R is run 
source("query.R", local = TRUE)
uploaded_file_path = "../CCAFE/uploaded_user_file.txt.gz"
user_data <- read.delim(uploaded_file_path, header = TRUE, sep = "\t")
# merge sample data and gnomAD chr1 data by chromosome, position combination values
combined_results <- merge(user_data, query_results, by = c("chrom", "pos"))
# merge on correct ref and alt allele
# combined_results <- combined_results[
#   (combined_results$ref.x == combined_results$alt.y & combined_results$ref.x == combined_results$ref.y) |
#   (combined_results$ref.x == combined_results$ref.y & combined_results$alt.x == combined_results$alt.y),
# ]

# This creates a data frame for each row in `variants` with extracted gnomAD population specific data
population_results <- lapply(combined_results$joint, function(populations) {
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
  
# Create a unique data frame for each "pos", "ref", "alt" in variants
final_results <- do.call(rbind, lapply(seq_along(combined_results$pos), function(i) {
  # Extract row-specific population data
  pop_data <- updated_population_results$populations[i, ]
  
  # Add columns for "pos", "ref", and "alt"
  cbind(
    combined_results,
    pop_data
  )
}))

# #### This was used to access a proxy gnomAD dataset ####
# # access file path to sampled and organized proxy gnomAD data set from a 
# # users working directory in math server
# # user_wd <- "/data001/projects/username/data"  # (CHANGE AS NEEDED)
# # read in the specified sampled gnomAD data set you want 
# # in this case it is assumed that all of chr1 has been
# # previously sampled and retrieved from the math server path:
# # "/storage/math/projects/gnomad-public/gnomADv3.1.2/chr1/chr1AFfilteredallNAremoved.txt.gz"
# # gnomAD_dat <- fread(paste0(user_wd, "gnomad_data.txt.gz")) # (CHANGE AS NEEDED)
# ########################################################
# 
# ###############################################################################
# #### Here is where the direction to query gnomAD would occur using GraphQL ####
# ###############################################################################
# 
# # manipulate/reshape sampled gnomAD chr1 data 
# # this may have to be changed depending on how the query processes fields
# # and how those fields are structured within GraphQL
# gnomAD_dat <- gnomAD_dat[,c("CHROM", "POS", "REF", "ALT", "AFafr")]
# # Additional super-population column variables to add for future MAF values:
# # "AFami", "AFamr", "AFasj", "AFeas", "AFfin", "AFnfe", "AFmid", "AFsas", "AFoth"
# 
# ######### Retrieve & Calculate Super-population Minor Allele Frequency Columns #########
# 
# # if African population AF greater than .5 it is not MAF 
# gnomAD_dat$is_AFR_MAF <- ifelse(gnomAD_dat$AFafr>.5, FALSE , TRUE)
# # if African population AF greater than .5 retrieve MAF (1-popAF) else pop AF == pop MAF
# gnomAD_dat$MAF_afr <- ifelse(!(gnomAD_dat$is_AFR_MAF), 1-gnomAD_dat$AFafr , gnomAD_dat$AFafr)
# # if not MAF the ref allele val is switched with alt allele val, otherwise remains the same (ref==ref)
# gnomAD_dat$MAF_reference <- ifelse(!(gnomAD_dat$is_AFR_MAF), gnomAD_dat$ALT, gnomAD_dat$REF)
# # if not MAF the alt allele val is switched with ref allele val, otherwise remains the same (alt==alt)
# gnomAD_dat$MAF_alternate <- ifelse(!(gnomAD_dat$is_AFR_MAF), gnomAD_dat$REF, gnomAD_dat$ALT)
# 
# ######### Merge Datasets #########
# # since not all user files are structured the same we
# # dynamically check the user file (in this case sample_dat)
# # to assure the matching string combination required to merge is used
# if(all(c("chr", "pos") %in% names(sample_dat))){
#   by_x <- c("chr", "pos")
# } else if(all(c("CHR", "POS") %in% names(sample_dat))){
#   by_x <- c("CHR", "POS")
# } else if(all(c("CHROM", "POS")) %in% names(sample_dat)){
#   by_x <- c("CHROM", "POS")
# }
# # merge sample data and gnomAD chr1 data by chromosome, position combination values
# joined <- merge(AoU_dat, gnomAD_dat, by.x = by_x, by.y = c("CHROM", "POS"))
# # merge on correct ref and alt allele
# joined <- joined[(joined$REF.x == joined$ALT.y & joined$ALT.x == joined$REF.y) | (joined$REF.x == joined$REF.y & joined$ALT.x == joined$ALT.y),]

# export file to given path (CHANGE AS NEEDED)
# write.table(joined, file="/data001/projects/lemusgov/CaseControl/Merging/merged_data.text", sep="\t", quote = FALSE, row.names=F,col.names=T)