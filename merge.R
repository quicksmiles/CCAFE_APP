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
# sample data from Hayley's CaseControAF library 
# all 500 variants are on chr1
data("sampleDat")
# gnomAD should be queried based on this dataset
# *note all data is on chromosome 1*
sample_dat <- as.data.frame(sampleDat)

#### This was used to access a proxy gnomAD dataset ####
# access file path to sampled and organized proxy gnomAD data set from a 
# users working directory in math server
# user_wd <- "/data001/projects/username/data"  # (CHANGE AS NEEDED)
# read in the specified sampled gnomAD data set you want 
# in this case it is assumed that all of chr1 has been
# previously sampled and retrieved from the math server path:
# "/storage/math/projects/gnomad-public/gnomADv3.1.2/chr1/chr1AFfilteredallNAremoved.txt.gz"
# gnomAD_dat <- fread(paste0(user_wd, "gnomad_data.txt.gz")) # (CHANGE AS NEEDED)
########################################################

###############################################################################
#### Here is where the direction to query gnomAD would occur using GraphQL ####
###############################################################################

# manipulate/reshape sampled gnomAD chr1 data 
# this may have to be changed depending on how the query processes fields
# and how those fields are structured within GraphQL
gnomAD_dat <- gnomAD_dat[,c("CHROM", "POS", "REF", "ALT", "AFafr")]
# Additional super-population column variables to add for future MAF values:
# "AFami", "AFamr", "AFasj", "AFeas", "AFfin", "AFnfe", "AFmid", "AFsas", "AFoth"

######### Retrieve & Calculate Super-population Minor Allele Frequency Columns #########

# if African population AF greater than .5 it is not MAF 
gnomAD_dat$is_AFR_MAF <- ifelse(gnomAD_dat$AFafr>.5, FALSE , TRUE)
# if African population AF greater than .5 retrieve MAF (1-popAF) else pop AF == pop MAF
gnomAD_dat$MAF_afr <- ifelse(!(gnomAD_dat$is_AFR_MAF), 1-gnomAD_dat$AFafr , gnomAD_dat$AFafr)
# if not MAF the ref allele val is switched with alt allele val, otherwise remains the same (ref==ref)
gnomAD_dat$MAF_reference <- ifelse(!(gnomAD_dat$is_AFR_MAF), gnomAD_dat$ALT, gnomAD_dat$REF)
# if not MAF the alt allele val is switched with ref allele val, otherwise remains the same (alt==alt)
gnomAD_dat$MAF_alternate <- ifelse(!(gnomAD_dat$is_AFR_MAF), gnomAD_dat$REF, gnomAD_dat$ALT)

######### Merge Datasets #########
# since not all user files are structured the same we
# dynamically check the user file (in this case sample_dat)
# to assure the matching string combination required to merge is used
if(all(c("chr", "pos") %in% names(sample_dat))){
  by_x <- c("chr", "pos")
} else if(all(c("CHR", "POS") %in% names(sample_dat))){
  by_x <- c("CHR", "POS")
} else if(all(c("CHROM", "POS")) %in% names(sample_dat)){
  by_x <- c("CHROM", "POS")
}
# merge sample data and gnomAD chr1 data by chromosome, position combination values
joined <- merge(AoU_dat, gnomAD_dat, by.x = by_x, by.y = c("CHROM", "POS"))
# merge on correct ref and alt allele
joined <- joined[(joined$REF.x == joined$ALT.y & joined$ALT.x == joined$REF.y) | (joined$REF.x == joined$REF.y & joined$ALT.x == joined$ALT.y),]

# export file to given path (CHANGE AS NEEDED)
# write.table(joined, file="/data001/projects/lemusgov/CaseControl/Merging/merged_data.text", sep="\t", quote = FALSE, row.names=F,col.names=T)