# ---------------------------------------------------------------------------- #
#                     Script to create auxiliary files                         #
# ---------------------------------------------------------------------------- #

#library(tidyverse)
# Define the list of required packages
packages_needed <- c("dplyr")

# Install missing packages and load them
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    require(pkg, character.only = TRUE)
  }
  else{
    library(pkg, character.only = TRUE)
  }
}

# By now we have created several versions of the auxiliary files. Define here
# which version we have here.
version <- 'V5'

smart_full_join <- function(left_df, right_df, by_col) {
  # Perform full join by 'id'
  joined_df <- full_join(left_df, right_df, by = by_col)
  
  # Find common columns (other than 'id') that appear in both dataframes
  common_columns <- intersect(names(left_df), names(right_df))
  common_columns <- setdiff(common_columns, by_col)  # Exclude 'id' from common columns
  
  # For each common column, prioritize the left side, and fallback to the right side if NA
  for (col in common_columns) {
    joined_df[[col]] <- coalesce(joined_df[[paste0(col, ".x")]], joined_df[[paste0(col, ".y")]])
    joined_df <- joined_df %>% select(-matches(paste0(col, "\\.x$|", col, "\\.y$")))
  }
  
  return(joined_df)
}

# Load all the data from the different csv files that you need to create the 
# auxiliaries for different files
socdem <- read.csv('/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/socdem01/csv/part-00000-f22b1f47-bad5-41f2-869b-845f7b0178cc-c000.csv')%>%
  dplyr::select(src_subject_id, chrdemo_hispanic_latino)
incexc <- read.csv('/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/iec01/csv/part-00000-74cc2bb4-23c4-4f29-9c9b-0b49aaa88e7c-c000.csv')%>%
  dplyr::select(src_subject_id, interview_age, sex, chrcrit_part, chrcrit_included)
nda <- read.csv('/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/ndar_subject01/csv/part-00000-4a245b6d-9a50-4d1e-80f8-489fd94b71b1-c000.csv')%>%
  dplyr::select(src_subject_id, interview_date, interview_age, sex, phenotype, race)
aux <- nda %>%
  smart_full_join(., incexc, 'src_subject_id') %>%
  smart_full_join(., socdem, 'src_subject_id')

# include only subjects that are included in the study  
aux_included <- aux %>%
  filter(chrcrit_included == 1)

print(paste0('Subjects not included: N=', 
             aux %>% nrow() - aux_included %>% nrow(), 
             sep = ''))

# only subjects that have a defined studygroup should be included
aux_included_studygroup <- aux_included %>%
  filter(chrcrit_part == 2|chrcrit_part == 1)
print(paste0('Subjects without a studygroup: N=', 
             aux_included %>% nrow() - aux_included_studygroup %>% nrow(), 
             sep = ''))

# create the site variable
auxiliaries <- aux_included_studygroup %>%
  mutate(site = substr(src_subject_id, 1, 2))
