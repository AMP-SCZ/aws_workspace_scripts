# in this scriptw e craete the clinical dataframe based on clinical data,
# available at AWS-workspace

# load the package
# library(tidyverse)
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

# load function
source("/volumes/shared/shared_scripts_aws/scripts/load_aux.R")

# define the version of the current files that you produce
version <- 'aws_V1'
data_path <- '/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/'

# Load all the general data
list_auxiliaries <- load_aux()
auxiliaries  <- list_auxiliaries$aux

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

# ---------------------------------------------------------------------------- #
# load the individual outcomes:
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
scztypy <- read.csv(paste0(data_path, 'scidvapd01/csv/part-00000-7a562a12-a7f4-42a4-992f-4ebed6e0f00a-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrschizotypal_threshold_2, chrschizotypal_summ)
nsipr <- read.csv(paste0(data_path, 'ampscz_nsipr01/csv/part-00000-e43b27ea-ad28-4cef-8705-d76c1edfd58d-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrnsipr_motivation_and_pleasure_dimension,
                chrnsipr_diminished_expression_dimension, chrnsipr_avolition_domain,
                chrnsipr_asociality_domain, chrnsipr_anhedonia_domain, 
                chrnsipr_blunted_affect_domain, chrnsipr_item11_rating)
psychs <- read.csv(paste0(data_path, 'ampscz_psychs01/csv/part-00000-cd37c0a6-2668-4532-bd2e-ca361b20ad68-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrpsychs_scr_ac31, psychs_pos_tot,
                psychs_sips_p1, psychs_sips_p2, 
                psychs_sips_p3, psychs_sips_p4,
                psychs_sips_p5)
sofas <- read.csv(paste0(data_path, 'dsm_iv_es01/csv/part-00000-642132b2-f752-486b-adbd-b98d541b33da-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrsofas_premorbid, chrsofas_currscore12mo, chrsofas_currscore, chrsofas_lowscore)
assist <- read.csv(paste0(data_path, 'assist01/csv/part-00000-e011ea57-fc55-43c2-9c9a-ab60bd0e1ede-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, ends_with(c('tobacco', 'alcohol', 'cannabis', 'cocaine', 'amphetamines',
                                            'inhalants', 'sedatives', 'hallucinogens', 'opiods', 'other')))
risk_pps <- read.csv(paste0(data_path, 'ampscz_pps01/csv/part-00000-6594e83e-1754-4461-a56d-a4df004c9636-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, contains('pps_sum'), chrpps_mocc, chrpps_focc)
cssrs <- read.csv(paste0(data_path, 'cssrs01/csv/part-00000-373f18a7-e681-4750-94d9-b7ece24c5c56-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrcssrs_intensity_lifetime, chrcssrs_intensity_pastmonth,
                chrcssrsb_si1l, chrcssrsb_si2l, chrcssrsb_css_sim1, chrcssrsb_css_sim2)
gfrs <- read.csv(paste0(data_path, 'gfs01/csv/part-00000-65de1e6e-6184-4051-a153-bc10c845ff99-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrgfrs_global_role_decline)
gfss <- read.csv(paste0(data_path, 'gfs01/csv/part-00000-4225d266-db15-4f6f-80f1-05d13b3c893f-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrgfss_global_social_decline)
promis <- read.csv(paste0(data_path, 'sri01/csv/part-00000-8c4f97fe-8cb2-4185-a9fd-81415662db76-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, chrpromis_total)
bprs <- read.csv(paste0(data_path, 'bprs01/csv/part-00000-dffa244a-1cf5-4bf6-87ac-6f96ca3cb0f1-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, ends_with(c('subscale', 'total')))
# perceived discrimination seems to be missing from amazon workspace
# wrote e-mail to Tashrif on August 26th for inclusion in upload NDA 3. 
# think about recalculating it here.
# dim <- read.csv(paste0(data_path, 'ampscz_dim01/csv/part-00000-fa9b18f1-6002-4dc1-8fe3-505af0c05aa9-c000.csv', sep = ''))
# many subjects have a -300 in oasis total.this is not the case at the server.
# what happens during data transfer?
oasis <- read.csv(paste0(data_path, 'oasis01/csv/part-00000-31c9cce0-2b4a-4d69-ac0e-35033f4dad09-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, contains(c('total')))
socdem <- read.csv(paste0(data_path, 'socdem01/csv/part-00000-f22b1f47-bad5-41f2-869b-845f7b0178cc-c000.csv', sep = ''))%>%
  dplyr::select(src_subject_id, starts_with(c('chrdemo_racial', 'chrdemo_country_situation',
                                              'chrdemo_parent_edu', 'chrdemo_hispanic_latino')))%>%
  mutate(chrdemo_country_situation_1 = case_when(chrdemo_country_situation == 1 ~ 1,
                                                 chrdemo_country_situation == -900 ~ -900,
                                                 chrdemo_country_situation == -300 ~ -300,
                                                 TRUE ~ 0),
         chrdemo_country_situation_2 = case_when(chrdemo_country_situation == 2 ~ 1,
                                                 chrdemo_country_situation == -900 ~ -900,
                                                 chrdemo_country_situation == -300 ~ -300,
                                                 TRUE ~ 0),
         chrdemo_country_situation_3 = case_when(chrdemo_country_situation == 3 ~ 1,
                                                 chrdemo_country_situation == -900 ~ -900,
                                                 chrdemo_country_situation == -300 ~ -300,
                                                 TRUE ~ 0),
         chrdemo_country_situation_4 = case_when(chrdemo_country_situation == 4 ~ 1,
                                                 chrdemo_country_situation == -900 ~ -900,
                                                 chrdemo_country_situation == -300 ~ -300,
                                                 TRUE ~ 0))%>%
  dplyr::select(-chrdemo_country_situation)
sex_age <- read.csv('/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/ndar_subject01/csv/part-00000-4a245b6d-9a50-4d1e-80f8-489fd94b71b1-c000.csv')%>%
  dplyr::select(src_subject_id, interview_age, sex)
cdss <- read.csv(paste0(data_path, 'clgry01/csv/part-00000-8af035de-c1a0-4aa4-8817-c541e1cb0af9-c000.csv'))%>%
  dplyr::select(src_subject_id, chrcdss_total)
# perceived stress scale seems to be missing from amazon workspace
# wrote e-mail to Tashrif on August 26th for inclusion in upload NDA 3. 
# think about recalculating it here.
#pss <- read.csv(paste0(data_path, 'pss01/csv/part-00000-08f9cc29-7f66-4254-827c-9a97caccc16b-c000.csv'))%>%
#  dplyr::select(src_subject_id, chrcdss_total)
pgi <- read.csv(paste0(data_path, 'cgis01/csv/part-00000-67a869d7-c693-4657-b59d-631687f30151-c000.csv'))%>%
  dplyr::select(src_subject_id, chrpgi_2)
prediction <- read.csv(paste0(data_path, 'ampscz_rap01/csv/part-00000-cd8e8aa7-3234-4e07-aa9e-a8a46ec401f6-c000.csv'))%>%
  dplyr::select(src_subject_id, chrpred_transition)
somatic <- read.csv(paste0(data_path, 'vitas01/csv/part-00000-f874138d-13a1-4dab-906a-72b170fbd846-c000.csv'))%>%
  dplyr::select(src_subject_id, chrchs_height, chrchs_heightunits,
                chrchs_weightkg, chrchs_bmi, chrchs_hr, chrchs_systolic,
                chrchs_diastolic)%>%
  mutate(chrchs_height_final = case_when(chrchs_heightunits == 1 ~ chrchs_height,
                                         chrchs_heightunits == 2 ~ chrchs_height*2.54))%>%
  dplyr::select(-c(chrchs_height, chrchs_heightunits))

outcomes_final <- scztypy %>%
  smart_full_join(., nsipr, 'src_subject_id')%>%
  smart_full_join(., psychs, 'src_subject_id')%>%
  smart_full_join(., sofas, 'src_subject_id')%>%
  smart_full_join(., assist, 'src_subject_id')%>%
  smart_full_join(., risk_pps, 'src_subject_id')%>%
  smart_full_join(., cssrs, 'src_subject_id')%>%
  smart_full_join(., gfrs, 'src_subject_id')%>%
  smart_full_join(., gfss, 'src_subject_id')%>%
  smart_full_join(., promis, 'src_subject_id')%>%
  smart_full_join(., bprs, 'src_subject_id')%>%
  smart_full_join(., oasis, 'src_subject_id')%>%
  smart_full_join(., socdem, 'src_subject_id')%>%
  smart_full_join(., sex_age, 'src_subject_id')%>%
  smart_full_join(., cdss, 'src_subject_id')%>%
  smart_full_join(., pgi, 'src_subject_id')%>%
  smart_full_join(., prediction, 'src_subject_id')%>%
  smart_full_join(., somatic, 'src_subject_id')%>%
  mutate(sex_num = case_when(sex == 'M' ~ 0,
                             sex == 'F' ~ 1))%>%
  dplyr::select(-sex)

# we rename the data, so that the domains can be more easily picked later.
colnames(outcomes_final) <- gsub('chrassist', 
                                 'substance_chrassist', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('psychs_', 
                                 'psychosis_psychs_', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrbprs', 
                                 'psychosis_chrbprs', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpred_', 
                                 'psychosis_chrpred_', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrsofas_', 
                                 'functioning_chrsofas_', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrgf', 
                                 'functioning_chrgf', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrcssrs', 
                                 'gsymptoms_chrcssrs', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrcssrsb', 
                                 'gsymptoms_chrcssrs', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpss', 
                                 'gsymptoms_chrpss', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrcdss', 
                                 'gsymptoms_chrcdss', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chroasis', 
                                 'gsymptoms_chroasis', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpromis', 
                                 'gsymptoms_chrpromis', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpgi', 
                                 'gsymptoms_chrpgi', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpps', 
                                 'risk_chrpps', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpds_perceived_discrimination', 
                                 'risk_chrpds_perceived_discrimination', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrschizotypal', 
                                 'risk_chrschizotypal', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrdemo_racial', 
                                 'sociocultural_racial', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrdemo_hispanic_', 
                                 'sociocultural_hispanic_', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrdemo_country_', 
                                 'sociocultural_country_', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrnsipr', 
                                 'psychosis_chrnsipr', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrdemo_parent', 
                                 'cov_chrdemo_parent', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('interview_age', 
                                 'cov_interview_age', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('sex_num', 
                                 'cov_sex_num', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrchs', 
                                 'somatic', colnames(outcomes_final))
colnames(outcomes_final) <- gsub('chrpsychosis_psychs', 
                                 'psychosis_psychs', colnames(outcomes_final))

outcomes_final <- outcomes_final %>%
  mutate(across(-src_subject_id, ~as.numeric(as.character(.)))) %>%  # Convert all columns to numeric
  mutate(across(-src_subject_id, ~case_when(. == -900 | . == -300 | . == -99 ~ NA_real_,
                                            TRUE ~ .)))

# Specify the directory that contains your CSV files (modify this path)
#directory_path <- "/volumes/prod-ampscz/collaboration-space-new/3705/release-2.0/65283/"

# Get a list of all CSV files recursively in the directory and subdirectories
#csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read all the CSV files into a list of dataframes
#list_of_dfs <- map(csv_files, read.csv)

# Optionally, name the list elements with the file names (without directory path)
#names(list_of_dfs) <- csv_files

# View the names of the dataframes in the list
#print(names(list_of_dfs))


# Specify the variables you want to search for
#search_vars <- c("weight")

# Function to extract the specific variables if they exist in the dataframe
#extract_columns <- function(df) {
#  df %>% 
#    select(matches(search_vars))  # Use any_of to select only the columns that exist in the dataframe
#}

# Apply the function to each dataframe in the list
#filtered_dfs <- map(list_of_dfs, extract_columns)

# Filter out empty dataframes where none of the search_vars were found
#filtered_dfs <- filtered_dfs[map_lgl(filtered_dfs, ~ ncol(.) > 0)]

# Optionally, print the names of the dataframes where the variables were found
#print(names(filtered_dfs))