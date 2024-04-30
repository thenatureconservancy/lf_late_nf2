
## ---------------------------
##
## Script name: Wrangle  BpS and Sclass data inputs
##
## Purpose of script: Clean and merge BpS-Sclass combine, BpS reference percents and sclass descriptions to calculate and make charts for historical vs. current sclass charts, and for exploring different stages of BpSs (e.g., late classes).  This script is an evolution of "wrangle_data" originally written for Mary Kelly's MZ2 late succession project.
##
## Author: Randy Swaty
##
## Date Created: December 18, 2023
##
##
## ---------------------------
##
## Notes:
##   Challenging to "keep" all sclass combinations
##   There are issues with LANDFIRE data including a duplicate of Laurentian-Acadian Northern Hardwoods-Hemlock.
##  
## ---------------------------

## ---------------------------


## load packages

library(janitor) # for cleaning column names
library(tidyverse) # for reading in and wrangling data

## ---------------------------


## read in raw data and sclass descriptions

# output from BpS-Sclass data combine in ArcGIS pro
raw_bps_scls <- read.csv("data/bps_scls_cmbn_RS.csv")

# output from SyncroSim that has labels and age-categories of all succession classes except AK
sclass_descriptions <- read.csv("data/scls_descriptions.csv")

# reference percents per sclass per BpS for all BpSs except AK.  Modified for this purpose from original LF ref con table.
reference_percents <- read.csv("data/ref_con_long.csv")

# bps attributes for landscape of interest.  Output from clipping BpS data with shapefile in GIS.
bps_atts <- read.csv("data/bps_atts.csv")

## ---------------------------


##  clean and prep raw combined data

clean_bps_scls_cmbn <- raw_bps_scls %>%
  mutate(LABEL = str_replace_all(LABEL, "Developed", "Urban")) %>%
  select(-c(OID_,
            Value,
            LC16_BPS_2,
            LC20_SCla_,
            GROUPVEG)) %>%
  unite("join_field", BPS_MODEL,LABEL, sep = "_", remove = FALSE ) %>%
  group_by(join_field,BPS_MODEL, BPS_NAME, LABEL)  %>%  # I want only one row per unique BpS-sclass combo
  summarize(count = sum(Count)) %>%
  clean_names() 
# note-some BpSs did not have sclasses A-E in mapping rules and/or may not have any pixels of a particular s-class mapped today so there will not be all 10 possible sclasses for each BpS (i.e., A-E, UN, UE, etc.) from the combine which represents current data.

## clean and prep sclass descriptions

sclass_descriptions_clean <- sclass_descriptions %>%
  select(-c(Description)) %>% # remove column
  rename("model_code" = "StratumID",
         "scls_label" = "ClassLabelID",
         "state_class_id" = "StateClassID" ) %>% # rename columns
  unite("join_field", model_code:scls_label, sep = "_", remove = FALSE ) %>%
  separate(state_class_id, into = c("age_category", "canopy_category"), sep = ":", remove = FALSE) 


## clean and prep reference percents

# get unique s-class labels from modified ref_con so we can have 'authoritative' list
unique_sclass_labels_ref <- unique(reference_percents$refLabel)
print(unique_sclass_labels_ref)

# there may be some differences in the mapped sclass labels in the cleaned bps_sclass combine
unique_sclass_lables_cmbn <- unique(clean_bps_scls_cmbn$label)
print(unique_sclass_lables_cmbn)
# there are differences, e.g., Urban-Developed between this and sclass label
# will assume Barren/Sparse, NoData and Snow/Ice is minimal; will change "Developed" to "Urban" in reference df cleaning code 

clean_ref_percents <- reference_percents %>%
  mutate(across('refLabel', str_replace, 'Developed', 'Urban')) %>%
  mutate(across('model_label', str_replace, 'Developed', 'Urban')) %>%
  rename("join_field" = "model_label" ) %>%
  clean_names()

## winnow this df to only the bps model codes in area of interest.   Used BPS_MODEL, thought note this may result in duplicate BpS names if the AoI is across multiple variants of a BpS.  There should be 10x the number of unique BPS_MODEL values. Note, this will not be 10x the number of rows in the bps_atts since that is parsed by the "VALUE" field in the clipped BpS dataset, i.e., there may be multiple rows for a single BPS_MODEL because each Map Zone gets a unique "VALUE".  We are working with the BPS_MODEL field as it will have the unique ref conditions we need.  Also, we use the a newly created 'join-field' for joining in GIS.  

# first see how many bpss there are
length(unique(bps_atts$BPS_MODEL))

clean_ref_percents <- clean_ref_percents %>%
  filter(model_code %in% bps_atts$BPS_MODEL)
# should be 10x number of unique BPS_Models (minus one for NAs that are 'water' etc)

## create 'final' dataframe with reference and current sclass percents, acres and labels

## first ref con and sclass descriptions, remove BPS_NAME column to avoid duplication below
final_ref_con <- left_join(clean_ref_percents, sclass_descriptions_clean) 


# looks OK, now full join to add reference percents then clean a bit

ref_con_slcs_count <- full_join(final_ref_con, clean_bps_scls_cmbn, by = "join_field") %>%
  select(-c(bps_model,
            bps_name,
            label,
            scls_label)) %>%
  rename("cur_scls_count" = "count",
          "bps_name" = "bp_s_name")

# now for the math: need count/acres per bps, cur sclass percents and differences

final_df_full <- ref_con_slcs_count %>%
  group_by(model_code) %>%
  mutate(bps_count = sum(cur_scls_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bps_acres = bps_count*0.2223945,
         ref_scls_acres = bps_acres*(ref_percent/100),
         cur_scls_acres = cur_scls_count*0.2223945,
         cur_percent = (cur_scls_acres/bps_acres)*100) %>%
  mutate(across(11:14, round, 0)) 

final_df_clean <- final_df_full %>%
  select(c(join_field,
           bps_name,
           model_code,
           ref_label,
           ref_percent,
           cur_percent,
           ref_scls_acres,
           cur_scls_acres,
           bps_acres
           ))

# save to csv 
write.csv(final_df_full, file = "data/final_df_full.csv", row.names=FALSE)

  
