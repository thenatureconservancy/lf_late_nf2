
### Background ----
# Wrangle LANDFIRE/NF data
# Randy Swaty, Amy Collins, Seth Spawn-Lee
# April 25, 2024

# Wrangle input datasets so that we have a dataframe with reference percent, and current percent for each BpS, along with NF and Region information



### Dependencies ----

## Packages
library(janitor)
library(tidyverse)

## Input data
bps_scl_nf <- read_csv("inputs/bps_scl_nf.csv")  
# raw output from ArcGIS Pro combine of  LF Biophysical Settings (bps, https://landfire.gov/bps.php), LF Succession Class (scl, https://landfire.gov/sclass.php) and USFS Proclamation boundaries (nf, https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries, 'Administrative Forest Boundaries')

ref_con_long <- read_csv("inputs/ref_con_long.csv") 
# reference percents per scls per bps from LANDFIRE.  Original data from (https://landfire.gov/zip/LANDFIRE_CONUS_Reference_Condition_Table_August_2020.zip) was wrangled by Randy (e.g., pivot longer, etc.)

scls_descriptions <- read_csv("inputs/scls_descriptions.csv")
# succession class descriptions used to get label information (e.g., "Late" and "Closed", from landfirevegmodels SyncroSim package,https://apexrms.github.io/landfirevegmodels/)


# Wrangle data -----


bps_scl_nf_wrangled <- bps_scl_nf %>%                   
  unite("join_field", BPS_MODEL, LABEL, sep = "_", remove = FALSE ) %>%
  # Combining BPS_MODEL and LABEL columns into a new column called "join_field" with "_" as separator
  clean_names()                                           
  # Standardizing column names to lowercase and underscores


ref_con_long_wrangled <- ref_con_long %>%         
  clean_names() %>%                                    
  # Standardizing column names to lowercase and underscores
  rename(bps_name = bp_s_name,
         join_field = model_label) %>%                     
  # Renaming the column bp_s_name to bps_name
  filter(!is.na(ref_percent))
######  Kori suggests removing where ref percent = NA

  
scls_descriptions_wrangled <- scls_descriptions %>% 
  select(-c(Description)) %>%                                         
  # Removing the column named "Description"
  rename("model_code" = "StratumID",                                  
         "scls_label" = "ClassLabelID",                               
         "state_class_id" = "StateClassID" ) %>%                      
  # Renaming columns
  unite("join_field", model_code:scls_label, sep = "_", remove = FALSE ) %>%  
  # Combining columns model_code to scls_label into a new column "join_field" with "_" separator
  separate(state_class_id, into = c("age_category", "canopy_category"), sep = ":", remove = FALSE)  
  # Separating the state_class_id column into age_category and canopy_category columns at the ":" separator


# Join data to create complete dataframe (before any calculations) -----

### From Amy!  Way to build a complete current table that has all the possible bps/scls combinations per forest
new_current_data <- bps_scl_nf_wrangled %>%   
  complete(label, nesting(bps_model, forestname)) 


new_current_reference_data <- left_join(new_current_data, ref_con_long_wrangled, by = 'join_field')

# remove sparse veg BpSs
remove_codes <- c(0, 
                  -1111,
                  10100,
                  10200,
                  10300,
                  10400,
                  10600,
                  10700)

new_current_reference_data_clean <- new_current_reference_data %>%
  select(-c(oid,
            lc20_bps_220,
            lc22_s_cla_230,
            forests_r,
            bps_name.y)) %>%
  rename(bps_name = bps_name.x) 


### NOTES ABOUT THIS DATASET
# bps_models that are in the remove_codes are sparsely vegetated so there are no reference conditions (LANDFIRE modeling rule) so could be removed.  I did not do this. 
# it looks to me that all NA cells should be filled with the content from the row above except for the value and count fields


# Calculate current scl percents NOT COMPLETED YET  -----

bps_scl_nf_complete_calcs <- bps_scl_nf_complete %>%
  group_by(bps_model) %>%
  mutate(bps_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bps_acres = bps_count*0.2223945,
         ref_scls_acres = bps_acres*(ref_percent/100),
         cur_scls_acres = count*0.2223945,
         cur_percent = (cur_scls_acres/bps_acres)*100) %>%
  mutate(across(30:33, round, 0))   
  
  
  
  
  
  

  







