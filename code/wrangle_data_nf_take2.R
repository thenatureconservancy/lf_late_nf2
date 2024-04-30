
### Background ----
# Title: Wrangle LANDFIRE/NF data
# Authors: Randy Swaty, Amy Collins, Seth Spawn-Lee
# Date created: pril 25, 2024
# Last edited: April 29 2024

# Wrangle input datasets so that we have a dataframe with reference percent, and current percent for each BpS, along with NF and Region information

### 1. Dependencies ----

## Packages
library(janitor)
library(tidyverse)
library(dplyr)

## Input data
bps_scl_nf <- read_csv("Data/bps_scl_nf.csv")  
# raw output from ArcGIS Pro combine of  LF Biophysical Settings (bps, https://landfire.gov/bps.php), LF Succession Class (scl, https://landfire.gov/sclass.php) and USFS Proclamation boundaries (nf, https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries, 'Administrative Forest Boundaries')

ref_con_long <- read_csv("Data/ref_con_long.csv") 
# reference percents per scls per bps from LANDFIRE.  Original data from (https://landfire.gov/zip/LANDFIRE_CONUS_Reference_Condition_Table_August_2020.zip) was wrangled by Randy (e.g., pivot longer, etc.)

scls_descriptions <- read_csv("Data/scls_descriptions.csv")
# succession class descriptions used to get label information (e.g., "Late" and "Closed", from landfirevegmodels SyncroSim package,https://apexrms.github.io/landfirevegmodels/)


# 2. Wrangle data -----


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


# 3. Join data to create complete dataframe (before any calculations) -----

### From Amy!  Way to build a complete current table that has all the possible bps/scls combinations per forest
new_current_data <- bps_scl_nf_wrangled %>%   
  complete(label, nesting(bps_model, forestname)) 

#bring in the ref %
new_current_reference_data <- left_join(new_current_data, ref_con_long_wrangled, by = 'join_field')


#remove some unnecessary columns
new_current_reference_data_clean <- new_current_reference_data %>%
  select(-c(oid,
            lc20_bps_220,
            lc22_s_cla_230,
            forests_r,
            bps_name.y)) %>%
  rename(bps_name = bps_name.x) 

names(new_current_reference_data_clean)

# remove sparse veg BpSs (no reference condition - LANDFIRE modeling rule)
#codes for bps_model column
remove_codes <- c(0, 
                  -1111,
                  10100,
                  10200,
                  10300,
                  10400,
                  10600,
                  10700)

#codes for label column
unique(new_current_reference_data_clean$label)

rc2 <- c("Barren or Sparse", "Fill-Not Mapped")


#remove conditions
landfire <- new_current_reference_data_clean %>% 
  filter(!bps_model %in% remove_codes) %>% 
  filter(!label %in% rc2)

#check values have been removed
unique(landfire$bps_model) 
names(landfire)

# 4. Calculate current scl percents -----

percent_calcs <- landfire %>%
  group_by(bps_model) %>%
  mutate(bps_count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bps_acres = bps_count*0.2223945,
         ref_scls_acres = bps_acres*(ref_percent/100),
         cur_scls_acres = count*0.2223945,
         cur_percent = (cur_scls_acres/bps_acres)*100) %>%
  mutate(across(22:25, ~round(.x,0)))  


# 5. add the canopy category -----
subset_scls <- scls_descriptions_wrangled %>%
  select(join_field, canopy_category)


final_df <- left_join(percent_calcs, subset_scls, by = 'join_field')
  
names(final_df)  

#6. fill in NA cells -----
# Randys comment:it looks to me that all NA cells should be filled with the content from the 
#row above except for the value and count fields
#Amy comment: do we need this step for our purposes (we're not summarizing by label for the EIS)? 
#Have left off this step for now

# 7. write csv -----
write.csv(final_df, "Outputs/landfire_conus_2022.csv")

# 8. some quick QAQC -----
# Acres per region- compared our outputs to published NF data (https://www.fs.usda.gov/land/staff/lar/LAR2021/LARTable28.pdf).  Was looking for close as I simply summed COUNT per forest per region.  There will be some discrepancy due to water/barren/etc.  OK.
# Spot check BpSs and amounts for several forests (compared to other work and Randy's opinion for what that's worth).  OK. 

  
  
  

  







