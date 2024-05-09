### Background ----
# Title: Wrangle LANDFIRE/NF data
# Authors: Randy Swaty, Amy Collins, Seth Spawn-Lee
# Date created: April 25, 2024
# Last edited: May 9 2024

# Wrangle input datasets so that we have a dataframe with reference percent, and current percent for each BpS, along with NF and Region information

### 1. Dependencies ----

## Packages
library(janitor)
library(tidyverse)
library(dplyr)

## Input data
bps_scl_nf <- read_csv("Data/bps_scl_nf.csv")  
# raw output from ArcGIS Pro combine of  LF Biophysical Settings (bps, https://landfire.gov/bps.php), LF Succession Class (scl, https://landfire.gov/sclass.php) and USFS Proclamation boundaries (nf, https://data.fs.usda.gov/geodata/edw/datasets.php?dsetCategory=boundaries, 'Administrative Forest Boundaries')
#only has 359 bps

ref_con_long <- read_csv("Data/ref_con_long.csv") 
# reference percents per scls per bps from LANDFIRE.  Original data from (https://landfire.gov/zip/LANDFIRE_CONUS_Reference_Condition_Table_August_2020.zip) was wrangled by Randy (e.g., pivot longer, etc.)
#this has all of the bps (415)

scls_descriptions <- read_csv("Data/scls_descriptions.csv")
# succession class descriptions used to get label information (e.g., "Late" and "Closed", from landfirevegmodels SyncroSim package,https://apexrms.github.io/landfirevegmodels/)
names(scls_descriptions)
unique(scls_descriptions$ClassLabelID)

# 2. Wrangle data -----

# a. reference % data
ref_con_long_wrangled <- ref_con_long %>%         
  clean_names() %>%                                    
  # Standardizing column names to lowercase and underscores
  rename(bps_name = bp_s_name,
         join_field = model_label) %>%                     
  # Renaming the column bp_s_name to bps_name
  filter(!is.na(ref_percent))
######  Kori suggests removing where ref percent = NA

# b. succession class descriptions
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

# c. raw output from ArcGIS
### build a complete current table that has all the possible bps/scls combinations per forest
# & Combining BPS_MODEL and LABEL columns into a new column called "join_field" with "_" as separator
new_current_d <- bps_scl_nf %>%  
  clean_names() %>%
  complete(label, nesting(bps_model, forestname)) %>% 
  unite("join_field", bps_model, label, sep = "_", remove = FALSE) 

# 3. merge dataframes -----
#bring in the ref %
new_current_reference_data <- left_join(new_current_d, ref_con_long_wrangled %>% 
                                          select(ref_percent, join_field), by = 'join_field')

#remove some unnecessary columns
#names(new_current_reference_data)

new_current_reference_data_clean <- new_current_reference_data %>%
  select(-c(oid,
            lc20_bps_220,
            lc22_s_cla_230,
            forests_r))

# 4. Calculate current sclass percents -----
#add in the sum area for each bps column (importnat that this happens before we remove bps's and labels)
n_c<-new_current_reference_data_clean %>% 
  group_by(bps_model) %>% 
  mutate(bps_model_count = sum(count, na.rm = T)) %>% 
  ungroup() %>%
  mutate(bps_model_acres = bps_model_count *0.2223945,
         ref_scls_acres = bps_model_acres *(ref_percent/100))

#now add in the current % and acreage
p_c<-n_c %>% 
  group_by(join_field) %>%      
  mutate(cur_scls_count = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cur_scls_acres = cur_scls_count*0.2223945,
         cur_percent = (cur_scls_acres/bps_model_acres)*100) %>%
  mutate(across(20:24, ~round(.x,2))) 

#cur_percent adds up to 100% for each bps


#altering cur_scls_acres and cur_percent values to zero
#if theres a ref_percent value, replace cur_scls_acres and cur_percent with zero, otherwise NA (bc that class never existed)
df <- p_c %>%
  mutate(cur_scls_acres = ifelse(is.na(cur_scls_acres) & !is.na(ref_percent), 0, cur_scls_acres),
         cur_percent = ifelse(is.na(cur_percent) & !is.na(ref_percent), 0, cur_percent))


# 5. Remove conditions -----
# remove sparse veg BpSs (no reference condition - LANDFIRE modeling rule)
#codes for bps_model column
remove_codes <- c(0, -1111, 10010, 10020, 10030, 10040, 10060, 10070)

#codes for label column
rc2<-c("Agriculture",
       "Barren or Sparse",
       "Developed",
       "Fill-NoData",
       "Fill-Not Mapped",
       "Snow/Ice",
       "UE",
       "UN",
       "Water",
       "Blank")

#remove conditions
landfire<-df %>% 
  filter(!bps_model %in% remove_codes) %>% 
  filter(!label %in% rc2)

#check values have been removed
unique(landfire$bps_model)
# unique(landfire$label)

#fill in the blank values
#names(landfire)
#replace bps_name and other columns by bps_model (use direction up and down to remove all NAs)
landfire2 <- landfire %>%
  group_by(bps_model) %>%
  fill(bps_code, bps_name, groupveg, fri_replac, fri_mixed, fri_surfac, fri_allfir, frg_new, .direction = "down")

landfire3 <- landfire2 %>%
  group_by(bps_model) %>%
  fill(bps_code, bps_name, groupveg, fri_replac, fri_mixed, fri_surfac, fri_allfir, frg_new, .direction = "up")

#now to get the region by forest names correct too (use up and down direction)
unique(landfire3$region)
landfire4 <- landfire3 %>%
  group_by(forestname) %>%
  fill(region, forestorgc, .direction = "down")

landfire5 <- landfire4 %>%
  group_by(forestname) %>%
  fill(region, forestorgc, .direction = "up")

#double check all NAs gone
unique(landfire5$region)

# 6. add the age and canopy category -----
names(scls_descriptions_wrangled)
final_df<-left_join(landfire5, scls_descriptions_wrangled %>% 
                      select(join_field, age_category, canopy_category), by = 'join_field')
  
# unique(final_df$canopy_category)
# class_counts <- table(final_df$canopy_category)
# print(class_counts)
# names(final_df)  

# 7. write csv -----
#write.csv(final_df, "Outputs/landfire_conus_2022_t2.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t3.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t4.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t5.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t6.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t7.csv")
#write.csv(final_df, "Outputs/landfire_conus_2022_t8.csv")
# write.csv(final_df, "Outputs/landfire_conus_2022_t9.csv")
write.csv(final_df, "Outputs/landfire_conus_2022_t10.csv")

#confirm by checking these bps codes
#13022
#10080

# 8. some quick QAQC -----
# Acres per region- compared our outputs to published NF data (https://www.fs.usda.gov/land/staff/lar/LAR2021/LARTable28.pdf).  Was looking for close as I simply summed COUNT per forest per region.  There will be some discrepancy due to water/barren/etc.  OK.
# Spot check BpSs and amounts for several forests (compared to other work and Randy's opinion for what that's worth).  OK. 

  
  
  

  







