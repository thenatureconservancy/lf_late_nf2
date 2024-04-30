
### Dependencies ----

## Packages
library(janitor)
library(tidyverse)

bps_scl_nf <- read_csv("inputs/bps_scl_nf.csv")  

ref_con_long <- read_csv("inputs/ref_con_long.csv") 

scls_descriptions <- read_csv("inputs/scls_descriptions.csv")



to_remove <- c("Water",
               "Fill-Not Mapped",
               "Snow/Ice",
               "Barren or Sparse")

current <- bps_scl_nf %>%                   
  unite("join_field", BPS_MODEL, LABEL, sep = "_", remove = FALSE ) %>%
  # Combining BPS_MODEL and LABEL columns into a new column called "join_field" with "_" as separator
  clean_names() %>%
 # Standardizing column names to lowercase and underscores
  select(c(count,
           join_field,
           bps_model,
           bps_name,
           label,
           forestorgc)) %>%
  filter(!label %in% to_remove) %>%
  unite(join_mc_nf, join_field, forestorgc, sep = "_", remove = FALSE) %>%
  unite(mc_nf, bps_model, forestorgc, sep = "_", remove = FALSE )


reference <- ref_con_long %>%         
  clean_names() %>%                                    
  # Standardizing column names to lowercase and underscores
  rename(bps_name = bp_s_name,
         join_field = model_label) %>%                     
  # Renaming the column bp_s_name to bps_name
  filter(!is.na(ref_percent)) %>%
  # remove na from ref
  select(c(join_field,
           ref_percent,
           ref_label))

expand <-  expand.grid(join_field = unique(reference$join_field), 
                       forestorgc = unique(current$forestorgc)) 


expand <- left_join(expand, reference, by = 'join_field') %>%
  unite(join_mc_nf, join_field, forestorgc, sep = "_", remove = FALSE) %>%
  mutate(mc = gsub("_[^_]*$", "", join_field)) %>%
  unite(mc_nf, mc, forestorgc, sep = "_", remove = FALSE )



bps_nf_keep <- current$mc_nf


# try to filter expand

filtered_expand <- expand %>%
  filter(mc_nf %in% bps_nf_keep)

# looks solid, do a left join 

full <- left_join(filtered_expand, current, by = 'join_mc_nf')

## UGLY BUT COMPLETE SANS CURRENT SCLASS PERCENT



### SCRAP CODE -----


full <- left_join(expand, current, by = 'join_mc_nf') %>%
  unite(mc_nf, bps_model, forestorgc, sep = "_", remove = FALSE)

# Define a function to fill NA values with any non-NA value in the group
fill_na_with_value <- function(x) {
  non_na_values <- na.omit(x)
  if (length(non_na_values) > 0) {
    non_na_value <- non_na_values[1]
    x[is.na(x)] <- non_na_value
  }
  return(x)
}

full <- full %>%
  group_by(forestorgc) %>%
  mutate(forestorgc = fill_na_with_value(forestorgc))
  


## Joins with Myles Walimaa

## need to clean coluymns, get rid of extras first
## try to join by two columns (join_field and nf) instead of concatenated join_field & nf
## consider making new tables to joins


## this join is suspect.  need a table with all bpss, all sclasses and forests
full_join_mc <- full_join(bps_scl_nf_wrangled, ref_con_long_wrangled, by = c('bps_model' = 'model_code'))  %>%
  select(-c(1:9)) %>%
  mutate(join_field_nf = paste0(join_field.y, "_", forestorgc)) %>%
  distinct(join_field_nf, .keep_all = TRUE)  ## need to filter duplicates in join_field_nf, then keep wanted columns
  
bps_scl_nf_wrangled <- bps_scl_nf_wrangled %>%
  mutate(join_field_nf = paste0(join_field, "_", forestorgc))

left_join_bps <- left_join(full_join_mc, bps_scl_nf_wrangled, by = "join_field_nf")

### this is a solid skeleton, but has incorrect data to remove (e.g., count field)
### unite USFS code and join field
### do the same with the current data
### use skeleton as left table in a left join
### inspect

### more thoughts
# make a table with 2 columns, nf and bps_model
# join that table to ref con to get bps_model, label, nf, percent.  try full join



### scrap code


## Try with real data-probably not useful

all_combinations_real <- expand.grid(join_field = unique(ref_con_long_wrangled$join_field), forestname = unique(bps_scl_nf_wrangled$forestname))