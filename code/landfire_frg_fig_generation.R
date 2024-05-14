##### Author: Amy Collins, Seth Spawn-Lee, Randy Swaty
##### Title: Generating figures for Landfire: regional v FRG
##### Date created: May 12 2024
##### Date last modified: May 14 2024

########### 1: packages and data ###########

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(stringr)
library(readr)
library(scales)

getwd()


# final_bps_scls_2 <- read_csv("Data/final_bps_scls 2.csv")
# demo <- read_csv("Data/demo.csv")
# demo2<-subset(demo, bps_name !='etc') #remove extra dummy data rows
complete<-read_csv("Outputs/landfire_conus_2022_t12.csv")
unique(complete$label)


#FS regions
# #all regions, inc. AK, Hawaii and Perto Rico
# admin<-st_read("Data/S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp")
# 
# #regions with Hawaii and Perto Rico excluded
# #then exclude Alaska too
# fsregion<-st_read("Data/S_USA.AdministrativeRegion_noHIorPR/S_USA.AdministrativeRegion_noHIorPR.shp")

#bring in region names for labeling on figs
region_name<-read_csv("inputs/region_names.csv")




# ######## 2: obtain denominator: total area is for each region (for cur and ref percent) #####
## mask out (aka filter out) any classes that are outside the regional area (fill - not mapped)
## or have the same pixel count for both reference and current condition that are of no interest (water, ice, snow) 
names(complete)
unique(complete$label)

rc<-c(#"Agriculture",
  #"Barren or Sparse",
  #"Developed",
  "Fill-Not Mapped",
  "Snow/Ice",
  #"UE",
  #"UN",
  "Water")

remove_codes <- c(#0, #barren
  -1111) #fill-not mapped, 
# 10010, #sparse
# 10020, #sparse
# 10030, #sparse
# 10040, #sparse
# 10060, #sparse
# 10070) #sparse

#remove conditions
complete2<-complete %>% 
  filter(!bps_model %in% remove_codes) %>% 
  filter(!label %in% rc) 

unique(complete2$label)


#area for each region (current)
reg<-complete2 %>% 
  group_by(region) %>% 
  mutate(region_acres = (sum(count, na.rm = T))*0.2223945)


#find out what the total area is for each bps_model and region (for ref percent)
hist_reg<-reg %>%
  group_by(bps_model, region) %>% 
  mutate(bps_region_acres = (sum(count, na.rm = T))*0.2223945)


##### filter pieces out of dataset ##
# remove sparse veg BpSs (no reference condition - LANDFIRE modeling rule)
#codes for bps_model column
remove_codes2 <- c(0, -1111, 10010, 10020, 10030, 10040, 10060, 10070)

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

old_classes <- c("Late1", "Late2")

#remove conditions
landfire<-hist_reg %>% 
  filter(!bps_model %in% remove_codes2) %>% 
  filter(!label %in% rc2) %>% 
  filter(age_category %in% old_classes) # keep just late succession

#check values have been removed
unique(landfire$bps_model)
# unique(landfire$label)
unique(landfire$label)
unique(landfire$age_category)

######## 3: obtain numerator: for every region, find out what % of late succession is in each FRG ######
### get FRGs grouped how we want them
# get FRG number by splitting "frg_new" strings
landfire['FRG'] = do.call(rbind.data.frame, strsplit(landfire$frg_new, '-'))[,1] # some NAs for FRGs

# FRG groupings requested by Gunnar
frg_grps = data.frame('FRG' = c('I', 'II', 'III', 'IV', 'V'),
                      'frg_grp' = as.factor(c('1 & 2', '1 & 2', '3', '4 & 5', '4 & 5')))

# set factor levels for plotting order
frg_grps$frg_grp = factor(frg_grps$frg_grp, levels = rev(levels(frg_grps$frg_grp)))

landfire2 <- merge(landfire, frg_grps, by = 'FRG', all = T)

### current calcs
cur_frg<-landfire2 %>% 
  group_by(region, frg_grp) %>% 
  mutate(cur_acre_frg = (sum(count, na.rm = T))*0.2223945, 
         cur_perc_frg = round((cur_acre_frg/region_acres)*100, 2))

## ref calcs
ref_frg<-cur_frg %>% 
  mutate(region_ref_area = round(bps_region_acres * (ref_percent/100), 2)) #get the s-class regional acreage by the proportional reference

ref_frg2<-ref_frg %>% 
  select(region, frg_grp,region_acres, cur_acre_frg, cur_perc_frg, region_ref_area) %>% 
  unique(.) %>% 
  group_by(region, frg_grp) %>% 
  mutate(ref_acre_frg = sum(region_ref_area, na.rm = T),
         ref_perc_frg = round((ref_acre_frg/region_acres)*100, 2),
         percent_change = round(cur_perc_frg - ref_perc_frg, 0),
         sign_change = (percent_change > 0),
         area_change = (cur_acre_frg - ref_acre_frg)/1000) #thousand acres


###create a unique table with the area from each bps, then sum those across FRGs
# names(landfire2)
# frgs<-landfire2 %>% 
#   select(region, bps_model, frg_grp, region_acres, bps_model_acres, ref_scls_acres, cur_scls_acres) %>% 
#   unique(.) %>% 
#   group_by(region, frg_grp) %>% 
#   mutate(ref_acre_frg = (sum(ref_scls_acres, na.rm = T))*0.2223945,
#          ref_perc_frg = round((ref_acre_frg/region_acres)*100, 2),
#          cur_acre_frg = (sum(cur_scls_acres, na.rm = T))*0.2223945,
#          cur_perc_frg = round((cur_acre_frg/region_acres)*100, 2),
#          percent_change = round(cur_perc_frg - ref_perc_frg, 0),
#          sign_change = (percent_change > 0),
#          area_change = (cur_acre_frg - ref_acre_frg)/100) #thousand acres
         

#create a summary of the regional FRG groupings
frgs2<-ref_frg2 %>% 
  select(region, frg_grp, percent_change, sign_change, percent_change, area_change) %>% 
  unique(.) %>% 
  na.omit()

#append region name onto the df
frgs2$region<-as.numeric(frgs2$region)

frg_data<-frgs2 %>% 
  left_join(., region_name, by = "region")

######## step 3: generate bar plot ######
# bar plot of late successional acreage within each FRG in each region during ref condition
label_x_offset = 2500

bar_plot = frg_data %>%
  ggplot(aes(
    x = area_change, 
    y = reorder(region_name, -region),
    fill = frg_grp))+
  geom_col(position = 'dodge')+
  scale_x_continuous(label = comma)+
  #scale_y_discrete(limits=rev)+  # ensures region 1 at top, 9 at bottom
  scale_fill_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  labs(
    x = "Net change in late succession forest area (thousand acres)", 
    y = ""
  )+
  theme_light(base_size = 14)+
  theme(panel.grid.major.y = element_blank())+
  geom_hline(yintercept=seq(1.5, 8.5, by = 1))+
  guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = frg_grp), position = position_dodge(0.9), hjust = 0.5)

bar_plot

ggsave('./Outputs/region_frg_bar_plot.png', bar_plot, width = 10.5, height = 6, units = 'in', dpi = 300)







