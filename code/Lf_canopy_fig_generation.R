##### Author: Amy Collins
##### Title: Generating figure for Landfire: regional open v closed
##### Date created: April 24 2024
##### Date last modified: May 13 2024

########### Step 1: packages and data ###########

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(stringr)
library(readr)

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
region_name<-read_csv("Data/region_names.csv")

# ######## Step 2: obtain the denominator: total area for each region #####
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


## find out what the total area is for each region (for cur percent)
reg<-complete2 %>% 
  group_by(region) %>% 
  mutate(region_acres = (sum(count, na.rm = T))*0.2223945)

## find out what the total area is for each bps_model and region (for ref percent)
hist_reg<-reg %>%
  group_by(bps_model, region) %>% 
  mutate(bps_region_acres = (sum(count, na.rm = T))*0.2223945)

## filter other classes out of dataset that we do not want to count in our ref or current condition ##
# remove sparse veg BpSs (no reference condition - LANDFIRE modeling rule)
#codes for bps_model column
remove_codes2 <- c(0, #barren
                  #-1111 #fill-not mapped, 
                  10010, #sparse
                  10020, #sparse
                  10030, #sparse
                  10040, #sparse
                  10060, #sparse
                  10070) #sparse

#codes for label column
rc2<-c("Agriculture",
  "Barren or Sparse",
  "Developed",
  #"Fill-Not Mapped",
  #"Snow/Ice",
  "UE",
  "UN")
  #"Water")

old_classes <- c("Late1", "Late2")
frg_class_one<-c("I-A", "I-B", "I-C") #only need FRG 1, focus of NOGA

#remove conditions
landfire<-hist_reg %>% 
  filter(!bps_model %in% remove_codes2) %>% 
  filter(!label %in% rc2) %>% 
  filter(age_category %in% old_classes) %>%   # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') %>% #note, we removed the 'all' canopy category in methods
  filter(frg_new %in% frg_class_one)

#check values have been removed
unique(landfire$bps_model)
# unique(landfire$label)
unique(landfire$label)
unique(landfire$age_category)

######## Step 3: obtain the numerator: find out what % of FRG I late succession is open and closed ###
#current
cur_reg<-landfire %>% 
  group_by(region, canopy_category) %>% 
  mutate(cur_area_canopy = (sum(count, na.rm = T))*0.2223945, 
         cur_perc_canopy = round((cur_area_canopy/region_acres)*100, 2))
  

#reference
open_cls<-cur_reg %>% 
  mutate(region_ref_area = round(bps_region_acres * (ref_percent/100), 2)) #checked
  
open_cls2<-open_cls %>% 
  select(region, canopy_category,region_acres, cur_area_canopy, cur_perc_canopy, region_ref_area) %>% 
  unique(.) %>% 
  group_by(region, canopy_category) %>% 
           mutate(ref_area_canopy = sum(region_ref_area, na.rm = T),
            ref_perc_canopy = round((ref_area_canopy/region_acres)*100, 2),
            percent_change = round(cur_perc_canopy - ref_perc_canopy, 0),
            sign_change = (percent_change > 0))

#check this doesnt make a difference
# #altering cur_scls_acres and cur_percent values to zero
# #if theres a ref_percent value, replace cur_scls_acres and cur_percent with zero, otherwise NA (bc that class never existed)
# df <- p_c %>%
#   mutate(cur_scls_acres = ifelse(is.na(cur_scls_acres) & !is.na(ref_percent), 0, cur_scls_acres),
#          cur_percent = ifelse(is.na(cur_percent) & !is.na(ref_percent), 0, cur_percent))


#create a summary of the regional open/closed
open_cls3<-open_cls2 %>% 
  select(region, canopy_category, cur_area_canopy, cur_perc_canopy, ref_area_canopy, ref_perc_canopy, percent_change, 
         sign_change, region_acres) %>% 
  unique(.) %>% 
  mutate(area_change = (cur_area_canopy - ref_area_canopy)/1000) #thousand acres


# now generate same table for national
# grab the region acre, ref acre and current acre
#recalculate the ref and cur percent

# national_condition <- open_cls2 %>%
#   group_by(canopy_category) %>% 
#   summarize(region_acres = sum(region_acres),
#             ref_area_canopy = sum(ref_area_canopy),
#             cur_area_canopy = sum(cur_area_canopy)) %>% 
#   mutate(ref_perc_canopy = (ref_area_canopy/region_acres)*100,
#          cur_perc_canopy = (cur_area_canopy/region_acres)*100,
#          percent_change = round(cur_perc_canopy - ref_perc_canopy, 0),
#          sign_change = (percent_change > 0),
#          area_change = (cur_area_canopy - ref_area_canopy)/1000,
#          region = 0)
# 
# #append to the regional info
# national_condition$region<-as.numeric(national_condition$region)
# open_cls3$region<-as.numeric(open_cls3$region)
# 
# total_condition <-bind_rows(open_cls3, national_condition)

#append region name onto the df
open_cls3$region<-as.numeric(open_cls3$region)

fig_data<-open_cls3 %>% 
  left_join(., region_name, by = "region")


######## step 4: generate bar plot ######
# how many x-axis units to position the percent label left or right of the bar
label_x_offset = 400

# # bar plot for acreage of each BPS
bar<-fig_data %>%
  #filter(canopy_category != 'OPN') %>%
  ggplot(aes(
    x=area_change,
    y=reorder(region_name, -region),
    fill = canopy_category)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8)) +
  theme_light(base_size = 12) +
  #coord_flip() + #need this for vertical
  scale_fill_manual(values = c("#91bfdb", "#fc8d59"), labels = c("Closed", "Open")) +
  #scale_fill_viridis_d(option = 'viridis', name = 'Canopy category', labels = c("Closed", "Open"), begin = 0.2, end = 0.8)+
  # scale_y_discrete(labels = function(y) paste("Region", y, sep = " ")) +  # Add "region" in front of each label
  labs(
    y = '',
    x = 'Net change in late succession FRG class I forest area (thousand acres)') +
  theme(
    axis.title.y = element_text(angle = -360, vjust = 0.5)) +  # Hide y-axis title
  #   axis.text.y = element_blank()) +   # Hide y-axis tick labels
  labs(fill = "Canopy category") +
  geom_hline(yintercept=seq(1.5, 7.5, by = 1)) +
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = canopy_category), position = position_dodge(0.9), hjust = 0.5)
bar

# #export plot
ggsave('./outputs/region_classI_canopy_bar_plot.png', bar, width = 10.5, height = 6, units = 'in', dpi = 300)



