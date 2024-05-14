##### Author: Seth Spawn-Lee & Amy Collins
##### Title: Generating figure for Landfire: national frg and open v closed
##### Date created: April 24 2024
##### Date last modified: May 14 2024

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
region_name<-read_csv("Inputs/region_names.csv")

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


# #create a summary of the regional open/closed
# open_cls3<-open_cls2 %>% 
#   select(region, canopy_category, cur_area_canopy, cur_perc_canopy, ref_area_canopy, ref_perc_canopy, percent_change, 
#          sign_change, region_acres) %>% 
#   unique(.) %>% 
#   mutate(area_change = (cur_area_canopy - ref_area_canopy)/1000) #thousand acres


# now generate same table for national
# grab the region acre, ref acre and current acre
# recalculate the ref and cur percent

national_condition <- open_cls2 %>%
  group_by(canopy_category) %>%
  summarize(region_acres = sum(region_acres),
            ref_area_canopy = sum(ref_area_canopy),
            cur_area_canopy = sum(cur_area_canopy)) %>%
  mutate(ref_perc_canopy = (ref_area_canopy/region_acres)*100,
         cur_perc_canopy = (cur_area_canopy/region_acres)*100,
         percent_change = round(cur_perc_canopy - ref_perc_canopy, 0),
         sign_change = (percent_change > 0),
         area_change = (cur_area_canopy - ref_area_canopy)/1000,
         region = 0)

label_x_offset = 50000
# # bar plot for acreage of each BPS
cond_bar<-national_condition %>%
  #filter(canopy_category != 'OPN') %>%
  ggplot(aes(
    x = area_change,
    y = as.factor(region),
    fill = canopy_category)) +
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  theme_light(base_size = 14) +
  guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
  #coord_flip() + #need this for vertical
  scale_x_continuous(label = comma)+
  scale_fill_manual(values = c("#91bfdb", "#fc8d59"), labels = c("Closed", "Open")) +
  #scale_fill_viridis_d(option = 'viridis', name = 'Canopy category', labels = c("Closed", "Open"), begin = 0.2, end = 0.8)+
  # scale_y_discrete(labels = function(y) paste("Region", y, sep = " ")) +  # Add "region" in front of each label
  labs(
    y = '',
    x = 'Net change in late succession FRG class 1 forest area (thousand acres)') +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_text(angle = -360, vjust = 0.5)) +  # Hide y-axis title
  #   axis.text.y = element_blank()) +   # Hide y-axis tick labels
  labs(fill = "Canopy category") +
  # geom_hline(yintercept=seq(1.5, 7.5, by = 1)) +
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = canopy_category), position = position_dodge(0.9), hjust = 0.5)
cond_bar



#===============================================================================


#remove conditions
landfire_frgplot<-hist_reg %>% 
  filter(!bps_model %in% remove_codes2) %>% 
  filter(!label %in% rc2) %>% 
  filter(age_category %in% old_classes) %>%   # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') #%>% #note, we removed the 'all' canopy category in methods
  # filter(frg_new %in% frg_class_one)

#check values have been removed
unique(landfire_frgplot$bps_model)
# unique(landfire$label)
unique(landfire_frgplot$label)
unique(landfire_frgplot$age_category)

######## for every region, find out what % of late succession is in each FRG (numerator) ###
### get FRGs grouped how we want them
# get FRG number by splitting "frg_new" strings
landfire_frgplot['FRG'] = do.call(rbind.data.frame, strsplit(landfire_frgplot$frg_new, '-'))[,1] # some NAs for FRGs

# FRG groupings requested by Gunnar
frg_grps = data.frame('FRG' = c('I', 'II', 'III', 'IV', 'V'),
                      'frg_grp' = as.factor(c('1 & 2', '1 & 2', '3', '4 & 5', '4 & 5')))

# set factor levels for plotting order
frg_grps$frg_grp = factor(frg_grps$frg_grp, levels = rev(levels(frg_grps$frg_grp)))

landfire2_frgplot <- merge(landfire_frgplot, frg_grps, by = 'FRG', all = T)

### current calcs
cur_frg<-landfire2_frgplot %>% 
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
         area_change = (cur_acre_frg - ref_acre_frg)/100) #thousand acres


national_frg <- ref_frg2 %>%
  group_by(frg_grp) %>%
  summarize(region_acres = sum(region_acres),
            ref_acre_frg = sum(ref_acre_frg),
            cur_acre_frg = sum(cur_acre_frg)) %>%
  mutate(ref_perc_frg = (ref_acre_frg/region_acres)*100,
         cur_perc_frg = (cur_acre_frg/region_acres)*100,
         percent_change = round(cur_perc_frg - ref_perc_frg, 0),
         sign_change = (percent_change > 0),
         area_change = (cur_acre_frg - ref_acre_frg)/1000,
         region = 0)%>%
  drop_na()

label_x_offset_cond = 50000
label_x_offset_frg = 40000

xmax = max(c(national_condition$area_change, national_frg$area_change))+(2*max(c(label_x_offset_cond, label_x_offset_frg)))
xmin =min(c(national_condition$area_change, national_frg$area_change))-(2*max(c(label_x_offset_cond, label_x_offset_frg)))


# # bar plot for acreage of each BPS
cond_bar<-national_condition %>%
  #filter(canopy_category != 'OPN') %>%
  ggplot(aes(
    x = area_change,
    y = as.factor(region),
    fill = canopy_category)) +
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  theme_light(base_size = 14) +
  guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
  #coord_flip() + #need this for vertical
  scale_x_continuous(label = comma, limits = c(xmin, xmax))+
  scale_fill_manual(values = c("#91bfdb", "#fc8d59"), labels = c("Closed", "Open")) +
  #scale_fill_viridis_d(option = 'viridis', name = 'Canopy category', labels = c("Closed", "Open"), begin = 0.2, end = 0.8)+
  # scale_y_discrete(labels = function(y) paste("Region", y, sep = " ")) +  # Add "region" in front of each label
  labs(
    y = '',
    x = 'Net change in late succession FRG class 1 forest area (thousand acres)') +
  theme(
    axis.title.y = element_text(angle = -360, vjust = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +  # Hide y-axis title
  #   axis.text.y = element_blank()) +   # Hide y-axis tick labels
  labs(fill = "Canopy category") +
  # geom_hline(yintercept=seq(1.5, 7.5, by = 1)) +
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = canopy_category), position = position_dodge(0.9), hjust = 0.5)
cond_bar

frg_bar = national_frg %>%
  ggplot(aes(
    x = area_change, 
    y = as.factor(region),
    # group = as.factor(region),
    fill = frg_grp))+
  geom_bar(stat="identity", position = position_dodge(width = 1))+
  scale_x_continuous(label = comma, limits = c(xmin, xmax))+
  #scale_y_discrete(limits=rev)+  # ensures region 1 at top, 9 at bottom
  scale_fill_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  labs(
    x = "Net change in late succession forest area (thousand acres)", 
    y = ""
  )+
  theme_light(base_size = 14)+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  #geom_hline(yintercept=seq(1.5, 8.5, by = 1))+
  guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = frg_grp), position = position_dodge(0.9), hjust = 0.5)

frg_bar

require(ggpubr)

comb = ggarrange(frg_bar, cond_bar, ncol = 1, align = 'hv', heights = c(2.65, 2))

ggsave('./outputs/national_frg_and_canopy_bar_plot.png', comb, width = 10.5, height = 6, units = 'in', dpi = 300)
