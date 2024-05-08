# Author: Amy Collins
##### Title: Generating figures for 
##### Date created: April 24 2024
##### Date last modified: May 7 2024

########### Step 1: packages and data ###########

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)
library(stringr)

getwd()


# final_bps_scls_2 <- read_csv("Data/final_bps_scls 2.csv")
# demo <- read_csv("Data/demo.csv")
# demo2<-subset(demo, bps_name !='etc') #remove extra dummy data rows
landfire<-read_csv("Outputs/landfire_conus_2022_t8.csv")
unique(landfire$label)


#FS regions
#all regions, inc. AK, Hawaii and Perto Rico
admin<-st_read("Data/S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp")

#regions with Hawaii and Perto Rico excluded
#then exclude Alaska too
fsregion<-st_read("Data/S_USA.AdministrativeRegion_noHIorPR/S_USA.AdministrativeRegion_noHIorPR.shp")


##################################################

# data request from Randy
# obtain number (%) of acre that encompasses UN
#landfire

  
#summarize the acres by label
region_UN <- landfire %>%
  group_by(region, label) %>% 
  reframe(ref_acres = sum(ref_scls_acres, na.rm = TRUE),
            cur_acres = sum(cur_scls_acres, na.rm = TRUE)) 

#add region acreage as column
region_Area <- region_UN %>%
  group_by(region) %>% 
  mutate(region_ref_acres = sum(ref_acres, na.rm = TRUE),
         region_cur_acres = sum(cur_acres, na.rm = TRUE),
         region_ref_perc = (ref_acres/region_ref_acres)*100,
         region_cur_perc = (cur_acres/region_cur_acres)*100) %>% 
  filter(label == "UN")
  
# ungroup() %>% 
# group_by(region,label) %>% 


# ######## Step 2: filter pieces out of dataset #############


###### more data wrangling
#filter out elements we dont want
old_classes <- c("Late1", "Late2")
frg_class_one<-c("I-A", "I-B", "I-C") #only need FRG 1, focus of NOGA


landfire_filter <- landfire %>% 
filter(age_category %in% old_classes) %>%   # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') %>%
  filter(frg_new %in% frg_class_one)  #class 1

unique(landfire_filter$label)

#####################################################################
#generate summary table for Sarah
names(landfire_filter)
summary(landfire_filter$canopy_category)

#sumamrize area
t <- landfire_filter %>%
  #slice_max(order_by = bps_acres, n = 10) %>%
  group_by(region) %>%
summarize(bps_acres = sum(bps_acres))


sum(landfire_filter$bps_acre)

#number of CLS (1221) and OPN (1110)
class_counts <- table(landfire_filter$canopy_category)
print(class_counts)

# ######## Step 3: group the data for fig generation #############
#group by region for the open/closed visual
#add up the bps acreage to get total per region
#add up the ref_scls_acres to get total reference acres per region
#add up the cur_scls_acres to get total current acres per region
#on the summarized df, do ref acres/total acres and current acres/total acres
#add column for % change
# add a column for net change in 1000 acres
# add a column for % change 
region_condition <- landfire_filter %>%
  group_by(region, canopy_category) %>% 
  summarize(region_acre = sum(bps_acres),
            ref_acres = sum(ref_scls_acres),
            cur_acres = sum(cur_scls_acres)) %>% 
  mutate(region_ref_perc = (ref_acres/region_acre)*100,
         region_cur_perc = (cur_acres/region_acre)*100,
         percent_change = round(region_cur_perc - region_ref_perc, 0),
         sign_change = (percent_change > 0),
         area_change = (cur_acres - ref_acres)/1000)

#save the csv to send to Seth
write_csv(region_condition, "Outputs/landfire_fig_generation.csv")
  

# ######## Step 4: fig generation #############
#plot 1, arrow plot - now not being used
# as.factor(region_condition$region)

# canopy_arrow_plot <- region_condition %>%
#   ggplot(aes(
#     y = region_ref_perc, yend = region_cur_perc, 
#     x = region,
#     color = canopy_category)) +
#   geom_segment(
#     # aes(y = y + 0.1, yend = yend - 0.1),
#     position = position_dodge(1),
#     arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
#     # Adjust the width as needed
#     linewidth = 2) +
#   labs(
#     y = 'Forest cover from reference (pre-European settlement) \n to current (2022) for USFS late succession FRG class I (%)', 
#     x = 'Forest Service regions') +
#   scale_color_manual(values = c("#91bfdb", "#fc8d59")) +
#  # scale_color_viridis_d(option = 'viridis', name = 'FRG', begin = 0.2, end = 0.8)+
#   #scale_color_viridis_d(option = 'viridis', name = 'FRG', begin = 0, end = 0.8)+
#   theme_light(base_size = 12) + 
#   coord_flip()+
#   theme(legend.position = "none") +
#   geom_vline(xintercept=seq(1.5, 7.5, by = 1))
# 
# canopy_arrow_plot
#
# # bar plot for acreage of each BPS
bar<-region_condition %>%
  #filter(canopy_category != 'OPN') %>%
  ggplot(aes(
    x=area_change,
    y=region,
    fill = canopy_category)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.8)) +
  theme_light(base_size = 12) +
  #coord_flip() + #need this for vertical
  scale_fill_manual(values = c("#91bfdb", "#fc8d59"), labels = c("Closed", "Open")) +
  #scale_fill_viridis_d(option = 'viridis', name = 'Canopy category', labels = c("Closed", "Open"), begin = 0.2, end = 0.8)+
  scale_y_discrete(labels = function(y) paste("Region", y, sep = " ")) +  # Add "region" in front of each label
  labs(
    y = 'Forest Service region',
    x = 'Net change in forested area for USFS Late Succession FRG class I (thousand acres)') +
  # theme(
  #   axis.title.y = element_blank(),  # Hide y-axis title
  #   axis.text.y = element_blank()) +   # Hide y-axis tick labels
  labs(fill = "Canopy category") +
  geom_hline(yintercept=seq(1.5, 7.5, by = 1)) +
  geom_text(aes(label = percent_change), vjust = -0, hjust = 0.5)
bar

#
# #bring the two figs together using cowplot
#fig<-plot_grid(canopy_arrow_plot, bar)
#fig

# #export plot
ggsave('./outputs/region_classI_canopy_bar_plot.png', bar, width = 10.5, height = 6, units = 'in', dpi = 300)

