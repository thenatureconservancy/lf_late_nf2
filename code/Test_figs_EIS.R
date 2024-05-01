##### Author: Amy Collins
##### Title: Generating figures for 
##### Date created: April 24 2024
##### Date last modified: April 30 2024

########### Step 1: packages and data ###########

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(cowplot)

getwd()


# final_bps_scls_2 <- read_csv("Data/final_bps_scls 2.csv")
demo <- read_csv("Data/demo.csv")
demo2<-subset(demo, bps_name !='etc') #remove extra dummy data rows
landfire<-read_csv("Outputs/landfire_conus_2022_t7.csv")

#FS regions
#all regions, inc. AK, Hawaii and Perto Rico
admin<-st_read("Data/S_USA.AdministrativeRegion/S_USA.AdministrativeRegion.shp")

#regions with Hawaii and Perto Rico excluded
#then exclude Alaska too
fsregion<-st_read("Data/S_USA.AdministrativeRegion_noHIorPR/S_USA.AdministrativeRegion_noHIorPR.shp")

# 
# ######## Step 2: generate fig 3 #############
# #### demo data first
# #might not be needed for actual df
# is.numeric(demo2$bps_acres)
# # demo2$bps_acres <- as.numeric(demo2$bps_acres)
# 
# is.numeric(demo2$ref_percent)
# is.numeric(demo2$cur_percent)
# is.numeric(landfire$cur_percent)
# # demo2$ref_percent <- as.numeric(demo2$ref_percent)
# # demo2$cur_percent <- as.numeric(demo2$cur_percent)
# 
# unique_levels <- unique(demo2$canopy_category)
# print(unique_levels)
# 
# # facet_names <- c(
# #   "CLS" = "Closed Canopy",
# #   "OPN" = "Open Canopy"
# # )
# 
# #demo2. data
# #plot 1, arrow plot
# canopy_arrow_plot <- demo2 %>%
#   ggplot(aes(
#     y = ref_percent, yend = cur_percent,
#     x = reorder(bps_name, bps_acres),
#     color = canopy_category)) +
#   geom_segment(
#     # aes(y = y + 0.1, yend = yend - 0.1),
#     position = position_dodge(1),
#     arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
#     # Adjust the width as needed
#     linewidth = 1.2) +
#   labs(
#     y = 'departure (%)',
#     x = 'BPS') +
#   scale_color_manual(
#     values = c("#3399FF", "#993366")) +
#   theme_light(base_size = 11) +
#   coord_flip()+
#   theme(legend.position = "none")
# 
# canopy_arrow_plot
# 
# # now the bar plot for acreage of each BPS
# bar<-demo2 %>%
#   ggplot(aes(
#     x=bps_acres,
#     y=reorder(bps_name, bps_acres),
#     fill = canopy_category)) +
#   geom_bar(stat="identity", position = position_dodge(width = 1)) +
#   theme_light() +
#   #coord_flip() + #need this for vertical
#   scale_fill_manual(values = c("#3399FF", "#993366"), labels = c("Closed", "Open")) +
#   labs(
#     y = 'BPS',
#     x = 'BPS Area (acres)') +
#   theme(
#     axis.title.y = element_blank(),  # Hide y-axis title
#     axis.text.y = element_blank()   # Hide y-axis tick labels
#   ) +
#   labs(fill = "Canopy category")
# bar
# 
# #theme(legend.position = "right")
#   #ylim(0, 450000)
# 
# 
# 
# #bring the two figs together using cowplot
# fig<-plot_grid(canopy_arrow_plot, bar)
# fig
# 
# 


#now try with the actual data #######
#filter out elements we dont want
old_classes <- c("Late1", "Late2")
frg_class_one<-c("I-A", "I-B", "I-C")
frg_class_two<-c("II-A", "II-B", "II-C")

landfire_filter <- landfire %>% 
filter(age_category %in% old_classes) %>%   # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') %>%
  filter(frg_new %in% frg_class_one)  #class 1


# 1. subset data to 10 largest bps 
#get a table of the largest acre values
unique_bps <- landfire_filter %>%
  arrange(desc(bps_acres)) %>%
  pull(bps_acres) %>%
  unique() %>%
  head(10)

#then filter the csv by those values
landfire_bps <- landfire_filter %>%
  filter(bps_acres %in% unique_bps)

# 2.summarize data by bps open and closed (for frg 1 and frg 2)
#ref acres for each bps label / total amount of bps acreage
#current acreas for each bps label/ total amount of bps acreage

test <- landfire_bps %>% 
  # calculate % (bps by open and closed)
  group_by(bps_name) %>% 
  mutate(bps_cur_prc = round((cur_scls_acres/sum(na.omit(cur_scls_acres)))*100, 2), #% of bps area
         bps_ref_prc = round((ref_scls_acres/sum(na.omit(ref_scls_acres)))*100, 2)) %>%
  group_by(bps_name, bps_acres, canopy_category) %>%
  summarize(bps_ref_prc = sum(bps_ref_prc, na.rm = TRUE),
            bps_cur_prc = sum(bps_cur_prc, na.rm = TRUE),
            ref_acres = max(ref_scls_acres)) %>%
  mutate(change = bps_cur_prc - bps_ref_prc,
         sign_change = (change > 0))

#plot 1, arrow plot
names(test)
canopy_arrow_plot <- test %>%
  ggplot(aes(
    y = bps_ref_prc, yend = bps_cur_prc, 
    x = reorder(bps_name, bps_acres),
    color = canopy_category)) +
  geom_segment(
    # aes(y = y + 0.1, yend = yend - 0.1),
    position = position_dodge(1),
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
    # Adjust the width as needed
    linewidth = 1.2) +
  labs(
    y = 'departure (%)', 
    x = 'BPS') +
  #scale_color_manual(values = c("#91bfdb", "#fc8d59")) +
  scale_color_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  theme_light(base_size = 9) + 
  coord_flip()+
  theme(legend.position = "none")

canopy_arrow_plot

# now the bar plot for acreage of each BPS
bar<-test %>% 
  ggplot(aes(
    x=bps_acres,
    y=reorder(bps_name, bps_acres),
    fill = canopy_category)) +
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  theme_dark() +
  #coord_flip() + #need this for vertical
  scale_fill_manual(values = c("#91bfdb", "#fc8d59"), labels = c("Closed", "Open")) +
  labs(
    y = 'BPS', 
    x = 'BPS Area (acres)') +
  theme(
    axis.title.y = element_blank(),  # Hide y-axis title
    axis.text.y = element_blank()   # Hide y-axis tick labels
  ) +
  labs(fill = "Canopy category")
bar

#theme(legend.position = "right")
#ylim(0, 450000)



#bring the two figs together using cowplot
fig<-plot_grid(canopy_arrow_plot, bar)
fig

#repeat for FRG 2

############### Step 3: generate choropleth map ###########

# ggplot(admin) +
#   geom_sf(fill = "#69b3a2", color = "white") +
#   theme_void()

#remove alaska
print(fsregion)
fsregion_filter <- fsregion%>%
  filter(REGIONNAME != "Alaska Region")
print(fsregion_filter)

#filter out elements we dont want
landfire_reg_filter <- landfire %>% 
  filter(age_category %in% old_classes) %>%   # keeping just Late1 and Late2
  filter(canopy_category != 'ALL')

#summarize dataset to get % change by region
#going to need help with this
# test_reg <- landfire_reg_filter %>% 
#   # calculate % (bps by open and closed)
#   group_by(region) %>% 
#   mutate(reg_cur_prc = round((cur_scls_acres/sum(na.omit(cur_scls_acres)))*100, 10),
#          reg_ref_prc = round((ref_scls_acres/sum(na.omit(ref_scls_acres)))*100, 10)) %>%
#   group_by(region) %>%
#   summarize(reg_ref_prc = sum(reg_ref_prc, na.rm = TRUE),
#             reg_cur_prc = sum(reg_cur_prc, na.rm = TRUE),
#             ref_acres = max(ref_scls_acres)) %>%
#   mutate(change = reg_cur_prc - reg_ref_prc,
#          sign_change = (change > 0))



#by region, add up all the bps acreage
#then do what % of the region is that bps
#then weight the % change by that

#mean rate of change
#do we want to weight it by area??

#for each region, 



#ref acres / total amount of region acreage
#current acres / total amount of region acreage

#ref acres / total amount of bps acreage
#current acreas / total amount of bps acreage




#admin$perc_change <- c(22, -15, 31, 2, 18, -7, 7, 10, -10) #make up some values


# Create a simple map
ggplot() +
  geom_sf(data = fsregion_filter, aes(fill = perc_change)) +
  #scale_fill_viridis(option = "D", direction = -1) +  # Use the viridis color scale
  scale_fill_gradient2(low = "#8856a7", mid = "white", high = "#1c9099", midpoint = 0) +
  theme_classic()+
  labs(fill = "% change")
