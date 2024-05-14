##### Author: Amy Collins
##### Title: Generating figures for Landfire: regional v forest
##### Date created: May 12 2024
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
library(RColorBrewer)

getwd()


# final_bps_scls_2 <- read_csv("Data/final_bps_scls 2.csv")
# demo <- read_csv("Data/demo.csv")
# demo2<-subset(demo, bps_name !='etc') #remove extra dummy data rows
complete<-read_csv("Outputs/landfire_conus_2022_t12.csv")
unique(complete$label)

# read in national forests shapefile
forest_admin = st_read('Data/S_USA.AdministrativeForest/S_USA.AdministrativeForest.shp') %>%
  filter(REGION != 10) %>%
  filter(FORESTORGC != "0816")

#read in regions
region_shp = st_read('Data/S_USA.AdministrativeRegion_noHIorPR/S_USA.AdministrativeRegion_noHIorPR.shp')

# ######## 2: obtain denominator: total area is for each forest (for ref percent) #####
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



#find out what the total area is for each forest (for cur percent)
names(complete2)
forest<-complete2 %>%
  group_by(forestname) %>% 
  mutate(forest_acres = (sum(count, na.rm = T))*0.2223945)


#find out what the total area is for each forest within a bps model (for ref percent)
forest2<-forest %>%
  group_by(forestname, bps_model) %>% 
  mutate(bps_forest_acres = (sum(count, na.rm = T))*0.2223945)


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
landfire<-forest2 %>% 
  filter(!bps_model %in% remove_codes2) %>% 
  filter(!label %in% rc2) %>% 
  filter(age_category %in% old_classes) # keep just late succession

#check values have been removed
unique(landfire$bps_model)
# unique(landfire$label)
unique(landfire$label)
unique(landfire$age_category)


######## 3: obtain numerator: for every forest, find out what % of late succession is in each forest area ######
#current forest area
cur_for<-landfire %>% 
  group_by(forestname) %>% 
  mutate(cur_acre_forest = (sum(count, na.rm = T))*0.2223945, 
         cur_perc_forest = round((cur_acre_forest/forest_acres)*100, 2)) #% of late succession in forest boundary area

#reference forest area
## ref calcs
ref_for<-cur_for %>% 
  mutate(ref_acre_sclass_forest = round(bps_forest_acres * (ref_percent/100), 2)) #get the s-class regional acreage by the proportional reference

ref_for2<-ref_for %>% 
 select(forestname, forestorgc, forest_acres, bps_forest_acres, cur_acre_forest, cur_perc_forest, ref_acre_sclass_forest) %>% 
  unique(.) %>% #gets rid of duplicate zones
  group_by(forestname) %>% 
  mutate(ref_acre_forest = sum(ref_acre_sclass_forest, na.rm = T),
         ref_perc_forest = round((ref_acre_forest/forest_acres)*100, 2)) %>% 
  select(forestname, forestorgc, forest_acres, cur_acre_forest, cur_perc_forest, ref_acre_forest, ref_perc_forest) %>% 
  unique(.) %>% 
  mutate(percent_change = round(cur_perc_forest - ref_perc_forest, 0),
         sign_change = (percent_change > 0),
         area_change = (cur_acre_forest - ref_acre_forest)/100) #thousand acres

############# 4. generate a map ######
# add summary to forests attribute table
forest_admin['forestorgc'] = as.numeric(forest_admin$FORESTORGC)
ref_for2$forestorgc <-as.numeric(ref_for2$forestorgc)
forests = merge(forest_admin, ref_for2, by = 'forestorgc')
names(forest_admin)
names(ref_for2)


# read in regions; remove alaska
regions <-region_shp %>%
  filter(REGION != 10) # %>%

# get [pseudo-]"centroids" within each region for labeling. Make label "R[region#]"
centroids = regions %>%
  st_point_on_surface() %>%
  mutate(label = paste0('R', as.integer(REGION)))

# omit unless want full text region labels
# # create text labels for each region
# labels = st_read('./inputs/spatial/label_locs.shp') %>% 
#   mutate(lon = st_coordinates(.)[,1],
#          lat = st_coordinates(.)[,2],
#          label = paste('Region', Region))
# 
# # create region 4 arrow linestring from origin and end points
# r4_arrow = st_read('./inputs/spatial/r4_arrow_pts.shp') %>%
#   st_union() %>%
#   st_cast('LINESTRING')


# map
map = ggplot() +
  geom_sf(data = regions, fill = 'gray25', color = 'NA')+ # gray60
  geom_sf(data = forests, aes(fill = percent_change), color = 'NA') +# color = 'gray80', linewidth = 0.1) +
  geom_sf(data = regions, fill = NA, color = 'black', , linewidth = 0.5)+ # gray60
  # geom_sf_text(data = labels, aes(label = label), color = 'gray65')+
  geom_sf(data = centroids, color = 'black', size = 11)+
  geom_sf(data = centroids, color = 'white', size = 10)+
  # geom_sf_text(data = centroids, aes(label = label), color = 'gray65')+
  geom_sf_text(data = centroids, aes(label = label), color = 'black')+
  # geom_sf(data = r4_arrow,
  #         arrow = arrow(angle = 30, 
  #                       ends = "first", 
  #                       type = "closed", 
  #                       length = unit(0.2, "cm")),
  #         fill = 'gray65',
  #         color = 'gray65',
  #         linewidth = 0.5)+
  # geom_sf(data = centroids)
  scale_fill_gradientn(name = 'Relative change in late succession forest area (%)',
                       colours = brewer.pal(8, 'PRGn'),
                       limits = c(min(forests$percent_change), abs(min(forests$percent_change))),
                       expand = T,
                       breaks = c(-50, -25,0,25,50),
                       guide = guide_colorbar(
                         frame.colour = "black",
                         ticks.colour = 'black',
                         title.position="top",
                         title.hjust = 0.5))+
  coord_sf(crs = 5070)+
  theme_void()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(2.5, "cm"),
        legend.key.height = unit(0.4, "cm"))

map
ggsave('Outputs/NF_change_map.png', map, width = 8, height = 5, units = 'in', dpi = 900)





