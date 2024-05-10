rm(list = ls())

# ==============================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "tidyverse",
  "sf",
  "RColorBrewer"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

# ==============================================================================

raw_data = read.csv('./outputs/landfire_conus_2022_t8.csv')

# read in nationa forests shapefile
forests = st_read('./inputs/spatial/S_USA.AdministrativeForest.shp') %>%
  filter(REGION != 10) %>%
  filter(FORESTORGC != "0816")

# relevant succession classes
old_classes <- c("Late1", "Late2")

### NEED TO UPDATE THIS:

# calculate net change and other summary stats
summary = raw_data %>% 
  # filter(age_category %in% old_classes) %>% # keeping just Late1 and Late2
  # filter(canopy_category != 'ALL') %>%
  group_by(bps_model, forestorgc) %>%
  summarize(region_acre = sum(bps_acres),
            ref_acres = sum(ref_scls_acres),
            cur_acres = sum(cur_scls_acres)) %>% 
  mutate(region_ref_perc = (ref_acres/region_acre)*100,
         region_cur_perc = (cur_acres/region_acre)*100,
         change = region_cur_perc - region_ref_perc,
         sign_change = (change > 0),
         area_change = (cur_acres - ref_acres)/1000,
         percent_change = round(region_cur_perc - region_ref_perc, 0)) %>%
  drop_na() 

# add summary to forests attribute table
forests['forestorgc'] = as.numeric(forests$FORESTORGC)
forests = merge(forests, summary, by = 'forestorgc')

# read in regions; remove alaska
regions = st_read('./outputs/S_USA.AdministrativeRegion_noHIorPR.shp') %>%
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
  scale_fill_gradientn(name = 'Change (%)',
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
ggsave('./outputs/NF_change_map.png', map, width = 8, height = 5, units = 'in', dpi = 900)
