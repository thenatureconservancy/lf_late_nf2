rm(list = ls())

# ==============================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "sf",
  "dplyr",
  "ggplot2"
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

# ==============================================================================

regions = st_read('./inputs/spatial/S_USA.AdministrativeRegion.shp')

# convert multipart to single part
regions_sp = st_cast(regions, 'POLYGON')

# identify which polygons are located north of 25 degrees
keep = unlist(lapply(regions_sp$geometry, function(g){
  return(g %>% st_coordinates() %>% .[, "Y"] %>% min())
})) > 25

# retain only polyons situated north of 25 degrees
# dissolve filtered polygons on "REGION" attribute
filtered = regions_sp[keep,] %>%
  group_by(REGION) %>%
  summarize()

# restore original [only accurate] attributes
filtered = merge(filtered, regions[c('ADMINREGIO', 'REGION', 'REGIONNAME', 'REGIONHEAD')] %>% st_drop_geometry(), by = 'REGION')

# write to shapefile
st_write(filtered, './outputs/S_USA.AdministrativeRegion_noHIorPR.shp')


ggplot() +
  geom_sf(data = filtered, fill = 'red')
