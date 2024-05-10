rm(list = ls())

# ==============================================================================
# load required packages, installing any that have not yet been installed

packages = c(
  "tidyverse",
  "scales",
  "ggpubr",
  'viridis'
)

install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

# ==============================================================================

raw_data = read.csv('./outputs/landfire_conus_2022_t11.csv')

# get FRG number by splitting "frg_new" strings
raw_data['FRG'] = do.call(rbind.data.frame, strsplit(raw_data$frg_new, '-'))[,1] # some NAs for FRGs

# FRG groupings requested by Gunnar
frg_grps = data.frame('FRG' = c('I', 'II', 'III', 'IV', 'V'),
                      'frg_grp' = as.factor(c('1 & 2', '1 & 2', '3', '4 & 5', '4 & 5')))

# set factor levels for plotting order
frg_grps$frg_grp = factor(frg_grps$frg_grp, levels = rev(levels(frg_grps$frg_grp)))

raw_data = merge(raw_data, frg_grps, by = 'FRG', all = T)

# relevant succession classes
old_classes <- c("Late1", "Late2")

# calculate net change and other summary stats
summary = raw_data %>% 
  # filter(age_category %in% old_classes) %>% # keeping just Late1 and Late2
  # filter(canopy_category != 'ALL') %>%
  group_by(bps_model, region, frg_grp) %>%
  
  # revised summarize()
  summarize(region_frg_count = sum(count),
            region_frg_acre = region_count *0.2223945,
            ref_reg_frg_acres = sum(ref_scls_acres),
            cur_reg_frg_acres = sum(cur_scls_acres)) 

  mutate(region_ref_perc = (ref_acres/region_acre)*100,
         region_cur_perc = (cur_acres/region_acre)*100,
         change = region_cur_perc - region_ref_perc,
         sign_change = (change > 0),
         area_change = (cur_acres - ref_acres)/1000,
         percent_change = round(region_cur_perc - region_ref_perc, 0))


### NEW FROM AMY:

# #codes for bps_model column
# remove_codes <- c(0, -1111, 10010, 10020, 10030, 10040, 10060, 10070)
# 
# #codes for label column
# rc2<-c("Agriculture",
#        "Barren or Sparse",
#        "Developed",
#        "Fill-NoData",
#        "Fill-Not Mapped",
#        "Snow/Ice",
#        "UE",
#        "UN",
#        "Water",
#        "Blank")
#
# Also remove all but late successional classes as before!!
# 
# #remove conditions
# landfire<-df %>% 
#   filter(!bps_model %in% remove_codes) %>% 
#   filter(!label %in% rc2)
# 
# #check values have been removed
# unique(landfire$bps_model)
# # unique(landfire$label)



# join region names
region_names = read.csv('./inputs/region_names.csv')

summary = merge(summary, region_names, by = 'region')
summary$region_name = factor(summary$region_name, levels = unique(summary$region_name))

#### NEED TO UPDATE THIS (conus) TO REFLECT CHANGES MADE ABOVE TO SUMMARY:

# calculate net change and other summary stats for conus for "national" bars
conus = raw_data %>% 
  filter(age_category %in% old_classes) %>% # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') %>%
  group_by(bps_model, frg_grp) %>%
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

# add region and region_name columns to match summary
conus = cbind(region = 0, conus, region_name = as.factor('National'))

# combine conus and summary
summary = rbind(conus, summary)

# factor region to allow for discrete y axis in figures
summary$region = as.factor(summary$region)

# how many x-axis units to position the percent label left or right of the bar
label_x_offset = 1000 

# bar plot of late successional acreage within each FRG in each region during ref condition
bar_plot = summary %>%
  ggplot(aes(
    x = area_change, 
    y = region_name,
    fill = frg_grp))+
  geom_col(position = 'dodge')+
  scale_x_continuous(label = comma)+
  scale_y_discrete(limits=rev)+  # ensures region 1 at top, 9 at bottom
  scale_fill_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  labs(
    x = "\nNet change in late succession forest area (thousand acres)", 
    y = ""
  )+
  theme_bw(base_size = 14)+
  geom_hline(yintercept=seq(1.5, 8.5, by = 1))+
  guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
  geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
                label = paste0(percent_change, '%'), group = frg_grp), position = position_dodge(0.9), hjust = 0.5)

bar_plot

ggsave('./outputs/region_frg_plots_v2.png', bar_plot, width = 9, height = 6, units = 'in', dpi = 300)



# #===========================
# # check to compare with amy's FRG 1 Fig:
# 
# summary4 = raw_data %>%
#   filter(age_category %in% old_classes) %>% # keeping just Late1 and Late2
#   filter(canopy_category != 'ALL') %>%
#   group_by(region, FRG) %>%  # grouping by FRG instead of frg_grp
#   summarize(region_acre = sum(bps_acres),
#             ref_acres = sum(ref_scls_acres),
#             cur_acres = sum(cur_scls_acres)) %>%
#   mutate(region_ref_perc = (ref_acres/region_acre)*100,
#          region_cur_perc = (cur_acres/region_acre)*100,
#          change = region_cur_perc - region_ref_perc,
#          sign_change = (change > 0),
#          area_change = (cur_acres - ref_acres)/1000,
#          percent_change = round(region_cur_perc - region_ref_perc, 0)) %>%
#   drop_na()
# 
# summary4[summary4$FRG == 'I',]
# 
# # factor region to allow for discrete y axis in figures
# summary4$region = as.factor(summary4$region)
# 
# summary4$FRG = factor(summary4$FRG, levels = rev(c('I', 'II', 'III', 'IV', 'V')))
# summary4 = summary4 %>% drop_na() 
# 
# # how many x-axis units to position the percent label left or right of the bar
# label_x_offset = 400 
# 
# # bar plot of late successional acreage within each FRG in each region during ref condition
# bar_plot = summary4 %>%
#   ggplot(aes(
#     x = area_change, 
#     y = region,
#     fill = FRG))+
#   geom_col(position = 'dodge')+
#   scale_x_continuous(label = comma)+
#   scale_y_discrete(limits=rev, labels = function(y) paste("Region", y, sep = " "))+  # ensures region 1 at top, 9 at bottom
#   scale_fill_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
#   labs(
#     x = "Net change in late succession forest area (acres)", 
#     y = ""
#   )+
#   theme_bw(base_size = 14)+
#   geom_hline(yintercept=seq(1.5, 7.5, by = 1))+
#   guides(fill = guide_legend(reverse=TRUE)) +# reverse legend to match bars
#   geom_text(aes(x = ifelse(area_change < 0, area_change - label_x_offset, area_change + label_x_offset), 
#                 label = paste0(percent_change, '%'), group = FRG), position = position_dodge(0.9), hjust = 0.5)
# 
# bar_plot
