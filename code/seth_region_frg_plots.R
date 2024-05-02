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

raw_data = read.csv('./outputs/landfire_conus_2022_t8.csv')

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

# summarize by region and frg
summary = raw_data %>% 
  
  # CALCULATE PERCENTAGES SPECIFIC TO MY TABULATION (REGION BY FRG)
  group_by(region) %>% 
  mutate(frg_cur_prc = round((cur_scls_acres/sum(na.omit(cur_scls_acres)))*100, 10),
         frg_ref_prc = round((ref_scls_acres/sum(na.omit(ref_scls_acres)))*100, 10)) %>%
  
  filter(age_category %in% old_classes) %>% # keeping just Late1 and Late2
  filter(canopy_category != 'ALL') %>%
  
  group_by(region, frg_grp) %>%
  summarize(frg_ref_prc = sum(frg_ref_prc, na.rm = TRUE),
            frg_cur_prc = sum(frg_cur_prc, na.rm = TRUE),
            ref_acres = max(ref_scls_acres)) %>%
  mutate(change = frg_cur_prc - frg_ref_prc,
         sign_change = (change > 0)) %>%
  drop_na() 

# checking to confirm the denominator
test = raw_data[raw_data$region == 1,]
sum(test$cur_scls_acres, na.rm = T)
sum(test[which(test$frg_grp == '1 & 2' & test$age_category %in% old_classes & test$canopy_category != 'ALL' ),]$cur_scls_acres, na.rm = T)/sum(test$cur_scls_acres, na.rm = T)*100
# arrow represents start and end late succession area in each FRG class as a percentage of all BPS pixels

# factor region to allow for discrete y axis in figures
summary$region = as.factor(summary$region)

# arrow plot of late successional change between ref and current within each FRG in each region
arrow_plot =  summary  %>%
  ggplot(aes(
    x = frg_ref_prc, xend = frg_cur_prc, 
    y = region,
    group = frg_grp,
    color = frg_grp)) +
  geom_segment(
    position = position_dodge(.5),
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
    linewidth = 2)+
  labs(
    x = "Late succession as % of USFS land area", 
    y = "USFS Region\n"
  )+
  scale_color_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  scale_y_discrete(limits=rev)+
  theme_bw(base_size = 14)+
  theme(legend.position = "none")+
  geom_hline(yintercept=seq(1.5, 7.5, by = 1))

# bar plot of late successional acreage within each FRG in each region during ref condition
bar_plot = summary %>%
  ggplot(aes(
    x = ref_acres, 
    y = region,
    fill = frg_grp))+
  geom_col(position = 'dodge')+
  scale_x_continuous(label = comma)+
  scale_y_discrete(limits=rev)+  # ensures region 1 at top, 9 at bottom
  scale_fill_viridis_d(option = 'plasma', name = 'FRG', begin = 0, end = 0.8)+
  labs(
    x = "USFS late succession during ref. period (acres)", 
    y = ""
  )+
  theme_bw(base_size = 14)+
  geom_hline(yintercept=seq(1.5, 7.5, by = 1))+
  guides(fill = guide_legend(reverse=TRUE)) # reverse legend to match bars

combined = ggarrange(arrow_plot, bar_plot, widths = c(0.9, 1.1))

ggsave('./outputs/region_frg_plots.png', combined, width = 10.5, height = 6, units = 'in', dpi = 300)
