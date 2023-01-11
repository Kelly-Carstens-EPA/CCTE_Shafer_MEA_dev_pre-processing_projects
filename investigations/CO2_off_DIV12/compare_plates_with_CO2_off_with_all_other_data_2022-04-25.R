# ------------------------------------------------------------------------ #
# Investigating case where 5% DMSO ran out during recording
# April 25, 2022
# ------------------------------------------------------------------------- 

library(data.table)
library(ggplot2)
library(cowplot)

# Load data----------------------------------------------------------------

# Load most recent snapshot
load('lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData')

# Load DNTFalseNegatives data
source('supplemental_scripts/get_latest_dat.R')
test.dat <- get_latest_dat(dataset_titles = 'DNTFalseNegatives', root.dir = 'L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
# # Loading longfile.Rdata for...
# DNTFalseNegatives 
ls()
names(test.dat)

dat <- rbind(mea_nfa_lvl0, test.dat, fill = T)

rm(test.dat, mea_nfa_lvl0)

# oops
dat[is.na(acnm), .N, by = .(dataset)]
# dataset     N
# 1: DNTFalseNegatives 12528
dat[is.na(acnm), acnm := acsn]

# remove all where wllq == 
dat <- dat[wllq == 1]


# View where CO2 turned off, confirm these okay -------------------------

dat[, CO2_off_DIV12 := as.numeric(grepl('5% CO2 ran out',wllq_notes))]


# Let's just compare the controls, by culture date

dat[, culture_date := as.Date(sub('_.*$','',apid), format = '%Y%m%d')]
dat[, culture_date_char := sub('_.*$','',apid)]

plotdat <- dat[grepl('firing_rate_mean_DIV12$',acnm)]

ggplot(plotdat[wllt == 'n'], aes(x = culture_date, y = rval)) +
  geom_jitter(aes(shape = factor(wllq), color = CO2_off_DIV12), width = 0.2, height = 0)+
  geom_boxplot(fill = 'transparent', outlier.shape = NA)+
  ggtitle('Comparison of Control wells by Plate, Mean Firing Rate DIV12')

# What if I instead compare teh plate means by culture?
plotdat <- dat[grepl('firing_rate_mean_DIV12$',acnm) & wllt == 'n', .(plate_med = median(rval[wllq == 1], na.rm = T)), by = .(apid, culture_date, CO2_off_DIV12)]
ggplot(plotdat, aes(x = culture_date, y = plate_med)) +
  geom_jitter(aes(color = CO2_off_DIV12), width = 0.2, height = 0)+
  ggtitle('Comparison of Control wells by Plate, Mean Firing Rate DIV12')

plotdat <- dat[grepl('firing_rate_mean_DIV12$',acnm) & wllt == 'n', .(plate_med = median(rval[wllq == 1], na.rm = T)), by = .(apid, culture_date_char, CO2_off_DIV12)]
ggplot(plotdat, aes(x = culture_date_char, y = plate_med)) +
  geom_jitter(aes(color = CO2_off_DIV12), width = 0, height = 0)+
  ggtitle('Comparison of Control wells by Plate, Mean Firing Rate DIV12')

plotdat <- dat[grepl('active_electrodes_number_DIV12$',acnm) & wllt == 'n', .(plate_med = median(rval[wllq == 1], na.rm = T)), by = .(apid, culture_date_char, CO2_off_DIV12)]
ggplot(plotdat, aes(x = culture_date_char, y = plate_med)) +
  geom_jitter(aes(color = CO2_off_DIV12), width = 0, height = 0)+
  ggtitle('Comparison of Control wells by Plate, Mean Firing Rate DIV12')


# What I really want to know:
# What is the usualy distribution of distances between the plate-wise control medians within a culture?
# Perhaps normalized to the mean?

medians.dat <- dat[wllt == 'n', .(plate_median = median(rval[wllq == 1], na.rm = T)), by = .(acnm, apid, culture_date, CO2_off_DIV12)]
dat2 <- medians.dat[, .(medians.dist = max(dist(plate_median), na.rm = T),
                        medians.median = median(plate_median, na.rm = T),
                        any_CO2_off_DIV12 = any(CO2_off_DIV12)), by = .(acnm, culture_date)]
dat2[, medians.dist.norm := medians.dist / medians.median]

pdf(file = 'DNTFalseNegatives/figs/distribution_of_distances_among_plate_medians_by_culture_to_compare_effects_of_CO2_Off.pdf', 
    width = 16, height = 8)
for (acnmi in unique(dat2[grepl('DIV12',acnm), acnm])) {
  
  p1 <- ggplot(dat2[acnm == acnmi], aes(x = medians.dist)) +
    geom_histogram()+
    geom_vline(xintercept = dat2[acnm == acnmi & any_CO2_off_DIV12 == TRUE, medians.dist],
               lwd = 2,
               col = 'cornflowerblue')+
    ggtitle(paste0(acnmi,' \nDistribution of distances among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates'))
  
  p2 <- ggplot(dat2[acnm == acnmi], aes(x = medians.dist.norm)) +
    geom_histogram()+
    geom_vline(xintercept = dat2[acnm == acnmi & any_CO2_off_DIV12 == TRUE, medians.dist.norm],
               lwd = 2,
               col = 'cornflowerblue')+
    ggtitle(paste0(acnmi,' \nDistribution of normalized distances among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates'))
  p <- ggdraw() +
    draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1)
  plot(p)
}
graphics.off()




# Updated Apr 26, 2022 - calculate quantiles ------------------------------

dat2[, medians.dist_rank_by_acnm := frank(medians.dist, ties.method = 'random'), by = .(acnm)]
dat2[, medians.dist.norm_rank_by_acnm := frank(medians.dist.norm, ties.method = 'random'), by = .(acnm)]

dat2[, medians.dist.target.percentile := medians.dist_rank_by_acnm[any_CO2_off_DIV12 == TRUE]/max(medians.dist_rank_by_acnm), by = .(acnm)]
dat2[, medians.dist.norm.target.percentile := medians.dist.norm_rank_by_acnm[any_CO2_off_DIV12 == TRUE]/max(medians.dist.norm_rank_by_acnm), by = .(acnm)]

View(dat2[grepl('DIV12',acnm), .N, by = .(acnm, medians.dist.target.percentile, medians.dist.norm.target.percentile)][order(medians.dist.target.percentile, medians.dist.norm.target.percentile)])

View(dat2[!grepl('DIV',acnm), .N, by = .(acnm, medians.dist.target.percentile, medians.dist.norm.target.percentile)][order(medians.dist.target.percentile, medians.dist.norm.target.percentile)])

ggplot(dat2[grepl('DIV12',acnm), .N, by = .(acnm, medians.dist.target.percentile, medians.dist.norm.target.percentile)],
       aes(x = medians.dist.target.percentile, y = medians.dist.norm.target.percentile))+
  geom_point()+
  geom_vline(xintercept = 0.5)+
  geom_hline(yintercept = 0.5)+
  ggtitle('Percentile of max differences among plates from 2021-08-18,\nwhere CO2 turned off for 2/3 plates')

# when 5/17 endpoints are off... what do you say?
# Including 1 really important 1- nAE


# Update Apr 27, 2022 -----------------------------------------------------

# Let's check out the endpoints where we are seeing some greater dissimilarity
# And determine if the difference is really due to the CO2 plates behaving differently
dat2[grepl('DIV12',acnm) & (medians.dist.target.percentile >= 0.5 | medians.dist.norm.target.percentile > 0.5), .N, by = .(acnm, medians.dist.norm.target.percentile, medians.dist.target.percentile)]
# acnm medians.dist.norm.target.percentile medians.dist.target.percentile  N
# 1:          CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12                           0.3538462                      0.6615385 65
# 2:          CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12                           0.6461538                      0.6461538 65
# 3:               CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12                           0.4000000                      0.5538462 65
# 4: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12                           0.4000000                      0.7230769 65
# 5:   CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12                           0.6153846                      0.4461538 65
check.acnmis <- dat2[grepl('DIV12',acnm) & (medians.dist.target.percentile >= 0.5 | medians.dist.norm.target.percentile > 0.5), unique(acnm)]

for (acnmi in check.acnmis) {
  
  plotdat <- dat[acnm == acnmi & wllt == 'n', .(plate_med = median(rval[wllq == 1], na.rm = T)), by = .(apid, culture_date_char, CO2_off_DIV12)]
  plotdat[, c('max_plate_med_by_culture','min_plate_med_by_culture') := list(max(plate_med, na.rm = T), min(plate_med, na.rm = T)), by = .(culture_date_char)]
  p <- ggplot(plotdat, aes(x = culture_date_char, y = plate_med)) +
    geom_jitter(aes(color = CO2_off_DIV12), width = 0, height = 0)+
    geom_segment(aes(x = culture_date_char, xend = culture_date_char, y = min_plate_med_by_culture, yend = max_plate_med_by_culture))+
    ggtitle(paste0('Comparison of Control wells by Plate\n',acnmi))
  plot(p)
            
}


# Previous thought:

# So I am seeing a real shift here between the first plate and teh other 2 plates.
# BUT, I think I need ot see this in a broader context to see if this magnitude of shift is significant
