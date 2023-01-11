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


# Calculate SD and CV of plate medians by culture -------------------------

dat.by.apid <- dat[, .(plate_med = median(rval[wllt == 'n'], na.rm = T),
                       plate_sd = sd(rval[wllt == 'n'], na.rm = T)), by = .(acnm, apid, culture_date, culture_date_char, CO2_off_DIV12)]
dat.by.culture <- dat.by.apid[, .(sd_of_plate_med = sd(plate_med, na.rm = T),
                                  mean_of_plate_med = mean(plate_med, na.rm = T),
                                  sd_of_plate_sd = sd(plate_sd, na.rm = T),
                                  mean_of_plate_sd = mean(plate_sd, na.rm = T),
                                  any_CO2_off_DIV12 = max(CO2_off_DIV12)), by = .(acnm, culture_date, culture_date_char)]
dat.by.culture[, `:=`(cv_of_plate_med = sd_of_plate_med / mean_of_plate_med,
                      cv_of_plate_sd = sd_of_plate_sd / mean_of_plate_sd)]


pdf(file = 'DNTFalseNegatives/figs/MEA_NFA_distribution_of_SDs_and_CVs_among_plate_medians_by_culture_to_compare_effects_of_CO2_Off_during_DIV12_recording.pdf', 
    width = 16, height = 8)
for (acnmi in unique(dat.by.culture[grepl('DIV12',acnm), acnm])) {
  
  p1 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = sd_of_plate_med)) +
    geom_histogram()+
    geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, sd_of_plate_med],
               lwd = 1.5,
               col = 'cornflowerblue')+
    geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(sd_of_plate_med, na.rm = T)],
               lwd = 1.5,
               col = 'darkred')+
    ggtitle(paste0(acnmi,' \nDistribution of standard deviations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))
  
  p2 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = cv_of_plate_med)) +
    geom_histogram()+
    geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, cv_of_plate_med],
               lwd = 1.5,
               col = 'cornflowerblue')+
    geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(cv_of_plate_med, na.rm = T)],
               lwd = 1.5,
               col = 'darkred')+
    ggtitle(paste0(acnmi,' \nDistribution of coefficient of variations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))
  p <- ggdraw() +
    draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1)
  plot(p)
}
graphics.off()


# For which endpoints is the culture of interest below the median?
summary.by.acnm <- dat.by.culture[, .(sd_of_plate_med_target = sd_of_plate_med[any_CO2_off_DIV12 == 1],
                                      cv_of_plate_med_target = cv_of_plate_med[any_CO2_off_DIV12 == 1],
                                      median_sd_of_plate_med = median(sd_of_plate_med, na.rm = T),
                                      median_cv_of_plate_med = median(cv_of_plate_med, na.rm = T)), by = .(acnm)]
summary.by.acnm[sd_of_plate_med_target >= median_sd_of_plate_med | cv_of_plate_med_target >= median_cv_of_plate_med]
#                                                           acnm sd_of_plate_med_target cv_of_plate_med_target median_sd_of_plate_med median_cv_of_plate_med
# 1:          CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12           5.204157e+00             0.14238850            2.896310809             0.16091284
# 2:                CCTE_Shafer_MEA_dev_interburst_interval_mean           6.288145e+01             0.40558385           28.133203697             0.13147612
# 3:                     CCTE_Shafer_MEA_dev_burst_duration_mean           2.039893e+00             0.36513485            0.669777736             0.15536687
# 4:           CCTE_Shafer_MEA_dev_per_burst_interspike_interval           6.310095e-02             0.14951674            0.048009738             0.16102088
# 5:                        CCTE_Shafer_MEA_dev_firing_rate_mean           1.153823e+00             0.18035457            1.527030110             0.17430764
# 6:           CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12           1.200706e-03             0.21109163            0.002613817             0.19694698
# 7:                 CCTE_Shafer_MEA_dev_mutual_information_norm           2.028982e-03             0.22698928            0.008613833             0.20596484
# 8:          CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12           5.000000e-01             0.03225806            0.288675135             0.01823211
# 9:               CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12           2.026629e-02             0.07607658            0.016492361             0.09130727
# 10:                     CCTE_Shafer_MEA_dev_spike_duration_mean           1.192747e-01             0.14792202            0.152651346             0.13022333
# 11:        CCTE_Shafer_MEA_dev_network_spike_duration_std_DIV12           1.029003e-02             0.17217763            0.012199695             0.16878834
# 12: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12           5.845296e+00             0.16578600            1.984662452             0.19098958
# 13:       CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean           1.040387e+02             0.20905148           52.328454422             0.19360434
# 14:   CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12           2.302689e+00             0.16496896            2.171497818             0.10648542
# 15:                 CCTE_Shafer_MEA_dev_per_burst_spike_percent           3.147312e+01             0.13858102           32.646632118             0.07675813
# Let's check otu these endpoints

# How often is it the SD vs the CV that's over the median?
# (auc and div12 endpionts only)
summary.by.acnm[(grepl('DIV12',acnm) | !grepl('DIV',acnm)) & !grepl('(AB)|(LDH)',acnm), length(unique(acnm))] # 34
summary.by.acnm[(grepl('DIV12',acnm) | !grepl('DIV',acnm)) & !grepl('(AB)|(LDH)',acnm),
                .N, by = .(target_sd_over_median = sd_of_plate_med_target >= median_sd_of_plate_med,
                           target_cv_over_median = cv_of_plate_med_target >= median_cv_of_plate_med)]
# target_sd_over_median target_cv_over_median  N
# 1:                 FALSE                 FALSE 19
# 2:                  TRUE                 FALSE  4
# 3:                  TRUE                  TRUE  5
# 4:                 FALSE                  TRUE  6
# a few cases of each
summary.by.acnm[, `:=`(target_sd_over_median = sd_of_plate_med_target >= median_sd_of_plate_med,
                           target_cv_over_median = cv_of_plate_med_target >= median_cv_of_plate_med)]
summary.by.acnm[(grepl('DIV12',acnm) | !grepl('DIV',acnm)) & !grepl('(AB)|(LDH)',acnm) & (target_sd_over_median == TRUE | target_cv_over_median == TRUE)]

check.acnmis <- summary.by.acnm[sd_of_plate_med_target >= median_sd_of_plate_med | cv_of_plate_med_target >= median_cv_of_plate_med, unique(acnm)]
acnmi <- 'CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12'
pdf(file = 'DNTFalseNegatives/figs/MEA_NFA_plate_medians_by_culture_to_compare_effects_of_CO2_Off_during_DIV12_recording.pdf',
    width = 16, height = 8)
for (acnmi in check.acnmis) {
  
  plotdat <- dat.by.apid[acnm == acnmi]
  p <- ggplot(plotdat, aes(x = culture_date_char, y = plate_med)) +
    geom_jitter(aes(color = CO2_off_DIV12), width = 0, height = 0, pch = 1, stroke = 1.5)+
    ggtitle(paste0('Comparison of Control wells by Plate\n',acnmi))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  plot(p)

}
graphics.off()

summary.by.acnm[(grepl('DIV12',acnm) | !grepl('DIV',acnm)) & !grepl('(AB)|(LDH)',acnm) & (target_sd_over_median == TRUE | target_cv_over_median == TRUE), cat(acnm,sep = '\n')]

# CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12 - semi problematic
# CCTE_Shafer_MEA_dev_interburst_interval_mean - good plate is actually in the middle. This seems less problematic
# CCTE_Shafer_MEA_dev_burst_duration_mean - good plate is actually in the middle. This seems less problematic
# CCTE_Shafer_MEA_dev_per_burst_interspike_interval - maybe an issue. But compared to variability in other recent plates, looks fine!
# CCTE_Shafer_MEA_dev_firing_rate_mean  - good plate is actually in the middle. This seems less problematic
# CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12 - visually compared to other plates this looks fine!! cv is barley over the meidan when look at distribution
# CCTE_Shafer_MEA_dev_mutual_information_norm - similar to above
# CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12 - the plate that had CO2 is actually the lowest here!! So doesn't seem to be due to a negative plate effect.
# CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12 - compared to others, this looks fine
# CCTE_Shafer_MEA_dev_spike_duration_mean - good plate in the middle.
# CCTE_Shafer_MEA_dev_network_spike_duration_std_DIV12 - strong effect here, but again compared to other cultures adn the fact that the good plate is lowest makes me not concerned
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12 - there is a clear increase in the variability in this parameter over time. So compared to recent cultures, SD here looks negligible
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean - weaker instance of above. Maybe some slight concern here
# CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12 - interesting case. Variability doesn't look problematic omcpared to other cultures. Perhaps bc values are tending to decrease over time, mroe recent CVs are relatively large?
# CCTE_Shafer_MEA_dev_per_burst_spike_percent - maybe slightly concnering?


# idea: have bins for each of these, chooes a representative image, put in a ppt, then move on!! (or rmd?)
# If only X of the Y endpoints have soem marginal effect - again, I thin kthis is okay than removing this data.

# How many/which endpoints have the unaffected plate as the median?
dat.by.apid[, median_of_plate_median_by_culture := median(plate_med, na.rm = T), by = .(acnm, culture_date)]
dat.by.apid[, median_plate_by_culture := unique(apid[plate_med == median_of_plate_median_by_culture])[1], by = .(acnm, culture_date)] # if there is a tie, choose one
dat.by.apid[acnm %in% check.acnmis & culture_date_char == '20210818' & median_plate_by_culture == '20210818_MW75-9205', .N, by = .(acnm)]
# acnm N
# 1: CCTE_Shafer_MEA_dev_interburst_interval_mean 3
# 2:      CCTE_Shafer_MEA_dev_burst_duration_mean 3
# 3:         CCTE_Shafer_MEA_dev_firing_rate_mean 3
# 4:      CCTE_Shafer_MEA_dev_spike_duration_mean 3

# endpoints with increase in variability over time (visually)
# CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12
# CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12

# decrease in variability (and value)
# CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12
# CCTE_Shafer_MEA_dev_mutual_information_norm
# CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12
# -> might make the CV seem smaller?
# If only the CV is above in these cases, I woudl say this is excusable
acnms.dec.in.variability <- c('CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12','CCTE_Shafer_MEA_dev_mutual_information_norm','CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12')
summary.by.acnm[acnm %in% acnms.dec.in.variability, .N, by = .(acnm, target_cv_over_median, target_sd_over_median)]
# acnm target_cv_over_median target_sd_over_median N
# 1:         CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12                  TRUE                 FALSE 1
# 2:               CCTE_Shafer_MEA_dev_mutual_information_norm                  TRUE                 FALSE 1
# 3: CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12                  TRUE                  TRUE 1

# Problematic, BUT, "good" plate actually seems to have less activity!
# CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean # (maybe also increase in variance)

# just problematic
# CCTE_Shafer_MEA_dev_network_spike_duration_std_DIV12
# CCTE_Shafer_MEA_dev_per_burst_spike_percent (but wow, the effect seems rather slight. see distributino to confirm it isnt' that far above median)
# CCTE_Shafer_MEA_dev_per_burst_interspike_interval

# histogram for the last problematic endpoint:
acnmi <- 'CCTE_Shafer_MEA_dev_per_burst_spike_percent'
p1 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = sd_of_plate_med)) +
  geom_histogram()+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, sd_of_plate_med],
             lwd = 1.5,
             col = 'cornflowerblue')+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(sd_of_plate_med, na.rm = T)],
             lwd = 1.5,
             col = 'darkred')+
  ggtitle(paste0(acnmi,' \nDistribution of standard deviations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))

p2 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = cv_of_plate_med)) +
  geom_histogram()+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, cv_of_plate_med],
             lwd = 1.5,
             col = 'cornflowerblue')+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(cv_of_plate_med, na.rm = T)],
             lwd = 1.5,
             col = 'darkred')+
  ggtitle(paste0(acnmi,' \nDistribution of coefficient of variations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))
p <- ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1)
plot(p)

# And another one that I missed
# histogram for the last problematic endpoint:
acnmi <- 'CCTE_Shafer_MEA_dev_per_burst_interspike_interval'
p1 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = sd_of_plate_med)) +
  geom_histogram()+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, sd_of_plate_med],
             lwd = 1.5,
             col = 'cornflowerblue')+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(sd_of_plate_med, na.rm = T)],
             lwd = 1.5,
             col = 'darkred')+
  ggtitle(paste0(acnmi,' \nDistribution of standard deviations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))

p2 <- ggplot(dat.by.culture[acnm == acnmi], aes(x = cv_of_plate_med)) +
  geom_histogram()+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi & any_CO2_off_DIV12 == TRUE, cv_of_plate_med],
             lwd = 1.5,
             col = 'cornflowerblue')+
  geom_vline(xintercept = dat.by.culture[acnm == acnmi, median(cv_of_plate_med, na.rm = T)],
             lwd = 1.5,
             col = 'darkred')+
  ggtitle(paste0(acnmi,' \nDistribution of coefficient of variations among plate medians in a culture for all MEA DEV to date\nblue line = culture where CO2 not on during DIV12 recording for 2/3 plates\nred line = median for all cultures'))
p <- ggdraw() +
  draw_plot(p1, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p2, x = 0.5, y = 0, width = 0.5, height = 1)
plot(p)


# Somethign I could do that seems more robust:
# - wilcoxon rank sum test to compare the distributions of the values from each plate (pairwise I guess?)
# - See where p-values are signficant
# - THEN, look at the magnitude of differences in the distributions - compare that to the rest of the cultures.
