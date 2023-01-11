# ------------------------------------------------------------------------ #
# Get an initial sense of the impact of the hot water shut off on the data
# To determine if it warrants further investigation
# July 27, 2022
# ------------------------------------------------------------------------- #

library(data.table)
library(ggplot2)

## # Load data ----
load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-07-27

# Note that this version saved 7/27/2022 5:15 PM
# wllq for controls from the following plates was still 1 
# [1] "20151125_MW1088-3"  "20150805_MW1038-36" "20210127_MW75-5620" "20160921_MW1160-23" "20170222_MW1146-3"  "20170222_MW1146-4"  "20170222_MW1146-5"  "20211110_MW78-6208"
# [9] "20211110_MW78-6209" "20211110_MW78-6210" "20211110_MW78-6211" "20211110_MW78-6212" "20211110_MW78-6213"
# (specifically, where culture_date == '20211110'. wllq for this culture now updated to 0 for this culture)


# Load all lvl0 data to date
load('lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData')

# merge
setdiff(names(mea_nfa_lvl0), names(dat))
dat[, acnm := acsn]
dat[, dataset := 'DNT_NTP2021']
dat[, spid := DNTP_blind_code]
all.dat <- rbind(mea_nfa_lvl0, dat, fill = T)
rm(dat, mea_nfa_lvl0)
all.dat[, culture_date := as.numeric(sub('_.*','',apid))]

# Note where hot water was shut off
all.dat[grepl('hot water shut off',tolower(wllq_notes)), .N, by = .(wllq_notes)]
all.dat[, hot_water_status := ifelse(grepl('hot water shut off',tolower(wllq_notes)),
                                     'shut off',
                                     'normal')]
all.dat$hot_water_status <- as.factor(all.dat$hot_water_status)

## # Analyze ----

# Plot all controls over time for mean firing rate (bc that's just a general indicator of activity)
plot.acnm <- 'CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12'
plotdat <- all.dat[acnm == plot.acnm & wllt == 'n' & wllq == 1]
plotdat[, summary(culture_date)]

rval_mean <- mean(plotdat$rval)
rval_sd <- sd(plotdat$rval)

ggplot(plotdat, aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = hot_water_status), width = 0.15, height = 0)+
  geom_hline(yintercept = rval_mean)+
  geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
  geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
  scale_color_manual(values = c('normal' = 'black',
                                'shut off' = 'blue'))+
  ggtitle(paste0('All data from MEA NFA solvent controls versus culture date\n',plot.acnm,
                 '\nsolid line = mean, dashed lines = +/-2SD'))+
    theme(legend.position = 'top')

# By apid
ggplot(plotdat, aes(x = apid, y = rval)) +
  geom_jitter(aes(color = hot_water_status), width = 0.15, height = 0)+
  geom_hline(yintercept = rval_mean)+
  geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
  geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
  scale_color_manual(values = c('normal' = 'black',
                                'shut off' = 'blue'))+
  ggtitle(paste0('All data from MEA NFA solvent controls versus apid\n',plot.acnm,
                 '\nsolid line = mean, dashed lines = +/-2SD'))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'top')


# Let's check out a few more endpoints, but should be fine.

# I'm just going to look at endpoints that roughly correlate to the overall activity level
# rather than the more complicated/nuanced endpoints - bc those can vary a lot, and I'm 
# mostly just testing the hypothesis that the hot water shutoff migh timpact the activiyt level of the enpdoints
plot.acnms <- paste0('CCTE_Shafer_MEA_dev_',
                     c('firing_rate_mean',
                       'firing_rate_mean_DIV12',
                       'active_electrodes_number',
                       'active_electrodes_number_DIV12',
                       'burst_rate',
                       'burst_rate_DIV12',
                       'network_spike_number',
                       'network_spike_number_DIV12',
                       'mutual_information_norm',
                       'mutual_information_norm_DIV12'))

# By culture
pdf(file = 'investigations/Hot Water Shutoff Impact/all_mea_nfa_solvent_controls_versus_culture_date_endpoints_related_to_activity_level_2022-07-27.pdf',
    width = 10, height = 8)

for (plot.acnm in plot.acnms) {
  plotdat <- all.dat[acnm == plot.acnm & wllt == 'n' & wllq == 1]
  plotdat[, summary(culture_date)]
  
  rval_mean <- mean(plotdat$rval)
  rval_sd <- sd(plotdat$rval)
  
  p <- ggplot(plotdat, aes(x = culture_date, y = rval)) +
    geom_jitter(aes(color = hot_water_status), pch = 1, width = 0.15, height = 0)+
    geom_hline(yintercept = rval_mean)+
    geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
    geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
    scale_color_manual(values = c('normal' = 'black',
                                  'shut off' = 'blue'))+
    ggtitle(paste0('All data from MEA NFA solvent controls versus culture date\n',plot.acnm,
                 '\nsolid line = mean, dashed lines = +/-2SD'))+
    theme(legend.position = 'top')
  print(p)
}
graphics.off()


# By apid
pdf(file = 'investigations/Hot Water Shutoff Impact/all_mea_nfa_solvent_controls_versus_apid_endpoints_related_to_activity_level_2022-07-27.pdf',
    width = 14, height = 8)

for (plot.acnm in plot.acnms) {
  plotdat <- all.dat[acnm == plot.acnm & wllt == 'n' & wllq == 1]
  plotdat[, summary(culture_date)]
  
  rval_mean <- mean(plotdat$rval)
  rval_sd <- sd(plotdat$rval)
  
  p <- ggplot(plotdat, aes(x = apid, y = rval)) +
    geom_jitter(aes(color = hot_water_status), pch = 1, width = 0.15, height = 0)+
    geom_hline(yintercept = rval_mean)+
    geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
    geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
    scale_color_manual(values = c('normal' = 'black',
                                  'shut off' = 'blue'))+
    ggtitle(paste0('All data from MEA NFA solvent controls versus apid\n',plot.acnm,
                   '\nsolid line = mean, dashed lines = +/-2SD'))+
    theme(axis.text.x = element_text(angle = 90, size = 3, vjust = 0.5),
          legend.position = 'top')
  print(p)

}
graphics.off()



# Observations --------------------------------------------------------------

# Things that look remotely concerning:
# - maybe nAE by culture... a fe more points than typical are below the -2SD threshold
# - mutual information norm - subset of points look a bit elevated... but there are older cultures that do that too

# by apid
# - active electrodes, ya

# Wait a sec... the nubmer of network spikes detected has fallen drastically over teh years... why is that??
# Did I do somethign to the scripts?


# What is the number of network spikes lookign liek these days?
all.dat[culture_date > 20180000 & wllt == 'n' & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & wllq == 1, summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   34.50   63.00   80.84   99.88  713.00 
# median of 63... that's not too bad

all.dat[culture_date < 20180000 & wllt == 'n' & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & wllq == 1, summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   249.9   381.0   484.6   638.8  1977.0 
# but the median used to be much higher!

# fascinating... but not the issue at hand

# But by apid - it looks like there are some plates where the # of network spikes median is close to 0, or might even be 0?
all.dat[, bval := median(rval[wllt == 'n' & wllq == 1], na.rm = T), by = .(apid, acnm)]
all.dat[culture_date > 20210000 & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number', summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    2.25   38.25   66.25   75.19   92.75  271.00      48 

# Wait a sec... where is the bval NA?
check.apid <- all.dat[culture_date > 20210000 & is.na(bval), .N, by = .(apid)][, apid]
View(all.dat[culture_date > 20210000 & is.na(bval) & wllt == 'n', .(apid, spid, wllt, wllq, rval)][order(apid)])
all.dat[, max_wllq_wlltn_apid_is0 := as.numeric(max(wllq[wllt == 'n']) == 0), by = .(apid, acnm)]
all.dat[, all_apid_rvalNA := as.numeric(sum(!is.na(rval[wllt == 'n'])) == 0), by = .(apid, acnm)]
all.dat[is.na(bval), .N, by = .(all_apid_rvalNA, max_wllq_wlltn_apid_is0)]
#    all_apid_rvalNA max_wllq_wlltn_apid_is0     N
# 1:               1                       0 90841
# 2:               0                       1 15792
# 3:               1                       1  1152
# ah, okay, these cases explain everything!
# probably occurs most often for obscure endpoints regardless.

# Back to network spike number
all.dat[culture_date > 20210000 & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number', summary(bval)]
all.dat[culture_date > 20210000 & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & bval < 5 & wllt == 'n', .(apid, treatment, wllq, wllq_notes)]
# apid treatment wllq                         wllq_notes
# 1: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# 2: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# 3: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# 4: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# 5: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# 6: 20211110_MW78-6210      DMSO    1 Hot water shut off in building; NA
# Just curious... are we using any of hte treatments from this plate?
all.dat[apid == '20211110_MW78-6210', .N, by = .(treatment, wllq)]
# treatment wllq   N
# 1:   7126 C1    0 609
# 2:   7126 C2    0 609
# 3:   7126 C3    0 609
# 4:   7126 C4    0 609
# 5:   7126 C5    0 609
# 6:   7126 C6    0 609
# 7:      DMSO    1 522
# NOpe! Looks like all samples from this culture were rescreened for various reasons...
View(all.dat[apid == '20211110_MW78-6210', .N, by = .(treatment, wllq, wllq_notes)])
# all due to producing noisy results
# So should i set wllq to 0 for the DMSO as well? (i.e., not include it in the calculation of bmad?)
# I think so. If everything else from the plate was thrown out, then yes

# Anything else on the low end?
all.dat[culture_date > 20210000 & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & bval < 10 & wllt == 'n', .(apid, treatment, wllq, wllq_notes)]
# Which all plates should be removed?
all.dat[, any_wllt_wllq1 := max(wllq[wllt == 't']), by = .(apid, acnm)]
exclude.apid <- all.dat[any_wllt_wllq1 == 0, unique(apid)]
all.dat[!apid %in% exclude.apid & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number', summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.25   65.00  119.00  273.82  357.00 1741.00 
all.dat[!apid %in% exclude.apid  & culture_date > 20210000 & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & bval < 10 & wllt == 'n', .(apid, treatment, bval, wllq, wllq_notes)]
# 5.25 is also quite low...

all.dat[apid == '20211110_MW78-6211', .N, by = .(treatment, wllq_notes)]

# all but 1 sample from this culture will also be repeated...
# the 1 other sample is being rescreened at a lower concentration
# I think it was so potent that it wasn't noisy, bc everything was dead!
# hmm...

all.dat[!apid %in% exclude.apid  & culture_date > 20210000 & !grepl('20211110',culture_date) & acnm == 'CCTE_Shafer_MEA_dev_network_spike_number' & bval < 10 & wllt == 'n', .(apid, treatment, bval, wllq, wllq_notes)]
# empty...

# So perhaps an argumetn could be made to remove the data from all plates from 20211110 as well....
# bc it's definitely on the lowe end

plot.acnm <- 'CCTE_Shafer_MEA_dev_network_spike_number'
plotdat <- all.dat[acnm == plot.acnm & wllt == 'n' & wllq == 1& culture_date > 20210000]
plotdat[, summary(culture_date)]

rval_mean <- mean(plotdat$rval)
rval_sd <- sd(plotdat$rval)

p <- ggplot(plotdat, aes(x = apid, y = rval)) +
  geom_jitter(aes(color = hot_water_status), pch = 1, width = 0.15, height = 0)+
  geom_point(aes(y = bval), pch = 19)+
  geom_hline(yintercept = rval_mean)+
  geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
  geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
  scale_color_manual(values = c('normal' = 'black',
                                'shut off' = 'blue'))+
  ggtitle(paste0('All data from MEA NFA solvent controls versus apid\n',plot.acnm,
                 '\nsolid line = mean, dashed lines = +/-2SD'))+
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.5),
        legend.position = 'top')
print(p)

#There are definitely some low values here...
# I was thinking that perhaps the really low values are coming from a colder time of year,
# but the culture on 20211124 is doing a lot better

# introduction of space heaters...?
# but also, the values typically follow a cyclic trend

# What is my current cutoff for inclusion/exclusion? (2 median AE, MFR > 10)

# NEXT STEP:
# re-do all of this with the data from the exclude.apids and 20211110 removed. Are there any apid of conern left?
# Ultimately, will just submit any concerns to Tim


# V2 - removing select apid -----------------------------------------------

# How many apid affected by hot water shutoff?
all.dat[hot_water_status == 'shut off', .N, by = .(apid)][order(apid)]
# 27 total plates
# of course some of them during a colder time of year than others


# Identify apid on which all samples were repeated
all.dat[, num_cultures := length(unique(culture_date)), by = .(spid)]
all.dat[dataset == 'DNT_NTP2021', .(min(num_cultures)), by = .(apid)][V1 > 1]
# wait... I just want the first culture in these cases

# Identify apid on which all treated wells have wllq 0
all.dat[, max_wllq_wllt := max(wllq[wllt == 't']), by = .(apid)]
all.dat[max_wllq_wllt == 0, .N, by = .(apid, dataset)]
#                  apid     dataset    N
# 1:  20151125_MW1088-3   Frank2017 4176
# 2: 20150805_MW1038-36   Frank2017 4176
# 3: 20210127_MW75-5620    PFAS2019 4176
# 4: 20160921_MW1160-23 ToxCast2016 4176
# 5: 20211110_MW78-6208 DNT_NTP2021 4176
# 6: 20211110_MW78-6209 DNT_NTP2021 4176
# 7: 20211110_MW78-6210 DNT_NTP2021 4176
# these plates would only affect the overall bmad, but wouldn't be used for anything else
exclude.apids <- all.dat[max_wllq_wllt == 0, .N, by = .(apid, dataset)][, apid]

# Identify the other set for apid from DNT NTP2021 on which all samples were noisy, but one sample was rescreened at a lower conc
all.dat[dataset == 'DNT_NTP2021' & grepl('noisy',wllq_notes), .(length(unique(spid))), by = .(apid, wllq)]
#                  apid wllq V1
# 1: 20211110_MW78-6208    0  6
# 2: 20211110_MW78-6209    0  6
# 3: 20211110_MW78-6210    0  6
# 4: 20211110_MW78-6211    0  5
# 5: 20211110_MW78-6212    0  5
# 6: 20211110_MW78-6213    0  5
# the first 3 are already in exclude.apids
all.dat[, num_usable_trts := length(unique(spid[wllq == 1 & wllt == 't'])), by = .(apid)] # any usable data from any acnm
all.dat[num_usable_trts <= 1, .N, by = .(apid, num_usable_trts, dataset)][order(apid)]
#                  apid num_usable_trts     dataset    N
# 1: 20150805_MW1038-36               0   Frank2017 4176
# 2:  20151125_MW1088-3               0   Frank2017 4176
# 3: 20160921_MW1160-23               0 ToxCast2016 4176
# 4:  20170222_MW1146-3               1 ToxCast2016 4176
# 5:  20170222_MW1146-4               1 ToxCast2016 4176
# 6:  20170222_MW1146-5               1 ToxCast2016 4176
# 7: 20210127_MW75-5620               0    PFAS2019 4176
# 8: 20211110_MW78-6208               0 DNT_NTP2021 4176
# 9: 20211110_MW78-6209               0 DNT_NTP2021 4176
# 10: 20211110_MW78-6210               0 DNT_NTP2021 4176
# 11: 20211110_MW78-6211               1 DNT_NTP2021 4176
# 12: 20211110_MW78-6212               1 DNT_NTP2021 4176
# 13: 20211110_MW78-6213               1 DNT_NTP2021 4176
# cool, well use these as exclude.apid

exclude.apids <- all.dat[num_usable_trts <= 1, unique(apid)]

all.dat2 <- all.dat[!apid %in% exclude.apids]

# Plots v2 ----------------------------------------------------------------

plot.acnms <- paste0('CCTE_Shafer_MEA_dev_',
                     c('firing_rate_mean',
                       'firing_rate_mean_DIV12',
                       'active_electrodes_number',
                       'active_electrodes_number_DIV12',
                       'burst_rate',
                       'burst_rate_DIV12',
                       'network_spike_number',
                       'network_spike_number_DIV12',
                       'spike_duration_mean',
                       'spike_duration_mean_DIV12',
                       'mutual_information_norm',
                       'mutual_information_norm_DIV12'))

# By culture
pdf(file = 'investigations/Hot Water Shutoff Impact/all_mea_nfa_solvent_controls_versus_culture_date_endpoints_related_to_activity_level_v2_2022-07-27.pdf',
    width = 10, height = 8)

for (plot.acnm in plot.acnms) {
  plotdat <- all.dat2[acnm == plot.acnm & wllt == 'n' & wllq == 1]
  plotdat[, summary(culture_date)]
  
  rval_mean <- mean(plotdat$rval)
  rval_sd <- sd(plotdat$rval)
  
  p <- ggplot(plotdat, aes(x = culture_date, y = rval)) +
    geom_jitter(aes(color = hot_water_status), pch = 1, width = 0.15, height = 0)+
    geom_hline(yintercept = rval_mean)+
    geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
    geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
    scale_color_manual(values = c('normal' = 'black',
                                  'shut off' = 'blue'))+
    ggtitle(paste0('All data from MEA NFA solvent controls versus culture date\n',plot.acnm,
                   '\nsolid line = mean, dashed lines = +/-2SD'))+
    theme(legend.position = 'top')
  print(p)
}
graphics.off()


# By apid
pdf(file = 'investigations/Hot Water Shutoff Impact/all_mea_nfa_solvent_controls_versus_apid_endpoints_related_to_activity_level_v2_2022-07-27.pdf',
    width = 14, height = 8)

for (plot.acnm in plot.acnms) {
  plotdat <- all.dat2[acnm == plot.acnm & wllt == 'n' & wllq == 1]
  plotdat[, summary(culture_date)]
  
  rval_mean <- mean(plotdat$rval)
  rval_sd <- sd(plotdat$rval)
  
  p <- ggplot(plotdat, aes(x = apid, y = rval)) +
    geom_jitter(aes(color = hot_water_status), pch = 1, width = 0.15, height = 0)+
    geom_hline(yintercept = rval_mean)+
    geom_hline(yintercept = rval_mean - 2* rval_sd, lty = 'dashed')+
    geom_hline(yintercept = rval_mean + 2* rval_sd, lty = 'dashed')+
    scale_color_manual(values = c('normal' = 'black',
                                  'shut off' = 'blue'))+
    ggtitle(paste0('All data from MEA NFA solvent controls versus apid\n',plot.acnm,
                   '\nsolid line = mean, dashed lines = +/-2SD'))+
    theme(axis.text.x = element_text(angle = 90, size = 3, vjust = 0.5),
          legend.position = 'top')
  print(p)
  
}
graphics.off()




# Thoughts on V2 ----------------------------------------------------------

# Remaining concerns:
# - network spike number still looks a lot lower than I'd like it to be in several cases (but this doesn't just affect where the hot water was shut off)
# - What's going on with the mutual information?? Why is the one set of plates SO high? (compared to cultures from 2014-2015, it's not that weird,
# but compared to more recent cultures, it's very weird)

# The nAE's no longer looks concerning with exclude.apids removed!

# Regarding the set of plates that have a really high mutual information:
# - It appears that these plates also have a longer network spike duration (on average), 
# and slightly higher than average # of network spikes (at least compared to local culture).
# So at the very least, it intuitively makes sense to me that these endpoints would be elevated together,
# so I don't think the scripts did anythign wrong. I think the neurons really were just more connected in these cultures!
# And I really don't think this increase in ... connectivity? is due to the hot water shutff,
# because we don't see an increase in connectivity in any of the other cultures during the hot water shutoff,
# and the cultures were we do see this affect are primary in september in october (vs novemeber and december), when we would expect colder temperatures to have more of an effect.

# So, the network spike nubmer
all.dat2[, bval := median(rval[wllq == 1  & wllt == 'n']), by = .(apid, acnm)]
all.dat2[grepl('network_spike_number$',acnm), summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.5    65.0   119.0   276.5   359.8  1741.0
all.dat2[dataset == 'DNT_NTP2021' & grepl('network_spike_number$',acnm), summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.75   38.25   74.25   83.73  101.00  271.00 
# the min is 15? That not too bad
all.dat2[grepl('network_spike_number_DIV12',acnm), summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   25.00   52.00   84.22  124.00  392.00 
all.dat2[dataset == 'DNT_NTP2021' & grepl('network_spike_number_DIV12',acnm), summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00   17.00   28.50   31.55   41.50   79.50 
# not liking what I'm seeing for DIV12...
# Are these low network spike plates coming from when the hot water was shut off?

all.dat2[dataset == 'DNT_NTP2021' & grepl('network_spike_number_DIV12',acnm), .N, by = .(apid, bval, hot_water_status)][order(bval)][1:20]
# apid bval hot_water_status  N
# 1: 20220601_MW78-7215  2.0           normal 27
# 2: 20220601_MW78-7216  6.0           normal 27
# 3: 20220601_MW78-7214  6.5           normal 27
# 4: 20220601_MW78-7218 11.5           normal 48
# 5: 20220601_MW78-7219 12.5           normal 48
# 6: 20220223_MW78-7107 13.0           normal 48
# 7: 20220413_MW78-7203 13.0           normal 48
# 8: 20220330_MW78-7118 14.5           normal 48
# 9: 20211208_MW78-6305 14.5         shut off 48
# 10: 20220223_MW78-7106 14.5           normal 48
# 11: 20211208_MW78-6303 15.5         shut off 48
# 12: 20211208_MW78-6307 16.0         shut off 48
# 13: 20220413_MW78-7206 16.0           normal 48
# 14: 20220413_MW78-7202 16.5           normal 48
# 15: 20220330_MW78-7119 17.0           normal 48
# 16: 20220601_MW78-7217 17.0           normal 48
# 17: 20220615_MW78-7304 17.0           normal 48
# 18: 20220615_MW78-7305 17.0           normal 48
# 19: 20220223_MW78-7110 19.5           normal 48
# 20: 20220413_MW78-7207 20.0           normal 48
# nope, most of the really low network spike bval plates occured when the hot water was working.

# Looking at the graphs, there is a clear trend in a decrease in the network spike number from 2014 - now
# While there is a less pronounced but still noticeable increase in the network spike duration (mean).
# It is what it is, but... calculating the bmad's based on the entire dataset seems less appropriate, 
# when certain epochs seem to be behavign much differently than others. seems... more tricky.

# So, what should I do here? I want to provide, convey information in a way that is goign to lead to better understanding,
# of the assay, of how to interpret results / our confidence level in it.
# I'm concerned that if DNT NTP folks do an assessment of the assay based on their data alone,
# they might draw conclusions that wouldn't apply if you used cutoffs based on the entire data set.
# Well, yes, of course.
# And as I've pondered here, I'm not even sure that basing the bmad's on the entire data set is the best.

# I think that the scripts really did detect a low number of network spikes for 20220601_MW78-7215.
# That's not good, but... likely Laura's team will just conclude that the network spike number just isn't
# as reliable/informative of an endpoint. And they might be right! Thankfully there are many endpoints.
# I guess my concern is if, as someone who has access to years of data from this assay,
# should I use my expertise to say -> the results from that plate don't look right/normal, so it should be thrown out.

# Note that a lot of the 2018-2019 projects, including PFAS, have pretty pathetic network spike values as well.
# And I let that data pass, bc I didn't look at it this way.

all.dat2[grepl('network_spike_number_DIV12',acnm), .N, by = .(apid, bval, dataset, hot_water_status)][order(bval)][1:20]
#                   apid bval     dataset hot_water_status  N
# 1: 20220601_MW78-7215  2.0 DNT_NTP2021           normal 27
# 2: 20190807_MW69-3805  4.5   DNTGF2019           normal 48
# 3: 20180912_MW1207-38  5.0    PFAS2018           normal 48
# 4: 20190807_MW69-3803  5.5   DNTGF2019           normal 48
# 5: 20220601_MW78-7216  6.0 DNT_NTP2021           normal 27
# 6: 20220601_MW78-7214  6.5 DNT_NTP2021           normal 27
# 7: 20190807_MW69-3804  7.0   DNTGF2019           normal 48
# 8: 20190807_MW69-3806  7.0   DNTGF2019           normal 48
# 9: 20190807_MW69-3802  7.5   DNTGF2019           normal 48
# 10: 20190807_MW69-3715 10.0   DNTGF2019           normal 48
# 11: 20210303_MW75-5816 10.0    PFAS2019           normal 48
# 12: 20180912_MW1207-39 10.5    PFAS2018           normal 48
# 13: 20180912_MW1207-40 11.0    PFAS2018           normal 48
# 14: 20180912_MW1207-37 11.5    PFAS2018           normal 48
# 15: 20220601_MW78-7218 11.5 DNT_NTP2021           normal 48
# 16: 20191016_MW70-2415 12.5   DNTGF2019           normal 48
# 17: 20220601_MW78-7219 12.5 DNT_NTP2021           normal 48
# 18: 20220223_MW78-7107 13.0 DNT_NTP2021           normal 48
# 19: 20220413_MW78-7203 13.0 DNT_NTP2021           normal 48
# 20: 20191016_MW70-2417 13.5   DNTGF2019           normal 48

# I think developing assay quality control threshold is a big question
# not something I can answer on this time frame,
# and if we did develop those thresholds, knowing the extent to which they should be applied is tricky.
# I guess the idea would be - we catch these "Bad" plates now, then rescreen.
# But I know that Tim would not want to repeat anything at this point!

# Were any of the chemicals are the plate with really low network spikes rescreened already?
all.dat2[apid == '20220601_MW78-7215', .(length(unique(spid))), by = .(repeated_samples, num_cultures)]
# oh wait, this was a very newly ran plate
# so it was a culture of repeats!
check.trts <- all.dat2[apid == '20220601_MW78-7215' & wllt == 't', unique(spid)]
all.dat2[spid %in% check.trts, .N, by = .(culture_date, spid, wllq)][order(spid)]
# culture_date spid wllq    N
# 1:     20210915 1852    0 1827
# 2:     20220601 1852    1 1827
# 3:     20211124 3313    0    7
# 4:     20211124 3313    1 1820
# 5:     20220601 3313    1 1827
# 6:     20211027 4004    1 1827
# 7:     20220601 4004    1 1827

# Okay, there is no specific reason I know off why this plate would be off.
# I really think it's going to be okay.
# I'm going to let this plate slide.

# ahhhhhh... what specifically am I worried will happen?
# - because the measurements related to connectivity are already so low, it will be difficult to detect any treatments that decrease the connectivity!
# So the assay might appear less sensitive than it... could be.
# But maybe some plates having low # of network spikes is just a component of the assay, which will affect the sensitivity?
# Ya, that makes sense to me!
# Let's go with that.

# So no changes needed now, other than set wllq == 0 for the controls from exclude.apids in DNT ntp 2021 project



# Confirm if want to remove data from apid with 1 non-noisy sample --------
all.dat[apid=='20211110_MW78-6213' & wllq == 1, unique(treatment)]
# [1] "7126 C11" "DMSO"

# Note that this sample was very potent, so it was rescreened at a lowre conc
all.dat[treatment == '7126 C11']

# My hypothesis is that the only reason this sample did not appear as "noisy" like the rest of the samples on this plate,
# is because it was so cytotoxic at these concentrations!!

# so, how would the dose- respone looke with only the rescreen for this sample?
all.dat[, bval := median(rval[wllt == 'n' & wllq == 1], na.rm = T), by = .(apid, acnm)]
all.dat[, logc := log10(conc)]
plotdat <- all.dat[treatment == '7126 C11' & !grepl('20211110',apid) & grepl('firing_rate_mean$',acnm)]
plotdat[, resp := (rval -bval)/(0-bval)*100]

ggplot(plotdat, aes(x = conc, y = resp))+
  geom_point(pch = 1)+
  scale_x_log10()

# Still appears totally efficacious at the top conc!
# What if did include all cultures?
plotdat <- all.dat[treatment == '7126 C11' & grepl('firing_rate_mean$',acnm)]
plotdat[, resp := (rval -bval)/(0-bval)*100]
ggplot(plotdat, aes(x = conc, y = resp))+
  geom_point(aes(color = as.factor(culture_date)), pch = 1)+
  scale_x_log10()

# I mean, I think it would give move confidence re the strength of this hit.

# Okay, how much would the bmad's be affected if we did vs did not include this plate?
exclude.apids <- all.dat[num_usable_trts == 0, unique(apid)] # only exclude apids with 0 usable trts
exclude.apids2 <- all.dat[num_usable_trts <= 1, unique(apid)] # only exclude apids with 0 usable trts

all.dat[, resp := (rval -bval)/(0-bval)*100]

testdat <- all.dat[!apid %in% exclude.apids]

bmad.tb <- all.dat[dataset == 'DNT_NTP2021', .(bmad_cur = mad(resp[wllt == 'n'], na.rm = T),
                                               bmad_alt1 = mad(resp[wllt == 'n' & !apid %in% exclude.apids], na.rm = T),
                                               bmad_alt2 = mad(resp[wllt == 'n' & !apid %in% exclude.apids2], na.rm = T)),
                   by = .(acnm)]

bmad.tb[, perc_diff_alt1 := (bmad_alt1 - bmad_cur) / ((bmad_cur+bmad_alt1)*0.5)*100]

bmad.tb[!grepl('DIV[579]',acnm)][order(perc_diff_alt1)]
# differences are with -7.5% to + 7%
# hhhhhhhhhhhhhhhhhh, does that matter?
# it is very small.
# I just want to do it right.


# Still, I'm having trouble putting 7.5% difference in bmad into context

# From another angle: Are all of the treatments that were found to be noisy occuring on these 2 sets of plates?
all.dat[dataset == 'DNT_NTP2021' & grepl('noisy',tolower(wllq_notes)), .(length(unique(treatment))), by = .(apid)]
# apid V1
# 1: 20211110_MW78-6208  6
# 2: 20211110_MW78-6209  6
# 3: 20211110_MW78-6210  6
# 4: 20211110_MW78-6211  5
# 5: 20211110_MW78-6212  5
# 6: 20211110_MW78-6213  5
all.dat[dataset == 'DNT_NTP2021' & num_usable_trts <= 1, .N, by = .(apid)]
# apid    N
# 1: 20211110_MW78-6208 4176
# 2: 20211110_MW78-6209 4176
# 3: 20211110_MW78-6210 4176
# 4: 20211110_MW78-6211 4176
# 5: 20211110_MW78-6212 4176
# 6: 20211110_MW78-6213 4176
# yep, all same plates!

# I think it totally makes sense to throw this culture out.
# I don't know if it's due to the hot water, 
# but it's difficult to maek that argument, because other cultures performed while hot water was off
# and in cold months did not appear this noisy or have low nAE

# So yep, let's remove all data from 2021110 (i.e., set wllq == 0)

# Okay, I'm doubting again
# The substance that currently has wllq == 1 in 20211110 is not goign to have a cyto hit if only use new culture
dat <- all.dat[dataset == 'DNT_NTP2021']

dat[, bval := median(rval[wllq == 1 & wllt == 'n']), by = .(apid, acsn)]
plotdat <- dat[treatment == '7126 C11' & grepl("LDH",acsn) & !grepl('20211110',apid)]
plotdat[, resp := (rval -bval)/(0-bval)*100]
ggplot(plotdat, aes(x = conc, y = resp))+
  geom_point(pch = 1)+
  scale_x_log10()
# likely would not be a hit

plotdat <- dat[treatment == '7126 C11' & grepl("AB",acsn) & !grepl('20211110',apid)]
plotdat[, resp := (rval -bval)/(0-bval)*100]
ggplot(plotdat, aes(x = conc, y = resp))+
  geom_point(pch = 1)+
  scale_x_log10()
# this is all over the place!

# Would def not be a hit

# So I think the "local" choice, what woudl be best for understanding this individual substance,
# would be to include the 2021110 culture
# But would that produce effects that are not desirable in the dataset as a whole, on the bmad?

# Would we get other hits with this substance?
plotdat <- dat[treatment == '7126 C11' & grepl("mutual_information_norm$",acsn) & !grepl('20211110',apid)]
plotdat[, resp := (rval -bval)/(0-bval)*100]
ggplot(plotdat, aes(x = conc, y = resp))+
  geom_point(pch = 1)+
  scale_x_log10()
# yes!!

plotdat <- dat[treatment == '7126 C11' & !grepl('20211110',apid)]
plotdat[, resp := (rval -bval)/(0-bval)*100]
plotdat[, max_conc_usable := max(conc[!is.na(rval) & wllq == 1]), by = .(treatment, acsn)]
plotdat[, med_resp := median(resp[wllq == 1], na.rm = T), by = .(treatment, cndx, acsn)]
plotdat[, max_med := max(med_resp, na.rm = T), by = .(treatment, acsn)]
plotdat[!grepl("DIV[579]",acsn), .N, by = .(acsn, max_med, max_conc_usable)][order(max_med)]
# 26/36 endpoints would have 100% efficacy!!
# all of the 10 that don't get to 100% efficacy are AB, LDH, or DIV12 endpoints
# and all of the DIV12 endpoints in the 10 that don't get to 100% efficacy don't have usable values 
# at the top conc tested (0.05) (probably just because NA at that highest conc)
# So, I really think it would be okay to just use the data from the later culture for this treatment
