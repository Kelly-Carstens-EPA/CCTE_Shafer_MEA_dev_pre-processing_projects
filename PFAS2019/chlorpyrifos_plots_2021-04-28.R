# preliminary CO plots
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-04-28.RData')

dat <- mea_nfa_lvl0[grepl('Chlorpyrifos',treatment)]
dat[, .N, by = .(spid, dataset, treatment)]
#            spid   dataset         treatment    N
# 1:     EX000378 Frank2017 Chlorpyrifos oxon 1827
# 2:     EX000384 Frank2017      Chlorpyrifos 1827
# 3: TT0000177E02   OPP2015      Chlorpyrifos 1827
# 4: TT0000177G02   OPP2015 Chlorpyrifos oxon 1827
# 5:     EX000596  PFAS2019      Chlorpyrifos 1827
# 6:     EX000595  PFAS2019 Chlorpyrifos oxon 5481
dat[, date := sub('_.*$','',apid)]
dat <- dat[order(treatment, date)]

pdf(file = 'PFAS2019/Chlorpyrifos_rvals_mean_firing_rate_2021-04-28.pdf', width = 8, height = 10)
par(mfcol = c(3,2))
ylim <- range(dat[grepl('firing_rate_mean$',acnm), rval])
for (spidi in unique(dat$spid)) {
  plotdat <- dat[spid == spidi]
  plot(rval ~ log10(conc), plotdat[grepl('firing_rate_mean$',acnm) & wllq == 1], type = 'p', xaxt = 'n', xlab = 'conc',
       main = paste0('Mean Firing Rate AUC Rval\n',unique(plotdat$treatment), ' (',unique(plotdat$spid),')'),
       ylim = ylim)
  axis(side = 1, at = log10(sort(unique(plotdat$conc))), labels = sort(unique(plotdat$conc)))
}

par(mfcol = c(3,2))
ylim <- range(dat[grepl('firing_rate_mean_DIV12',acnm), rval])
for (spidi in unique(dat$spid)) {
  plotdat <- dat[spid == spidi]
  plot(rval ~ log10(conc), plotdat[grepl('firing_rate_mean_DIV12',acnm)  & wllq == 1], type = 'p', xaxt = 'n', xlab = 'conc',
       main = paste0('Mean Firing Rate DIV12 Rval\n',unique(plotdat$treatment), ' (',unique(plotdat$spid),')'),
       ylim = ylim)
  axis(side = 1, at = log10(sort(unique(plotdat$conc))), labels = sort(unique(plotdat$conc)))
}
graphics.off()

# I guess I'm kinda concerned about the last sample of Chlorpyrifos oxon... so many replicates!!
dat[spid == 'EX000595' & wllq == 1, .N, by = .(apid)]
# apid   N
# 1: 20210217_MW75-5809 609
# 2: 20210217_MW75-5810 609
# 3: 20210217_MW75-5811 609
# 4: 20210331_MW75-8102 609
# 5: 20210331_MW75-8103 609
# 6: 20210331_MW75-8104 609
# so there are 6 plates, isntead of the usual 3
# Let's just check something out...
dat[, date := sub('_.*$','',apid)]
dat[spid == 'EX000595', spid := paste0(date, '_',spid)]

pdf(file = 'PFAS2019/Chlorpyrifos_oxon_EX000595_split_by_culture_mean_firing_rate_DIV12_2021-04-28.pdf', width = 8, height = 10)
par(mfcol = c(2,1))
ylim <- range(dat[grepl('firing_rate_mean_DIV12',acnm), rval])
for (spidi in unique(dat[grepl('EX000595',spid) & wllq == 1,spid])) {
  plotdat <- dat[spid == spidi]
  plot(rval ~ log10(conc), plotdat[grepl('firing_rate_mean_DIV12',acnm)  & wllq == 1], type = 'p', xaxt = 'n', xlab = 'conc',
       main = paste0('Mean Firing Rate DIV12 Rval\n',unique(plotdat$treatment), ' (',unique(plotdat$spid),')'),
       ylim = ylim)
  axis(side = 1, at = log10(sort(unique(plotdat$conc))), labels = sort(unique(plotdat$conc)))
}
graphics.off()
