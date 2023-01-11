# NFA SPS Hit call determination
# for PFAS 2019
# Jan 25, 2021
# wllq set to 0 for 72-8203 LDH

rm(list=ls())
library(data.table)
library(stringi)

# Change working directory to desired output directory
setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_nfa/SPS_endpoint_investigation")
load(file.path(paste0("dat_SPS_PFAS2019_2021-01-06.RData"))) # loading pre-processing data

# Setting wllq to 0 where needed --------------------------------
dat[srcf == '20201209_NFA_PFAS_Group_3_SPS__Calculations.xlsx' & grepl('LDH',acnm) & apid == '20201209_MW72-8203', .(spid, rowi, coli, wllq, rval)]
dat[srcf == '20201209_NFA_PFAS_Group_3_SPS__Calculations.xlsx' & grepl('LDH',acnm) & apid == '20201209_MW72-8203', wllq := 0]

# Review
dat[wllt == 'n', unique(spid)] # DMSO. Media wells labelled with 'b'

# -----------------------------------------------------------------
# Comparing with mc data
# -----------------------------------------------------------------
# library(sinaplot)
# load("../data/ccte_mea_dev_data_12nov2020.RData")
# rm(list = c('hit.rates','hitc.table'))
# comp_controls <- rbind(dat[wllq == 1 & wllt == 'n', .(rval, apid, acnm, source = 'sps')], 
#                        mc0[wllt == 'n', .(rval, apid, acnm, source = 'mc')])
# graphics.off()
# pdf(file = 'comparing_mc_sps_DMSO_rvals_by_acnm.pdf', width = 8.5, height = 11)
# par(mfrow = c(2,1))
# for (acnmi in unique(dat$acnm)) {
#   sinaplot(rval ~ source, comp_controls[acnm == acnmi], main = paste0('Control wells ',sub('CCTE_Shafer_MEA_dev_','',acnmi),ifelse(grepl('DIV',acnm,'',' AUC'))))
#   stripchart(V1 ~ source, comp_controls[acnm == acnmi, .(median(rval, na.rm = T)), by = .(source)], vertical = T, col = 'blue', pch = 19, add = T)
#   legend(x = 'topright', pch = 10, col = 'blue', legend = c('median'), bg = 'transparent')
# }
# graphics.off()
# # - DIV12 mfr looks pretty sad for the SPS data
# # - as wells as burst rate DIV12
# 
# # so, controls are relatively lower in this sps data
# # which means that the bvals will be lower
# # which means that the resp's for teh treated wells will be... huh.
# # still just normalized to controls
# # I'm not sure...
# 
# # quick check: is the highest conc data truly derived from the edge wells?
# str(mc3)
# mc3[, logc_max := max(logc, na.rm = t), by = .(spid)]
# mc3[, logc_max_col := paste0(sort(unique(coli[logc == logc_max])),collapse = ','), by = .(spid)]
# mc3[, .(length(unique(spid))), by = .(logc_max_col)]
# mc3[logc == logc_max & wllt == 't', .(length(unique(spid))), by = .(coli)]

# -----------------------------------------------------------------
# Level 1:
# remove where wllq == 0 
# calculate bval
# calculate resp.pc
# multiply by -1 by dn aeid
# -----------------------------------------------------------------
setkey(dat, NULL)

# duplicate for up/dn aenm
acnms <- sort(unique(dat$acnm))
acnm_aenm <- data.table(acnm = c(acnms, acnms), aenm = paste0(acnms, rep(c('_up','_dn'), times = rep(length(acnms),2))))
acnm_aenm <- acnm_aenm[!(aenm %in% c('CCTE_Shafer_MEA_dev_AB_up','CCTE_Shafer_MEA_dev_LDH_up'))]
setkey(dat, acnm)
setkey(acnm_aenm, acnm)
dat <- acnm_aenm[dat, allow.cartesian = TRUE] # borrowing from tcpl

# Any spid/aenm without any usable points?
dat[!grepl('electrodes_number_DIV12_',aenm), .(pts = sum(wllq == 1)), by = .(spid, aenm)][pts == 0]
# 8 DIV12 aenm's are affected 
dat[!grepl('electrodes_number_DIV12_',aenm), .(pts = sum(wllq == 1)), by = .(spid, aenm)][pts == 0, .N, by = .(spid)]
# 7 spid affected
check.spid <- dat[!grepl('electrodes_number_DIV12_',aenm), .(pts = sum(wllq == 1)), by = .(spid, aenm)][pts == 0, unique(spid)]

# Other checks
dat[wllt == 't', length(unique(spid))] # 131
dat[wllt == 't', .N, by = .(aenm)] # 393 points for each endpoint
dat[, .N, by = .(aenm)] # 70 endpoints = 17*2+17*2+2
dat[wllq == 1 & !grepl('DIV12',aenm), .N, by = .(aenm)] # all non-DIV12 endpoints have 480 points, except LDH, as noted
# (DIV12 endpiotns might have some NA values)
dat[wllt == 't', .N, by = .(spid, aenm)][N != 3, .N, by = .(aenm)]
# empty

# remove points where wllq == 0
dat <- dat[wllq == 1]
dat[is.na(rval)] # empty

# Calculated bval
dat[, bval := median(rval[wllt == "n"], na.rm = TRUE), by = .(aenm, apid)]

# calculate resp.pc
dat[, resp := (rval - bval)/(bval)*100]
dat[grepl('dn',aenm), resp := -1*resp]

# checking for na resp
dat[is.na(resp)]
# empty


# -----------------------------------------------------------------
# Level 2:
# bmad of n wells
# med (aka max_med, but doesn't matter bc all single conc)
# -----------------------------------------------------------------
# Calculate bmad
# (there actually is a sc lvl2 method for this method!)
dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE), by = .(aenm)]

# Are any spids tested at multiple conc's?
dat[, .(length(unique(conc))), by = .(spid)][V1 > 1]
# empty. cool, so med == max_med

# get max_med (bascially just med)
dat[, well_id := paste(apid, rowi, coli, sep='_')]
dat[, .(num_reps = length(unique(well_id))), by = .(spid)][num_reps != 3]
# spid num_reps
# 1: Bisphenol       10
# 2:      DMSO       60
# 3:     Media       17
# cool, all treated wells have exactly 3 reps with wllq == 1
# (even though value may be NA for soem aeid)
dat[, well_id := NULL]
dat[, max_med := median(resp), by = .(spid, aenm)]


# -----------------------------------------------------------------
# Decide: Use coff's directly from MC analysis,
# or use new bmad*bmad_multiplier?
# -----------------------------------------------------------------

# See how bmad's compare
dat[, unique(bmad), by = .(aenm)]

# Load endset options, mc bmad values
load("L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/plots_12_NOV_2020/ccte_mea_dev_data_12nov2020.RData")
load('endset_tables_2021-01-25.RData')
aenm_list <- lapply(estl_unique, function(x) x$aenm)
shared_aenm <- intersect(intersect(aenm_list[[1]], aenm_list[[2]]), aenm_list[[3]])
variable_aenm <- setdiff(unique(unlist(aenm_list)), shared_aenm)

# Fix the possible mismatch in aenm
shared_aenm <- stri_replace_all_fixed(shared_aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                      replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')
variable_aenm <- stri_replace_all_fixed(variable_aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                      replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')
mc5$aenm <- stri_replace_all_fixed(mc5$aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                   replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')

# Compare bmad values for select endpoints
comp_bmads <- merge(mc5[, .(bmad_mc = unique(bmad)), by = .(aeid, aenm)], dat[, .(bmad_sps = unique(bmad)), by = .(aenm)], by = 'aenm')
comp_bmads[, aeid_status := 'not using']
comp_bmads[aenm %in% shared_aenm, aeid_status := 'in all endsets']
comp_bmads[aenm %in% variable_aenm, aeid_status := 'in some endsets']
comp_bmads[, .(aenm, bmad_mc, bmad_sps)]
comp_bmads[bmad_sps < bmad_mc]
# aenm aeid  bmad_mc bmad_sps
# 1:                 CCTE_Shafer_MEA_dev_AB_dn 2530 6.754145 6.205525
# 2: CCTE_Shafer_MEA_dev_network_spike_peak_dn 2512 9.058650 7.607834
# 3: CCTE_Shafer_MEA_dev_network_spike_peak_up 2513 9.058650 7.607834
# again, almost all sps bmad's are above mc bmad's, 
# So I will use the coff's from MC analysis directly
# rather than coff := bmad*bmad_multipler derived from mc coff results

# Compare number of points included in each set
dat[wllt == 'n', .N, by = .(aenm)][, unique(N)] # 60 54 59 58
dat[, culture := sub('_.*$','',apid)]
dat[, range(as.numeric(culture))] # 20201118 20201209
dat[, culture := NULL]
mc3[wllt == 'n', .N, by = .(aenm)][, sort(unique(N))] # 1424 1508 1510 1512 1518
mc3[, culture := sub('_.*$','',apid)]
mc3[, range(as.numeric(culture))] # 20140205 20191113

# Save an image
png(filename = paste0('bmad_comparison_sps_mc_',as.character.Date(Sys.Date(),'.png')))
plot(comp_bmads[, .(bmad_mc, bmad_sps)], ylab = 'bmad_SPS_PFAS2019', col = 'gray80', lwd = 2)
points(comp_bmads[aeid_status == 'in all endsets', .(bmad_mc, bmad_sps)], col = 'blue', lwd = 2)
points(comp_bmads[aeid_status == 'in some endsets', .(bmad_mc, bmad_sps)], col = 'mediumseagreen', lwd = 2)
title(main = 'Comparison of BMADs for each AEID\nfrom SPS_PFAS2019 and all MC data to date (01/25/21)')
abline(0,1)
legend(x = 'topleft', legend = c('the 10 key aeid','the 3 variable aeid','unused aeid'), pch = 1, col = c('blue','mediumseagreen','gray80'),bg = 'transparent')
dev.off()

rm(list = setdiff(ls(),c('dat','check.spid')))


# -----------------------------------------------------------------
# Comparing the 3 unique endset_tb options
# -----------------------------------------------------------------

# Load the 3 endset_tb options
load('endset_tables_2021-01-25.RData')

# Compare the PFAS spid hit with each endset
hit_spid <- list()
for (endset_tb in estl_unique) {
  endset_tb[, aenm := stri_replace_all_fixed(pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',replacement='CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean',aenm)]
  dat2 <- merge(dat, endset_tb, by = c('aenm')) # endpoints not in endset_tb will be dropped
  update_cols <- c('max_med','coff')
  dat2[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
  dat2[, sc_hit := as.integer(max_med >= coff)]
  dat2[, sc_pos := max(sc_hit), by = .(spid)] # only 1 hit requitment
  hit_spid[[length(hit_spid)+1]] <- dat2[sc_pos == 1, unique(spid)]
}

length(unique(hit_spid[[1]])) # 37
length(unique(hit_spid[[2]])) # 36
length(unique(hit_spid[[3]])) # 38

unique(hit_spid)
setdiff(hit_spid[[1]], hit_spid[[2]]) # "1210314363"
setdiff(hit_spid[[2]], hit_spid[[1]]) # empty
setdiff(hit_spid[[1]], hit_spid[[3]]) # empty
setdiff(hit_spid[[3]], hit_spid[[1]]) # "1210314382"

setdiff(hit_spid[[2]], hit_spid[[3]]) # empty
setdiff(hit_spid[[3]], hit_spid[[2]]) # "1210314363" "1210314382"

# So basically, there are 2 spid that could change hit call depending on the endset_tb used
# It looks like the hits of estl_unique[[3]] contain all of the hits from the other 2 sets:
setdiff(c(hit_spid[[1]], hit_spid[[2]]), hit_spid[[3]]) # empty
# so let's use estl_unique[[3]]
# (If 1 set did not contain all of the hits from the other sets, 
# I guess I would just use all of them? perhaps it doesn't matter/there really shouldn't be much of a difference)

endset_tb <- estl_unique[[3]]
endset_tb$aenm <- stri_replace_all_fixed(endset_tb$aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                   replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')


# -----------------------------------------------------------------
# Assign the hit calls
# -----------------------------------------------------------------

# getting coff's from mc analysis
dat2 <- merge(dat, endset_tb[, .(aenm, coff)], by = c('aenm'))
# aeid's not in the endset_tb have been dropped

# assign hit calls -----------------------------------------
update_cols <- c('max_med','coff')
dat2[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
dat2[, sc_hit := as.integer(max_med >= coff)]
dat2[, sc_pos := max(sc_hit), by = .(spid)] # only 1 hit requitment
dat2[, hit_count := length(unique(aenm[sc_hit == 1])), by = .(spid)]

# View the results -----------------------------------------
dat2[wllt == 't', .(length(unique(spid))), by = .(sc_pos)]
# sc_pos V1
# 1:      0 94
# 2:      1 37
dat2[sc_pos == 1 & wllt != 't', unique(spid)] # Bisphenol. No more Media!
dat2[, length(unique(spid))] # 134
View(dat2[, .(length(unique(aenm[sc_hit == 1]))), by = .(spid, treatment)][order(-V1)]) # DMSO and Median not hits, thankfully. 

# number of hits by spid
dat2[, .(spid_with_this_many_hits = length(unique(spid))), by = .(hit_count)][order(hit_count)]

# number of hits by endpoint
dat2[wllt == 't', .(num_hits = length(unique(spid[sc_hit == 1]))), by = .(aenm, coff)][order(-num_hits)]
# aenm     coff rank num_hits
# 1:                                       CCTE_Shafer_MEA_dev_LDH_dn  30.0000    1       18
# 2:                                        CCTE_Shafer_MEA_dev_AB_dn  30.0000    2       11
# 3:                CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn  47.0176    3       26
# 4: CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_DIV12_up  63.3080    4        3
# 5:                   CCTE_Shafer_MEA_dev_mutual_information_norm_up 118.4340    5        2
# 6:                    CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12_dn  55.5246    6       13
# 7:       CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12_up  68.4416    7        9
# 8:                        CCTE_Shafer_MEA_dev_network_spike_peak_dn  53.0434    8       10
# 9:         CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_up 472.5900    9        0
# 10:       CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12_dn  52.8475   10        0
# 11:                  CCTE_Shafer_MEA_dev_network_spike_peak_DIV12_dn  26.6127   11        3


# other checks ------------------------------------
# what about the 7 spids that were NA for a few endpoints?
dat2[spid %in% check.spid, .(sc_pos = unique(sc_pos)), by = .(spid, treatment, hit_count)] # hey, all 7 are positives!
# spid treatment hit_count sc_pos
# 1: 1208990396  3612 B02         5      1
# 2: 1208990399  3612 E02         5      1
# 3: 1208990415  3612 E04         5      1
# 4: 1210313685  3361 D02         5      1
# 5: 1210314416  3360 G09         5      1
# 6: 1210314428  3360 C11         5      1
# 7: 1210314431  3360 F11         5      1
# great, don't need to worry about potentially adding hits for these

# do all of my hits match Seline's cytotox hits?
seline.cytotox.hits <- c('3360 B12','3360 C11','3360 B04','3360 F11','3360 G09','3360 H02','3360 H07','3360 G10','3361 A03',
                         '3361 D02','3361 E05','3612 B02','3612 D03','3612 E01','3612 E02','3612 E04')
hit_treatments <- dat2[sc_pos == 1, unique(treatment)]
setdiff(seline.cytotox.hits, hit_treatments) # empty!!


# Save the results --------------------------------

# save the compounds, order by hit count
spid.table <- dat2[, unique(.SD), .SDcols = c('spid','treatment','sc_pos','hit_count')]
write.csv(spid.table[order(-hit_count)], file = paste0('pfas_sps_hits_',as.character.Date(Sys.Date()),'.csv'), row.names = F)

# include aenm info
spid.aenm.table <- dat2[, unique(.SD), .SDcols = c('spid','treatment','aenm','max_med','coff','sc_hit','sc_pos','hit_count')]
write.csv(spid.aenm.table[order(-hit_count, spid, aenm)], file = paste0('pfas_sps_hits_with_aenm_',as.character.Date(Sys.Date()),'.csv'), row.names = F)

# save the data and hit call results
setkey(dat, NULL)
save(dat, file = paste0('SPS_PFAS2019_normalized_dat_',as.character.Date(Sys.Date()),'.RData'))
setkey(dat2, NULL)
save(dat2, file = paste0('SPS_PFAS2019_hit_calls_',as.character.Date(Sys.Date()),'.RData'))


# -----------------------------------------------------------------
# Random visualizations
# -----------------------------------------------------------------

# it's not like the last few aeid are causing a bunch of additional hits
stripchart(max_med ~ aenm, dat2[, .(max_med = unique(max_med)), by = .(spid, aenm)], vertical = T, pch = '')
stripchart(max_med ~ aenm, dat2[sc_pos == 1, .(max_med = unique(max_med)), by = .(spid, aenm)], vertical = T, pch = 1, col = 'red', add = T)
stripchart(max_med ~ aenm, dat2[sc_pos == 0, .(max_med = unique(max_med)), by = .(spid, aenm)], vertical = T, pch = 1, add = T)

dat2[, aenm_short := sub('CCTE_Shafer_MEA_dev_','',aenm)]
dat2$aenm_short <- factor(dat2$aenm_short, levels = sub('CCTE_Shafer_MEA_dev_','',endset_tb$aenm), ordered = T)
par(mar = c(15,4,3,1))
stripchart(max_med/coff ~ aenm_short, dat2[, .(max_med = unique(max_med)), by = .(spid, aenm_short, coff)], vertical = T, pch = '', las = 2, cex.axis = 0.8)
stripchart(max_med/coff ~ aenm_short, dat2[sc_pos == 1, .(max_med = unique(max_med)), by = .(spid, aenm_short, coff)], vertical = T, method = 'jitter', pch = 1, col = 'red', add = T)
stripchart(max_med/coff ~ aenm_short, dat2[sc_pos == 0, .(max_med = unique(max_med)), by = .(spid, aenm_short, coff)], vertical = T, method = 'jitter', pch = 1, add = T)
abline(h=1)
legend(x = 'topright', legend = c('SPS hit','SPS non hit'), pch = 1, col = c('red','black'), bg = 'transparent')
title(main = paste0('Number of SPS hits by endpoint'))

# remove the positive/negative coloring, I think that is a bit circular
par(mar = c(12,4,3,1))
stripchart(max_med/coff ~ aenm_short, dat2[, .(max_med = unique(max_med)), by = .(spid, aenm_short, coff)], vertical = T, method = 'jitter', pch = 1, las = 2, cex.axis = 0.8, xaxt = 'n')
axis(side = 1, at = 1:length(levels(dat2$aenm_short)), labels = F)
text(x = 1:length(levels(dat2$aenm_short)), y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), 
     labels = levels(dat2$aenm_short), srt = 45, adj = 1, xpd = TRUE, cex=0.7)
abline(h=1)
text(x = 11, y = c(-3,2.5), labels = c('sc_hit=0','sc_hit=1'), cex = 0.8)
title(main = paste0('SPS hits by endpoint'))

# sinaplot?
library(sinaplot)
graphics.off()
pdf(file = paste0('sps_hits_by_endpoints',as.character.Date(Sys.Date()),'.pdf'))
par(mar = c(12,4,3,1))
sinaplot(max_med/coff ~ aenm_short, dat2[, .(max_med = unique(max_med)), by = .(spid, aenm_short, coff)], xaxt = 'n', xlab = '')
axis(side = 1, at = 1:length(levels(dat2$aenm_short)), labels = F)
text(x = 1:length(levels(dat2$aenm_short)), y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), 
     labels = levels(dat2$aenm_short), srt = 45, adj = 1, xpd = TRUE, cex=0.7)
abline(h=1)
text(x = 11, y = c(-3,2.5), labels = c('sc_hit=0','sc_hit=1'), cex = 0.8)
title(main = paste0('SPS hits by endpoint'))
graphics.off()

# Anything else stand out in other endpoints?
hit_spids <- dat2[sc_pos == 1,unique(spid)]
sinaplot(max_med ~ aenm, dat[grepl('up',aenm), .(max_med = unique(max_med)), by = .(spid, aenm)], pch = '', las = 2, xlab = '')
sinaplot(max_med ~ aenm, dat[grepl('up',aenm) & (spid %in% hit_spids), .(max_med = unique(max_med)), by = .(spid, aenm)], col = 'red', add = T, las = 2, xaxt = 'n')
sinaplot(max_med ~ aenm, dat[grepl('up',aenm) & !(spid %in% hit_spids), .(max_med = unique(max_med)), by = .(spid, aenm)], add = T, las = 2, xaxt = 'n')
# Yes, but remember:
# The whole point of the MC analysis was to determine which endpoints are the best at separately positives and negatives
# based on med resp max conc only
# So even though there are a few negatives with e.g. insanely high burst duration mean up max_med values,
# I determined that that endpoints is not the best indicator

# spid with only 1 hit
dat2[, hit_count := length(unique(aenm[sc_hit == 1])), by = .(spid)]
dat2[, unique(hit_count)]
dat2[hit_count == 1 & sc_hit == 1, .(length(unique(spid))), by = .(aenm)]

