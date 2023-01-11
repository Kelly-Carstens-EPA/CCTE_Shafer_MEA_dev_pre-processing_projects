#-----------------------------------------------------------------------------------#
# Script for finding a good combination of cutoffs and endpoints
# to detect MC positives based on median response at max conc tested only
# 
# Mehod:
# Greedy Algorithm 
# with cytotox as initial metric
# 3 hits requirement for mc positives
# no exceptions for cytotox hits

# Jan 20, 2021
# author: carpenter.amy@epa.gov
#-----------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------#
# loading libraries
#-----------------------------------------------------------------------------------#

rm(list = ls())

library(data.table)

# Change working directory to desired output directory
setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_nfa/SPS_endpoint_investigation")

#-----------------------------------------------------------------------------------#
# Prep data
#-----------------------------------------------------------------------------------#

# load the most recent mea nfa data
load("L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/plots_12_NOV_2020/ccte_mea_dev_data_12nov2020.RData")
mc6_collapsed <- mc6[, .(flag_length = length(unique(flag)), mc6_mthd = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid","m5id")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
rm(list = c("mc6","mc0","mc5","mc6_collapsed","hit.rates","hitc.table"))

# Define "hitc.f" for hitc filtered
mc5_mc6[, hitc.f := hitc]
mc5_mc6[fitc %in% c(36,45), hitc.f := 0]
mc5_mc6[!is.na(flag_length) & flag_length >= 3, hitc.f := 0]

# remove these 4 endpoints
dat <- mc5_mc6[!grepl('electrodes_number_DIV12',aenm)]
rm(mc5_mc6)

# define mc_pos positives
dat[, mc_pos := as.numeric(sum(hitc.f == 1) >= 3), by = .(spid)]

# any compounds that are a hit only in LDH/AB? (so don't have enough hits to be mc_pos?)
dat[mc_pos == 0 & hitc == 1 & grepl('(LDH)|(AB)',aenm), .(aenm, spid, chnm, max_med, modl, coff, hitc, hitc.f, fitc, mc6_flags)][order(spid)] # many of these hits were removed
dat[mc_pos == 0 & hitc.f == 1 & grepl('(LDH)|(AB)',aenm), .(aenm, spid, chnm, max_med, modl, coff, hitc, hitc.f, fitc, mc6_flags)][order(spid)] 
# aenm          spid                                      chnm  max_med modl     coff hitc hitc.f fitc mc6_flags
# 1: CCTE_Shafer_MEA_dev_LDH_dn       1475818 3,3-Bis(trifluoromethyl)-2-propenoic acid 28.97673 hill 24.09707    1      1   42      17,6
# 2: CCTE_Shafer_MEA_dev_AB_dn EPAPLT0167H07                               Tembotrione 25.33354 hill 20.26243    1      1   42      6,17
# 3: CCTE_Shafer_MEA_dev_AB_dn      EX000351                                    Pyrene 28.59267 gnls 20.26243    1      1   50      7,17
# 4: CCTE_Shafer_MEA_dev_LDH_dn  TP0001649D06         2-(m-Chlorophenoxy)propionic acid 36.87594 hill 24.09707    1      1   42      17,7
# 5: CCTE_Shafer_MEA_dev_LDH_dn  TT0000177C04                                 Omethoate 33.61127 hill 24.09707    1      1   42      17,6
# only 5, all have flag 17
dat[!grepl('17',mc6_flags) & grepl('(LDH)|(AB)',aenm) & hitc == 1, .N, by = .(aenm)] #  okay, so not every cyto hit has flag 17
# I'm still not going to include these 5, see ppt

# how many compounds have 1-2 hits?
dat[, hit_count := sum(hitc.f == 1), by = .(spid)]
dat[mc_pos == 0 & hit_count > 0, .(length(unique(spid)))] # 83. That's quite a few
# but as Tim says, these are equivocal positives anyhow

# breakdown of positives and negatives
dat[!is.na(chnm), .(length(unique(spid))), by = .(mc_pos)]
# mc_pos  V1
# 1:      1 236
# 2:      0 186

# create "sdat," which contains 1 row for each spid/aeid pair (includes the median response at max conc tested, mc_pos, and mc_hit)
mc3 <- mc3[!grepl('electrodes_number_DIV12',aenm)]
max_conc_tb <- mc3[, .(med_resp_max_conc = median(resp[signif(logc,3) == max(signif(logc,3))])), by = .(spid, aeid)]
lvl5_tb <- dat[, unique(.SD), .SDcols = c('spid', 'chnm', 'mc_pos', 'aenm', 'aeid', 'bmad','hitc.f')]
sdat <- merge(max_conc_tb, lvl5_tb, by = c('spid','aeid'), all = T)
setnames(sdat, old = 'hitc.f', new = 'mc_hit')

control_spid <- mc3[wllt == 'n', unique(spid)]
rm(mc3)
sdat <- sdat[!(spid %in% control_spid)]
sdat[, length(unique(spid))]
# [1] 422
# rm(dat)


#-----------------------------------------------------------------------------------#
# Run Method C to determine the best endpoints and cutoffs
#-----------------------------------------------------------------------------------#
# Old method, arbitrarily chooses endpoints where there is a tie
# source('functions/greedy_algo_methodC_3.R')
# 
# # hit cnt threshold back to 1
# sdat[, coff := 30] # initialize coff for LDH/AB endpoints
# sdat[, sc_pos := 0] # initialize all as not-hit
# endset_tb.1c <- greedy_algo_methodC_bmad_multipliers_3(sdat, initial_endset = sdat[grepl('(LDH)|(AB)',aenm),unique(aeid)],
#                                                     hit_cnt_threshold = 1,
#                                                     finish_with_method_B = FALSE, prompt_when_tie = FALSE)
# endset_tb.1c[, FP_rate := ttl_FP/sdat[mc_pos == 0, length(unique(spid))]*100]
# endset_tb.1c[, TP_rate := ttl_TP/sdat[mc_pos == 1, length(unique(spid))]*100]
# endset_tb.1c[, accuracy := (ttl_TP + sdat[mc_pos == 0, length(unique(spid))] - ttl_FP)/sdat[, length(unique(spid))]*100]
# endset_tb.1c
# 
# # save the results
# write.csv(endset_tb.1c, file = paste0('nfa_proposed_endpoint_coffs_',as.character.Date(Sys.Date()),'.csv'), row.names = F)

# 01/20/2021 update, return list of all endpoint/coff combinations given intial settings
source('functions/greedy_algo_methodC_3_list.R')

# coff :=3*Bmad for ldh and ab ---
sdat[, coff := 3*bmad] # initialize coff for LDH/AB endpoints
sdat[, sc_pos := 0] # initialize all as not-hit
estl <- greedy_algo_methodC_bmad_multipliers_3_list(sdat, initial_endset = sdat[grepl('(LDH)|(AB)',aenm),unique(aeid)],
                                                    hit_cnt_threshold = 1,
                                                    finish_with_method_B = FALSE, initial_i = 1)
length(estl) # 240 combinations!
estl[[1]]
estl[[240]]
ttl_TP_vector <- unlist(lapply(estl, function(x) x[nrow(x),ttl_TP]))
max(ttl_TP_vector) # 227
ttl_FP_vector <- unlist(lapply(estl, function(x) x[nrow(x),ttl_FP]))
min(ttl_FP_vector) # 21
# 3 addition TP at the coset of 10 add FP doesn't seem worth it...
rm(list= c('estl','ttl_TP_vector','ttl_FP_vector'))

# cof:=30 for ldh and ab ---
sdat[, coff := 30] # initialize coff for LDH/AB endpoints
sdat[, sc_pos := 0] # initialize all as not-hit
estl <- greedy_algo_methodC_bmad_multipliers_3_list(sdat, initial_endset = sdat[grepl('(LDH)|(AB)',aenm),unique(aeid)],
                                                       hit_cnt_threshold = 1,
                                                       finish_with_method_B = FALSE, initial_i = 1)

length(estl) # 36 combinations

# how many truly unique endset tables are we looking at?
estl_condensed <- lapply(estl, function(x) x[, .(aeid, aenm, coff)][order(aeid)])
estl_unique <- unique(estl_condensed)
estl_unique
# cool! So there are only 3 truly unique combinations of endpoints and coff's

# Do they all have the same final accuracy, TPs and FPs?
sdat[, coff := NULL]
for (endset_tb in estl_unique) {
  test <- merge(sdat, endset_tb, by = c('aeid','aenm'))
  update_cols <- c('med_resp_max_conc','coff')
  test[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
  test[, sc_hit := as.integer(med_resp_max_conc >= coff)]
  test[, sc_pos := max(sc_hit), by = .(spid)]
  print(test[, .(length(unique(spid))), by = .(sc_pos, mc_pos)][order(sc_pos, mc_pos)])
  rm(test)
}
# sc_pos mc_pos  V1
# 1:      0      0 175
# 2:      0      1  12
# 3:      1      0  11
# 4:      1      1 224
# sc_pos mc_pos  V1
# 1:      0      0 175
# 2:      0      1  12
# 3:      1      0  11
# 4:      1      1 224
# sc_pos mc_pos  V1
# 1:      0      0 175
# 2:      0      1  12
# 3:      1      0  11
# 4:      1      1 224

# okay, all appear to have same number

# Are the exact same spid hit with use set of endpoints&coffs?
hit_spid <- list()
for (endset_tb in estl_unique) {
  test <- merge(sdat, endset_tb, by = c('aeid','aenm'))
  update_cols <- c('med_resp_max_conc','coff')
  test[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
  test[, sc_hit := as.integer(med_resp_max_conc >= coff)]
  test[, sc_pos := max(sc_hit), by = .(spid)]
  hit_spid[[length(hit_spid)+1]] <- test[sc_pos == 1, unique(spid)]
  rm(test)
}

setdiff(hit_spid[[1]], hit_spid[[2]]) # empty
setdiff(hit_spid[[2]], hit_spid[[1]]) # empty
setdiff(hit_spid[[1]], hit_spid[[3]]) # empty
setdiff(hit_spid[[3]], hit_spid[[1]]) # empty

setdiff(hit_spid[[2]], hit_spid[[3]]) # empty
setdiff(hit_spid[[3]], hit_spid[[2]]) # empty


# Hmm... so it seems that no matter which of these 3 endpoints I choose, the results of the mc_analysis will be identical.
# Makes sense - these sets are not all that different

# Which endpoints are different?
setdiff(estl_unique[[1]]$aenm, estl_unique[[2]]$aenm) # "CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12_up"
setdiff(estl_unique[[2]]$aenm, estl_unique[[1]]$aenm) # "CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12_up"
setdiff(estl_unique[[1]]$aenm, estl_unique[[3]]$aenm) # "CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12_up"
setdiff(estl_unique[[3]]$aenm, estl_unique[[1]]$aenm) # "CCTE_Shafer_MEA_dev_mutual_information_norm_up"

setdiff(estl_unique[[2]]$aenm, estl_unique[[3]]$aenm) # "CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12_up"
setdiff(estl_unique[[3]]$aenm, estl_unique[[2]]$aenm) # "CCTE_Shafer_MEA_dev_mutual_information_norm_up"

# So, it looks like only 1 endpoint varies among the 3 groups.
# Same 10, plus 1 of the following:
# "CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12_up"
# "CCTE_Shafer_MEA_dev_mutual_information_norm_up"
# "CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12_up"

# Compare the cutoff values
comp.coffs <- merge(estl_unique[[1]], estl_unique[[2]], by = c('aenm','aeid'), all = T, suffixes = c('.1','.2'))
comp.coffs <- merge(comp.coffs, estl_unique[[3]], by = c('aenm','aeid'), all = T, suffixes = c('','.3'))
comp.coffs
#                                                                 aenm aeid   coff.1   coff.2     coff
# 1:                                         CCTE_Shafer_MEA_dev_AB_dn 2530  30.0000  30.0000  30.0000
# 2:                                        CCTE_Shafer_MEA_dev_LDH_dn 2529  30.0000  30.0000  30.0000
# 3:                 CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn 2500  47.0176  47.0176  47.0176
# 4:                     CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12_dn 3042  55.5246  55.5246  55.5246
# 5:              CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12_up 3043 121.3630       NA       NA
# 6:                    CCTE_Shafer_MEA_dev_mutual_information_norm_up 2527       NA       NA 118.4340
# 7:                   CCTE_Shafer_MEA_dev_network_spike_peak_DIV12_dn 3060  26.6127  26.6127  26.6127
# 8:                         CCTE_Shafer_MEA_dev_network_spike_peak_dn 2512  53.0434  53.0434  53.0434
# 9:        CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12_dn 3040  52.8475  52.8475  52.8475
# 10:        CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12_up 3039  68.4416  68.4416  68.4416
# 11:              CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12_up 3063       NA  30.5276       NA
# 12: CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_up 2519 472.5900 472.5900 472.5900
# 13:  CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_DIV12_up 3055  63.3080  63.3080  63.3080
comp.coffs[coff.1 != coff.2] # empty
comp.coffs[coff.2 != coff.1] # empty
comp.coffs[coff != coff.1] # empty
# the coffs are equal... must mean that the next highest true positve to detect for each endpoint does not change


# looking at the 5 compounds hit in ldh/ab mc only
# (using any set, since they all have same results)
test <- merge(sdat, estl_unique[[1]], by = c('aeid','aenm'))
update_cols <- c('med_resp_max_conc','coff')
test[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
test[, sc_hit := as.integer(med_resp_max_conc >= coff)]
test[, sc_pos := max(sc_hit), by = .(spid)]
test[mc_hit == 1 & mc_pos == 0 & grepl('(LDH)|(AB)',aenm), 1:11]
#    aeid                       aenm          spid med_resp_max_conc                                      chnm mc_pos   bmad.x mc_hit sc_pos sc_hit coff
# 1: 2529 CCTE_Shafer_MEA_dev_LDH_dn       1475818           28.9767 3,3-Bis(trifluoromethyl)-2-propenoic acid      0 8.032355      1      0      0   30
# 2: 2529 CCTE_Shafer_MEA_dev_LDH_dn  TP0001649D06           23.3429         2-(m-Chlorophenoxy)propionic acid      0 8.032355      1      0      0   30
# 3: 2529 CCTE_Shafer_MEA_dev_LDH_dn  TT0000177C04           33.6113                                 Omethoate      0 8.032355      1      1      1   30
# 4: 2530  CCTE_Shafer_MEA_dev_AB_dn EPAPLT0167H07           25.3335                               Tembotrione      0 6.754145      1      0      0   30
# 5: 2530  CCTE_Shafer_MEA_dev_AB_dn      EX000351           10.3079                                    Pyrene      0 6.754145      1      0      0   30
# okay, so only 1/5 of these turns out to be positive regardless.

# the false negatives
fns <- test[mc_pos == 1 & sc_pos == 0, unique(spid)]
sdat[spid %in% fns & mc_hit == 1, .N, by = .(aenm)]
View(test[spid %in% fns, .(spid, aenm, med_resp_max_conc, mc_hit, coff)[order(spid)]])
dat[spid %in% fns & hitc.f == 1, .(spid, aenm, modl, modl_ga, mc6_flags, max_med_conc, logc_max)][order(spid)]

# Print estl for one example
endset_tb <- estl[[36]] # could choose any, last one has accuracy add_tp_hit counts by endpoint
print(endset_tb[, .(aenm, coff, bmad, bmad_multiplier, rank)])

save(estl_unique, file = paste0('endset_tables_',as.character.Date(Sys.Date()),'.RData'))
wb <- createWorkbook()
addWorksheet(wb, sheetName = 'endset_table_a')
addWorksheet(wb, sheetName = 'endset_table_b')
addWorksheet(wb, sheetName = 'endset_table_c')
writeData(wb, sheet = 'endset_table_a', estl_unique[[1]])
writeData(wb, sheet = 'endset_table_b', estl_unique[[2]])
writeData(wb, sheet = 'endset_table_c', estl_unique[[3]])
saveWorkbook(wb, file = paste0('endset_tables_',as.character.Date(Sys.Date()),'.xlsx'))


#-----------------------------------------------------------------------------------#
# Visualize results to intuit overfit
#-----------------------------------------------------------------------------------#

sdat[, coff := NULL]
endset_tb <- estl[[36]]
sdat <- merge(sdat, endset_tb[, .(aeid, coff, rank)], by = 'aeid', all.x=T)
sdat[is.na(coff), coff := 3*bmad] # defining default coff for aeid not included in endset, totally arbitrary, can be moved
update_cols <- c('med_resp_max_conc','coff')
sdat[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
sdat[, sc_hit := as.numeric(med_resp_max_conc >= coff)]
hit_cnt_threshold <- 1
sdat[, sc_pos := as.numeric(length(unique(aenm[sc_hit == 1 & aeid %in% endset_tb$aeid])) >= hit_cnt_threshold), by = .(spid)]
sdat[, .(length(unique(spid))), by = .(mc_pos, sc_pos)][order(mc_pos, sc_pos)]
# mc_pos sc_pos  V1
# 1:      0      0 175
# 2:      0      1  11
# 3:      1      0  12
# 4:      1      1 224

# Label as "detected" if hit in any aeid with rank less than or equal to current aeid
# (rather than only less than)
# this way makes sense numerical, but less so visually I think
aeid_ordered <- endset_tb[,aeid]
remaining_aeid <- unique(sdat$aeid)
sdat[, detected := 0]
for (aeidi in aeid_ordered) {
  detected_spid <- sdat[aeid == aeidi & sc_hit == 1, unique(spid)]
  sdat[spid %in% detected_spid & aeid %in% remaining_aeid, detected := 1]
  remaining_aeid <- setdiff(remaining_aeid, aeidi)
}

# stats on # of spids added after each aeid
endset_tb_detail <- sdat[aeid %in% endset_tb$aeid, .(ttl_TP = length(unique(spid[mc_pos == 1 & sc_pos == 1 & detected == 1])),
                                                     ttl_FP = length(unique(spid[mc_pos == 0 & sc_pos == 1 & detected == 1])), coff = unique(coff)), by = c('aeid', 'aenm','rank')][order(rank)]
endset_tb_detail[, FP_rate := ttl_FP/sdat[mc_pos == 0, length(unique(spid))]*100]
endset_tb_detail[, TP_rate := ttl_TP/sdat[mc_pos == 1, length(unique(spid))]*100]
endset_tb_detail[, accuracy := (ttl_TP + sdat[mc_pos == 0, length(unique(spid))] - ttl_FP)/sdat[, length(unique(spid))]*100]
endset_tb_detail[, add_TP := endset_tb_detail$ttl_TP - c(0, endset_tb_detail[1:(nrow(endset_tb_detail)-1),ttl_TP])]
endset_tb_detail[, add_FP := endset_tb_detail$ttl_FP - c(0, endset_tb_detail[1:(nrow(endset_tb_detail)-1),ttl_FP])]
endset_tb_detail[, .(aeid, aenm, coff, add_TP, add_FP, ttl_TP, ttl_FP, FP_rate, TP_rate, accuracy, rank)]

# Save the example
wb <- createWorkbook()
addWorksheet(wb, sheetName = 'endpoint_set_example')
addWorksheet(wb, sheetName = 'Key')
writeData(wb, sheet = 'endpoint_set_example',endset_tb_detail[, .(aeid, aenm, coff, add_TP, add_FP, ttl_TP, ttl_FP, FP_rate, TP_rate, accuracy, rank)])
key_tb <- data.table(aeid = 'assay endpoint id', aenm = 'endpoint name', coff = 'cutoff', add_TP = '# of true positive added with the current endpoint',
                     add_FP = '# of false positives added with the current endpoint', ttl_TP = 'total true positives', ttl_FP = 'total false positives', 
                     FP_rate = 'false positive rate', TP_rate= 'true positive rate', accuracy = 'accuracy', rank = 'iteration # at which the current endpoint was added')
key_tb.long <- melt(key_tb, measure.vars = names(key_tb), variable.name = 'Column', value.name = 'Description')
writeData(wb, sheet = 'Key', key_tb.long)
saveWorkbook(wb, file = paste0('endpoint_set_example_with_mc_dat_',as.character.Date(Sys.Date()),'.xlsx'), overwrite = T)
rm(wb)

# Update "Detected" col for visualization 
# (label as dected if sc_hit == 1 for aeid with rank less than current aeid)
aeid_ordered <- endset_tb[,aeid]
remaining_aeid <- unique(sdat$aeid)
sdat[, detected := 0]
for (aeidi in aeid_ordered) {
  remaining_aeid <- setdiff(remaining_aeid, aeidi)
  detected_spid <- sdat[aeid == aeidi & sc_hit == 1, unique(spid)]
  sdat[spid %in% detected_spid & aeid %in% remaining_aeid, detected := 1]
}

# make stripchart with all aeids
sdat[, aenm_short := sub('CCTE_Shafer_MEA_dev_','',aenm)]
extra_aenms <- setdiff(unique(sdat$aenm), endset_tb$aenm)
sdat$aenm_short <- factor(sdat$aenm_short, levels = sub('CCTE_Shafer_MEA_dev_','',c(endset_tb$aenm,extra_aenms)), ordered = T)

graphics.off()
pdf(file = paste0('nfa_sps_coffs_visualization_all_aeids_',as.character.Date(Sys.Date()),'.pdf'), width = 15, height = 8)
par(mar = c(11, 4.1, 3, 2), cex = 0.8)
positives_x <- seq(from = 0.85, by = 1, length = length(levels(sdat$aenm_short)))
negatives_x <- seq(from = 1.15, by = 1, length = length(levels(sdat$aenm_short)))
sdat[, range(med_resp_max_conc/coff)] # -153, 153
sdat[mc_pos == 1 & sc_pos == 0, range(med_resp_max_conc/coff)] # [1] -3.79594  3.79594 - false negatives are all in this range
sdat[mc_pos == 0, range(med_resp_max_conc/coff)] # -14 to 14
sdat[mc_pos == 0 & sc_pos == 1, range(med_resp_max_conc/coff)] #  -4.541943  5.949968
# so on -6,6, I will see all false negatives and false positives
# I will miss a few true positives and a few true negatives
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat[mc_pos == 1 & detected == 1], vertical = T, method = 'jitter', pch = 1, col = rgb(0.95,0.85,0.85), 
           at = positives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)))
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat[mc_pos == 0 & detected == 1], vertical = T, method = 'jitter', pch = 1, col = rgb(0.7,0.7,0.7), 
           at = negatives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)), add = T)
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat[mc_pos == 1 & detected == 0], vertical = T, method = 'jitter', pch = 1, col = 'red', 
           at = positives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)), add = T)
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat[mc_pos == 0 & detected == 0], vertical = T, method = 'jitter', pch = 1, col = 'black', at = negatives_x, add = T)
axis(side = 1, at = (positives_x + negatives_x)*0.5, labels = F)
text(x = (positives_x + negatives_x)*0.5, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), 
     labels = levels(sdat$aenm_short), srt = 45, adj = 1, xpd = TRUE, cex=0.7)
abline(h = 1)
abline(v = nrow(endset_tb) + 0.5, lty = 'dashed')
title(main = 'Detection of MC positives using med_resp_max_conc\nand cutoffs derived from Method C')
legend(x = 'topright', legend = c('remaining MC positives','detected MC positives','remaining MC negatives','detected MC negatives'), col = c('red',rgb(0.95,0.85,0.85),'black',rgb(0.7,0.7,0.7)), pch = 1, bg = 'transparent')
text(x = -0.2, y = 1, labels = c('SPS hit','not detected'), pos = c(3,1), cex = 0.5)
text(x = c(nrow(endset_tb)*0.5, (nrow(endset_tb)+length(levels(sdat$aenm_short)))*0.5), y = 4.5, 
     labels = c("Method C Aeids","Unused Aeids"))
graphics.off()


# same thing, but with only the aeid's in endset
sdat2 <- sdat[aeid %in% endset_tb$aeid]
sdat2$aenm_short <- factor(sdat2$aenm_short, levels = sub('CCTE_Shafer_MEA_dev_','',c(endset_tb$aenm)), ordered = T)

graphics.off()
pdf(file = paste0('nfa_sps_coffs_visualization_selected_aeids_',as.character.Date(Sys.Date()),'.pdf'), width = 8, height = 8)
par(mar = c(11, 4.1, 3, 2), cex = 0.8)
positives_x <- seq(from = 0.85, by = 1, length = length(levels(sdat2$aenm_short)))
negatives_x <- seq(from = 1.15, by = 1, length = length(levels(sdat2$aenm_short)))
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat2[mc_pos == 1 & detected == 1], vertical = T, method = 'jitter', pch = 1, col = rgb(0.95,0.85,0.85), 
           at = positives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)))
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat2[mc_pos == 0 & detected == 1], vertical = T, method = 'jitter', pch = 1, col = rgb(0.7,0.7,0.7), 
           at = negatives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)), add = T)
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat2[mc_pos == 1 & detected == 0], vertical = T, method = 'jitter', pch = 1, col = 'red', 
           at = positives_x, ylim = c(-6,6), xaxt = 'n', xlim = c(0, max(negatives_x)), add = T)
stripchart(med_resp_max_conc/coff ~ aenm_short, sdat2[mc_pos == 0 & detected == 0], vertical = T, method = 'jitter', pch = 1, col = 'black', at = negatives_x, add = T)
axis(side = 1, at = (positives_x + negatives_x)*0.5, labels = F)
text(x = (positives_x + negatives_x)*0.5, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), 
     labels = levels(sdat2$aenm_short), srt = 45, adj = 1, xpd = TRUE, cex=0.7)
abline(h = 1)
title(main = 'Detection of MC positives using med_resp_max_conc\nand cutoffs derived from Method C')
legend(x = 'topright', legend = c('remaining MC positives','detected MC positives','remaining MC negatives','detected MC negatives'), col = c('red',rgb(0.95,0.85,0.85),'black',rgb(0.7,0.7,0.7)), pch = 1, bg = 'transparent')
text(x = 0, y = 1, labels = c('SPS hit','not detected'), pos = c(3,1), cex = 0.7)
graphics.off()



# how do the compounds with 1-2 mc_hits look?
sdat[mc_hit == 1 & mc_pos == 0, .(length(unique(spid))), by = .(sc_pos)]
# sc_pos V1
# 1:      1 10
# 2:      0 73
