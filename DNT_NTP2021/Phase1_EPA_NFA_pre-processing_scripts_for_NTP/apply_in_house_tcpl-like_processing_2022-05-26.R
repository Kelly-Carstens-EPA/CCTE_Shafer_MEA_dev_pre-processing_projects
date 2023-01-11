# ------------------------------------------------------------------------ #
# Implement tcpl-like normalization
# To predict which chemicals seem to be having a response at the lowest conc tested
# And so should be repeated at lower conc's
#
# May 17 2022 - fixing inconsistency in naming for 1 assay endpoint
# between my assay name map and current invitrodb aenm's
# Update May 26, 2022 - don't set wllq to 0 for precipitate
# ------------------------------------------------------------------------ #

# NOTE:
# moved all to rMD for easier save of the output

rm(list = ls())
library(data.table)
library(ggplot2)


# Load data ---------------------------------------------------------------

load('DNT_NTP2021/output/DNT_NTP2021_preliminary_longfile.RData')
cat(description)
# Saving a preliminary version of DNT NTP 2021
# So that we can evalute which chemicals need to be repeated.
# What is left to do:
#   - Confirm that all concentrations have been corrected to the exact conc consistently
# - convert all concentration units to uM
# - Assign sample ids/blind codes
# Date Ran: May 17 2022

# General rule on wllq for this version (from the run_me:)
# preciptate -> wllq == 0
# dosing error, cell debris -> wllq == 0
# need to repeat at lower conc -> note, but wllq still == 1
# note about color of chemical/solution -> note, but wllq still == 1

# UPDATE - set wllq to 1 where precipitate observed, so that can still run dose-response
# RESUME HERE
dat[ ,.N, by = .(wllq_notes)]
dat[grepl('precipitate',tolower(wllq_notes)) ,.N, by = .(wllq_notes, wllq)]
# wllq_notes wllq    N
# 1:                                                                                                     7126 A12 precipitates in media, need to be repeated on lower concentration    0 1827
# 2: Chemical 7126 B2 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well\nwill be repeated with lower concentration    0 1827
# 3:                                           Chemical 7126 B11 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well    0 1827
# Keep the notes, but update teh wllq
dat[grepl('precipitate',tolower(wllq_notes)), wllq := 1]


# Note that the acsn is the same as the acnm
dat[, acnm := acsn]


# Load latest mea tcpl data
load('ccte_mea_dev_data_10may2021.RData') # (FOLDER PATH REDACTED)
ls() #  "dat"         "description" "hit.rates"   "hitc.table"  "mc0"         "mc3"         "mc5"         "mc6"   
# ** Note that 1 aenm has yet to be updated in invitrodb
mc5[, acnm_for_illustration := sub('_.{2}$','',aenm)]
setdiff(unique(dat[!grepl('DIV',acnm), acnm]), unique(mc5$acnm_for_illustration))
# "CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean"
setdiff(unique(mc5[!grepl('DIV',acnm_for_illustration), acnm_for_illustration]), unique(dat[!grepl('DIV',acnm), acnm]))
# "CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean"
# Note that the "inter_network_spike_interval_mean" aligns more closely with what I have gleaned from the code
mc5[, aenm := sub('per_network_spike_interspike_interval_mean',
                  'inter_network_spike_interval_mean', aenm)]
coff.tb <- mc5[, unique(.SD), .SDcols = c('aeid','aenm','coff','bmad')]
# I'm curious how much the bmad is estimated to change with the addition of htis data
setDT(mc3)
control.resp.tb <- mc3[wllt == 'n', .(aeid, aenm, resp)]
rm(mc0, mc3, mc5, mc6)


# Load the methods
load('mea_nfa_tcpl_methods_2022-05-02.RData') # (FOLDER PATH REDACTED)
cat(description)
# All MEA NFA sc and mc TCPL methods as of May 2, 2022.
# These methods were used for the latest udpates to the MEA NFA data that took place in
# June 2021 for the sc and May 2021 for the mc.


# Implement simplified tcpl processing ------------------------------------

# > Level 1 ---------------------------------------------------------------
# (no aeid-specific methods)

# Add the cndx (my simplified version of this - just going by treatment, apid, aenm, and wllt for kicks)
dat[, conc := signif(conc, 3)]
dat[, cndx := frank(conc, ties.method = 'dense'), by = .(treatment, apid, acnm, wllt)]


# > Level 2 ---------------------------------------------------------------

mc2.mthds[, .N, by = .(mthd)]
# mthd  N
# 1: none 36
# method none for all
# rval renamed to cval
dat[, cval := rval]

# all wllq 0 points are removed at this step
dat <- dat[wllq == 1]


# > Level 3 ---------------------------------------------------------------

# split into up and dn endpoints
dat.dn <- dat
dat.dn[, aenm := paste0(acnm, '_dn')]
dat.up <- dat[!grepl('(LDH)|(AB)',acnm)] # cyto endponts are only dn
dat.up[, aenm := paste0(acnm, '_up')]
dat2 <- rbind(dat.dn, dat.up)
rm(dat.dn, dat.up, dat)

# See methods
mc3.mthds <- merge(aeid.tb, mc3.mthds, all = T, by = 'aeid')
mc3.mthds[grepl('_dn',aenm), endpoint_type := 'dn']
mc3.mthds[grepl('_up',aenm), endpoint_type := 'up']
mc3.mthds[grepl('(LDH)|(AB)',aenm), endpoint_type := 'cyto']
mc3.mthds[endpoint_type == 'dn', .N, by =.(mthd, mthd_id, ordr)]
#                   mthd mthd_id ordr  N
# 1: bval.apid.nwlls.med      11    1 34
# 2:           pval.zero      32    2 34
# 3:             resp.pc       5    3 34
mc3.mthds[endpoint_type == 'cyto', .N, by =.(mthd, mthd_id, ordr)]
# mthd mthd_id ordr N
# 1: bval.apid.nwlls.med      11    1 2
# 2:           pval.zero      32    2 2
# 3:             resp.pc       5    3 2
# same as for all other dn endpoints
mc3.mthds[endpoint_type == 'up', .N, by =.(mthd, mthd_id, ordr)]
# mthd mthd_id ordr  N
# 1: bval.apid.nwlls.med      11    1 34
# 2:           pval.zero      32    2 34
# 3:             resp.pc       5    3 34
# 4:       resp.multneg1       6    4 34
# same as dn, just also multiple -1

# Calculate bvals
# copying from tcpl:
# bval.apid.nwlls.med = function(aeids) {
#   e1 <- bquote(dat[J(.(aeids)), 
#                    bval := median(cval[wllt == "n"], na.rm = TRUE), 
#                    by = list(aeid, apid)])
#   list(e1)
#   
# }
dat2[, bval := median(cval[wllt == 'n'], na.rm = T), by = .(apid, aenm)]

# Set the pval
dat2[, pval := 0]

# Calculate the resp values
# resp.pc = function(aeids) {
#   
#   e1 <- bquote(dat[J(.(aeids)),
#                    resp := (cval - bval)/(pval - bval)*100])
#   list(e1)
#   
# }
dat2[, resp := (cval - bval)/(pval - bval)*100]

# Multiple -1 for up
dat2[grepl('_up',aenm) & !grepl('(LDH)|(AB)',aenm), resp := resp*-1]

# Calculate the median resp by cndx
dat2[, med_resp_by_cndx := median(resp, na.rm = T), by = .(treatment, cndx, aenm)]


# > Level 4 ---------------------------------------------------------------

mc4.mthds[, .N, by = .(mthd)]
# mthd  N
# 1: bmad.aeid.lowconc.nwells 70

# Calculate bmad
# bmad.aeid.lowconc.nwells = function() {
#   e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
#   list(e1)
#   
# }
dat2[, bmad := mad(resp[wllt == "n"], na.rm = TRUE), by = .(aenm)] # by aenm, since this is ran by acnm


# > Level 5 ---------------------------------------------------------------

# cutoff methods
mc5.mthds[, .N, by = .(mthd)]
# mthd  N
# 1: bmad3 70
# 3bmad for all

# But, since this applies to the entire dataset - I want the bmads from tcpl, not from this data subset

# quick comparison of bmad's - to estimate how much the coffs will change with teh addition of this
subset.bmads <- dat2[, unique(.SD), .SDcols = c('aenm','bmad')]
comp.bmads <- merge(subset.bmads, coff.tb, by = 'aenm', suffixes = c('.subset','.invitrodb'), all = T)
ggplot(comp.bmads, aes(x = bmad.subset, y = bmad.invitrodb)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0)
# bmads are fairly similar
# only significant deviation is where bmad.subset is a bit larger
# So some coffs will possibly be a bit larger after the addition of this data
# just keep in mind



# Okay, so the endpoint "inter network spike interval mean dn" is not present in coff.tb



# Add in invitrodb coff
setnames(dat2, old = 'bmad', new = 'bmad_subset')
setdiff(dat2$aenm, coff.tb$aenm) # oh right, all of the div 5-9. I don't want these
dat2 <- merge(dat2, coff.tb[, .(aenm, coff_invitrodb = coff, bmad_invitrodb = bmad)], by = 'aenm', all = F)

# Check out the bmad if pooled together
control.resp.tb.pooled <- rbind(control.resp.tb, dat2[wllt == 'n', .(resp, aenm)], fill = T)
bmads.pooled.tb <- control.resp.tb.pooled[, .(bmad_pooled = mad(resp, na.rm = TRUE)), by = .(aenm)]
dat2 <- merge(dat2, bmads.pooled.tb, by = 'aenm', all.x = T)

ggplot(dat2, aes(x = bmad_invitrodb, y = bmad_pooled)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0)
dat2[, summary(bmad_invitrodb - bmad_pooled)]
dat2[, bmad_pooled_vs_idb := (bmad_pooled - bmad_invitrodb)/abs(mean(c(bmad_pooled, bmad_invitrodb))), by = .(aenm)]
dat2[, summary(bmad_pooled_vs_idb)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -0.068  -0.004   0.026   0.033   0.056   0.316   11100
# NA's probs occur where bmads are 0?
dat2[is.na(bmad_pooled_vs_idb), .N,by = .(aenm)] # yep, DIV12 AE adn ABE
dat2[bmad_pooled_vs_idb > 0.075, .N, by = .(aenm, bmad_pooled_vs_idb)]
# aenm bmad_pooled_vs_idb    N
# 1:              CCTE_Shafer_MEA_dev_burst_duration_mean_dn         0.13194303 2775
# 2:              CCTE_Shafer_MEA_dev_burst_duration_mean_up         0.13194303 2775
# 3:       CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn         0.08632485 2775
# 4:       CCTE_Shafer_MEA_dev_bursting_electrodes_number_up         0.08632485 2775
# 5:         CCTE_Shafer_MEA_dev_interburst_interval_mean_dn         0.07953841 2775
# 6:         CCTE_Shafer_MEA_dev_interburst_interval_mean_up         0.07953841 2775
# 7: CCTE_Shafer_MEA_dev_network_spike_duration_std_DIV12_dn         0.08867325 2775
# 8: CCTE_Shafer_MEA_dev_network_spike_duration_std_DIV12_up         0.08867325 2775
# 9:       CCTE_Shafer_MEA_dev_network_spike_duration_std_dn         0.09354588 2775
# 10:       CCTE_Shafer_MEA_dev_network_spike_duration_std_up         0.09354588 2775
# So a few do change by some percentage points
# I guess might as well use the pooled, but again, we know it's an estimate of what the "final" cutoff will be,
# and borderlien cases will be borderline!!

# assign cutoff based on this bmad
dat2[, coff_pooled := 3*bmad_pooled]


# > Level 6 ---------------------------------------------------------------

# assign flags - NA for my present purposes


# > Add'l processing for my analysis --------------------------------------

# Calculate the resp vs the cutoff
dat2[, med_resp_vs_coff := med_resp_by_cndx / coff_pooled]

dat2[, any_wllq1 := as.numeric(any(wllq == 1)), by = .(treatment, aenm)]

rm(list = setdiff(ls(),'dat2'))


# Save it -----------------------------------------------------------------

description <- 'An mc3-like table containing resp values along with current invitrodb cutoffs.
Created from DNT_NTP2021_preliminary_longfile.RData.
Wllq for wells that had precipitate set to 1.
Created with apply_in_house_tcpl-like_processing_2022-05-26.R'
setkey(dat2, NULL)
save(dat2, description, file = file.path('DNT_NTP2021','check_for_chem_to_rescreen','data','DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-26.RData'))
