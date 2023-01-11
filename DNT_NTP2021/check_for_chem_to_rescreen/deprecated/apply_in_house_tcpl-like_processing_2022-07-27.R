# ------------------------------------------------------------------------ #
# Implement tcpl-like normalization
# To predict which chemicals seem to be having a response at the lowest conc tested
# And so should be repeated at lower conc's
#
# July 28, 2022 - running this script after repeats have been performed
# ------------------------------------------------------------------------ #

# NOTE:
# moved all to rMD for easier save of the output

rm(list = ls())
library(data.table)
library(ggplot2)


# Load data ---------------------------------------------------------------

load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-07-28

dat[, .N, by = .(wllq)]
# all 1 or 0 now, cool

# Note that the acsn is the same as the acnm
dat[, acnm := acsn]


# Load data with all control resp values, for calculating the bmad
load('tcpl_results/mc3_controls_resp_tb_10may2021.RData')

# Load the methods
load('tcpl_results/mea_nfa_tcpl_methods_2022-05-02.RData')
cat(description)
# All MEA NFA sc and mc TCPL methods as of May 2, 2022.
# These methods were used for the latest udpates to the MEA NFA data that took place in
# June 2021 for the sc and May 2021 for the mc.
# Created with the script supplemental_scripts/get_mea_nfa_tcpl_methods_2022-05-02.R

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
dat2[, bmad_subset := mad(resp[wllt == "n"], na.rm = TRUE), by = .(aenm)] # by aenm, since this is ran by acnm

# Use existing control resp values to calculate the bmad of the existing and new data
control.resp.tb.pooled <- rbind(control.resp.tb, dat2[wllt == 'n', .(resp, aenm)], fill = T)
bmads.pooled.tb <- control.resp.tb.pooled[, .(bmad_pooled = mad(resp, na.rm = TRUE)), by = .(aenm)]
dat2 <- merge(dat2, bmads.pooled.tb, by = 'aenm', all.x = T)
dat2[is.na(bmad_pooled)] # dmpty, cool

# compare bmad's, make sure not too different
dat2[, bmad_subset_perc_diff := (bmad_pooled - bmad_subset)/((bmad_pooled+bmad_subset)*0.5)*100]
dat2[, summary(bmad_subset_perc_diff)]
# hmm, some varysubstanially
dat2[, .N, by = .(aenm, bmad_subset, bmad_pooled, bmad_subset_perc_diff)][order(bmad_subset_perc_diff)]

# More than I would expect...

ggplot(dat2[, unique(.SD), .SDcols = grep('bmad',names(dat2),val = T)], aes(x = bmad_subset, y = bmad_subset_perc_diff)) +
  geom_point()

# Well, this is why I'm using the pooled bmad's, rather than just the bmad's in this dataset

# # compare to previous results:
# current.dat2 <- dat2
# load('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-26.RData')
# comp.bmads <- merge(current.dat2[, unique(.SD), .SDcols = c('aenm',grep('bmad',names(current.dat2),val = T))],
#                     dat2[, unique(.SD), .SDcols = c('aenm',grep('bmad',names(dat2),val = T))],
#                     by = 'aenm', all = T)
# ggplot(comp.bmads, aes(x = bmad_pooled.x, y = bmad_pooled.y))+
#   geom_point()+
#   geom_abline(slope = 1, intercept = 0)+
#   xlab('current bmad pooled')+
#   ylab('previous bmad pooled')
# # looks quite similar to what I did before
# 
# comp.bmads[abs(bmad_subset_perc_diff) > 5, .N, by = .(aenm, bmad_pooled.x, bmad_pooled.y, bmad_subset.x, bmad_subset_perc_diff)][order(bmad_subset_perc_diff)]
# # yep, bmad and bmad_pooled look quite similar
# 
# # I guess it's believable that the bmad's would vary?
# dat2 <- current.dat2
# rm(current.dat2)



# > Level 5 ---------------------------------------------------------------

# cutoff methods
mc5.mthds[, .N, by = .(mthd)]
# mthd  N
# 1: bmad3 70
# 3bmad for all

# assign cutoff based on this bmad
dat2[, coff_pooled := 3*bmad_pooled]

# (not needed)
# Add in invitrodb coff
# setdiff(dat2$aenm, coff.tb$aenm) # oh right, all of the div 5-9. I don't want these
# dat2 <- merge(dat2, coff.tb[, .(aenm, coff_invitrodb = coff, bmad_invitrodb = bmad)], by = 'aenm', all = F)


# > Level 6 ---------------------------------------------------------------

# assign flags - NA for my present purposes


# > Add'l processing for my analysis --------------------------------------

# Calculate the resp vs the cutoff
dat2[, med_resp_vs_coff := med_resp_by_cndx / coff_pooled]

dat2[, any_wllq1 := as.numeric(any(wllq == 1)), by = .(treatment, aenm)]

rm(list = setdiff(ls(),'dat2'))


# Save it -----------------------------------------------------------------

description <- 'An mc3-like table containing resp values along with bmads and coff estimated from pooled current invitrodb and this dataset\'s response values.
Created from DNT_NTP2021_longfile.RData.
Created with apply_in_house_tcpl-like_processing_2022-07-27.R'
setkey(dat2, NULL)
save(dat2, description, file = file.path('DNT_NTP2021','check_for_chem_to_rescreen','data','DNT_NTP2021_preliminary_in_house_normalized_values_2022-07-27.RData'))
