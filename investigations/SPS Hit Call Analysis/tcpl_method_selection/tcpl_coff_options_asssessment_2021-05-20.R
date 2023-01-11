#-----------------------------------------------------------------------------------#
# Transferring SPS hit calls to TCPL
# 5/20/2021
# cleaned up/finalized version to share
#-----------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------#
# loading libraries
#-----------------------------------------------------------------------------------#

rm(list = ls())

library(data.table)
library(stringi)
library(ggplot2)
library(openxlsx)

setwd("L:/Lab/NHEERL_MEA/Project PFAS 2019/MEA NFA/SPS Hit Call Analysis/tcpl_method_selection")

load('../SPS_PFAS2019_normalized_dat_2021-01-25.RData')
load('../SPS_PFAS2019_hit_calls_2021-01-25.RData')
load('../endset_tables_2021-01-25.RData')

#-----------------------------------------------------------------------------------#
# Add pc30 options, select closest sc2_mthd that doesn't add any hits
#-----------------------------------------------------------------------------------#

# Re-creating plotdat for convient acess ----------------------------------
# create coff.tb with coff's developed by mc analysis
for (i in 1:length(estl_unique)) {
  # renaming to match aenm as appears in sc data (inter_network_spike_interval is the correct version)
  estl_unique[[i]]$aenm <- stri_replace_all_fixed(estl_unique[[i]]$aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                                  replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')
}
coff.tb <- estl_unique[[1]]
for (i in 2:length(estl_unique)) {
  coff.tb <- merge(coff.tb, estl_unique[[i]], by = c('aenm','aeid'), all = T, suffixes = c('',paste0('.',i)))
}
coff.tb[, nonexchangeable_endpoint := as.numeric(!(is.na(coff) | is.na(coff.2) | is.na(coff.3)))]

# Confirmation that the coffs for the nonexchangeable endpoints are the same with every table
coff.tb[nonexchangeable_endpoint == 1 & coff != coff.2] # empty
coff.tb[nonexchangeable_endpoint == 1 & coff != coff.3] # empty
coff.tb[nonexchangeable_endpoint == 1 & coff.2 != coff.3] # empty

# Merge all into 1 set of coffs
coff.tb[is.na(coff) & !is.na(coff.2), coff := coff.2]
coff.tb[is.na(coff) & !is.na(coff.3), coff := coff.3]
coff.tb[, `:=`(coff.2 = NULL, coff.3 = NULL)]

# Create table with coffs and bmad's together
bmad.coff <- merge(dat[, .(bmad = unique(bmad)), by = .(aenm)], coff.tb[, .(aenm, coff.current = coff, nonexchangeable_endpoint)], by = c('aenm'), all.y = T)
bmad.coff[, `:=`(coff.bmad1.5 = bmad*1.5, coff.bmad2 = bmad*2, coff.bmad3 = bmad*3, coff.bmad5 = bmad*5, coff.bmad6 = bmad*6, coff.bmad10 = bmad*10,
                 coff.pc20 = 20, coff.pc25 = 25, coff.pc30 = 30, coff.pc88 = 88)]
bmad.coff.long <- melt(bmad.coff, id.vars = c('aenm','nonexchangeable_endpoint','bmad','coff.current'), variable.name = 'sc2_mthd', variable.factor = F, value.name = 'coff_value')
bmad.coff.long[, coff_value_per_bmad := coff_value/bmad]
bmad.coff.long[, coff_current_per_bmad := coff.current/bmad]
bmad.coff.long[, sc2_mthd_value_dist_to_coff_current := signif(abs(coff_value - coff.current),5), by = .(aenm)]

# Get the max_med's for each test chemical
max_med.tb <- merge(dat[wllt == 't', .(max_med = (unique(max_med))), by = .(aenm, bmad, spid)], coff.tb, all.y = T, by = 'aenm') # dropping aenm in dat not in coff.tb
max_med.tb[, cur_hitc := as.numeric(max_med >= coff)]
max_med.tb[, any_hit := as.numeric(sum(cur_hitc)>0), by = .(spid)]
max_med.tb[, max_med_per_bmad := max_med/bmad]

# Determine the number of added positives with each potential coff
max_med.tb.coffs <- merge(max_med.tb, bmad.coff, by = c('aenm','bmad','nonexchangeable_endpoint'), all = T) # adding in all potential coffs
max_med.tb.coffs[, .(length(unique(spid))), by = .(aenm)] # 131, but 125 for some div12 endpoints
max_med.tb.coffs[, (paste0('hitc_',grep('coff\\.',names(max_med.tb.coffs),val=T))) := lapply(.SD, function(x) as.numeric(max_med >= x)), .SDcols = grep('coff\\.',names(max_med.tb.coffs),val=T)]
max_med.tb.coffs[, (paste0('num_add_pos_',grep('^coff\\.',names(max_med.tb.coffs),val=T))) := lapply(.SD, function(x) sum(x[any_hit == 0])), .SDcols = grep('hitc_coff\\.',names(max_med.tb.coffs),val=T), by = .(aenm)]
num_added_pos.tb <- unique(melt(max_med.tb.coffs, id.vars = c('aenm','bmad','nonexchangeable_endpoint'), measure.vars = grep('num_add_pos_coff\\.',names(max_med.tb.coffs),val=T),
                                 variable.factor = F, variable.name = 'sc2_mthd', value.name = 'num_add_pos'))
num_added_pos.tb[, sc2_mthd := sub('num_add_pos_','',sc2_mthd)]
# 0 added hits -> best option
# <0 added hits (i.e., removed hits) -> second best option
# >0 added hits -> worst option. Sort these after the worst possible scenario of removing all hits

# Is there at least 1 method that will result in 0 hits for sc negatives?
num_added_pos.tb[, .(sum(num_add_pos == 0)), by = .(aenm)][V1 == 0]
# empty -> yes

# Add the num_added_pos.tb
setdiff(num_added_pos.tb$sc2_mthd, bmad.coff.long$sc2_mthd) # "coff.current"
setdiff(bmad.coff.long$sc2_mthd, num_added_pos.tb$sc2_mthd) # empty
bmad.coff.long <- merge(bmad.coff.long, num_added_pos.tb[sc2_mthd != 'coff.current'], by = c('aenm','bmad','nonexchangeable_endpoint','sc2_mthd'), all = T)

# Select the winning method
setkey(bmad.coff.long, sc2_mthd_value_dist_to_coff_current)
bmad.coff.long[, sc2_mthd_selection := sc2_mthd[num_add_pos == 0][1], by = .(aenm)]
bmad.coff.long[, sc2_mthd_selection_value_per_bmad := coff_value_per_bmad[sc2_mthd == sc2_mthd_selection], by = .(aenm)]
bmad.coff.long[sc2_mthd == sc2_mthd_selection, unique(.SD), .SDcols = c('aenm','bmad','sc2_mthd_selection','sc2_mthd_selection_value_per_bmad','num_add_pos','sc2_mthd_value_dist_to_coff_current')]

# Melt the data again, excluding multiple-of-bmad coffs, since these will be shown with vertical lines
add.dat <- melt(bmad.coff.long[!grepl('bmad',sc2_mthd)], id.vars = c('aenm','nonexchangeable_endpoint','bmad','sc2_mthd_selection'),
                measure.vars = c('coff_current_per_bmad','sc2_mthd_selection_value_per_bmad'), variable.factor = F, variable.name = 'coff_type', value = 'coff_value_per_bmad')
plotdat <- rbind(bmad.coff.long[!grepl('bmad',sc2_mthd), .(coff_type = sc2_mthd, aenm, nonexchangeable_endpoint, bmad, coff_value_per_bmad, sc2_mthd_selection)], add.dat)
plotdat$coff_type <- factor(plotdat$coff_type, levels = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad'), ordered = T)
plotdat[, coff_current_per_bmad := unique(coff_value_per_bmad[coff_type == 'coff_current_per_bmad']), by = .(aenm)] # adding back this as a col, just for sorting
plotdat <- plotdat[order(coff_current_per_bmad)]
plotdat$aenm <- factor(plotdat$aenm, levels = unique(plotdat$aenm), ordered = T)
max_med.tb$aenm <- factor(max_med.tb$aenm, levels = levels(plotdat$aenm), ordered = T)


# Plot it -------------------------------------------
# I tried making png's and pdf's, but those looked terrible (would ahve to option text sizes...)
# so I'm just going to take screen shots

# Note that warning is due to max_med's below 0, cutoff by the x limits
# I don't really care about these points, so I will let them get dropped

# Fig: original cutoffs, max_med's
# current_cutoffs_max_meds_by_endpoint_per_bmad_2021-05-20.png
ggplot(data = plotdat[coff_type == 'coff_current_per_bmad'], aes(y = aenm, x = coff_value_per_bmad)) +
  geom_jitter(data = max_med.tb, aes(x = max_med_per_bmad, y = aenm, color = factor(any_hit), shape = factor(any_hit), size = factor(any_hit)), width = 0, height = 0.2)+
  geom_point(aes(color = factor(coff_type), shape = factor(coff_type), size = factor(coff_type)), stroke = 2)+
  scale_color_manual(values = c('black','red','purple','blue','forestgreen','orange', rgb(0.6,0.8,0.2,0.5),rgb(0.54,0.27,0.075,0.5)),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_shape_manual(values = c(1, 18, 18, 18, 18, 1, 19, 19),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_size_manual(values =  c(4,  3,  3,  3,  3, 4,  2,  2),
                    breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                    labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                    name = 'Key')+
  geom_vline(xintercept = c(1,2,3,5,6,10), linetype = 'dashed')+
  scale_x_continuous(name = 'bmad multiples', breaks = c(1,2,3,5,6,10), labels = c('1','2','3','5','6','10'), limits = c(0,1.2*max(max_med.tb$max_med_per_bmad)))+
  geom_text(data = plotdat[, .(text = paste0('bmad=',signif(unique(bmad),3),', coff=',signif(unique(coff_current_per_bmad*bmad),3))), by = .(aenm)],
            aes(x = 1.05*max(max_med.tb$max_med_per_bmad), label=text, group=`aenm`),  size=3, hjust = 0, color = 'black')+
  ggtitle(label = 'Current Cutoffs and Max_med\'s by Endpoint per BMAD\nfor MEA NFA SPS')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10))
graphics.off()

# Fig: original cutoffs, possible sc2_methods, no max meds
# current_cutoffs_available_sc2_methods_no_max_meds_2021-05-20.png
ggplot(data = plotdat, aes(y = aenm, x = coff_value_per_bmad)) +
  geom_point(aes(color = factor(coff_type), shape = factor(coff_type), size = factor(coff_type)), stroke = 2)+
  scale_color_manual(values = c('black','red','purple','blue','forestgreen','orange', rgb(0.6,0.8,0.2,0.5),rgb(0.54,0.27,0.075,0.5)),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_shape_manual(values = c(1, 18, 18, 18, 18, 1, 19, 19),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_size_manual(values =  c(4,  3,  3,  3,  3, 4,  2,  2),
                    breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                    labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                    name = 'Key')+
  geom_vline(xintercept = c(1,2,3,5,6,10), linetype = 'dashed')+
  scale_x_continuous(name = 'bmad multiples', breaks = c(1,2,3,5,6,10), labels = c('1','2','3','5','6','10'), limits = c(0,1.2*max(max_med.tb$max_med_per_bmad)))+
  geom_text(data = plotdat[, .(text = sub('coff\\.','',unique(sc2_mthd_selection))), by = .(aenm)], 
            aes(x = 1.05*max(max_med.tb$max_med_per_bmad), label=text, group=`aenm`),  size=5, hjust = 0, color = 'orange')+
  ggtitle(label = 'Comparison of Current Coffs and Available TCPL SC2_mthds (plus pc30, bmad1.5)\nfor MEA NFA SPS, with bmad = mad(resp[wllt == \'n\')')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10))

# Fig: original cutoffs, possible sc2 methods, max_meds (a bit busy)
# current_cutoffs_available_sc2_methods_max_meds_2021-05-20.png
ggplot(data = plotdat, aes(y = aenm, x = coff_value_per_bmad)) +
  geom_jitter(data = max_med.tb, aes(x = max_med_per_bmad, y = aenm, color = factor(any_hit), shape = factor(any_hit), size = factor(any_hit)), width = 0, height = 0.2)+
  geom_point(aes(color = factor(coff_type), shape = factor(coff_type), size = factor(coff_type)), stroke = 2)+
  scale_color_manual(values = c('black','red','purple','blue','forestgreen','orange', rgb(0.6,0.8,0.2,0.5),rgb(0.54,0.27,0.075,0.5)),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_shape_manual(values = c(1, 18, 18, 18, 18, 1, 19, 19),
                     breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                     labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                     name = 'Key')+
  scale_size_manual(values =  c(4,  3,  3,  3,  3, 4,  2,  2),
                    breaks = c('coff_current_per_bmad','coff.pc20','coff.pc25','coff.pc30','coff.pc88','sc2_mthd_selection_value_per_bmad','0','1'),
                    labels = c('current coff (per bmad)','20%','25%','30%','88%','sc2_mthd with:\n- no hits for SPS neg,\n- closest to current coff','max_med, SPS neg','max_med, SPS pos'),
                    name = 'Key')+
  geom_vline(xintercept = c(1,2,3,5,6,10), linetype = 'dashed')+
  scale_x_continuous(name = 'bmad multiples', breaks = c(1,2,3,5,6,10), labels = c('1','2','3','5','6','10'), limits = c(0,1.2*max(max_med.tb$max_med_per_bmad)))+
  geom_text(data = plotdat[, .(text = sub('coff\\.','',unique(sc2_mthd_selection))), by = .(aenm)], 
            aes(x = 1.05*max(max_med.tb$max_med_per_bmad), label=text, group=`aenm`),  size=5, hjust = 0, color = 'orange')+
  ggtitle(label = 'Comparison of Current Coffs and Available TCPL SC2_mthds (plus pc30, bmad1.5)\nfor MEA NFA SPS, with bmad = mad(resp[wllt == \'n\')')+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10))


# Assess changes in hit call if I used the selected sc_mthd
max_med.tb2 <- merge(max_med.tb, bmad.coff.long[sc2_mthd == sc2_mthd_selection, .(aenm, sc2_mthd, coff_value)], by = 'aenm')
max_med.tb2[, sc2_mthd_hitc := as.numeric(max_med >= coff_value)]
max_med.tb2[, sc2_mthd_any_hit := as.numeric(sum(sc2_mthd_hitc) > 0), by = .(spid)]
max_med.tb2[, .(length(unique(spid))), by = .(any_hit, sc2_mthd_any_hit)][order(-any_hit)]
#    any_hit sc2_mthd_any_hit V1
# 1:       1                1 34
# 2:       1                0  3
# 3:       0                0 94

# How many hit calls overall change?
max_med.tb2[cur_hitc != sc2_mthd_hitc, .(spid, aenm, any_hit, cur_hitc, sc2_mthd_hitc, max_med, coff, sc2_mthd, sc2_mthd_coff = coff_value)]
# 12 removed hits, 3 added hits. 
# 8 removed from firing rate mean DIV12
# 3 added hits are for compounds that are SPS positive regardless

