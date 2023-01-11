load('lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData')

# Make the artificial point at DIV = 2 clear
add.div2 <- mea_nfa_lvl0[grepl('DIV12',acnm)]
add.div2[, acnm := sub('DIV12','DIV2',acnm)]
add.div2[, rval := NA_real_]
mea_nfa_lvl0 <- rbind(mea_nfa_lvl0, add.div2)
rm(add.div2)

# Add DIV
mea_nfa_lvl0[, DIV_char := stri_extract(acnm, regex = 'DIV[0-9]*')]
mea_nfa_lvl0[, DIV := as.numeric(stri_extract(DIV_char, regex = '[0-9]{1,2}'))]

# Note where NA rvals set to 0 for AUC calculation
mea_nfa_lvl0[, plot_rval := ifelse(!is.na(rval), rval, 0)]
mea_nfa_lvl0[, added_0 := as.numeric(is.na(rval))]
mea_nfa_lvl0$added_0 <- factor(mea_nfa_lvl0$added_0, levels = c('0','1'), ordered = T)


# Prepare control data

controldat <- mea_nfa_lvl0[wllt == 'n' & wllq == 1]
rm(mea_nfa_lvl0)
controldat[, culture := sub('_.*$','',apid)]
controldat[, well_id := paste0(apid, '_', rowi,'_',coli)]
controldat[, plate_id := frank(apid, ties.method='dense'), by = .(culture)]
controldat[, plate_char := paste0('Plate',plate_id)]
use.cultures <- sample(unique(controldat[plate_id >= 6, culture]), size = 5)


# Use loop function

## Create plots
acnms <- sort(unique(controldat[is.na(DIV) & !grepl('(LDH)|(AB)',acnm), acnm]))
plotdat <- controldat[culture %in% use.cultures]

plot.object <- list()

for (acnm_sub in paste0(acnms,'_DIV')) {
  
  p <- ggplot(plotdat[grepl(acnm_sub, acnm)], aes(x = DIV, y = plot_rval)) +
    geom_point(aes(color = added_0))+
    geom_line(aes(group = well_id))+
    facet_grid(rows = vars(culture), 
               cols = vars(plate_char),
               scales = 'free_y',
               switch = 'y')+
    scale_color_manual(values = c('0' = 'black',
                                  '1' = 'red'))+
    scale_x_continuous(breaks = c(2,5,7,9,12))+
    # scale_y_continuous(limits = quantile(plotdat$plot_rval, c(0, 0.99))) + # don't let outliers throw off the scale
    ggtitle(paste0(sub('_DIV','',acnm_sub),' over DIV'))+
    theme(legend.position = 'top')+
    ylab('rval')
  plot.object[[acnm_sub]] <- p
}



# Save pdf ----------------------------------------------------------------

pdf(file = 'investigations/DIV_to_AUC_explorations/activity_over_DIV_controls_random_sample_cultures_2022-05-19.pdf',
    width = 10, height = 10)
for (i in seq_along(plot.object)) {
  print(plot.object[[i]])
}
graphics.off()


# View values for all 1 plate on each plot in addition

acnms <- sort(unique(controldat[is.na(DIV) & !grepl('(LDH)|(AB)',acnm), acnm]))
plotdat <- controldat[culture %in% use.cultures]

plot.object <- list()
plot.apids <- controldat[culture %in% use.cultures, unique(apid)]

for (acnm_sub in paste0(acnms,'_DIV')) {
  
  p <- ggplot(plotdat[grepl(acnm_sub, acnm)], aes(x = DIV, y = plot_rval)) +
    geom_point(aes(color = added_0))+
    geom_line(aes(group = well_id))+
    facet_grid(rows = vars(culture), 
               cols = vars(plate_char),
               scales = 'free_y',
               switch = 'y')+
    scale_color_manual(values = c('0' = 'black',
                                  '1' = 'red'))+
    scale_x_continuous(breaks = c(2,5,7,9,12))+
    # scale_y_continuous(limits = quantile(plotdat$plot_rval, c(0, 0.99))) + # don't let outliers throw off the scale
    ggtitle(paste0(sub('_DIV','',acnm_sub),' over DIV'))+
    theme(legend.position = 'top')+
    ylab('rval')
  plot.object[[acnm_sub]] <- p
}
