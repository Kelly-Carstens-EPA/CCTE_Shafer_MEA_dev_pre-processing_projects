# ------------------------------------------------------------------------ #
# Save table with the latest response values in MEA NFA mc3
# That way, can estimate bmad's based on pooled data set
# July 27, 2022
# ------------------------------------------------------------------------ #

library(data.table)

# Load latest mea tcpl data
load('L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/CCTE_SHAFER_MEA_DEV_10MAY2021/plots_10_MAY_2021/ccte_mea_dev_data_10may2021.RData')
ls() #  "hit.rates"   "hitc.table"  "mc0"         "mc3"         "mc5"         "mc6"   

# ** Note that 1 aenm has yet to be updated in invitrodb, even though the acnm has been
mc5[, acnm_inferred := sub('_[^_]*$','',aenm)]
comp.names <- merge(mc0[, unique(.SD), .SDcols = c('acnm','acid')], mc5[, unique(.SD), .SDcols = c('aenm','aeid','acnm_inferred')], by.x = 'acnm', by.y = 'acnm_inferred', all = T)
comp.names[is.na(acid) | is.na(aeid)]
#                                                               acnm acid                                                              aenm aeid
# 1:          CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean 2483                                                              <NA>   NA
# 2: CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean   NA CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_dn 2518
# 3: CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean   NA CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_up 2519
# Note that the "inter_network_spike_interval_mean" aligns more closely with what I have gleaned from the source code
# Will rename the aenm for the output table
mc5[, aenm := sub('per_network_spike_interspike_interval_mean',
                  'inter_network_spike_interval_mean', aenm)]

# Save a table with the reponse values
setDT(mc3)
control.resp.tb <- mc3[wllt == 'n', .(aeid, aenm, resp)]
rm(mc0, mc3, mc5, mc6)

description <- paste0("Input RDAta: ccte_mea_dev_data_10may2021.RData
\ncontrol.resp.tb <- mc3[wllt == 'n', .(aeid, aenm, resp)].
\nCreated with supplemental_scripts/mc3_controls_resp_tb_2022-07-27.R
\nDate ran: ",as.character.Date(Sys.Date()))

save(control.resp.tb, description, file = 'tcpl_results/mc3_controls_resp_tb_10may2021.RData')
