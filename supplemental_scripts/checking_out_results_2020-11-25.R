# checking out the active electrodes endpoints...

mc5 <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/ccte_mea_dev_mc_summary_hitcalls_12nov2020.csv"))

# questions I have:
# - what would be a good cutoff for the AE/ABE at DIV12? (since bmad is 0)
# - Tim seems very curious about the up hits, and verifying whether these are a true biological effect. 
# Which endpoints are seeing a lot of up hits? If it is mostly endpoints that have a clear inverse, I think that informative
# but if it is endpoints like burst duration... that would be interesting.
# - Katie's ideas for questioning BMAD for each response, etc.
# - Katie's ideas for filtering endpoints...
# - also, Tim's idea of the standard deviation...

# at soem point:
# can we do better than just div12 or auc?


mc5[grepl('_up',aenm) & hitc == 1 & flag.length < 3, .N, by = .(aenm, aeid)][order(-N)] # just the top few...
# aenm aeid  N
# 1:    CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12_up 3053 95
# 2:             CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12_up 3035 64
# 3:        CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12_up 3039 59
# 4:                  CCTE_Shafer_MEA_dev_burst_duration_mean_DIV12_up 3037 30
# 5:  CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_DIV12_up 3055 22
# 6:      CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_DIV12_up 3061 20
# 7: CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_up 2519 20
# 8:            CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up 2523 17
# 9:                  CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12_up 3049 14
# 10:         CCTE_Shafer_MEA_dev_correlation_coefficient_mean_DIV12_up 3065 13
# 11:                        CCTE_Shafer_MEA_dev_burst_duration_mean_up 2507 13

# inter network spike interval
dtxsids <- mc5[aeid == 3053 & hitc == 1 & flag.length < 3, unique(dsstox_substance_id)]
mc5[dsstox_substance_id %in% dtxsids & grepl('network_spike_number_DIV12_dn',aenm), .N, by = .(hitc)]
# hitc  N
# 1:    0 45
# 2:    1 67
# so moooost of these do have network spike number dn hit, but not all

# interburst interval
dtxsids <- mc5[aeid == 3035 & hitc == 1 & flag.length < 3, unique(dsstox_substance_id)]
mc5[dsstox_substance_id %in% dtxsids & grepl('(burst_duration_mean_DIV12_dn)|(burst_rate_DIV12_dn)',aenm), .N, by = .(hitc, aenm)]
# hitc                                             aenm  N
# 1:    1          CCTE_Shafer_MEA_dev_burst_rate_DIV12_dn 51
# 2:    1 CCTE_Shafer_MEA_dev_burst_duration_mean_DIV12_dn  4
# 3:    0          CCTE_Shafer_MEA_dev_burst_rate_DIV12_dn 25
# 4:    0 CCTE_Shafer_MEA_dev_burst_duration_mean_DIV12_dn 60
# 5:   -1 CCTE_Shafer_MEA_dev_burst_duration_mean_DIV12_dn  1

# hmmm... could I calculate teh correlation between certain up and down endpoints?

mc5[, summary(modl_ga)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -4.995  -0.239   0.637   0.366   1.175   3.917   21448 
mc5[hitc == 0, modl_ga := 5]

plot_cor <- function(aenmx, aenmy) {
  x_tb <- mc5[hitc %in% c(1,0) & aenm == aenmx, .(dsstox_substance_id, modl_ga)]
  y_tb <- mc5[hitc %in% c(1,0) & aenm == aenmy, .(dsstox_substance_id, modl_ga)]
  # use_chem <- intersect(x_tb$dsstox_substance_id, y_tb$dsstox_substance_id)
  # use_chem <- use_chem[!is.na(use_chem)]
  plotdat <- merge(x_tb, y_tb, by = "dsstox_substance_id", suffixes = c(".x",".y"))
  plot(modl_ga.x ~ modl_ga.y, plotdat, xlab = aenmx, ylab = aenmy, main = "modl_ga correlation plot")
  abline(a = 0, b = 1)
}

aenmx <- "CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12_up"
aenmy <- "CCTE_Shafer_MEA_dev_network_spike_number_DIV12_dn"
plot_cor(aenmx, aenmy)
plot_cor("CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12_up",
         "CCTE_Shafer_MEA_dev_burst_duration_mean_DIV12_dn") # wow, def no correlation here!
plot_cor("CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12_up",
         "CCTE_Shafer_MEA_dev_burst_rate_DIV12_dn") # some correlation here...

# how about 2 "unrelated" up and dn endpoints?
plot_cor("CCTE_Shafer_MEA_dev_interburst_interval_mean_DIV12_up",
         "CCTE_Shafer_MEA_dev_spike_duration_mean_DIV12_dn") # ya, only a few endpoints are hits in both

# huh... so there is some relationship between these endpoints I have identified... but not a ton



