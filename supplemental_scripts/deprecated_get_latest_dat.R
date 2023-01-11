# function to pool everything togeter!!

get_latest_dat <- function(dataset_titles= c('Brown2014','DNTGF2019','Frank2017','NTP91','OPP2015','PFAS2018','PFAS2019','ToxCast2016'), 
                           file = NULL, file_type = "longfile.Rdata",
                           root.dir = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl") {
  
  require(data.table)
  cat("Loading",file_type,"for...\n")
  alldat <- data.table()
  msg <- ""
  for (diri in dataset_titles) {
    cat(diri,"\n")
    tryCatch(load(file.path(root.dir, diri, "output", paste0(diri,"_",file_type))),
             error = function(error) error,
             warning = function(warning) assign("msg", paste0(file.path(root.dir, diri, "output", paste0(diri,"_",file_type))," does not exist"), envir = parent.frame(n=4))
    )
    if (exists("dat")) {
      dat$dataset <- diri
      alldat <- rbind(alldat, dat, fill = TRUE)
      rm(dat)
    }
  }
  cat(msg,"\n")
  setDT(alldat)
  
  # some basic checks
  
  # any duplicated data?
  if (nrow(alldat[, .N, by = .(apid, rowi, coli, acsn)][N != 1]) > 0) {
    cat("There appears to be duplicated data for the following apid/row/coli/acsn:\n")
    print(alldat[, .N, by = .(apid, rowi, coli, acsn)][N != 1])
  }
  
  # any NA spid?
  if(any(is.na(alldat$spid))) {
    cat("Some spid are NA in the following datasets:\n")
    cat(alldat[is.na(spid), unique(dataset)],sep = "\n")
  }
  
  # any NA conc
  if(any(is.na(alldat$conc))) {
    cat("Some conc's are NA for the following spids:\n")
    cat(alldat[is.na(conc), unique(spid)],sep = "\n")
  }
  
  if(!is.null(file)) {
    save(alldat, file = file)
  }
  
  return(alldat)
}

save_lvl0_snapshot <- function(root.dir = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl") {
  mea_nfa_lvl0 <- get_latest_dat()
  
  cat("Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...\n")
  remove_apid <- mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 10/60, unique(apid)]
  remove_apid <- intersect(remove_apid,
                   mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 2, unique(apid)])
  cat("Setting wllq:=0 for these apid:",remove_apid,"\n")
  print(mea_nfa_lvl0[apid %in% remove_apid & (grepl("active_electrodes_number_DIV12",acsn) | grepl("firing_rate_mean_DIV12",acsn)) & wllt == "n", .(apid, sub("CCTE_Shafer_MEA_dev_","",acsn), rval)])
  mea_nfa_lvl0[apid %in% remove_apid, `:=`(wllq = 0,
                                           wllq_notes = paste0(wllq_notes,"; Median MFR < 10 spikes per min or nAE < 2 on DIV12"))]
  
  cat("Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints:",mea_nfa_lvl0[!grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
  cat("Number of points where wllq==1 and rval is NA for DIV endpoints:",mea_nfa_lvl0[grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
  
  cat("Renaming 'acsn' to 'acnm'...\n")
  setnames(mea_nfa_lvl0, old = "acsn", new = "acnm", skip_absent = TRUE)
  
  # save snapshot with all columns
  setkey(mea_nfa_lvl0, NULL) # remove keys I inadvertantly added, to make the file smaller
  save(mea_nfa_lvl0, file = file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_lvl0_extra_cols_",as.character.Date(Sys.Date()),".RData")))
  cat(file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_lvl0_extra_cols_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")
  
  # save snapshot with just TCPL mc0 cols
  cat("Restricting to only AUC, DIV12, and LDH/AB endpoints...\n")
  mea_nfa_lvl0 <- mea_nfa_lvl0[!grepl("_DIV[579]",acnm)]
  
  cat("Settting wllq==0 where rval is NA...\n")
  mea_nfa_lvl0[is.na(rval), wllq := 0]
  
  usecols <- c("acnm","spid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")
  mea_nfa_lvl0 <- mea_nfa_lvl0[, .SD, .SDcols = usecols]
  setkey(mea_nfa_lvl0, NULL) # remove keys I inadvertantly added, to make the file smaller
  save(mea_nfa_lvl0, file = file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_lvl0_",as.character.Date(Sys.Date()),".RData")))
  
  cat(file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_lvl0_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")
}

save_lvl0_snapshot_sps <- function(dataset_titles, root.dir = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl") {
  require(stringi)
  
  mea_nfa_sc0 <- get_latest_dat(dataset_titles)
  
  cat("Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...\n")
  remove_apid <- mea_nfa_sc0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 10/60, unique(apid)]
  remove_apid <- intersect(remove_apid,
                           mea_nfa_sc0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 2, unique(apid)])
  cat("Setting wllq:=0 for these apid:",remove_apid,"\n")
  print(mea_nfa_sc0[apid %in% remove_apid & (grepl("active_electrodes_number_DIV12",acsn) | grepl("firing_rate_mean_DIV12",acsn)) & wllt == "n", .(apid, sub("CCTE_Shafer_MEA_dev_","",acsn), rval)])
  mea_nfa_sc0[apid %in% remove_apid, `:=`(wllq = 0,
                                           wllq_notes = paste0(wllq_notes,"; Median MFR < 10 spikes per min or nAE < 2 on DIV12"))]
  
  cat("Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints:",mea_nfa_sc0[!grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
  cat("Number of points where wllq==1 and rval is NA for DIV endpoints:",mea_nfa_sc0[grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
  
  # Restrict the the endpoints selected from the analysis of the mc data
  # Collapse all endpoints from the 3 final sets of endpoints
  load('L:/Lab/NHEERL_MEA/Project PFAS 2019/MEA NFA/SPS Hit Call Analysis/endset_tables_2021-01-25.RData')
  use_aenms <- unique(unlist(lapply(estl_unique, function(x) x$aenm)))
  use_acnms <- unique(sub('_[a-z]{2}$','',use_aenms)) # remove the up/dn ending to get acnm's
  # fix anomaly in naming from mc aenm
  use_acnms <- stri_replace_all_fixed(use_acnms, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                      replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')
  cat('Restricting to the following acsn\'s...\n')
  cat(sort(use_acnms), sep = '\n')
  if (length(setdiff(use_acnms, mea_nfa_sc0$acsn)) > 0) stop(paste0('The following acnm\'s are not present in the mea_nfa_sc0:',setdiff(use_acnms, mea_nfa_sc0$acsn), collapse='\n'))
  mea_nfa_sc0 <- mea_nfa_sc0[acsn %in% use_acnms]
  
  cat("Renaming 'acsn' to 'acnm'...\n")
  setnames(mea_nfa_sc0, old = "acsn", new = "acnm", skip_absent = TRUE)
  
  # save snapshot with all columns
  setkey(mea_nfa_sc0, NULL) # remove keys I inadvertantly added, to make the file smaller
  save(mea_nfa_sc0, file = file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_sc0_extra_cols_",as.character.Date(Sys.Date()),".RData")))
  cat(file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_sc0_extra_cols_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")
  
  # save snapshot with just TCPL mc0 cols
  cat("Settting wllq==0 where rval is NA...\n")
  mea_nfa_sc0[is.na(rval), wllq := 0]
  
  usecols <- c("acnm","spid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")
  mea_nfa_sc0 <- mea_nfa_sc0[, .SD, .SDcols = usecols]
  setkey(mea_nfa_sc0, NULL) # remove keys I inadvertantly added, to make the file smaller
  save(mea_nfa_sc0, file = file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_sc0_",as.character.Date(Sys.Date()),".RData")))
  
  cat(file.path(root.dir,"lvl0_snapshots",paste0("mea_nfa_sc0_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")
}

