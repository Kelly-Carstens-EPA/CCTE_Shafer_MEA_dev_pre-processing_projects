# reading the h5files that have already been ran
# as of 09/10/2020
# in order to determine if any of these spike list files had an offset from file start
library(rhdf5)
main_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
dataset_folders <- list.files(path = main_dir, pattern = "^[[:alpha:]]+[[:digit:]]{2,4}$", full.names = T)
dataset_folders <- c(file.path(main_dir, "RejectedCultures"),dataset_folders)
for (dataset_folder in dataset_folders) {
  cat("\n",basename(dataset_folder),"\n",sep="")
  h5_dir <- file.path(dataset_folder,"h5Files")
  h5files <- list.files(h5_dir, pattern = "\\.h5")
  
  for (h5file in h5files) {
    time.min <- tryCatch(h5read(file.path(h5_dir,h5file), name = "summary.table")[,"time.min"],
                         error = function(e) {
                           print(h5file)
                           print(e)
                           return(0) }
                         )
    if (time.min > 1.0) {
      time.max <- h5read(file.path(h5_dir,h5file), name = "summary.table")[,"time.max"]
      cat(h5file, time.min, time.max, "\n")
    }
  }
}


# checking for min nspikes in a recording
nspikes.min <- 2*900
for (dataset_folder in dataset_folders) {
  cat("\n",basename(dataset_folder),"\n",sep="")
  h5_dir <- file.path(dataset_folder,"h5Files")
  h5files <- list.files(h5_dir, pattern = "\\.h5")
  
  for (h5file in h5files) {
    nspikes <- tryCatch(h5read(file.path(h5_dir,h5file), name = "summary.table")[,"nspikes"],
                         error = function(e) {
                           print(h5file)
                           print(e)
                           return(nspikes.min) }
    )
    if (nspikes < nspikes.min) {
      nspikes.min <- nspikes
      cat(h5file, nspikes, "\n")
    }
  }
}
# RejectedCultures
# ON_20160817_MW1140-4_05_00(000).h5 1453 
# ON_20170517_MW1146-13_05_00(000).h5 21 
# 
# Brown2016
# 
# DNTGF2019
# 
# Example2020
# 
# Frank2017
# 
# NTP91
# 
# OPP2015
# 
# PFAS2018
# # [1] "ON_20181114_MW1234-25_07_00(000).h5"
# # <simpleError in H5Fopen(file, "H5F_ACC_RDONLY", native = native): HDF5. File accessibilty. Unable to open file.>
#   
# ToxCast2016

