# pasting in thoughts from previous investgiations -------------------------------------
# what changes have I made in the h5file creation that would not be accounted for?
# - spike file chopping, which I can implement now (as long as the recording wasn't too short)
# - checking for shorted recording
# - checking that the correct master chem file matches, no "overmatching" - I doubt this was a prob for them, since they ran the script for each culture or plate individually
# - allowing ( ) in the h5file name - that is okay
# - made elec col char, not factor, to eliminate a warning message. But I determined this should not be an issue
# - adjust when spike list files not in first row - would have thrown an error
# - Fix when no rows have "" in Electrode column, still can run the check with no error

# Did they check for "" in Electrode column? -> They used a filter for NA in the .map files in meadq:map2list
# well information ever an issue? -> I think this was a totally different structure with the .map files
# ---------------------------------------------------------------------------------

# preparing the existing h5Files for incoporation into the updated data stream
plate_folders <- c("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-98",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-100",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-39",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-68",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140924 Ontogeny/MW 1007-70",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140924 Ontogeny/MW 1007-81",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140924 Ontogeny/MW 1007-84")
h5Files <- c()
for (plate_folder in plate_folders) {
  add_files <- list.files(file.path(plate_folder,"h5Files"), pattern = "\\.h5", full.names = T, recursive = T)
  h5Files <- c(h5Files, add_files)
  rm(add_files)
}
h5Files 
# let's not include the "partialTimepointFiles"
h5Files[grepl("partialTimepointFiles",h5Files)]
# [1] "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140924 Ontogeny/MW 1007-81/h5Files/partialTimepointFiles/ON_20140924_MW1007-81_12_01_001.h5"
# [2] "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140924 Ontogeny/MW 1007-81/h5Files/partialTimepointFiles/ON_20140924_MW1007-81_12_01_002.h5"
h5Files <- h5Files[!grepl("partialTimepointFiles",h5Files)]

# remove DIV 2 and BIC recordings
h5Files <- h5Files[!grepl("_02_",basename(h5Files))]
h5Files <- h5Files[!grepl("_12_01_",basename(h5Files))]
length(h5Files) == 4*length(plate_folders) # TRUE!

# to do when adjust the existing h5 data:
# - length, is it reasonable?
#- no expected spike list file filtering

# for every h5file, we want it to look like this, mostly
str(new_h5dat)
# List of 12
# $ array        : chr [1(1d)] "Axion 48 well"
# $ channels     : chr [1:444(1d)] "A1_11" "A1_12" "A1_13" "A1_14" ...
# $ dose         : chr [1:48(1d)] "0" "0.03" "0.1" "0.3" ...
# $ epos         : num [1:444, 1:2] 0 0 0 0 200 200 400 400 400 600 ...
# $ names        : chr [1:444(1d)] "A1_11" "A1_12" "A1_13" "A1_14" ...
# $ sCount       : int [1:444(1d)] 385 5121 85 6717 95 563 782 921 543 100 ...
# $ size         : chr [1:48(1d)] "NA" "NA" "NA" "NA" ...
# $ spikes       : num [1:180891(1d)] 1.66 1.66 3.38 6.76 10.07 ...
# $ summary.table:'data.frame':	1 obs. of  5 variables:
#   ..$ file       : chr [1(1d)] "ON_20141015_MW1007-107_05_00_001"
# ..$ nelectrodes: num [1(1d)] 444
# ..$ nspikes    : num [1(1d)] 180891
# ..$ time.min   : num [1(1d)] 0.0176
# ..$ time.max   : num [1(1d)] 900
# $ treatment    : chr [1:48(1d)] "Saccharin" "Saccharin" "Saccharin" "Saccharin" ...
# $ units        : chr [1:48(1d)] "uM" "uM" "uM" "uM" ...
# $ well         : chr [1:48(1d)] "A1" "A2" "A3" "A4" ...

new_h5names <- c('array','channels','dose','epos','names','sCount','size','spikes','treatment','units','well')
for (h5File in h5Files) {
  cat(basename(h5File),"\n")
  h5_dat <- h5read(h5File, name = "/")
  
  # check for missing categories
  missing_names <- setdiff(new_h5names, names(h5_dat))
  if (length(missing_names) > 0) {
    cat(missing_names, "\n", sep = " ")
  }
  
  # check file timing
  first_spike_time <- min(h5_dat$spikes)
  last_spike_time <- max(h5_dat$spikes)
  if (last_spike_time - first_spike_time < (900.00 - 3*60)) {
    cat("File only goes from ",first_spike_time," to ",last_spike_time," seconds\n",sep = "")
  }
  else if (last_spike_time - first_spike_time > 900.00) {
    cutoff_time <- first_spike_time + 900.00
    # re-creating 'spikes' object
    spikes_org <- h5_dat$spikes
    names(spikes_org) <- rep(h5_dat$channels, times = h5_dat$sCount)
    spikes <- spikes_org[spikes_org < cutoff_time]
    spikes<-split(spikes, names(spikes))
    
    # from sjemea::map.to.h5 (which is called by meadq::map.to.h5.dh)
    nspikes <- sapply(spikes, length)
    channels <- names(spikes)
    # plateinfo <- plateinfo(array)
    # epos <- axion.elec.name.to.xy(channels, plateinfo)
    h5write(unlist(spikes), h5File, "/spikes")
    h5write(nspikes, h5File, "/sCount")
    h5write(epos, h5File, "/epos")
    h5write(channels, h5File, "/names")
    
    # a check
    all.equal(spikes_org[spikes_org < cutoff_time], unlist(spikes))
  }
  
  
  str(h5_dat)
}

# during a test:  h5write(nspikes, h5File, "/sCount")
# Error in H5Dwrite(h5dataset, obj, h5spaceMem = h5spaceMem, h5spaceFile = h5spaceFile) : 
#   HDF5. Dataset. Write failed.
# but it still says that I modified this file!!
# based on no change below, perhaps I just opened and closed it, but did not modifiy

# from sjemea::map.to.h5 (which is called by meadq::map.to.h5.dh)
# where spikes is the output from spikes2list, where the spikes from each electrode are in sep element in list
# nspikes <- sapply(spikes, length)
# channels <- names(spikes)
# wells <- axion.guess.well.number(channels)
# array <- sprintf("Axion %d well", wells)
# plateinfo <- plateinfo(array)

save_h5file <- paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/h5file_recovery/",basename(h5File))
h5createFile(save_h5file)

for (i in 1:length(names(h5_dat))) {
  h5write(h5_dat[i], save_h5file, paste0("/",names(h5_dat)[i]))
}
# epos <- axion.elec.name.to.xy(channels, plateinfo)
# h5write(unlist(spikes), h5file, "/spikes")
# h5write(nspikes, h5file, "/sCount")
# h5write(epos, h5file, "/epos")
# h5write(channels, h5file, "/names")
# h5write(array, h5file, "/array")


save_h5file <- paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/h5file_recovery/",basename(h5File))
if (file.exists(save_h5file))
  unlink(save_h5file)
h5createFile(save_h5file)
for (i in 1:length(names(h5_dat))) {
  h5write(h5_dat[[i]], save_h5file, paste0("/",names(h5_dat)[i]))
}

test <- h5read(save_h5file, name = "/")
str(test)
max(test$spikes) - min(test$spikes) # [1] 904.975
max(test$spikes) # [1] 1204.485
cutoff_time # [1] 1199.51
# okay! So it looks like I did indeed not alter h5_dat

# this is how it should look
str(h5_dat)
# List of 12
# $ array        : chr [1(1d)] "Axion 48 well"
# $ channels     : chr [1:444(1d)] "A1_11" "A1_12" "A1_21" "A1_22" ...
# $ dose         : chr [1:48(1d)] "0" "0.03" "0.1" "0.3" ...
# $ epos         : num [1:444, 1:2] 0 0 200 200 200 400 400 400 400 600 ...
# $ names        : chr [1:444(1d)] "A1_11" "A1_12" "A1_21" "A1_22" ...
# $ sCount       : int [1:444(1d)] 47 6 90 517 3583 175 24 139 3 44 ...
# $ size         : chr [1:48(1d)] "NA" "NA" "NA" "NA" ...
# $ spikes       : num [1:82960(1d)] 329 330 348 348 348 ...
# $ summary.table:'data.frame':	1 obs. of  5 variables:
#   ..$ file       : chr [1(1d)] "ON_20140827_MW1007-91_05_00_001"
# ..$ nelectrodes: num [1(1d)] 444
# ..$ nspikes    : num [1(1d)] 82960
# ..$ time.min   : num [1(1d)] 300
# ..$ time.max   : num [1(1d)] 1204
# $ treatment    : chr [1:48(1d)] "Acetaminophen" "Acetaminophen" "Acetaminophen" "Acetaminophen" ...
# $ units        : chr [1:48(1d)] "uM" "uM" "uM" "uM" ...
# $ well         : chr [1:48(1d)] "A1" "A2" "A3" "A4" ...

# I am now going to restore this file back to it's proper location
h5File
# [1] "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91/h5Files/ON_20140827_MW1007-91_05_00_001.h5"
if (file.exists(h5File))
  unlink(h5File)
# Warning message:
#   In unlink(h5File) :
#   cannot delete reparse point 'L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91/h5Files/ON_20140827_MW1007-91_05_00_001.h5', reason 'Access is denied'
# huh?
# h5createFile(h5File)
# for (i in 1:length(names(h5_dat))) {
#   h5write(h5_dat[[i]], h5File, paste0("/",names(h5_dat)[i]))
# }

test2 <- h5read(h5File, name = "/")
str(test2)
all.equal(test2$sCount, h5_dat$sCount) # TRUE?? okay... huh
# but is says I that modified it!

rm(test2)
rm(test)
rm(spikes)

save(h5_dat, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/h5_dat_Oct13_2020.RData")
rm(h5_dat)
save(h5dat, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/h5dat_Oct13_2020.RData")
rm(h5dat)

rm(electrodes)
# Okay, so I still need to checkout/confirm if the h5File on the L drive is uncorrupted,
# but I think it's okay
# futhermore, I think I should be able to use teh h5files just fine
# and i want to take advantage of the work I have already done here, 
# so I won't ask Kahtleen to find the spike list files.

# Similar check for Specific Aim 1 h5files, will those work?
sa1h5files <- list.files("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/20140716 Ontogeny/h5Files", pattern = "\\.h5",full.names = T)
test <- h5read(sa1h5files[1], name = "/")
str(test) # looks good, totally useable!!

sa1h5files <- list.files("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/20140730 Ontogeny/h5Files", pattern = "\\.h5",full.names = T)
test <- h5read(sa1h5files[1], name = "/")
str(test) # looks good, totally useable!!
# except that DIV 9 is missing
# even though there def was a recording - have solid spike_counts for DIV 9!

# So really, I only need the spike list file for DIV 9.

# 10/14/2020 -----------------------------------
# checking out this h5file I supposedly modified.. not sure how/when
library(rhdf5)
h5_dat <- h5read("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-39/h5Files/ON_20140910_MW1008-39_02_00_001.h5", name = "/")
str(h5_dat)
h5_dat$spikes
# about 100 spikes for teh whole recording
# from 3 electrodes
# that looks right for DIV 2
# I am going to assume that this file was not modified, even though file explorer says that I did 10/14/2020
file.info("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-39/h5Files/ON_20140910_MW1008-39_02_00_001.h5")
# size isdir mode               mtime               ctime
# L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-39/h5Files/ON_20140910_MW1008-39_02_00_001.h5 31903 FALSE  666 2014-10-14 13:39:04 2014-10-14 13:39:03
# atime exe
# L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140910 Ontogeny/MW1008-39/h5Files/ON_20140910_MW1008-39_02_00_001.h5 2014-10-14 13:39:03  no