# 10/22/2020
# Spike List Files are missing for the following cultures:
# 20140716, 20140730
# However, the h5files are present
# I am goign to collect the data from these h5Files,
# apply the file chopping that I currently do in spk2List(),
# then resave these h5Files 

library(rhdf5)
library(sjemea)
library(data.table)

# how h5File is created in my version of the scripts:
# - axion.spkList.to.h5 takes in 1 spike list file and master chem data. 
# - axion.spkList.to.h5 calls spk2List to read the csv file and create spikes.sep
# - axion.spkList.to.h5 does some manipulations of spikes.sep
# - map.to.h5.dh is called, which call sjemea::map.to.h5(spikes, h5file)
# - here's what map.to.h5 does:
#   ## Given a list of spikes, save the HDF5 file.
# 
# h5file <- path.expand(h5file)
# if (file.exists(h5file))
#   unlink(h5file)
# 
# nspikes <- sapply(spikes, length)
# channels <- names(spikes)
# wells <- axion.guess.well.number(channels)
# array <- sprintf("Axion %d well", wells)
# plateinfo <- plateinfo(array)
# epos <- axion.elec.name.to.xy(channels, plateinfo)
# h5createFile(h5file)
# 
# ## Let's compress the spike train by first creating chunks and
# ## compression options.  TODO, fix, it, not yet working for me.
# ## 2013-01-24
# sum.spikes <- sum(nspikes)
# ##h5createDataset(h5file, "/spikes", dims=sum.spikes, chunk=1e5, level=7)
# h5write(unlist(spikes), h5file, "/spikes")
# h5write(nspikes, h5file, "/sCount")
# h5write(epos, h5file, "/epos")
# h5write(channels, h5file, "/names")
# h5write(array, h5file, "/array")
# print(h5ls(h5file))

# - Then, meadq::map.to.h5.dh adds these to the h5file:
#   * channels (literally taken from names(spikes), same as "names")
#   * well
#   * treatment, dose, units, size, array (I assume 'array' overwrite sjemea's write of array)
# - Lastly, axion.spkList.to.h5 adds the summary.table to the h5file (which I dont' think is used for anything)

# Okay, so, all of that to say, the only value in the h5file that is modified after sjemea::map.to.h5 is the "array"
# (which just says that we are on a 48 well array). And I am not going to modify that section, so no worries
# So I can just write the object as they are written in sjemea::map.to.h5



# Gather the needed h5Files -----------------------------------------------------------------------------
plate_folders <- c("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/20140716 Ontogeny",
                   "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/20140730 Ontogeny")
h5Files <- c()
for (plate_folder in plate_folders) {
  add_files <- list.files(file.path(plate_folder,"h5Files"), pattern = "\\.h5", full.names = T, recursive = T)
  h5Files <- c(h5Files, add_files)
  rm(add_files)
}
h5Files

# remove DIV 2 and BIC recordings
h5Files <- h5Files[!grepl("_02_",basename(h5Files))]
h5Files <- h5Files[!grepl("_12_01_",basename(h5Files))]
h5Files

# create names and locations for the update h5files I will save
h5.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Brown2014/h5Files/"
new_h5Files <- paste0(h5.dir, basename(h5Files))
# just confirm that no weird re=ordering occured?
dt <- data.table(V1 = basename(h5Files), V2 = basename(new_h5Files))
dt[V1 != V2] # empty

# for every h5file, we want it to look like this, mostly
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

# Write the updated h5Files -----------------------------------------------------------------------
new_h5names_to_copy <- c('array','dose','size','treatment','units','well')
do_not_copy_h5names <- c("sCount","spikes","names","channels","epos","summary.table")
for (i in 1:length(h5Files)) {
  cat(basename(h5Files[i]),"\n")
  h5_dat <- h5read(h5Files[i], name = "/")
  
  # check for missing categories that are needed
  missing_names <- setdiff(c(new_h5names_to_copy,"spikes"), tolower(names(h5_dat)))
  if (length(missing_names) > 0) {
    if (missing_names == "size") {
      h5_dat$size <- rep("NA",48)
    }
    else {
      stop(paste("Missing names:",missing_names, "\n", sep = " "))
    }
  }
  
  # I know this was a prob for a few files...
  if (length(h5_dat$dose) != 48 | length(h5_dat$well) != 48) stop()

  # Create the new h5 file
  if (file.exists(new_h5Files[i]))
    unlink(new_h5Files[i])
  h5createFile(new_h5Files[i])
  
  # just copy the portions that don't need to be change
  # (taking all values, even if not needed)
  for (tb in setdiff(names(h5_dat), do_not_copy_h5names)) {
    h5write(h5_dat[[tb]], new_h5Files[i], name = paste0("/",tb))
  }
  
  # check file timing
  first_spike_time <- min(h5_dat$spikes)
  last_spike_time <- max(h5_dat$spikes)
  if (last_spike_time - first_spike_time < (900.00 - 3*60)) {
    cat("File only goes from ",first_spike_time," to ",last_spike_time," seconds\n",sep = "")
  }

  # re-create 'spikes' object with cutoff_time
  cutoff_time <- first_spike_time + 900.00
  spikes_org <- h5_dat$spikes
  if(is.element(c("channels"),names(h5_dat))) {
    names(spikes_org) <- rep(h5_dat$channels, times = h5_dat$sCount)
  } else {
    names(spikes_org) <- rep(h5_dat$names, times = h5_dat$sCount)
  }
  cat(sum(spikes_org >= cutoff_time), "spikes that occured after",cutoff_time,"s will be removed\n")
  spikes <- spikes_org[spikes_org < cutoff_time]
  spikes<-split(spikes, f = names(spikes))
  
  # Calculate the updated values wanted
  # from sjemea::map.to.h5 (which is called by meadq::map.to.h5.dh)
  nspikes <- sapply(spikes, length)
  channels <- names(spikes)
  wells <- axion.guess.well.number(channels)
  array <- sprintf("Axion %d well", wells)
  plateinfo <- plateinfo(array)
  epos <- axion.elec.name.to.xy(channels, plateinfo) # not sure what this is used for, might remove
  
  # write the desired data to the new h5File
  h5write(unlist(spikes), new_h5Files[i], "/spikes")
  h5write(nspikes, new_h5Files[i], "/sCount")
  h5write(epos, new_h5Files[i], "/epos")
  h5write(channels, new_h5Files[i], "/names")
  h5write(channels, new_h5Files[i], "/channels") # done in meadq::map.to.h5.dh
  
  rm(list = c("h5_dat","spikes_org","spikes","nspikes","channels","wells","array","plateinfo","epos"))
}

# ON_20140716_MW1007-26_05_00_001.h5 
# 28 spikes that occured after 1436.797 s will be removed
# ON_20140716_MW1007-26_07_00_001.h5 
# 562 spikes that occured after 1275.528 s will be removed
# ON_20140716_MW1007-26_09_00_001.h5 
# 1803 spikes that occured after 1350.76 s will be removed
# ON_20140716_MW1007-26_12_00_001.h5 
# 1689 spikes that occured after 1281.008 s will be removed
# You created a large dataset with compression and chunking.
# The chunk size is equal to the dataset dimensions.
# If you want to read subsets of the dataset, you should testsmaller chunk sizes to improve read times.
# ON_20140730_MW1007-104_05_00_001.h5 
# 94 spikes that occured after 912.3576 s will be removed
# ON_20140730_MW1007-104_07_00_001.h5 
# 695 spikes that occured after 1230.008 s will be removed
# ON_20140730_MW1007-104_12_00_001.h5 
# 852 spikes that occured after 939.7474 s will be removed

# 10/22/2020
# fixing this issue in the h5file, due to a faulty master chem file
h5Files <- list.files("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Brown2014/h5Files/", pattern = "20140716", full.names = T)
for (h5File in h5Files) {
  cat(h5File, "\n")
  h5_dat <- h5read(h5File, name = "/")
  
  update_well <- which(h5_dat$well == "E8")
  h5_dat$dose[update_well] <- "0"
  update_well <- which(h5_dat$well == "E7")
  h5_dat$dose[update_well] <- "10"
  
  # Create the new h5 file
  if (file.exists(h5File))
    unlink(h5File)
  h5createFile(h5File)
  
  # add back everything, with the updated master chem info
  for (tb in names(h5_dat)) {
    cat(tb, "\n")
    h5write(h5_dat[[tb]], h5File, name = paste0("/",tb))
  }
  rm("h5_dat")
}

# save(h5_dat, file = file.path(root_output_dir, dataset_title, "h5_dat_20140716_DIV9_correct.RData"))
