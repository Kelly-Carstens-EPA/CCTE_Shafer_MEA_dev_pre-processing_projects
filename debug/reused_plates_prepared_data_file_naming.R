# 10/07/2020
# I discovered a bug in create_burst_ont_Data
# the plate.SN for the prepared data file names is taken from a list of the *unique* plates in teh dataset
# so, if a plate is repeated, that will throw off the plate.SN name for all following prepared data files

# checking out which data sets have reused plates
# that I have already finsihed the prepared data for
# as of 10/07/2020
# To check:
# - DNTGF2019
# - NTP91
# - OPP2015
# - PFAS2018
# - ToxCast2016
check_dirs <- c("DNTGF2019","NTP91","OPP2015","PFAS2018","ToxCast2016")
# I am going to check h5file names for all of these

main.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
for(check_dir in check_dirs) {
  cat(check_dir,"\n")
  h5Files <- list.files(file.path(main.dir, check_dir,"h5Files"))
  # date_list <- lapply(h5Files, function(x) strsplit(basename(x), split = "_")[[1]][2])
  unique_plate.SN <- unique(unlist(lapply(h5Files, function(x) strsplit(basename(x), split = "_")[[1]][3])))
  # date_plates <- paste(date_list, plate_list, sep = "_")

  # get teh number of unique culutre dates associated with each plate
  unique_plate.SN <- sapply(unique_plate.SN, function(x) {
    matching_files <- grep(paste0("_",x,"_"), h5Files, val = T)
    matching_dates <- unique(lapply(matching_files, function(y) strsplit(y, split = "_")[[1]][2]))
    length(matching_dates)
  })
  
  # display the plates with multiple matching dates
  print(unique_plate.SN[unique_plate.SN != 1])
}

# result:
# DNTGF2019 
# named integer(0)
# NTP91 
# named integer(0)
# OPP2015 
# named integer(0)
# PFAS2018 
# named integer(0)
# ToxCast2016 
# named integer(0)

# depreacted output, before I updated for plate overmatching in this loop with paste0("_",x,"_")
# DNTGF2019 
# named integer(0)
# NTP91 
# named integer(0)
# OPP2015 
# named integer(0)
# PFAS2018 
# MW1208-1 
# 2 
# ToxCast2016 
# MW1146-3 MW1147-1 MW1147-2 MW1147-3 
# 3        3        3        2


# --------------------------------------------------------
# okay, so now I am going to check if there are any files where the Plate.SN in the file name is different than
# the plate.SN in the file body
# (I will use what is in the file body as the source of "truth" - this comes from the h5/spike list file name, 3rd tag)
# If i used the updated create_Burst_ont_Data, then I would get the plate.SN for the file name from the h5 file
check_dirs <- c("DNTGF2019","NTP91","OPP2015","PFAS2018","ToxCast2016")
main.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
for(check_dir in check_dirs) {
  cat(check_dir,"\n")
  pfiles <- list.files(file.path(main.dir, check_dir,"prepared_data"), full.names = T)
  for (pfile in pfiles) {
    pdat <- read.csv(pfile)
    filename_date <- strsplit(basename(pfile),split = "_")[[1]][5]
    filename_plate.SN <- strsplit(basename(pfile),split = "_")[[1]][6]
    filename_plate.SN <- sub("\\.csv$","",filename_plate.SN)
    body_date <- unique(pdat$date)
    body_plate.SN <- unique(pdat$Plate.SN)
    if (filename_date != body_date) cat(filename_date, " ", body_date, "\n", sep = "")
    if (filename_plate.SN != body_plate.SN) cat(filename_plate.SN, " ", body_plate.SN, "\n", sep = "")
    rm(pdat)
  }
}

# result:
# DNTGF2019 
# NTP91 
# OPP2015 
# PFAS2018 
# ToxCast2016

# in other words, 
# no files were flagged for Plate or date file and body mismatch

# using same for loop on the deprecated Brown prepared data as confirmation:
check_dirs <- c("Brown2014")
for(check_dir in check_dirs) {
  cat(check_dir,"\n")
  pfiles <- list.files(file.path(main.dir, check_dir,"test_save"), full.names = T)
  for (pfile in pfiles) {
    pdat <- read.csv(pfile)
    filename_date <- strsplit(basename(pfile),split = "_")[[1]][5]
    filename_plate.SN <- strsplit(basename(pfile),split = "_")[[1]][6]
    filename_plate.SN <- sub("\\.csv$","",filename_plate.SN)
    body_date <- unique(pdat$date)
    body_plate.SN <- unique(pdat$Plate.SN)
    if (filename_date != body_date) cat(filename_date, " ", body_date, "\n", sep = "")
    if (filename_plate.SN != body_plate.SN) cat(filename_plate.SN, " ", body_plate.SN, "\n", sep = "")
    rm(pdat)
  }
}

# result:
# Brown2014 
# MW1007-38 MW1007-27
# NA MW1007-38

# yep, these are the mis-matches, as expected.

# Okay, so even with the bug in the code, I did not have any issues with the incorrect Plate being in teh file name
# I guess this is because 1 - no plate.SN's were reused in this data set, and 
# 2 - when I took unique(all plates), the unique function maintained the correct order.
