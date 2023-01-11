# checking out the data that is present for Jasmine's 2016 data
library(data.table)
final_dat <- fread("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Prepared Data (Brown et al. 2016)/Final_Data_Set_SA1_DNT_Paper1 (2)(updated).csv")

unique(final_dat$date)
# 20140205 20140212 20140402 20140423 20140716 20140730 (6 / 8 cultures in the Specific Aim 1 folder)

unique(final_dat$Plate.SN)
# "MW1007-26"  "MW1007-27"  "MW1007-38"  "MW1007-104" Okay... so this was back in the day when they re-used plates!

unique(final_dat$DIV)
# 5  7  9 12 Intersting, no DIV 2, no added anomalies

final_dat[, list(DIVs = paste0(paste0(sort(unique(DIV))), collapse = ",") ), by = c("date","Plate.SN")]
# date   Plate.SN     DIVs
# 1: 20140205  MW1007-26 5,7,9,12
# 2: 20140212  MW1007-27 5,7,9,12
# 3: 20140402  MW1007-27 5,7,9,12
# 4: 20140423  MW1007-38 5,7,9,12
# 5: 20140716  MW1007-26 5,7,9,12
# 6: 20140730 MW1007-104   5,7,12

# okay, so 1007-104 is missing DIV 9. Is the h5 file missing and/or spike list file?
# no h5file or spike list file in the culture drive
# Let's see what this rData file has
load("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/20140730 Ontogeny/h5Files/s_20140730_MW1007-104_thru12.Rdata",verbose=T)
str(s)
length(s) # only 3 -> 3 h5 files used as input :(

# but, there is a .map and .mapTimeStamp file for DIV 9... hmm, this pipelien looks interesting
# okay, so I could use the script convert)mapTimestamp_to_h5_MetaData.R function, which calls map2list function
# BUT, the maptimestamps file is reeeeeealy small, like 1 KB
# Hmm, so the DIV 2 maptimstamps file is only 2 KB. But there is no h5 file for DIV 2
# Perhaps there was so little activity on DIV 9, and those values shoudl be zero...
# but more likely, there was not a full recording of 15min, so there is essentially no DIV 9 data

final_dat[Plate.SN == "MW1007-104",unique(trt)] # "Bisindolymaleimide 1" "Glyphosate"           "Sodium Orthovanadate"

# 4 of the compounds were tested 4 times, but Bis 1 and SO were tested 8 times - once per plate, then doubling-up on the last 2 plates
final_dat[, list(trts = paste0(paste0(sort(unique(trt))), collapse = ",") ), by = c("date","Plate.SN")]
# date   Plate.SN                                                                                      trts
# 1: 20140205  MW1007-26 Acetaminophen,Bisindolymaleimide 1,Domoic Acid,Loperamide,Mevastatin,Sodium Orthovanadate
# 2: 20140212  MW1007-27 Acetaminophen,Bisindolymaleimide 1,Domoic Acid,Loperamide,Mevastatin,Sodium Orthovanadate
# 3: 20140402  MW1007-27 Acetaminophen,Bisindolymaleimide 1,Domoic Acid,Loperamide,Mevastatin,Sodium Orthovanadate
# 4: 20140423  MW1007-38 Acetaminophen,Bisindolymaleimide 1,Domoic Acid,Loperamide,Mevastatin,Sodium Orthovanadate
# 5: 20140716  MW1007-26                                      Bisindolymaleimide 1,Glyphosate,Sodium Orthovanadate
# 6: 20140730 MW1007-104                                      Bisindolymaleimide 1,Glyphosate,Sodium Orthovanadate

# any other wells missing or unexpected num of rows?
final_dat[, list(num_data_rows = .N, num_DIV = length(unique(DIV))), by = c("date","Plate.SN")]
# date   Plate.SN num_data_rows num_DIV
# 1: 20140205  MW1007-26           192       4
# 2: 20140212  MW1007-27           192       4
# 3: 20140402  MW1007-27           192       4
# 4: 20140423  MW1007-38           192       4
# 5: 20140716  MW1007-26           136       4
# 6: 20140730 MW1007-104           102       3

# all Glyposate wells removed?
final_dat[Plate.SN == "MW1007-26" & date == "20140716" & trt == "Glyphosate"] # only in wells A1 and D1, at conc==0
final_dat[Plate.SN == "MW1007-26" & date == "20140716", length(unique(well))] # 34. 34 wells x 4 DIV = 136 data rows
final_dat[Plate.SN == "MW1007-26" & date == "20140716", sort(unique(well))] # missing A2:A8, D2:D8
# right, so the glyphyosate-treated wells were removed

final_dat[Plate.SN == "MW1007-104" & date == "20140730" & trt == "Glyphosate"] # again, just control wells in rows of glypho
final_dat[Plate.SN == "MW1007-104" & date == "20140730", length(unique(well))] # 34. 34 wells x 3 DIV = 102 data rows

# so no other data appears to be removed or missed. Only:
# - glyphosate-treated wells removed from last 2 plates
# - DIV 9 data for MW1007-104 is missing

# 08/28/2020
# checking out this similar file that I downloaded, versus took off the L drive
library(data.table)
pubfile <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Brown2016/Published Downloaded Data/Final_Data_Set_SA1_DNT_Paper1 (2)(updated)_CF.csv")
setDT(pubfile)
pubfile[, .(paste0(sort(unique(DIV)),collapse=",")),by = c("date","Plate.SN")]
# date   Plate.SN       V1
# 1: 20140205  MW1007-26 5,7,9,12
# 2: 20140212  MW1007-27 5,7,9,12
# 3: 20140402  MW1007-27 5,7,9,12
# 4: 20140423  MW1007-38 5,7,9,12
# 5: 20140716  MW1007-26 5,7,9,12
# 6: 20140730 MW1007-104   5,7,12
# same as above

# treatments?
pubfile[, .(paste0(sort(unique(trt)),collapse=",")),by = c("date","Plate.SN")]

# any wells removed?
pubfile[, .N,by = c("date","Plate.SN","DIV")]
# date   Plate.SN DIV  N
# 1: 20140205  MW1007-26   5 46
# 2: 20140205  MW1007-26   7 46
# 3: 20140205  MW1007-26   9 46
# 4: 20140205  MW1007-26  12 46
# 5: 20140212  MW1007-27   5 47
# 6: 20140212  MW1007-27   7 47
# 7: 20140212  MW1007-27   9 47
# 8: 20140212  MW1007-27  12 47
# 9: 20140402  MW1007-27   5 48
# 10: 20140402  MW1007-27   7 48
# 11: 20140402  MW1007-27   9 48
# 12: 20140402  MW1007-27  12 48
# 13: 20140423  MW1007-38   5 48
# 14: 20140423  MW1007-38   7 48
# 15: 20140423  MW1007-38   9 48
# 16: 20140423  MW1007-38  12 48
# 17: 20140716  MW1007-26   5 33
# 18: 20140716  MW1007-26   7 33
# 19: 20140716  MW1007-26   9 33
# 20: 20140716  MW1007-26  12 33
# 21: 20140730 MW1007-104   5 34
# 22: 20140730 MW1007-104   7 34
# 23: 20140730 MW1007-104  12 34
# yes, several wells have been removed.


# 10/08/2020
# There are 2 copies of the spike list files for 20140423
# I want to check if there is a difference between the data in these files

dir1 <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423"
dir2 <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1"
files1 <- list.files(dir1,full.names = T, pattern = "_spike_list")
files2 <- list.files(dir2,full.names = T, pattern = "_spike_list")

prep_data <- function(file) {
  data.raw<-read.csv(file,header=F,colClasses=c("character", "NULL", "character","character","character")) # make electrode column char, not factor
  
  # remove the rows after the "Well Information" tag
  # adapted from "CG Additions 1/13/201"
  well_information_row <- which(data.raw$V1 == "Well Information")
  if (length(well_information_row) == 0) {
    cat("Note to Amy: 'Well Information' tag phrase not present in",file,"\n")
    well_information_row <- nrow(data.raw) + 1
  }
  data.raw <- data.raw[1:(well_information_row - 1),]
  
  # remove first column and get the header
  # (using this instead of header=T, because sometimes the colnames are not in the first row in the spike list file)
  data.raw <- data.raw[, c(2,3,4)]
  header_row <- which(data.raw[,1] == "Time (s)")
  colnames(data.raw) <- data.raw[header_row,]
  data.raw <- data.raw[-c(header_row),]
  
  # remove any remaning empty rows, then convert to time and amplitude to numeric
  data.raw <- data.raw[data.raw$Electrode != "",] # works even if no rows in Electrode are == ""
  data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
  data.raw[,3]<-as.numeric(as.character(data.raw[,3]))
  return(data.raw)
}

for (i in 1:length(files1)) {
  data.raw1 <- prep_data(files1[i])
  data.raw2 <- prep_data(files2[i])
  print(all.equal(data.raw1, data.raw2))
  rm(list = c("data.raw1","data.raw2"))
}

# output:
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_02_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_05_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_07_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_09_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_12_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE
# Note to Amy: 'Well Information' tag phrase not present in L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_12_01(000)_Spike Detector (8 x STD)(000)_spike_list.csv 
# [1] TRUE

# (there are 6 file sin each foldre, for DIV 2 - 12 + 12_01)
# Aweseome! So it doesn't mattter which spike list files I choose.
# The only difference in teh files is that the files from dir2 do not have the "WEll Information"
# (hence the "Note to Amy."But, once that part is stippped away, the actual data values are the same