# Checking where to get the spike list files where outlier ectrodes have not been removed
# 08/25/2020

# checking if these 2 spikes list files are identical
div5_1 <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/MW1207-43/Spike List Files/ON_20181017_MW1207-43_05_00(000)_spike_list.csv")
nrow(div5_1)
head(div5_1)
length(which(div5_1$Electrode == "A3_34")) # 0
length(which(div5_1$Electrode == "A3_11"))
sort(unique(div5_1$Electrode))
rm(div5_1) # f2_21 is not here either

div7 <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv")
file.info("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv")
# size isdir mode
# /MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 15284838 FALSE  666
# mtime
# /MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-10-24 13:20:27
# ctime
# /MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-10-24 13:16:54
# atime exe
# /MW1207-43/Spike List Files/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-10-24 13:20:27  no
length(which(div7$Electrode == "A2_44"))
sort(unique(div7$Electrode))
length(which(div7$Electrode == "")) # 65209. #woah, that's a lot! is that just at the end of the file, or interspersed?
which(div7$Electrode == "")[1:50] # Wow, these are not all at the end of the file!
# It really looks like some lines may have been removed where there are outlier electrodes!
head(div7[, 3:5], n = 25)
# just gonna trim this a bit for memory
div7 <- div7[1:1000,]


div7_o <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/Original Spike Lists/ON_20181017_MW1207-43_07_00(000)_spike_list.csv")
# File 
file.info("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/Original Spike Lists/ON_20181017_MW1207-43_07_00(000)_spike_list.csv")
# size isdir mode
# /ON_20181017_MW1207-43_07_00(000)_spike_list.csv 9376692 FALSE  666
# mtime
# /Original Spike Lists/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-10-24 09:00:31
# ctime
# /Original Spike Lists/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-11-28 11:56:18
# atime exe
# /Original Spike Lists/ON_20181017_MW1207-43_07_00(000)_spike_list.csv 2018-11-28 11:56:18  no

length(which(div7_o$Electrode == "A2_44"))# 2708. So these are present here!!!
length(which(div7_o$Electrode == "A3_11")) # not 0!
length(which(div7_o$Electrode == "C2_12")) # not 0!
length(which(div7_o$Electrode == "F7_12")) # not 0!
length(which(div7_o$Electrode == "")) # 31
which(div7_o$Electrode == "") # ahh, looks like all at the end of the file
tail(div7_o, n = 25)
rm(list=ls())

# Okay, so! I have confirmed that I should use teh files in the "Original Spike Lists", not the files in teh individual plate folders.
# But, unfortunately, not every culture has a folder of all of the "Original Spike Lists"
# So, I will check if the spike list files in the plate folders are correct for those cultures

# Checking each culure --------------------------------------------------------------------------

# 20180801 Culture PFAS Group_1
# checking the electrodes listed in the text file "Electrodes removed" under the csv Files folder
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-22/Spike List Files/ON_20180801_MW1207-22_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A5_12")) # 821
length(which(plate_file$Electrode == "A7_33")) # 604
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # yep, looks like the end of the file
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-22/Spike List Files/ON_20180801_MW1207-22_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A4_14")) # not 0!
length(which(plate_file$Electrode == "D5_42")) # not 0!
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # yep, looks like the end of the file
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-23/Spike List Files/ON_20180801_MW1207-23_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "F4_34")) # 2491
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # yep, looks like the end of the file
rm(plate_file)
# I am confident using the spike list files in the original plate folders for this culture

# 20180815 Culture PFAS Group_2-1
# checking the electrodes listed in the text file "Electrodes removed" or "Atypical Electrodes"
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/MW1207-24/Spike List Files/ON_20180815_MW1207-24_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A3_21")) # 2637
length(which(plate_file$Electrode == "D4_12")) # 1999
length(which(plate_file$Electrode == "F7_22")) # 1947
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # yep, looks like the end of the file
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/MW1207-27/Spike List Files/ON_20180815_MW1207-27_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A1_22")) # not 0!
length(which(plate_file$Electrode == "C5_31")) # not 0!
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # yep, looks like the end of the file
rm(plate_file)

# yep, I'm confident that the spike list files in the Plate folders do not have outlier electrodes remvoed
# which is good, because the Original spike list files aren't anywhere else in this folder


# 20180905 Culture PFAS Group 2-2
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/MW1207-30/Spike List Files/ON_20180905_MW1207-30_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A1_22")) # 0
length(which(plate_file$Electrode == "B4_32")) # 0
length(which(plate_file$Electrode == "C7_12")) # 0
length(which(plate_file$Electrode == "D5_24")) # 0
length(which(plate_file$Electrode == "E1_12")) # 0
length(which(plate_file$Electrode == "")) # 5
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

org_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/Original Spike Lists/ON_20180905_MW1207-30_05_00(000)_spike_list.csv")
length(which(org_file$Electrode == "A1_22")) # not 0!
length(which(org_file$Electrode == "B4_32")) # not 0!
length(which(org_file$Electrode == "C7_12")) # not 0!
length(which(org_file$Electrode == "D5_24")) # not 0!
length(which(org_file$Electrode == "E1_12")) # not 0!
length(which(org_file$Electrode == "")) # 31
which(org_file$Electrode == "")
rm(org_file)
# so yep, need to use "Original Spike List" file here

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/MW1207-33/Spike List Files/ON_20180905_MW1207-33_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A6_22")) # 0
length(which(plate_file$Electrode == "A6_41")) # 0
length(which(plate_file$Electrode == "B7_44")) # 0
length(which(plate_file$Electrode == "C5_22")) # 0
length(which(plate_file$Electrode == "")) # 5
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

org_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/Original Spike Lists/ON_20180905_MW1207-33_05_00(002)_spike_list.csv")
length(which(org_file$Electrode == "A6_22")) # not 0!
length(which(org_file$Electrode == "A6_41")) # not 0!
length(which(org_file$Electrode == "B7_44")) # not 0!
length(which(org_file$Electrode == "C5_22")) # not 0!
length(which(org_file$Electrode == "")) # 31
which(org_file$Electrode == "") # def end of file
rm(org_file)

# okay, just want to check div 7 as well
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/MW1207-33/Spike List Files/ON_20180905_MW1207-33_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A3_34")) # 0
length(which(plate_file$Electrode == "B1_13")) # 0
length(which(plate_file$Electrode == "F1_31")) # 0
length(which(plate_file$Electrode == "")) # 5
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

org_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180905 Culture PFAS Group 2-2/Original Spike Lists/ON_20180905_MW1207-33_07_00(000)_spike_list.csv")
length(which(org_file$Electrode == "A3_34")) # not 0!
length(which(org_file$Electrode == "B1_13")) # not 0!
length(which(org_file$Electrode == "F1_31")) # not 0!
length(which(org_file$Electrode == "")) # 31
which(org_file$Electrode == "") # def end of file
rm(org_file)
# Def use files in Original spike lists files folder for this culture!


# 20180912 Culture PFAS Group 3-1
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180912 Culture PFAS Group 3-1/MW1207-39/Spike List Files/ON_20180912_MW1207-39_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A8_32")) # 0
length(which(plate_file$Electrode == "B5_42")) # 0
length(which(plate_file$Electrode == "C2_41")) # 0
length(which(plate_file$Electrode == "F4_11")) # 0
length(which(plate_file$Electrode == "")) # 5
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

# from original files folder
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180912 Culture PFAS Group 3-1/OriginalFiles/MW1207-39/ON_20180912_MW1207-39_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A8_32")) # not 0!
length(which(plate_file$Electrode == "B5_42")) # not 0!
length(which(plate_file$Electrode == "C2_41")) # not 0!
length(which(plate_file$Electrode == "F4_11")) # not 0!
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180912 Culture PFAS Group 3-1/MW1207-42/Spike List Files/ON_20180912_MW1207-42_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "C1_21")) # 0
length(which(plate_file$Electrode == "D7_23")) # 0
length(which(plate_file$Electrode == "F3_42")) # 0
length(which(plate_file$Electrode == "F5_13")) # 0
length(which(plate_file$Electrode == "")) # 21804
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180912 Culture PFAS Group 3-1/OriginalFiles/MW1207-42/ON_20180912_MW1207-42_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "C1_21")) # not 0!
length(which(plate_file$Electrode == "D7_23")) # not 0!
length(which(plate_file$Electrode == "F3_42")) # not 0!
length(which(plate_file$Electrode == "F5_13")) # not 0!
length(which(plate_file$Electrode == "")) # 31
which(plate_file$Electrode == "") # huh, this does look liek the end of the file
rm(plate_file)
# Will use files in "OriginalFiles" folder

# 20181017 Culture PFAS Group 3-2
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/MW1207-43/Spike List Files/ON_20181017_MW1207-43_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A3_34")) # 0
length(which(plate_file$Electrode == "F2_21")) # 0
length(which(plate_file$Electrode == "")) # many
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2/Original Spike Lists/ON_20181017_MW1207-43_05_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A3_34")) # mane
length(which(plate_file$Electrode == "F2_21")) # many
length(which(plate_file$Electrode == "")) # 31
rm(plate_file)
# will use files in "Original Spike Lists"

# 20181024 Culture PFAS Group 4-1
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181024 Culture PFAS Group 4-1/MW1208-8/Spike List Files/ON_20181024_MW1208-8_07_00(001)_spike_list.csv")
length(which(plate_file$Electrode == "F6_32")) # 0
length(which(plate_file$Electrode == "")) # many
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181024 Culture PFAS Group 4-1/Original Spike Lists/ON_20181024_MW1208-8_07_00(001)_spike_list.csv")
length(which(plate_file$Electrode == "F6_32")) # 2626
length(which(plate_file$Electrode == "")) # 31
rm(plate_file)
# will use files in "Original Spike Lists"

# 20181114 Culture PFAS Group 4-2
plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181114 Culture PFAS Group 4-2/MW1230-53/ON_20181114_MW1230-53_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A1_22")) # 0
length(which(plate_file$Electrode == "A2_12")) # 0
length(which(plate_file$Electrode == "E4_13")) # 0
length(which(plate_file$Electrode == "")) # many
rm(plate_file)

plate_file <- read.csv("L:/Lab/NHEERL_MEA/Project PFAS 2018/20181114 Culture PFAS Group 4-2/Original Spike Lists/ON_20181114_MW1230-53_07_00(000)_spike_list.csv")
length(which(plate_file$Electrode == "A1_22")) # mean
length(which(plate_file$Electrode == "A2_12")) # many
length(which(plate_file$Electrode == "E4_13")) # many
length(which(plate_file$Electrode == "")) # 31
rm(plate_file)
# will use files in "Original Spike Lists"
