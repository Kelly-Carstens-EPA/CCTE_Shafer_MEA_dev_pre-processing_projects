# investigating these files!
file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv"
data.raw<-read.csv(file,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
# confirm that this file contains teh outlier electrodes
# confirm that this file is not BIC recording

# using this instead of header=T, because sometimes the colnames are not in the first row in the spike list file
header_row <- which(data.raw[,1] == "Time (s)")
colnames(data.raw) <- data.raw[header_row,]
data.raw <- data.raw[-which(data.raw[,1] == "Time (s)"),]
data.raw <- data.raw[data.raw$Electrode != "",] # works even if no rows in Electrode are == ""
data.raw<-data.raw[1:(nrow(data.raw)-40),]
data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
data.raw[,3]<-as.numeric(as.character(data.raw[,3]))

data.raw[data.raw$Electrode == "A5_31"] # empty
data.raw[data.raw$Electrode %in% c("A5_31","C1_21","C1_41","E1_23","E3_31","E6_33")] 
# empty! This is not our file

check_info <- read.csv(file, header = F, colClasses = c("character","character","NULL","NULL","NULL"), nrows = 100)
# 6                   Original File Time                       10/28/2019 9:55                                                                      
# 7                   Sampling Frequency                              12.5 kHz                                                                      
# 8                        Voltage Scale        -5.48486178148311E-08 V/sample                                                                      
# 9                Experiment Start Time                       10/28/2019 9:26
data.raw_u_head <- data.raw[1:500,] # save top slice


# comparing with this secodn DIV 12 file
file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(001)_spike_list.csv"
data.raw<-read.csv(file,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor

# using this instead of header=T, because sometimes the colnames are not in the first row in the spike list file
header_row <- which(data.raw[,1] == "Time (s)")
colnames(data.raw) <- data.raw[header_row,]
data.raw <- data.raw[-which(data.raw[,1] == "Time (s)"),]
data.raw <- data.raw[data.raw$Electrode != "",] # works even if no rows in Electrode are == ""
data.raw<-data.raw[1:(nrow(data.raw)-40),]
data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
data.raw[,3]<-as.numeric(as.character(data.raw[,3]))

data.raw[data.raw$Electrode == "A5_31",] # many rows!
sapply(c("A5_31","C1_21","C1_41","E1_23","E3_31","E6_33"), function(x) nrow(data.raw[data.raw$Electrode == x,]))
# A5_31 C1_21 C1_41 E1_23 E3_31 E6_33 
# 16043  5424  5121  5884  8942  7149 
# sweet! Now to determien if this is just the BIC

check_info <- read.csv(file, header = F, colClasses = c("character","character","NULL","NULL","NULL"), nrows = 100)
# 6                    Original File Time                   10/28/2019 09:55:21
# 7                    Sampling Frequency                              12.5 kHz
# 8                         Voltage Scale        -5.48486178148311E-08 V/sample
# 9                 Experiment Start Time                   10/28/2019 09:26:37
# interesting! this has the exact same Experiment start time as the "modified" spike list file

# okay, so the real way to check would be to see if the spikes line up for the electrodes that are not removed
data.raw_o_head <- data.raw[1:500,]

# now compare! Using first 444 rows, because that is how many rows in data.raw_o_head have electrodes in the modified file
all.equal(data.raw_o_head[data.raw_o_head$Electrode %in% unique(data.raw_u_head$Electrode),],
          data.raw_u_head[1:444,])
# [1] "Length mismatch: comparison on first 3 components"
# (hmmm...)
all.equal(data.raw_o_head[data.raw_o_head$Electrode %in% unique(data.raw_u_head$Electrode),"Time (s)"],
          data.raw_u_head[1:444,"Time (s)"])
# TRUE
all.equal(data.raw_o_head[data.raw_o_head$Electrode %in% unique(data.raw_u_head$Electrode),"Electrode"],
          data.raw_u_head[1:444,"Electrode"])
# TRUE
all.equal(data.raw_o_head[data.raw_o_head$Electrode %in% unique(data.raw_u_head$Electrode),"Amplitude(mV)"],
          data.raw_u_head[1:444,"Amplitude(mV)"])
# TRUE... huh

tail(data.raw_o_head[data.raw_o_head$Electrode %in% unique(data.raw_u_head$Electrode),])
# Time (s) Electrode Amplitude(mV)
# 496  698.534     B4_23         0.030
# 497  698.534     B4_13         0.020
# 498  698.534     B4_21         0.018
# 499  698.536     B4_34         0.051
# 500  698.537     B4_21         0.029
# 501  698.537     B4_22         0.033
tail(data.raw_u_head[1:444,])
# Time (s) Electrode Amplitude(mV)                          
# 496  698.534     B4_23         0.030                          
# 497  698.534     B4_13         0.020                          
# 498  698.534     B4_21         0.018                          
# 499  698.536     B4_34         0.051                          
# 500  698.537     B4_21         0.029                          
# 501  698.537     B4_22         0.033

# alright! Not sure why I get the mismatch when I compare all 3 col's
# but with individual cols and in chunks I can see the spike times, elect's and amp;s are indentical!!

# which file did Melissa use? I see it was modified in 12/20/2019...
file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/All Spike List Files/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv"
data.raw<-read.csv(file,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor

# using this instead of header=T, because sometimes the colnames are not in the first row in the spike list file
header_row <- which(data.raw[,1] == "Time (s)")
colnames(data.raw) <- data.raw[header_row,]
data.raw <- data.raw[-which(data.raw[,1] == "Time (s)"),]
data.raw <- data.raw[data.raw$Electrode != "",] # works even if no rows in Electrode are == ""
data.raw<-data.raw[1:(nrow(data.raw)-40),]
data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
data.raw[,3]<-as.numeric(as.character(data.raw[,3]))

data.raw[data.raw$Electrode == "A5_31",] # many rows!
sapply(c("A5_31","C1_21","C1_41","E1_23","E3_31","E6_33"), function(x) nrow(data.raw[data.raw$Electrode == x,]))
# A5_31 C1_21 C1_41 E1_23 E3_31 E6_33 
# 16042  5424  5121  5883  8941  7149 
# almost same as above, just a few are missing a single spike
# she probably chopped at 900 seconds
head(data.raw)
# Time (s) Electrode Amplitude(mV)                          
# 2  0.00000     E1_13         0.013                          
# 3  0.00248     A5_34         0.046                          
# 4  0.00320     D6_42         0.030                          
# 5  0.00352     B2_32         0.024                          
# 6  0.00904     A8_43         0.014                          
# 7  0.01224     A2_23         0.019
# ya, she started this at 0 seconds.
tail(data.raw)

# okay, so options:
# - use this file from Melissa
# - use the file from Modified folder
# I don't like manual modifications, so I will use this file from the "Modified" folder which was not actually modified