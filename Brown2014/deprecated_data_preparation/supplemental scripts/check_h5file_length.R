# checking the h5 Files for the Brown 2016 data

require('rhdf5')

h5Files <- list.files(path = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/h5files", full.names = T)

for (h5File in h5Files) {
  
  # spikes <- h5read(h5File, '/spikes')
  well <- h5read(h5File, '/well')
  # names <- h5read(h5File, '/names') # every electrode that spiked at least once?
  # sCount <- h5read(h5File, '/sCount') # same length as names - I think lists number of spikes on ea electrode in names
  treatment<-h5read(h5File, '/treatment')
  dose<-h5read(h5File, '/dose')
  units<-h5read(h5File, '/units')
  
  if(! all(sapply(list(well, treatment, dose, units), function(x) length(x) == 48)) ) {
    print(paste0("something is off for ",h5File))
  }
  
}
# [1] "something is off for L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/h5files/ON_20140212_MW1007-27_DIV05_001.h5"
# [1] "something is off for L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/h5files/ON_20140212_MW1007-27_DIV07_001.h5"
# [1] "something is off for L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/h5files/ON_20140212_MW1007-27_DIV09_001.h5"

# okay, so good to know that these are the only problem files


h5File <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/h5files/ON_20140212_MW1007-27_DIV05_001.h5"
well <- h5read(h5File, '/well')
treatment<-h5read(h5File, '/treatment')
dose<-h5read(h5File, '/dose')
units<-h5read(h5File, '/units')
length(well) # 50
length(treatment)
length(dose)
length(units)
# all 50
meta.df <- data.frame(well, treatment, dose, units)
head(meta.df)
# well     treatment dose units
# 1   A1 Acetaminophen    0    uM
# 2   A1 Acetaminophen    0    uM
# 3   A2 Acetaminophen  0.1    uM
# 4   A2 Acetaminophen  0.1    uM
# 5   A3 Acetaminophen  0.3    uM
# 6   A4 Acetaminophen    1    uM
# yep, it is just those first 2 wells that got duplicated

unique(meta.df) # this method does indeed take the "unique" wells, and doesn't do any funky re-arranging


# ------------------------ in the ontogeny scripts
library(sjemea)
library(meadq)

s=h5.read.spikes( h5File )
# get meta-data
g=c(); #remove any information stored in g
g<-h5read(path.expand( h5File ), name = "/")
names(g)<-tolower(names(g))
names(g)[names(g)=="scount"]<-"sCount"
s$cw = substring(s$channel,1,2 )
s$treatment = g$treatment         
names(s$treatment) = g$well
s$dose = g$dose
names(s$dose) = g$well
s$sCount = g$sCount
names(s$sCount) = g$names
s$units = g$units
names(s$units) = g$well # appears to list all the wells, regardless of whether or not they spiked
s$well = g$well 
s$summary.table = g$summary.table
s$array = g$array     
s$DIV = strsplit( basename(s$file), split="_")[[1]][4]

# okay, so still have the 50 rows of meta data.
length(s$channels) # 104 - I'm guessing this is each spike electrode?
names <- h5read(h5File, '/names') # these look identical
s$spikes # a list of 104. Gives list of the spike times of each electrode. Nothing for A seems to be duplicated
length(s$nspikes) # 104
s$NCells # just the value 104
s$layout # interesting... but again, doesn't look like any A well data is duplicated. Looks like the coordinates of ea electrode in teh whole plate??
length(s$rates) # 4
s$rates # large matricies here. I would assume that anythign here is at teh electrode level, not the well level. And it looks like teh electrode book-keeping has been un affected
s$cw # this is based on the well portion of the electrode names in "channels", so this should be good
length(s$treatment) # yep, 50
S$well; s$dose; s$units # all lists of 50

# most of the sjemea scripts calculate things at the electrode level. the well-level values are calculated in the script create_burst_ont_Data.R
# how the trt, dose, etc data is collected into the data frame
well.names<-unique( subset(s[[cur.file]]$cw, ae.index.v) ) #this will be a unique list for ea well with spiking electrodes
well = well.names
trt = s[[cur.file]]$treatment[ well.names ]
dose = s[[cur.file]]$dose[ well.names ]
units = s[[cur.file]]$units[ well.names ]

# quick test:
s$treatment[c("A1","A2","A3","B4")]
# A1                     A2                     A3                     B4 
# "Acetaminophen"        "Acetaminophen"        "Acetaminophen" "Bisindolymaleimide 1" 
# wow! so even though the A1 appears twice in s$treatment, only 1 value is returned when you subset like this. Yay!

# Okay! So even if for soem of the endpoint we had duplicated A1 or A2 well values, we would only get 1 copy if it
# because the script create_burst_ont_Data.R always subsets on [well.names] for each endpoint

# I was worried that there were problems in the spike channel name indexing, which would really mess up the calculations. But that does not appear to be the case
