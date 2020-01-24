# script to check for plate serial numbers that were reused in different culture dates
# these plates should be considered distinct for when we normalize by the platewise median of controls

###################################################################################
# USER INPUT
###################################################################################
num_acsn = 19 # usually 17, plus LDH and AB endpoints
num_wells_per_plate = 48

###################################################################################
# END USER INPUT
###################################################################################

library(tcltk)
mc0_filenames = tk_choose.files(caption = "Select mc0 files to compare apid's")

mc0data_list = lapply(mc0_filenames, function(x) read.csv(x, stringsAsFactors = F))
# combine all into 1 data frame. there might be a better way to do this...
mc0data = list()
for (i in 1:length(mc0data_list)) {
  mc0data = rbind(mc0data, mc0data_list[[i]])
}

plates = unique(mc0data$apid)

# there should be 48 wells per plate, times 19 endpoints
# there could be less than that, if some wells were removed

for (plate in plates) {
  n = nrow(mc0data[mc0data$apid == plate,])
  test_val = num_acsn*num_wells_per_plate
  if (n > test_val) {
    print(paste("There are ",n,"data rows for ",plate,sep = ""))
  }
  else if (n < num_acsn*num_wells_per_plate) {
    print(paste("Only ",n,"data rows for ",plate,sep = ""))
  }
}
