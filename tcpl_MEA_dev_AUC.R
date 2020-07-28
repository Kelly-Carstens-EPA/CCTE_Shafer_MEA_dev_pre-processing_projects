###################################################################################
# USER INPUT
###################################################################################
# set working directory for where the output file will go
setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing")
# set the output file name
filename = "mc0_test_apid_update.csv"
AUCsourcefilename = "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing/burst_param_colreorder_confirmation/PFAS_2018_AUC_update_test.csv"
cytotox_filename = "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing/test_updates.csv"
default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c("3","11") # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Enter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c("Lava","Water") # e.g. c("Water", "Water")
###################################################################################
# END USER INPUT
###################################################################################

# load libraries
library(data.table)

## read in the data
AUC <- fread(AUCsourcefilename)
cytotox_data <- fread(cytotox_filename)

## rename columns before melting
names(AUC)[names(AUC) == 'dose'] <- "conc"
AUC[, apid := paste(date, plate.SN, sep = "_")]
cytotox_data[, apid := paste(date, plate.SN, sep = "_")]

## split well_id to rows and columns
## Defining coli
AUC$coli <- substring(AUC$well, 2, 2) # could use as.numeric, as leave as character

## Defining rowi
AUC$rowi <- substring(AUC$well, 1, 1)
AUC[rowi == 'A', rowi := '1']
AUC[rowi == 'B', rowi := '2']
AUC[rowi == 'C', rowi := '3']
AUC[rowi == 'D', rowi := '4']
AUC[rowi == 'E', rowi := '5']
AUC[rowi == 'F', rowi := '6']

# add index
AUC$ID <- seq.int(nrow(AUC))
# remove unneeded columns
AUC_smaller <- AUC[, -c("units", "well")]
#names(AUC_smaller)[names(AUC_smaller) == "trt"] = "treatment"
# reshape - all of the columns not listed become rows. 
# parameter names become a column "variable", and corresponding values are under "value"
AUC_smaller_melted <- melt(AUC_smaller, id = c("date","plate.SN","apid","treatment","ID", "rowi", "coli", "conc"), 
                           variable.name = "acsn", value.name = "rval", variable.factor = FALSE)

# add srcf , acsn, wllt, wllq
AUC_smaller_melted$wllt = "t"
AUC_smaller_melted$wllq = 1
AUC_smaller_melted$srcf <- basename(AUCsourcefilename)

# remove unneeded columns
usecols <- c("apid","treatment","rowi","coli","wllt","wllq","conc","rval", "srcf","acsn")
AUC_smaller_melted = AUC_smaller_melted[, ..usecols]
# remove columns and make sure columns in same order
cytotox_data = cytotox_data[, ..usecols]
mc0_data = rbind(AUC_smaller_melted, cytotox_data)

# change untreated wells to Control Treatment
compoundlist = unique(mc0_data$treatment)
for (compound in compoundlist) {
  if (is.element(compound, different_vehicleControlCompounds)) {
    # then assign treatment column to corresponding value in vehicle control list
    vehicle_controli <- different_vehicleControls[which(different_vehicleControlCompounds == compound)]
    mc0_data[treatment == compound & conc == 0, treatment := vehicle_controli]
  } else {
    mc0_data[treatment == compound & conc == 0, treatment := default_ControlTreatmentName]
  }
}

# change wllt for untreated wells to n
mc0_data[ conc == 0, wllt := "n"]

fwrite(mc0_data, file = filename, row.names = FALSE, sep = ",")
cat(filename, "is ready\n")