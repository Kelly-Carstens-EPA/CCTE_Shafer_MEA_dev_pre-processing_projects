###################################################################################
# USER INPUT
###################################################################################
# set working directory as the location of the source file
# this is also where the output file will go
setwd("L:/Lab/NHEERL_MEA/Frank 86 tcpl prep/Intermediate Output/")
# set the output file name
outfile = "MEA_Frank_mc0.csv"
AUCsourcefilename = "L:/Lab/NHEERL_MEA/Frank 86 tcpl prep/Intermediate Output/Frank86_AUC_2.csv"
cytotox_filename = "L:/Lab/NHEERL_MEA/Frank 86 tcpl prep/Intermediate Output/Frank86_cytotoxicity_2.csv"
default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Eneter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c() # e.g. c("Water", "Water")
###################################################################################
# END USER INPUT
###################################################################################

# load libraries
library(data.table)
#library(tcpl)
library(reshape)
## read in the data
AUC <- fread(AUCsourcefilename)
cytotox_data = read.csv(cytotox_filename)

# make data table for simplicity
setDT(AUC)
## prepare/reshape the AUC raw data for tcplLite
## extract components
components <- names(unique(AUC[, 7:ncol(AUC)])) # replaced 23 with ncol(AUC)

## rename columns before melting
names(AUC)[names(AUC) == 'dose'] <- "conc"
names(AUC)[names(AUC) == 'plate.SN'] <- "apid"

## split well_id to rows and columns
## Defining coli
# library(stringr)
# numextract <- function(string){ 
#   str_extract(string, "\\-*\\d+\\.*\\d*")
# } 
# 
# AUC[, coli := numextract(well)]
# function above might be faster, but this is only one line of code
AUC$coli <- substring(AUC$well, 2, 2) # could use as.numeric, as leave as character

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
AUC_smaller <- AUC[, -c("date", "units", "well")]
#names(AUC_smaller)[names(AUC_smaller) == "trt"] = "treatment"
# reshape - all of the columns not listed become rows. 
# parameter names become a column "variable", and corresponding values are under "value"
AUC_smaller_melted <- melt(AUC_smaller, id = c("treatment", "apid", "ID", "rowi", "coli", "conc"))
# rename response column
names(AUC_smaller_melted)[names(AUC_smaller_melted) == 'value'] <- "rval"
# add srcf , acsn, wllt, wllq
AUC_smaller_melted[, acsn := variable]
AUC_smaller_melted$wllt = "t"
AUC_smaller_melted$wllq = 1
AUC_smaller_melted[, srcf := AUCsourcefilename]


# remove unneeded columns
AUC_smaller_melted = AUC_smaller_melted[, c("treatment","apid","rowi","coli","wllt","wllq","conc","rval", "srcf", "acsn")]
# remove columns and make sure columns in same order
cytotox_data = cytotox_data[, c("treatment","apid","rowi","coli","wllt","wllq","conc","rval", "srcf", "acsn")]

# rbind the cytotox data
mc0_data = rbind(AUC_smaller_melted, cytotox_data)

# change untreated wells to Control Treatment
compoundlist = unique(mc0_data$treatment)
for (compound in compoundlist) {
  if (is.element(compound, different_vehicleControlCompounds)) {
    # then assign treatment column to corresponding value in vehicle control list
    mc0_data[(mc0_data$treatment == compound)&(mc0_data$conc == 0), "treatment"] = different_vehicleControls[which(different_vehicleControlCompounds == compound)]
  } else {
    mc0_data[(mc0_data$treatment == compound)&(mc0_data$conc == 0), "treatment"] = default_ControlTreatmentName
  }
}

# change wllt for untreated wells to n
mc0_data[ conc == 0, wllt := "n"] # I changed AUC_smaller_melted to mc0_data
stop()
write.table(mc0_data, file = outfile, row.names = FALSE, sep = ",")
cat(outfile, "is ready\n")