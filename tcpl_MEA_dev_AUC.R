# this script is not fully functional
# - need to fix check_unique_apid.R section

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
check_unique_apid = TRUE # recommended. This will check that plate ID's have not been used previously
# compares with data located here L:\Lab\NHEERL_MEA\tcpl_nheerl_mea_dev\source_files
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

# use check_unique_apid.R to confirm that all apid's are unique for each culture date
# source("check_unique_apid.R")

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

if (check_unique_apid) {
  
  num_acsn = 19 # usually 17, plus LDH and AB endpoints
  num_wells_per_plate = 48
  cur_plates = unique(mc0_data$apid)
  
  # first, check for re-used plate ID in current data set
  for (cur_plate in cur_plates) {
    n = nrow(mc0_data[apid == cur_plate,])
    test_val = num_acsn*num_wells_per_plate
    if (n > test_val) {
      print(paste("There are ",n,"data rows for ",plate,sep = ""))
      
      # need to rename some
      
      
    }
    else if (n < num_acsn*num_wells_per_plate) {
      # Flag the user if fewer than expected wells
      # but not necessarily a problem if well removed intentionally
      print(paste("Only ",n,"data rows for ",plate,sep = ""))
    }
  }
  
  
  # get all source files
  allfilenames <- list.files(path = "L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/source_files", pattern = ".csv", full.names = TRUE, recursive = FALSE)
  allplates <- lapply(allfilenames, function(x) unique(fread(x, select = "apid"))) # each file is [[1]], [[2]], etc in the list
  allplates <- unlist(allplates, use.names = FALSE)
  
  # Now checking for re-used plate ID's with previous dataset
  for (cur_plate in cur_plates) {
    if (is.element(cur_plate, allplates)) {
      print(paste(cur_plate, " has been used previously.",sep = ""))
      # now rename in the data set
      # see if cur_plate already has a suffix
      if (grepl(pattern = "[a-z]", cur_plate)) {
        # search for the most recent suffix in allplates... ugh, this will probs never happen...
        
        prev_suff <- grep(pattern = "[a-z]", cur_plate, value = TRUE)
        new_apid <- paste0(cur_plate, intToUtf8( utf8ToInt(prev_suff) + 1))
      }
      mc0_data[apid == cur_plate, apid] <- paste(cur_plate, "a", sep = "")
    }
    
    
  }
  

}

# change untreated wells to Control Treatment
compoundlist = unique(mc0_data$treatment)
for (compound in compoundlist) {
  if (is.element(compound, different_vehicleControlCompounds)) {
    # then assign treatment column to corresponding value in vehicle control list
    mc0_data[(treatment == compound)&(conc == 0), "treatment"] = different_vehicleControls[which(different_vehicleControlCompounds == compound)]
  } else {
    mc0_data[(treatment == compound)&(conc == 0), "treatment"] = default_ControlTreatmentName
  }
}

# # change untreated wells to Control Treatment
# compoundlist = unique(mc0_data$treatment)
# for (compound in compoundlist) {
#   if (is.element(compound, different_vehicleControlCompounds)) {
#     # then assign treatment column to corresponding value in vehicle control list
#     mc0_data[(mc0_data$treatment == compound)&(mc0_data$conc == 0), "treatment"] = different_vehicleControls[which(different_vehicleControlCompounds == compound)]
#   } else {
#     mc0_data[(mc0_data$treatment == compound)&(mc0_data$conc == 0), "treatment"] = default_ControlTreatmentName
#   }
# }

# change wllt for untreated wells to n
mc0_data[ conc == 0, wllt := "n"] # I changed AUC_smaller_melted to mc0_data

write.table(mc0_data, file = outfile, row.names = FALSE, sep = ",")
cat(outfile, "is ready\n")