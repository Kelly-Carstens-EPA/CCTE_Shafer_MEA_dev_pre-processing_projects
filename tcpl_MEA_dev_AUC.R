# this script is not fully functional
# - need to fix check_unique_apid.R section

###################################################################################
# USER INPUT
###################################################################################
# set working directory for where the output file will go
setwd("C:/Users/Acarpe01/Documents/tcpl MEA DEV")
# set the output file name
outfile = "MEA_Frank_mc0_test.csv"
AUCsourcefilename = "L:/Lab/NHEERL_MEA/Frank 86 tcpl prep/Intermediate Output/deprecated/Frank86_AUC_2.csv"
cytotox_filename = "L:/Lab/NHEERL_MEA/Frank 86 tcpl prep/Intermediate Output/Frank86_cytotoxicity3.csv"
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
cytotox_data = fread(cytotox_filename)

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
AUC_smaller_melted <- melt(AUC_smaller, id = c("date","treatment", "apid", "ID", "rowi", "coli", "conc"))
# rename response column
names(AUC_smaller_melted)[names(AUC_smaller_melted) == 'value'] <- "rval"
# add srcf , acsn, wllt, wllq
AUC_smaller_melted[, acsn := variable]
AUC_smaller_melted$wllt = "t"
AUC_smaller_melted$wllq = 1
AUC_smaller_melted[, srcf := AUCsourcefilename]


# remove unneeded columns
AUC_smaller_melted = AUC_smaller_melted[, c("date","treatment","apid","rowi","coli","wllt","wllq","conc","rval", "srcf", "acsn")]
# remove columns and make sure columns in same order
cytotox_data = cytotox_data[, c("date","treatment","apid","rowi","coli","wllt","wllq","conc","rval", "srcf", "acsn")]

# rbind the cytotox data
mc0_data = rbind(AUC_smaller_melted, cytotox_data)

if (check_unique_apid) {
  
  num_acsn = 19 # usually 17, plus LDH and AB endpoints
  num_wells_per_plate = 48
  cur_plates = unique(mc0_data$apid)
  
  # first, check for re-used plate ID in current data set
  for (cur_plate in cur_plates) {
    # check if there are multiple dates corresponding to the current plate
    dates <- unique(mc0_data[apid == cur_plate, date])
    if (length(dates) > 1) {
      cat("\nThe plate ",cur_plate," was used in multiple culture dates (",dates,").\n")
      # assign a suffix to these plates
      firstsuffix = "a"
      for (d in 1:length(dates)) {
        new_apid <- paste0(cur_plate, intToUtf8( utf8ToInt(firstsuffix) + d - 1))
        mc0_data[(apid == cur_plate)&(date == dates[d]), "apid"] <- new_apid
        print(paste0(cur_plate, ", ", dates[d], " is now assigned to ", new_apid))
      }
    }
  }
  cat("\n")
  
  # get list of updated plate names
  cur_plates = unique(mc0_data$apid)
  
  # get all source files
  allfilenames <- list.files(path = "L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/source_files", pattern = ".csv", full.names = TRUE, recursive = FALSE)
  allplates <- lapply(allfilenames, function(x) unique(fread(x, select = "apid"))) # each file is [[1]], [[2]], etc in the list
  allplates <- unlist(allplates, use.names = FALSE)
  
  # Now checking for re-used plate ID's with previous dataset
  for (cur_plate in cur_plates) {
    if (is.element(cur_plate, allplates)) {
      print(paste(cur_plate, " has been used in a previous data sets.",sep = ""))
      # just going to add the suffix "a" to the new version of the apid
      new_apid <- paste0(cur_plate, "a")
      mc0_data[apid == cur_plate, "apid"] <- new_apid
      print(paste0(cur_plate, " is now assigned to ", new_apid, " in  the current data set."))
    }
  }
}

# now that we know that each apid is unique to each date, we can remove that column
mc0_data <- mc0_data[, -c("date")]

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

# change wllt for untreated wells to n
mc0_data[ conc == 0, wllt := "n"] # I changed AUC_smaller_melted to mc0_data

fwrite(mc0_data, file = outfile, row.names = FALSE, sep = ",")
cat(outfile, "is ready\n")