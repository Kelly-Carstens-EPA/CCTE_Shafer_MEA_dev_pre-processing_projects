# USER INPUT will now be passed to the function below
# ###################################################################################
# # USER INPUT
# ###################################################################################
# # set working directory for where the output file will go
# setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing")
# # set the output file name
# filename = "mc0_test_dnt.csv"
# AUCsourcefilename = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/AUC_DNT2019_All.csv"
# cytotox_filename = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/DNT2019_all_cytotoxicity_rawVals.csv"
# default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# # Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
# different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# # Enter the names of the vehicle controls as they correspond to the compounds in the previous list
# different_vehicleControls = c() # e.g. c("Water", "Water")
# spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
# use_sheet <- "NFA Groups"
# trt_col <- "Chemical ID...2"
# stock_conc_col <- "Conc"
# spid_col <- "NCCT ID...3" # need to iron out some stuff with spids here... not sure where to do dataset specific stuff
# ###################################################################################
# # END USER INPUT
# ###################################################################################

tcpl_MEA_dev_AUC <- function(basepath, dataset_title, spidmap_file, use_sheet, trt_col, stock_conc_col, spid_col, default_ControlTreatmentName,
                             filename = file.path(basepath, "output", paste0(dataset_title, "_longfile.csv")), AUCsourcefilename = file.path(basepath, "output", paste0(dataset_title, "_AUC.csv")), 
                             cytotox_filename = file.path(basepath, "output", paste0(dataset_title, "_cytotox_longfile.csv")),
                             different_vehicleControlCompounds = c(), different_vehicleControls = c())
{
  
  # load libraries
  library(data.table)
  library(readxl)
  
  ## read in the data
  AUC <- fread(AUCsourcefilename)
  cytotox_data <- fread(cytotox_filename)
  
  ## rename columns before melting
  names(AUC)[names(AUC) == 'dose'] <- "conc"
  AUC[, apid := paste(date, plate.SN, sep = "_")]
  cytotox_data[, apid := paste(date, plate.SN, sep = "_")]
  
  
  ## split well_id to rows and columns
  ## Defining coli
  AUC$coli <- substring(AUC$well, 2, 2)
  
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
  
  # FINALIZE WLLQ ------------------------------------------------------------------
  # (I just changed cytotox_prep so that wllq will not be initialized there)
  # so I can initialize it all here
  # perhaps include in final summary where had to estimate or interpolate values
  
  # will probably move all below to a run_me, so can do dataset-specific modifiations
  # read in spidmap file
  spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
  setnames(spidmap, old = c(trt_col, stock_conc_col, spid_col), new = c("treatment","stock_conc","spid"))
  unique(mc0_data$treatment)
  mc0_data <- merge(mc0_data, spidmap[, .(spid, treatment)], by = "treatment", all.x = TRUE)
  if (any(is.na(mc0_data$spid))) {
    warning("some spids are NA/not assigned in spidmap")
  }
  
  # confirm that the conc's collected from master chem lists and Calc files match
  # and that the correct concentration-corrections has been done for each compound
  mc0_data <- confirm_concs(mc0_data)
  
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
}