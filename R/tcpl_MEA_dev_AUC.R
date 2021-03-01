# USER INPUT will now be passed to the function below
# ###################################################################################
# # USER INPUT
# ###################################################################################
# # set working directory for where the output file will go
# setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing")
# # set the output file name
# output_file = "mc0_test_dnt.csv"
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

tcpl_MEA_dev_AUC <- function(basepath, dataset_title, 
                             AUCsourcefilename = file.path(basepath, "output", paste0(dataset_title, "_AUC.csv")), 
                             DIVsourcefilename = file.path(basepath, "output", paste0(dataset_title, "_parameters_by_DIV.csv")), 
                             cytotox_filename = file.path(basepath, "output", paste0(dataset_title, "_cytotox.csv")),
                             assay_component_map_filename = file.path(dirname(basepath), "mea_nfa_component_name_map.csv"))
{
  
  require(data.table)
  
  # get DIV data and melt
  DIV_data <- fread(DIVsourcefilename)
  idcols <- c("date","Plate.SN","well","treatment","dose","units","wllq","wllq_notes")
  endpoint_cols <- setdiff(names(DIV_data),c(idcols,"DIV","file.name"))
  DIV_data[, (endpoint_cols) := lapply(.SD, as.numeric), .SDcols = endpoint_cols]
  DIV_data <- melt(DIV_data, id.vars = c(idcols,"DIV"), measure.vars = endpoint_cols, variable.name = "src_acsn",
                value.name = "rval", variable.factor = FALSE)
  DIV_data[, `:=`(src_acsn = paste0(src_acsn,"_DIV",DIV),
                  srcf = basename(DIVsourcefilename))]
  DIV_data[, DIV := NULL]
  
  # get AUC data and melt
  AUC <- fread(AUCsourcefilename)
  AUC <- melt(AUC, id.vars = idcols, measure.vars = paste0(endpoint_cols,"_auc"), variable.name = "src_acsn",
                value.name = "rval", variable.factor = FALSE)
  AUC[, srcf := basename(AUCsourcefilename)]
  
  # rbind DIV and AUC data
  longdat <- rbind(DIV_data, AUC)
  longdat[, `:=`(coli = as.numeric(sub("[[:alpha:]]","",well)), rowi = match(sub("[[:digit:]]","",well), LETTERS))]
  longdat[, c("well","units") := list(NULL)]
  setnames(longdat, old = "dose", new = "conc")
  rm(list = c("DIV_data","AUC"))
  
  # add cytotox data
  cytotox_data <- fread(cytotox_filename)
  longdat <- rbind(longdat, cytotox_data)
  rm(list = c("cytotox_data"))
  longdat[, treatment := as.character(treatment)] # sometimes the treatment is read as an integer instead of a char
  
  # replace the src_acsn with the TCPL acsn
  assay_component_map <- as.data.table(read.csv(assay_component_map_filename, stringsAsFactors = FALSE))
  longdat <- merge(longdat, assay_component_map, by = c("src_acsn"), all.x = T)
  if (any(is.na(unique(longdat$acsn)))) {
    print(longdat[is.na(acsn), unique(src_acsn)])
    stop(paste0("The above src_acsn's are not found in ",assay_component_map_filename))
  }
  
  # Define apid
  longdat[, apid := paste(date, Plate.SN, sep = "_")]
  
  # Assign wllt
  longdat[, wllt := "t"]
  longdat[ conc == 0, wllt := "n"]
  
  # get the desired columns, in the desired order
  longdat <- longdat[, .(apid, rowi, coli, treatment, conc, wllq, wllq_notes, wllt, rval, acsn, srcf)]
  
  cat("long-format data is ready.\n")
  return(longdat)
}


# additional functions to prepare the data for TCPL mc0 format

update_control_well_treatment <- function(dat, control_compound, culture_date = c(), plates = c(), control_rowi) {
  apids <- Reduce(f = union, x = lapply(culture_date, function(x) grep(x, unique(dat$apid), val = T)))
  if (length(plates) > 0) {
    apids <- Reduce(f = union, x = lapply(plates, function(x) grep(x, apids, val = T)))
  }
  cat("Control treatment will be updated to",control_compound,"for the following wells:\n")
  print(dat[wllt == "n" & apid %in% apids & rowi %in% control_rowi, unique(.SD), .SDcols = c("apid","treatment","rowi","coli","conc")][order(apid,rowi,coli)])
  dat[wllt == "n" & apid %in% apids & rowi %in% control_rowi, treatment := control_compound]
}

update_treatment_names <- function(date, root_output_dir, dataset_title) {
  trt_name_map <- as.data.table(read.csv(file.path(root_output_dir, "supplemental_mea_treatment_name_map.csv"), stringsAsFactors = F))
  trt_name_map <- trt_name_map[dataset == dataset_title, .(mea_treatment_name, updated_treatment_name)]
  unused_trt_names <- setdiff(unique(trt_name_map$mea_treatment_name), unique(dat$treatment))
  if(length(unused_trt_names)> 0 ){
    cat("Some expected mea treatment names in 'supplemental_mea_treatment_name_map.csv' are not in the input data table:", unused_trt_names, "\n", sep = "\n")
  }
  dat <- merge(dat, trt_name_map, by.x = "treatment", by.y = "mea_treatment_name", all.x = T)
  dat[is.na(updated_treatment_name), updated_treatment_name := treatment] # for compound names that do not need to be updated
  setnames(dat, old = c("treatment","updated_treatment_name"), new = c("mea_treatment_name","treatment"))
  return(dat)
}

check_and_assign_spids <- function(dat, spidmap) {
  if (length(setdiff(c("treatment","spid"), names(spidmap))) > 0) {
    stop("The following columns are not found in spidmap: ",paste0(setdiff(c("treatment","spid"), names(spidmap)),collapse =","))
  }
  if (spidmap[!is.na(spid) & treatment %in% unique(dat$treatment), .(length(unique(spid))), by = "treatment"][,any(V1 !=1)]) {
    stop(paste0("The following treatments map to multiple spids: ",
                spidmap[!is.na(spid) & treatment %in% unique(dat$treatment), .(length(unique(spid))), by = "treatment"][V1 != 1,paste0(treatment,collapse=", ")]))
  }
  dat <- merge(dat, spidmap[, .(spid, treatment)], by = "treatment", all.x = TRUE)
  dat[wllt != 't' & is.na(spid), spid := treatment]
  if (dat[wllt == "t", any(is.na(spid))]) {
    cat("The following treatments don't have a corresponding spid in the spidmap:\n")
    print(dat[wllt == "t" & is.na(spid), unique(treatment)])
    stop("Adjust input dat before continuing")
  }
  return(dat)
}
