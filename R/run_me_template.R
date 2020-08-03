###################################################################################
# USER INPUT
###################################################################################
start.dir <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing" # "L:/Lab/NHEERL_MEA/"
dataset_title <- "testpipeline2020" # e.g. "name2020"
remake_all <- FALSE # re-create all data files, even if they already exist?

# for tpl mea dev script
default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Enter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c() # e.g. c("Water", "Water")
spidmap_file <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing/full_test_material/All Assays_list_toxcast_OECD 20190524.xlsx"
use_sheet <- "NFA Groups"
trt_col <- "Chemical ID...2"
stock_conc_col <- "Conc"
spid_col <- "NCCT ID...3"

# optional adjutsment; usually can use defaults:
# set the directory for the scripts to be sourced in the ultimate fun below
# maybe this will be standardized? but eh, I want this to be export-able/moveable
scripts.dir <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing" # where the dataset_title folder will be created
# override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
# plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
# noisy_functions <- TRUE
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)

# source the ultimate function!
source(file.path(scripts.dir, 'source_steps.R'))

# source tpcl mea dev - this fun has lots of user-defined inputs
# source(file.path(scripts.dir, "tcpl_MEA_dev_AUC.R"))
# tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir, dataset_title), dataset_title, spidmap_file, use_sheet, trt_col, stock_conc_col, spid_col, 
#                  default_ControlTreatmentName, different_vehicleControlCompounds = c(), different_vehicleControls = c())

# data-set specific stuff, such as
# update wllq
# check conc's, update for individual spid if needed
# map spid's
# other clean up
# final data checks