###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/" # set start.dir to the main project folder for your data set. Does not have to be exact.
dataset_title <- "NTP91" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
remake_all <- TRUE # re-create all intermediate output files, even if they already exist? can set to TRUE or FALSE

# for tpl mea dev script (still under development, can ignore for now)
# default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# # Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
# different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# # Enter the names of the vehicle controls as they correspond to the compounds in the previous list
# different_vehicleControls = c() # e.g. c("Water", "Water")
# spidmap_file <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing/full_test_material/All Assays_list_toxcast_OECD 20190524.xlsx"
# use_sheet <- "NFA Groups"
# trt_col <- "Chemical ID...2"
# stock_conc_col <- "Conc"
# spid_col <- "NCCT ID...3"

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts" # where the dataset_title folder will be created
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)

# source the ultimate function!
source(file.path(scripts.dir, 'source_steps.R'))


# Future steps:

# source tpcl mea dev - this fun has lots of user-defined inputs
# source(file.path(scripts.dir, "tcpl_MEA_dev_AUC.R"))
# tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir, dataset_title), dataset_title, spidmap_file, use_sheet, trt_col, stock_conc_col, spid_col, 
#                  default_ControlTreatmentName, different_vehicleControlCompounds = c(), different_vehicleControls = c())


# data-set specific stuff, such as
# - update wllq
# - check conc's, update for individual spid if needed
# - map spid's
# - other clean up
# - final data checks
