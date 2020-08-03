# script to run all things...
# this script will run each step of the analysis
# it will check if a step has already been completed before doing it
# it will select files, create h5files, create prepared data, AUC table, collect cytotox data,
# and merge data into a single mc0-style file.

setwd(scripts.dir)
main.output.dir <- file.path(root_output_dir, dataset_title)

# create a way to check if a data level has already been created
# select files - just check that spike list files, calc files already exist

# Ideas:
# I could make it wait for a response, but have some kind of loop in my run_me function that generates a default response....
# I don't want user to have to babysit if they already know what they want.

# final options for the user:
# - run all, easily
# specify where you want to start


# select all files needed for analysis
source('gather_files_functions.R')
if (remake_all || length(list.files(path = main.output.dir, pattern = "_files_log_", recursive = F)) == 0) {
  selectInputFiles(start.dir, main.output.dir, dataset_title)
}

# h5_conversion.R
source('spike_list_functions.R')
source('h5_conversion.R')
cat("h5files are ready in folder",file.path(main.output.dir,"h5files"),"\n")

# create_ont_csv
source('create_burst_ont_Data.R')
source('local.corr.all.ont.ae.filter.R')
source('create_ont_csv.R')
create_ont_csv(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = remake_all)

# normalized mutual information calculation
source('spikeLoadRoutines.R')
source('nmi2_final.R')
source('nmi_wrapper.R')
source('MI_script_all.R')
run_mi_functions(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = remake_all)

# burst parameter to AUC
source('DIV-interpolation-functions.R')
source('estimate_missing_DIV.R')
source('burst_parameter_to_AUC.R')

# cytotox prep
source('cytotox_prep06.R')
run_cytotox_functions(basepath = main.output.dir, get_files_from_log = TRUE, filename = paste0(dataset_title,"_cytotox_longfile.csv"))

cat("'source_steps.R' is complete.\n")

# tcpl mea dev burst paramter to AUC
# I think I'll run this separately

