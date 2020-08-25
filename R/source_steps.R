# this script will run the first main steps of the data preparation
# it will check if a step has already been completed before doing it
# it will select files, create h5files, create prepared data, AUC table, and collect cytotox data
# Merging the data in a single mc0-style file will be done separately

setwd(scripts.dir)
main.output.dir <- file.path(root_output_dir, dataset_title)

# select all files needed for analysis
source('gather_files_functions.R')
if (remake_all || length(list.files(path = main.output.dir, pattern = "_files_log_", recursive = F)) == 0) {
  selectInputFiles(start.dir = strsplit(getwd(), .Platform$file.sep)[[1]][1], main.output.dir, dataset_title, append = append)
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
run_cytotox_functions(basepath = main.output.dir, get_files_from_log = TRUE, filename = paste0(dataset_title,"_cytotox_longfile.csv"), remake_all = remake_all, append = append)

cat("\n'source_steps.R' is complete.\n")

