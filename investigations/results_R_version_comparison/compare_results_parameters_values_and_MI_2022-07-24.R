# ------------------------------------------------------------------------ #
# Comparison of data values calculated on different machines with different versions of R
# and differetn versions of the used packages
# July 24, 2022
# ------------------------------------------------------------------------ #

# From my notes, the following files were created using R version 4.1.1:

# all files here:
# L:\Lab\NHEERL_MEA\Carpenter_Amy\pre-process_mea_nfa_for_tcpl\DNT_NTP2021\prepared_data

# The first 8 files here: (starting 20210915 - 20210929)
# L:\Lab\NHEERL_MEA\Carpenter_Amy\pre-process_mea_nfa_for_tcpl\DNT_NTP2021\All_MI

# let's calculate these same files using R 3.6.1, then compare the values

# actually... let's just pull a subset, run it through both from spike list files to AUC

rm(list = ls())

# R 3.6.1 trial ------------------------------------------ ----------------

dataset_title <- "investigations/results_R_version_comparison/test_R3" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed
project.dir <- "L:/Lab/NHEERL_MEA/Project - DNT_NTP_2021" # project main folder (where will look for README files)
scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R" # update to the folder where the scripts are located
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created

library(data.table)
library(openxlsx)
library(RMySQL)
library(stringi)

main.output.dir <- file.path(root_output_dir, dataset_title)
dataset_title <- 'test_R3'

append = FALSE

keep_items <- c(ls(), 'keep_items') # will clear all items except for these after each step

# select all files needed for analysis
source('gather_files_functions.R')
selectInputFiles(start.dir = dirname(dirname(main.output.dir)), main.output.dir, dataset_title, append = append)
# Writing 59 files to test_R3_files_log_2022-07-24.txt ...
# [1] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/investigations/results_R_version_comparison/test_R3/test_R3_files_log_2022-07-24.txt is ready."
# (manually edited so that there would be 60 files, bc I missed an MCL file)
# Selected files from 4 different projects
# view summary of files
file_names <- readLogFile(main.output.dir)
# cultures <- sort(unique(sapply(strsplit(file_names, "\\\\"), function(x) grep("([Cc]ulture)|([Oo]ntogeny)",x,val = T)[1])))
# cultures <- lapply(cultures, function(x) ifelse(length(x)==0, "", x)) # for datasets that don't have Culture tag phrase
cultures <- unique(stri_extract(file_names, regex = '[0-9]{8}'))
for (culture in cultures) {
  cat("\n\n",culture,"\n",sep = "")
  culture <- sub("\\(","\\\\(",culture)
  culture <- sub("\\)","\\\\)",culture)
  cat("Number of spike list files: ")
  cat(sum(grepl(culture, file_names) & grepl("_spike_list",file_names)),"\n")
  cat("Number of Master chem lists: ")
  cat(sum(grepl(culture, file_names) & grepl("MaestroExperimentLog",file_names)),"\n")
  cat("Calculations/Summary files:\n")
  cat(basename(file_names[grepl(culture, file_names) & grepl("(Calculations)|(Summary)",file_names)]),sep = ", ")
}

# h5_conversion.R
cat("\n- Create h5 files:\n")
basepath <- main.output.dir
remake_all <- TRUE
source(file.path(scripts.dir,'spike_list_functions.R'))
source(file.path(scripts.dir,'h5_conversion.R'))
rm(list = setdiff(ls(), keep_items))

# create_ont_csv
cat("\n- Calculate the components:\n")
basepath <- main.output.dir
source(file.path(scripts.dir,'create_ont_csv.R'))
source(file.path(scripts.dir,'create_burst_ont_Data.R'))
source(file.path(scripts.dir,'local.corr.all.ont.ae.filter.R'))
create_ont_csv(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = TRUE)
# Just selected files through 20210929


# normalized mutual information calculation
cat("\n- Calculate the Mutual Information:\n")
source(file.path(scripts.dir,'spikeLoadRoutines.R'))
source(file.path(scripts.dir,'nmi2_final.R'))
source(file.path(scripts.dir,'nmi_wrapper.R'))
source(file.path(scripts.dir,'MI_script_all.R'))
run_mi_functions(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = !append)

# burst parameter to AUC
cat("\n- Check over component values by DIV, calculate AUC:\n")
# append has no effect here. This fun is relatively fast, so will remake all regardless
source(file.path(scripts.dir,'DIV-interpolation-functions.R'))
source(file.path(scripts.dir,'estimate_missing_DIV.R'))
source(file.path(scripts.dir,'burst_parameter_to_AUC.R'))

# (not going to check cytotox scripts, because that is just data extraction, not calculating any values (other than maybe some simple blank-correction - nothing fancy, nothing that should change)
# If there were any issues with extracting the values, then that would have come up as NA or something. 
