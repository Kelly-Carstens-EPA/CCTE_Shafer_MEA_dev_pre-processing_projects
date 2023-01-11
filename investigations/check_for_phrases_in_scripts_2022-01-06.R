# Checking for the presence of any phrases that in the scripts that I might not want to include
# Before I push this to github
# Jan 6, 2022

check.phrases <- c('L:/Lab',
                   'MySQL',
                   ***REMOVED***,
                   'dbConnect',
                   'tcplConf')

files <- list.files(path = 'L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R', full.names = T)


for (filei in files) {
  
  cat(basename(filei), '\n')
  script_txt <- scan(file = filei, what = character(), sep = '\n', quiet = T)
  
  # check for any matches with the check.phrases
  res <- sapply(check.phrases, function(stri) grepl(pattern = stri, script_txt))
  
  # If any matches, spit them out
  if (any(unlist(lapply(res, any)))) {
    print(which(res == TRUE, arr.ind = T))
  }
}

# burst_parameter_to_AUC.R 
# confirm_concs.R 
# row col
# [1,]   3   2
# [2,]  13   2
# [3,]  13   3
# [4,]  13   4
# This is a real issue

# create_burst_ont_Data.R 
# create_ont_csv.R 
# cytotox_prep06.R 
# row col
# [1,] 315   3
# using the word pass, not short for password

# dataset_checks.R 
# DIV-interpolation-functions.R 
# estimate_missing_DIV.R 
# gather_files_functions.R 
# row col
# [1,]  54   3
# using the word pass, not short for password

# get_spike_list_files.R 
# row col
# [1,]  76   1
# [2,]  77   1
# [3,]  10   3
# Just removed specific references to file paths
# using the word pass, not short for password

# h5_conversion.R 
# local.corr.all.ont.ae.filter.R 
# MI_script_all.R 
# nmi_wrapper.R 
# nmi2_final.R 
# normalize_auc_summary.R 

# Not going to include any of the run_me's, just the templates:
# run_me_Brown2014.R 
# row col
# [1,]  10   1
# [2,]  11   1
# [3,] 302   1
# [4,] 363   1
# [5,] 378   1
# [6,] 386   1
# [7,] 233   2
# [8,] 233   3
# [9,] 233   4
# run_me_DNTGF2019.R 
# row col
# [1,]  10   1
# [2,]  12   1
# [3,]  13   1
# [4,] 120   1
# [5,] 217   1
# [6,] 218   1
# [7,] 268   1
# [8,] 335   1
# [9,] 352   1
# [10,] 367   1
# [11,] 376   1
# [12,] 385   1
# [13,] 389   1
# [14,] 411   1
# [15,] 423   1
# [16,] 445   1
# [17,] 473   1
# run_me_Example2020.R 
# row col
# [1,]  14   1
# [2,]  16   1
# [3,]  17   1
# run_me_Frank2017.R 
# row col
# [1,]  12   1
# [2,]  13   1
# [3,] 182   1
# run_me_NTP91.R 
# row col
# [1,]  10   1
# [2,]  12   1
# [3,]  13   1
# run_me_OPP2015.R 
# row col
# [1,]  10   1
# [2,]  11   1
# run_me_PFAS2018.R 
# row col
# [1,]  10   1
# [2,]  12   1
# [3,]  13   1
# run_me_PFAS2019.R 
# row col
# [1,]  12   1
# [2,]  13   1
# [3,] 156   1
# [4,] 159   1
# [5,] 258   1
# run_me_RejectedCultures.R 
# row col
# [1,]  10   1
# [2,]  11   1
# run_me_SPS_PFAS2019.R 
# row col
# [1,]  13   1
# [2,]  14   1
# [3,] 179   1
# run_me_template.R 
# row col
# [1,]  12   1
# [2,]  13   1
# Just removed these

# run_me_ToxCast2016.R 
# row col
# [1,]  10   1
# [2,]  11   1
# [3,]  96   2
# [4,]  96   3
# [5,]  96   4
# run_me_wide_template.R 
# row col
# [1,]  10   1
# [2,]  11   1
# Just removed these

# source_steps.R 
# spike_list_functions.R 
# spikeLoadRoutines.R 
# tcpl_MEA_dev_AUC.R 
#      row col
# [1,]   9   1
# [2,]  10   1
# [3,]  16   1
# [4,]   1   3
# this was all in a commented out seciton, has been removed now



# Checking if any files_log used get_spike_list_files.R --------------
check.phrases <- c('Created with the script get_spike_list_files')

files <- list.files(path = 'L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl', pattern = '\\.txt', full.names = T, recursive = T)

for (filei in files) {
  
  cat(basename(filei), '\n')
  script_txt <- scan(file = filei, what = character(), sep = '\n', quiet = T)
  
  # check for any matches with teh check.phrases
  res <- sapply(check.phrases, function(stri) grepl(pattern = stri, script_txt))
  
  # If any matches, spit them out
  if (any(unlist(lapply(res, any)))) {
    print(which(res == TRUE, arr.ind = T))
  }
}

# No occurence of this phrase => indicates that I never actually used this script,
# just set with gather_Files_function!
