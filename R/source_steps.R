# this script will run the first main steps of the data preparation
# it will check if a step has already been completed before doing it
# it will select files, create h5files, create prepared data, AUC table, and collect cytotox data

check_existing <- function(path, pattern, pause_between_steps) {
  # save_objects <- c("check_existing","main.output.dir",
  #                   "dataset_title","pause_between_steps","save_notes_graphs",
  #                   "default_ControlTreatmentName",
  #                   "different_vehicleControlCompounds",
  #                   "different_vehicleControls",
  #                   "spidmap_file",
  #                   "spid_sheet",
  #                   "scripts.dir",
  #                   "root_output_dir")
  # rm(list = setdiff(ls(parent.frame()), save_objects), envir = parent.frame()) # free up memory...
  if (pause_between_steps) {
    next_step <- readline(prompt = "Continue? (y/n): ")
    if (next_step == "n") {
      stop("User elected to stop.")
    }
  }
  resp <- "r" # default response
  num_files <- length(list.files(path, pattern, recursive = F)) # check if any of these files already exist
  if (num_files > 0) {
    cat(num_files, "files already exist.")
    if (num_files == 1) cat(" (",list.files(path, pattern, recursive = F),")")
    if (pause_between_steps) 
      repeat {
        resp <- readline(prompt = "Do you want to Continue with only the current files, Remake all files, Append to existing files, or Quit? (c/r/a/q): ")
        if (resp %in% c("c","r","a","q")) break
      }
    else 
      resp <- "c"
  }
  if (resp == "q") {
    stop("User elected to stop.")
  }
  assign("append",switch(resp,
                         "a" = TRUE,
                         "r" = FALSE,
                         "c" = FALSE),
         envir = parent.frame())
  return(resp)
}

setwd(scripts.dir)
main.output.dir <- file.path(root_output_dir, dataset_title)
if (!dir.exists(main.output.dir)) dir.create(main.output.dir)

# select all files needed for analysis
source('gather_files_functions.R')
cat("\n- Select files for files_log:\n")
resp <- check_existing(path = main.output.dir, pattern = "_files_log_", pause_between_steps)
if(resp %in% c("r","a")) {
  selectInputFiles(start.dir = strsplit(getwd(), .Platform$file.sep)[[1]][1], main.output.dir, dataset_title, append = append)
  # view summary of files
  file_names <- readLogFile(main.output.dir)
  cultures <- unique(sapply(strsplit(file_names, "\\\\"), function(x) grep("[Cc]ulture",x,val = T)))
  for (culture in cultures) {
    cat("\n\n",culture,"\n",sep = "")
    cat("Number of spike list files: ")
    cat(sum(grepl(culture, file_names) & grepl("_spike_list",file_names)),"\n")
    cat("Number of Master chem lists: ")
    cat(sum(grepl(culture, file_names) & grepl("_MaestroExperimentLog_Ontogeny",file_names)),"\n")
    cat("Calculations/Summary files:\n")
    cat(basename(file_names[grepl(culture, file_names) & grepl("(Calculations)|(Summary)",file_names)]),sep = ", ")
  }
  rm(file_names)
}

# h5_conversion.R
cat("\n- Create h5 files:\n")
resp <- check_existing(path = file.path(main.output.dir,"h5files"), pattern = "\\.h5", pause_between_steps)
if (resp %in% c("r","a")) {
  source('spike_list_functions.R')
  remake_all <- !append
  source('h5_conversion.R')
  cat("h5files are ready in folder",file.path(main.output.dir,"h5files"),"\n")
}

# create_ont_csv
cat("\n- Calculate the components:\n")
resp <- check_existing(path = file.path(main.output.dir,"prepared_data"), pattern = "\\.csv", pause_between_steps)
if (resp %in% c("r","a")) {
  source('create_burst_ont_Data.R')
  source('local.corr.all.ont.ae.filter.R')
  source('create_ont_csv.R')
  create_ont_csv(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = !append)
}

# normalized mutual information calculation
cat("\n- Calculate the Mutual Information:\n")
resp <- check_existing(path = file.path(main.output.dir,"All_MI"), pattern = "\\.csv", pause_between_steps)
rm(list = Filter( exists, c("create_burst_ont_Data","create_ont_csv","spkList2list","local.corr.all.ont.ae.filter"))) # remove a few larger unneeded functions
if (resp %in% c("r","a")) {
  source('spikeLoadRoutines.R')
  source('nmi2_final.R')
  source('nmi_wrapper.R')
  source('MI_script_all.R')
  run_mi_functions(basepath = main.output.dir, get_h5Files_under_basepath = TRUE, remake_all = !append)
}

# burst parameter to AUC
cat("\n- Check over component values by DIV, calculate AUC:\n")
resp <- check_existing(path = file.path(main.output.dir, "output"), pattern = "_AUC", pause_between_steps)
if (resp %in% c("r","a")) {
  # append has no effect here. This fun is relatively fast, so will remake all regardless
  source('DIV-interpolation-functions.R')
  source('estimate_missing_DIV.R')
  source('burst_parameter_to_AUC.R')
}

# cytotox prep
cat("\n- Extract the cytotoxicity data from Calculations files:\n")
resp <- check_existing(path = file.path(main.output.dir, "output"), pattern = "_cytotox", pause_between_steps)
if (resp %in% c("r","a")) {
  source('cytotox_prep06.R')
  run_cytotox_functions(basepath = main.output.dir, get_files_from_log = TRUE, filename = paste0(dataset_title,"_cytotox.csv"), append = append)
}

cat("\n'source_steps.R' is complete.\n")

