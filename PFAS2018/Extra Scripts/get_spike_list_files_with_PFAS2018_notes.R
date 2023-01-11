# script to extract all spike list files from the folder
# Note Jan 6 2022: appears that i never ended up using this function,
# decided to stick with gather_files_funciton instead
# But saving this script in case I ever attempt to go back to this

getspikeListFiles <- function(log_file, start.dir, dataset_title = "") {

  sink(log_file)
  cat(paste0(dataset_title," Spike List Files used for MEA NFA pre-processing for TCPL\n"))
  cat("Created with the script get_spike_list_files.R\n")
  cat("Date ran: ")
  cat(as.character.Date(Sys.Date()))
  cat("\n\n")

# these are folders which do not contain exactly 4 spike list files
# I let them pass the check in the for loop, then address the issues at teh end
nonstandard_plate_folders <- c()
# - MW1207-26 - 2 of the spike list files are in a different folder. I add them after the for loop
# - MW1207-38 - There is a README in the spike list files folder stating that this plate should be removed
# I am going to add these files anyways, then set wllq = 0 for this plate in tcpl_AUC_MEA.R script
# - MW1230-53 - There is no DIV9 recording for this plate. So ther are only 3 spike list files

# make grep statement for nonstandard plates
inner <- paste(nonstandard_plate_folders, collapse = ")|(")
special_plate_check <- paste0("(",inner,")")

spike_list_files <- c()
num_plates <- 0

# start in root directory
all.dirs <- list.dirs(path = start.dir, full.names = T, recursive = F)

# get the folders that contain and 8-digit number (corresponding to the culture date)
culture.dirs <- grep(pattern = "[0-9]{8}", all.dirs, value=T)

# exclude unwanted cultures folders:
# - 20180725 culture plate seal
# culture.dirs <- culture.dirs[!(grepl("20180725 culture plate seal",culture.dirs))]

# for each culture directory, get the plate folders
for (i in 1:length(culture.dirs)) {
  plate.dirs <- list.dirs(path = culture.dirs[i], recursive = F)
  plate.dirs <- grep(pattern = "MW[0-9]{4}-",plate.dirs, value=T)
  num_plates <- num_plates + length(plate.dirs)
  cat(basename(culture.dirs[i]),":\n")
  
  for (j in 1:length(plate.dirs)) {
    # in each plate directory, go to the spike list files folder
    plate.subfolders <- list.dirs(path = plate.dirs[j], recursive = F)
    spike.files.folder <- grep("Spike List Files", plate.subfolders, value = T)
    
    # exceptions/special cases
    # if (grepl(pattern = "MW1207-29",plate.dirs[j])) {
    #   spike.files.folder <- grep(pattern = "Spike List Files - Copy", spike.files.folder, value = T)
    # }
    # if (grepl(pattern = "20181114 Culture PFAS Group 4-2",culture.dirs[i])) {
    #   # the spike list files are outside of the spike list files folders for this culture
    #   spike.files.folder <- plate.dirs[j]
    # }
    
    # get all of the files in the spike files folder
    plate_spike_lists <- list.files(path = spike.files.folder, pattern = "spike_list",full.names = T)
    
    # more special cases
    # if (grepl("MW1235-1",plate.dirs[j])) {
    #   # want to use 05_01, not 05_00 for this plate
    #   plate_spike_lists <- plate_spike_lists[!grepl(pattern = "05_00\\(000\\)_spike_list.csv", plate_spike_lists)]
    # }
    # if(grepl("MW1208-9",plate.dirs[j])) {
    #   # want to used the DIV 5 recording with suffix '05_00(000)_fixed_header_spike_list.csv'
    #   plate_spike_lists <- plate_spike_lists[!grepl(pattern = "05_00\\(000\\)_spike_list.csv", plate_spike_lists)]
    # }
    
    # check that there is the correct number of files, or if plate is "special"
    if (grepl(pattern = special_plate_check, plate.dirs[j]) || length(plate_spike_lists) == 4) {
      spike_list_files <- c(spike_list_files, plate_spike_lists)
      cat("\t")
      cat(plate_spike_lists, sep = "\n\t")
    }
    else {
      stop(paste0("there are not 4 spike list files for ",spike.files.folder))
    }
  }
  cat("\n")
}


# specific additions, files that were in different places:
## 20180815 Culture PFAS Group_2-1 - 2 of the 4 spike list files were not in the plate folder. They were moved ot the folder "Original Files (Spike Counts)"
# MW1207_26_DIV5 <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/Original Files (Spike Counts)/ON_20180815_MW1207-26_05_00(000)_spike_list.csv"
# MW1207_26_DIV12 <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/Original Files (Spike Counts)/ON_20180815_MW1207-26_12_00(000)_spike_list.csv"

additional_files <- c()
cat("Addtional files (added for non-standard folders):\n\t")
cat(additional_files,sep = "\n\t")

spike_list_files <- c(spike_list_files, additional_files)

cat(paste0("\n\nCollected ",length(spike_list_files)," spike list files from ",num_plates," plates and ",length(culture.dirs), " cultures.\n"))
# sink()
# perhaps more reliable than sink?
closeAllConnections()
return(spike_list_files)
}


getMasterChemFiles <- function(log_file, start.dir, dataset_title = "") {
  
  sink(log_file)
  cat(paste0(dataset_title," Master Chem Files used for MEA NFA pre-processing for TCPL\n"))
  cat("Created with the script get_spike_list_files.R\n")
  cat("Date ran: ")
  cat(as.character.Date(Sys.Date()))
  cat("\n\n")
  
  nonstandard_plate_folders <- c()

  # make grep statement for nonstandard plates
  inner <- paste(nonstandard_plate_folders, collapse = ")|(")
  special_plate_check <- paste0("(",inner,")")
  
  master_chem_files <- c()
  num_plates <- 0
  
  # start in root directory
  all.dirs <- list.dirs(path = start.dir, full.names = T, recursive = F)
  
  # get the folders that contain and 8-digit number (corresponding to the culture date)
  culture.dirs <- grep(pattern = "[0-9]{8}", all.dirs, value=T)
  
  # exclude unwanted cultures folders:
  # - 20180725 culture plate seal
  # culture.dirs <- culture.dirs[!(grepl("20180725 culture plate seal",culture.dirs))]
  
  # for each culture directory, get the plate folders
  for (i in 1:length(culture.dirs)) {
    plate.dirs <- list.dirs(path = culture.dirs[i], recursive = F)
    plate.dirs <- grep(pattern = "MW[0-9]{4}-",plate.dirs, value=T)
    num_plates <- num_plates + length(plate.dirs)
    cat(basename(culture.dirs[i]),":\n")
    
    for (j in 1:length(plate.dirs)) {
      # in each plate directory, go to the csv Files folder
      plate.subfolders <- list.dirs(path = plate.dirs[j], recursive = F)
      csv.files.folder <- grep("csv Files", plate.subfolders, value = T)
      
      # get the master chem list from the csv Files folder
      plate_master_chem_files <- list.files(path = csv.files.folder, pattern = "MaestroExperimentLog_Ontogeny.csv",full.names = T)
      
      # check that there is the correct number of files, or if plate is "special"
      if (grepl(pattern = special_plate_check, plate.dirs[j]) || length(plate_master_chem_files) == 1) {
        master_chem_files <- c(master_chem_files, plate_master_chem_files)
        cat("\t")
        cat(plate_master_chem_files, sep = "\n\t")
      }
      else {
        stop(paste0("there is not 1 master chem file in ",csv.files.folder))
      }
    }
    cat("\n")
  }
  
  additional_files <- c()
  cat("Addtional files (added for non-standard folders):\n\t")
  cat(additional_files,sep = "\n\t")
  
  master_chem_files <- c(master_chem_files, additional_files)
  
  cat(paste0("\n\nCollected ",length(master_chem_files)," master chemical list files from ",num_plates," plates and ",length(culture.dirs), " cultures.\n"))

  closeAllConnections()
  
  return(master_chem_files)
  
}
