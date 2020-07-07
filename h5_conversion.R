# convert_spkListFiles_to_h5_MetaData.R
# Diana Hall
#load required package
library(sjemea)
library(rhdf5)
library(tcltk)
library(meadq)

###################################################################################
# USER INPUT
###################################################################################
select_or_search_for_files <- "select" # if 'select' - manually select files from dialog box. if 'search' - uses get_spike_list_files.R to search for spike list files
start.dir <- "" # starting directory for spike list files. Only need if using "search", otherwise leave as ""

# if some of the h5 files already exists, do you want to remake them, or use the existing files?
remakeAll_choice = TRUE
###################################################################################
# END USER INPUT
###################################################################################

#get files
analysis <- switch(select_or_search_for_files, 
       select = tk_choose.files(default = "", caption="Select spike list files"),
       search = getspikeListFiles(log_file = paste0(start.dir,"Spike_List_Files_log.txt"), dataset_title = "", start.dir = start.dir))

spkListFiles=sort(analysis)

###################################################################################
# USER INPUT
###################################################################################
# set location where you want the "h5Files" folder to be created
# basepath = ""
# # or, if you want the h5Files folder to be created next the spike list files folder, use the following line
basepath = dirname(dirname(spkListFiles[1]))
# If an h5Files folder already exists in  this location, the h5Files created will overwrite what is there
###################################################################################
# END USER INPUT
###################################################################################

# create h5Files directory
suppressWarnings( dir.create(paste(basepath,'/h5Files',sep='') ) )
h5.dir<-paste(basepath, "/h5Files",sep="")

#get master chemical lists
masterChemFiles <- switch(select_or_search_for_files, 
                          select = tk_choose.files(default = basepath, caption="Select Master Chemical lists"),
                          search = getMasterChemFiles(log_file = paste0(basepath,"Master_Chemical_Lists_log.txt"), dataset_title = "", start.dir = start.dir))

if (length(spkListFiles)/4 > length(masterChemFiles)) {
  stop("Too few Master Chemical Lists selected")
}

L=length(spkListFiles)

#convert .mapTimestamps to .h5 files
for (i in 1:L){
  
  # Find the plate number of current spike list file
  spikefilename = basename(spkListFiles[i])
  spikefilename_split = strsplit(spikefilename, split = "_")
  platename = spikefilename_split[[1]][ grep(pattern = "-", spikefilename_split[[1]]) ]
  # Get the masterChemFile with the same plate number
  masterChemFile = grep(pattern = paste0("_",platename,"_"), masterChemFiles, value = T)
  if (length(masterChemFile) != 1) {
    stop(paste("master chem file match not found for",spikefilename,sep = " "))
  } 

  #load data from all files in folder (listed above)
  title<-strsplit(basename(spkListFiles[i]), "[.]")[[1]][1]
  #get plate chemical info for each file in the list
  plate.chem.info<-chem.info.2(spkListFiles[i], masterChemFile)
  #check to see if there's data
  matching.log.file<-!(length(plate.chem.info)==0)
  while(!matching.log.file){
    info<-unlist(strsplit(basename(spkListFiles[i]),split="_") )[1:3]
    tkmessageBox(message=paste(
      paste( basename(masterChemFile),"contents do not match", basename(spkListFiles[i]), "       " ),
      paste("Please select a Expermental log file that contains data on: " ),
      paste( "Project:",info[1]), 
      paste("Date:",info[2]), 
      paste( "Plate #: ", info[3]),
      sep="\n" ),  icon = "error", type = "ok"  )
    #get master chemical list
    masterChemFile<-tk_choose.files(caption="Select Master Chemical File")
    #get plate chemical info for each file in the list
    plate.chem.info<-chem.info.2(spkListFiles[i], masterChemFile)
    matching.log.file<-!(length(plate.chem.info)==0)
  }
  
  #make h5 files that contain chemical info 
  axion.spkList.to.h5(title, spkListFiles[i], plate.chem.info, remakeAll = remakeAll_choice)
  
}
