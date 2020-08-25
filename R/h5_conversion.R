# convert_spkListFiles_to_h5_MetaData.R
# Diana Hall
#load required package
library(sjemea)
library(rhdf5)
library(meadq)

#get files
analysis <- readLogFile(main.output.dir, files_type = "spike_list")
spkListFiles <- sort(analysis)

###################################################################################
# USER INPUT
###################################################################################
# set location where you want the "h5Files" folder to be created
basepath <- main.output.dir
# # or, if you want the h5Files folder to be created next the spike list files folder, use the following line
# basepath = dirname(dirname(spkListFiles[1]))
# If an h5Files folder already exists in  this location, the h5Files created will overwrite what is there
# remake_all <- TRUE # re-create existing h5files from spike list files?
###################################################################################
# END USER INPUT
###################################################################################

# create h5Files directory
suppressWarnings( dir.create(paste(basepath,'/h5Files',sep='') ) )
h5.dir<-paste(basepath, "/h5Files",sep="")

#get master chemical lists
masterChemFiles <- readLogFile(main.output.dir, files_type = "MaestroExperimentLog")

if (length(spkListFiles)/4 > length(masterChemFiles)) {
  stop("Too few Master Chemical Lists selected")
}

L=length(spkListFiles)

#convert .mapTimestamps to .h5 files
for (i in 1:L){
  
  # Find the plate number of current spike list file
  spikefilename = basename(spkListFiles[i])
  date_plate <- paste(strsplit(spikefilename, split = "_")[[1]][2:3],collapse = "_")
  
  # Get the masterChemFile with the date_plate
  masterChemFile <- grep(pattern = paste0(date_plate,"_"), masterChemFiles, value = T)
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
    masterChemFile<-choose.files(caption="Select Master Chemical File")
    #get plate chemical info for each file in the list
    plate.chem.info<-chem.info.2(spkListFiles[i], masterChemFile)
    matching.log.file<-!(length(plate.chem.info)==0)
  }
  
  #make h5 files that contain chemical info 
  axion.spkList.to.h5(title, spkListFiles[i], plate.chem.info, remake_all = remake_all)
  
}
