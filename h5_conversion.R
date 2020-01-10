# convert_spkListFiles_to_h5_MetaData.R
# Diana Hall
#load required package
library(sjemea)
library(rhdf5)
library(tcltk)
library(meadq)

#get files
analysis<-tk_choose.files(caption="Select spike list Files") 
spkListFiles=sort(analysis)

###################################################################################
# USER INPUT
###################################################################################
# set location where you want the "h5Files" folder to be created
basepath = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA"
# # if you want the h5Files folder to be created next the spike list files folder, use the following line
# basepath = dirname(dirname(spkListFiles[1]))
# If an h5Files folder already exists in  this location, the h5Files created will overwrite what is there
###################################################################################
# END USER INPUT
###################################################################################

# create h5Files directory
suppressWarnings( dir.create(paste(basepath,'/h5Files',sep='') ) )
h5.dir<-paste(basepath, "/h5Files",sep="")

#get master chemical list
masterChemFiles <- tk_choose.files(caption="Select Master Chemical File")

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
  masterChemFile = masterChemFiles[ grep(pattern = platename, masterChemFiles) ]
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
  axion.spkList.to.h5(title, spkListFiles[i], plate.chem.info)
  
}
