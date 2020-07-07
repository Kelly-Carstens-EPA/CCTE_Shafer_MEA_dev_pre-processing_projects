# This script was copied from NFA Spike List to mc0 R Scripts Bit Bucket repository, branch MI-related-updates on 04/17/2020 10:08am

# This allows you to select the h5 files
# then calls the functions to calculate the NMI

library(rhdf5) # this is the library from Bio Conductor
library(data.table)
require('pracma')
library('compiler')
require('gtools')

h5Files<-choose.files(caption="Select .h5 Files")
h5Files <- sort(h5Files)

###################################################################################
# USER INPUT
###################################################################################
# set the location where you want the "All_MI" folder to be created
# basepath = "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Organophosphates"
# or, use the following line if you want the "All_MI" folder to be created next to the h5Files folder
basepath = dirname(dirname(h5Files[1]))

###################################################################################
# END USER INPUT
###################################################################################

# create All_MI directory
suppressWarnings( dir.create(paste(basepath,'/All_MI',sep='') ) )
mi.dir<-paste(basepath, "/All_MI",sep="")

streamline_one_folder <- function(h5Files) {
  
  # get a list of the date-plate combinations selected
  date_plates <- unique( lapply(h5Files, function(x) strsplit(basename(x), split = "_")[[1]][2:3]) )
  
  for (date_plate in date_plates) {
    
    # select the files corresponding to the current plate
    date <- date_plate[1]
    plate <- date_plate[2]
    plate_files <- h5Files[grepl(pattern = paste0(date,"_",plate,"_"), basename(h5Files))]
    file_split<-split(plate_files,1:length(plate_files)) # legacy thing, haven't had the gumption to change to vector yet
    
    cat("Starting", date, plate, "at", as.character.Date(Sys.time()), "\n")

    #Begin MI analysis
    parsed_data = load.spikedata_final(333,file_split)
    MI_output<-list()
    MI_output<-nmi_wrapper(parsed_data)

    # save the result as csv file
    file_name <- paste0(mi.dir,"/NMI_",date,"_",plate,".csv")
    fwrite(MI_output, file = file_name,col.names=TRUE, append =FALSE, row.names=FALSE, sep=",")
    
    cat("\tCompleted",basename(file_name),"at",as.character.Date(Sys.time()),"\n")
    
  }
  print(paste("MI files are ready in folder",mi.dir))
}

# Execute the function
streamline_one_folder(h5Files)
