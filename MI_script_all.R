# This allows you to select the h5 files
# then calls the functions to calculate the NMI

library(rhdf5) # this is the library from Bio Conductor
require('pracma')
library('compiler')
require('gtools')

h5Files<-choose.files(caption="Select .h5 Files")
h5Files = sort(h5Files)

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
suppressWarnings( dir.create(paste(basepath,'/All_MI_test',sep='') ) )
mi.dir<-paste(basepath, "/All_MI_test",sep="")

streamline_one_folder <- function(h5Files) {
  
  # get a list of the plates selected
  plates <- sapply(h5Files, function(x) strsplit(basename(x), split = "_")[[1]][3], USE.NAMES = FALSE)
  plates <- unique(plates)
  
  for (plate in plates) {
    
    # select the files corresponding to the current plate
    plate_files <- h5Files[grepl(pattern = plate, basename(h5Files))]
    date <- strsplit(basename(plate_files[1]), split = "_")[[1]][2]
    plate <- strsplit(basename(plate_files[1]), split = "_")[[1]][3]
    file_split<-split(plate_files,1:length(plate_files)) # legacy thing, haven't had the gumption to change to vector yet
    
    cat("Starting plate", plate, "at", as.character.Date(Sys.time()), "\n")
    
    #Begin MI analysis
    parsed_data = load.spikedata_final(333,file_split)
    MI_output<-list()
    MI_output<-nmi_wrapper(parsed_data)

    # save the result as csv file
    write.table(MI_output, paste(mi.dir,"/NMI_",date,"_",plate,".csv",sep=""),col.names=TRUE, append =FALSE, row.names=FALSE, sep=",")

  }
  print("MI files are ready")
}

# Execute the function
streamline_one_folder(h5Files)
