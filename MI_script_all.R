# This script calculates the MI parameter
# Notes:
# - Must input all 4 DIV h5 files for each plate

require('pracma')
library('compiler')
require('h5')
require('Matrix')
require('gtools')

h5Files<-choose.files(caption="Choose .h5 Files")
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
suppressWarnings( dir.create(paste(basepath,'/All_MI',sep='') ) )
mi.dir<-paste(basepath, "/All_MI",sep="")

# Use this function if all h5files are in same folder, and there is an h5File for each DIV 5,7,9,12 for each culture date
streamline_one_folder = function(h5Files) {

  N = length(h5Files)
  prevdate = ""
  
  for (i in seq(1,N,by=4)) {
    
    filename = basename(h5Files[i])
    date = strsplit(filename, split = "_")[[1]][2]
    plate = strsplit(filename, split = "_")[[1]][3]
    files = h5Files[grep(plate, h5Files)] # select all the DIV files for this plate
    file_split<-split(files,1:length(files))
    cat("Starting plate", plate, "at", as.character.Date(Sys.time()), "\n")

    #Begin MI analysis
    parsed_data = load.spikedata_final(333,file_split)
    MI_output<-list()
    MI_output<-nmi_wrapper(parsed_data)

    # want to add col.names only if starting a new file for a new culture date
    if (date == prevdate) {
      write.table(MI_output, paste(mi.dir,"/MI_",date,".csv",sep=""),col.names=FALSE, append = TRUE, row.names=FALSE, sep=",")
    } else {
      write.table(MI_output, paste(mi.dir, "/MI_",date,".csv",sep=""), col.names=TRUE, append = FALSE, row.names=FALSE, sep=",")
      prevdate = date
    }
  }
  print("MI files are ready")
}

# Execute the function
streamline_one_folder(h5Files)