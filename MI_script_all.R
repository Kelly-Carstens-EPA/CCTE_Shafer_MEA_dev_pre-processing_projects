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
suppressWarnings( dir.create(paste(basepath,'/All_MI',sep='') ) )
mi.dir<-paste(basepath, "/All_MI",sep="")

streamline_one_folder <- function(h5Files) {
  N = length(h5Files)
  prevdate = ""
  
  for (i in 1:N) {
    
    filename <- sub(pattern = "\\.h5", replacement="", basename(h5Files[i]))
    cat("Starting ", filename, "at", as.character.Date(Sys.time()), "\n")

    #Begin MI analysis
    parsed_data <- load.spikedata_final(333,h5Files[i])
    MI_output<-list()
    MI_output<-nmi_wrapper(parsed_data)
    
    # save the result as csv file
    write.table(MI_output, paste(mi.dir,"/NMI_",filename,".csv",sep=""),col.names=TRUE, append =FALSE, row.names=FALSE, sep=",")
  
  }
  print("MI files are ready")
}

# Execute the function
streamline_one_folder(h5Files)
