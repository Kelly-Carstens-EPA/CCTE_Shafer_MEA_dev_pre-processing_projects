# create_ont_csvFile.R
# Diana Hall
# 3-14-2014
# purpose: to create a burst data automatically from package

#load necessary packages
library(sjemea)
library(rhdf5)
library(lattice)
# library(tcltk) # tk_choose.files() is intolerant of spaces in file names
library(meadq)

create_ont_csv<-function( h5Files = NULL, save.rdata = FALSE, param.file = NULL, AEfile = FALSE){  
  
  #get the directory containing the .h5 files
  if (is.null(h5Files)){
    h5Files<-sort(choose.files(caption="Choose .h5 Files") )
  }
  
  #create directories
  assign("h5.dir", dirname(h5Files[1]), envir = .GlobalEnv )
  
  ###################################################################################
  # USER INPUT
  ###################################################################################
  # Set location where the prepared_data folder should be created
  # Can set create your own folder and input address such as, 
  #basepath = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/prepared_data"
  # or, use the following line to create the prepared_data folder next to the h5Files folder
  basepath = dirname(h5.dir)
  
  ###################################################################################
  # END USER INPUT
  ###################################################################################
  
  assign("prepared.dir", paste(basepath, "prepared_data", sep="/"), envir = .GlobalEnv )
  
  dir.create( prepared.dir )

  # output file names 
  # amy  name temporary changes here
  assign( "csv.filename.AEfilt",paste( prepared.dir, "/ont_data_summary_AEfilt",sep=""),
          envir = .GlobalEnv )
  assign( "csv.filename.ABEfilt",paste( prepared.dir, "/ont_data_summary_ABEfilt",sep=""  ),
          envir = .GlobalEnv )
  
 
  if ( is.null( param.file ) ){
    data('chgv_parameters')
  } else {
    if ( grepl(x=basename( param.file) , pattern=".rda") ){
      load(param.file)
    } else{
      source( param.file, local=TRUE  ) 
    }
  }

  

  create_burst_ont_Data(h5Files=h5Files, save.rdata=save.rdata, AEfile=AEfile)

} # end of create_ont_csv.R


# Execute the function

create_ont_csv()
