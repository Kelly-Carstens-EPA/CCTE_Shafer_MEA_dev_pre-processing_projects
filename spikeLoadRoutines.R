require('rhdf5')
require('gtools')

load.spikedata_final <- function(pseudoSamplingRate,fileName){
  # Loads spike data for a particular recording from h5 file.
  # Designed for [6,8] plate array with 4x4 MEAs in each well.
  # Returns a [6,8] data.frame of sparse [16,N] matrices. 
  #fileName<-choose.files("choose the h5 files to be converted")  ### important that all files from same plate
  x<-list()
  meta<-list()
  for (j in 1:length(fileName)){
    
  # file <- h5read(fileName[[j]], 'r') # changing this to instead read each object from h5file individually
  # other option: file <- H5Fopen(fileName[[j]])
    # then access objects by file$spikes, etc. Then don't have to open as often...? see which is faster/more data efficient
  test_h5<-try(h5read(fileName[[j]], '/Well'), silent=T) # checking capitalization of objects in fileName[[j]]
  if (inherits(test_h5,"try-error")){
  
  spikes <- h5read(fileName[[j]], '/spikes')
  well <- h5read(fileName[[j]], '/well')
  names <- h5read(fileName[[j]], '/names')
  sCount <- h5read(fileName[[j]], '/sCount')
  treatment<-h5read(fileName[[j]], '/treatment')
  dose<-h5read(fileName[[j]], '/dose')
  units<-h5read(fileName[[j]], '/units')
  #plate<-h5read(fileName[[j]], '/Plate.SN')
  #date<-h5read(fileName[[j]], '/Experiment.Date')
  #div<-h5read(fileName[[j]], '/DIV')
  }  else{
  spikes <- h5read(fileName[[j]], '/spikes')
  well <- h5read(fileName[[j]], '/Well')
  names <- h5read(fileName[[j]], '/names')
  sCount <- h5read(fileName[[j]], '/sCount')
  treatment<-h5read(fileName[[j]], '/Treatment')
  dose<-h5read(fileName[[j]], '/Dose')
  units<-h5read(fileName[[j]], '/Units')
  plate<-h5read(fileName[[j]], '/Plate.SN')
  date<-h5read(fileName[[j]], '/Experiment.Date')
  }
  file.name<-basename(fileName[[j]])
  
  t <- seq(min(spikes[]),max(spikes[]),by=1/pseudoSamplingRate)
  t0 <- min(spikes[])
  
  spikeStreamArray <- as.list(matrix(nrow = 6, ncol = 8))
  dim(spikeStreamArray) <- c(6,8)
  # Initialize spiking array of zeros for each well
  for (rr in 1:6) {
    for (cc in 1:8) {
      spikeStreamArray[[rr,cc]] <- matrix(0,nrow = 16, ncol = (length(t)))
    }
  }
  
  # Populate each electrode in each well.
  # Electrode conversion chart:
  # #| 1  2  3  4
  # --------------
  # 1| 1  2  3  4
  # 2| 5  6  7  8
  # 3| 9  10 11 12
  # 4| 13 14 15 16
  
  recallIndex = 1
  
  for (ii in 1:length(sCount[])) {
    rr <- substr(names[ii],1,1) # the spiking electrode names
    # Well row number:
    # Convert character to ascii code value.. then normalize:
    # [A,B,C,D,E,F] -> [65,66,67,68,69,70] -> '' - 64 ->[1,2,3,4,5,6]
    rr <- as.integer(asc(rr))-64L
    # Well column number:
    cc <- strtoi(substr(names[ii],2,2))
    # Electrode Number (see conversion chart above)
    ee <- strtoi(substr(names[ii],4,4))-1
    ee <- ee*4 + strtoi(substr(names[ii],5,5))
    
    # Now that we know where we are, we assign spikes at each time
    # that they occur
    #
    # Retrieve the spiking times:
    spikeTimes <- spikes[recallIndex:(recallIndex+sCount[ii]-1)]
    # Convert to indices:
    spikeTimes <- floor((spikeTimes-t0)*pseudoSamplingRate)+1 # convert time in s to num of 'samples'/bins after start
    # Assign binary 1 values to array where spikes occur:
    spikeStreamArray[[rr,cc]][ee,spikeTimes] <- 1 # error here: Error in .local(x, i, j, ..., value) : not-yet-implemented 'Matrix[<-' method
    
    # Update the recallIndex value so we crawl ahead through spikes
    recallIndex <- recallIndex + sCount[ii]
  }
  x[[j]]<-spikeStreamArray
  
  wells_vector<-c()
  trt_vector<-c()
  dose_vector<-c()
  unit_vector<-c()
  plate_vector<-c()
  date_vector<-c()
  div_vector<-c()
  file.name_vector<-c()
  
  if (inherits(test_h5,"try-error")){
  
  
  for (i in 1:dim(well)){
    wells_vector[i]<-well[i]
    trt_vector[i]<-treatment[i]
    dose_vector[i]<-dose[i]
    unit_vector[i]<-units[i]
    #plate_vector[i]<-plate[i]
    #date_vector[i]<-date[i]
    #div_vector[i]<-div[i]
  }
  # div<-substr(fileName[[j]],nchar(fileName[[j]])-11,nchar(fileName[[j]])-10)
  # plate<-substr(fileName[[j]],nchar(fileName[[j]])-21,nchar(fileName[[j]])-13)
  # date<-substr(fileName[[j]],nchar(fileName[[j]])-30,nchar(fileName[[j]])-23)
  # Amy changed this 11/18/2019
  # files with plate sn with fewer digits did not turn out the same
  date<-strsplit(file.name,split = "_")[[1]][2]
  plate<-strsplit(file.name,split = "_")[[1]][3]
  div<-strsplit(file.name,split = "_")[[1]][4]
  
    
  div_vector<-rep(div,length(wells_vector))
  plate_vector<-rep(plate,length(wells_vector))
  date_vector<-rep(date,length(wells_vector))
  } else{
    
    for (i in 1:dim(well)){
      wells_vector[i]<-well[i]
      trt_vector[i]<-treatment[i]
      dose_vector[i]<-dose[i]
      unit_vector[i]<-units[i]
      plate_vector[i]<-plate[i]
      date_vector[i]<-date[i]
      #div_vector[i]<-div[i]
    }
    div<-substr(fileName[[j]],nchar(fileName[[j]])-11,nchar(fileName[[j]])-10)
    #plate<-substr(fileName[[j]],nchar(fileName[[j]])-22,nchar(fileName[[j]])-13)
    #date<-substr(fileName[[j]],nchar(fileName[[j]])-31,nchar(fileName[[j]])-23)
    
    div_vector<-rep(div,length(wells_vector))
  }
  file.name_vector<-rep(file.name, length(wells_vector))
  
  meta[[j]]<-cbind(date_vector,plate_vector,div_vector,wells_vector,trt_vector,dose_vector,unit_vector,file.name_vector)
  
  }
  
  
  
  
  
  #date5<-substr(fileName[[1]],nchar(fileName[[1]])-30,nchar(fileName[[1]])-23)
  #date7<-substr(fileName[[2]],nchar(fileName[[2]])-30,nchar(fileName[[2]])-23)
  #date9<-substr(fileName[[3]],nchar(fileName[[3]])-30,nchar(fileName[[3]])-23)
  #date12<-substr(fileName[[4]],nchar(fileName[[4]])-30,nchar(fileName[[4]])-23)
  
  #plate5<-substr(fileName[[1]],nchar(fileName[[1]])-21,nchar(fileName[[1]])-12)
  #plate7<-substr(fileName[[2]],nchar(fileName[[2]])-21,nchar(fileName[[2]])-12)
  #plate9<-substr(fileName[[3]],nchar(fileName[[3]])-21,nchar(fileName[[3]])-12)
  #plate12<-substr(fileName[[4]],nchar(fileName[[4]])-21,nchar(fileName[[4]])-12)
  
  #div5<-rep(div5,nrow(meta))
  #div7<-rep(div7,nrow(meta))
  #div9<-rep(div9,nrow(meta))
  #div12<-rep(div12,nrow(meta))
  
  #date5<-rep(date5,nrow(meta))
  #date7<-rep(date7,nrow(meta))
  #date9<-rep(date9,nrow(meta))
  #date12<-rep(date12,nrow(meta))
  
  #plate5<-rep(plate5,nrow(meta))
  #plate7<-rep(plate7,nrow(meta))
  #plate9<-rep(plate9,nrow(meta))
  #plate12<-rep(plate12,nrow(meta))
  
  meta5<-meta[[1]]
  meta7<-meta[[2]]
  meta9<-meta[[3]]
  meta12<-meta[[4]]
  
  L=length(x)
  x[[L+1]]<-meta5
  x[[L+2]]<-meta7
  x[[L+3]]<-meta9
  x[[L+4]]<-meta12
  
  
  # Return the collection of recordings across wells
  # for the given plate.
  return(x)
}