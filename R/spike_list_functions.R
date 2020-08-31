# spike_list_functions.R
# Diana Hall
# May 5, 2015
# purpose: functions to run the pipeline with spike list files
# one electrode and one timestampe per row



spkList2list <-function (file) {
    
    
    # data.raw<-read.csv(file,header=T,colClasses=c("NULL", "NULL", NA, NA, NA))
    data.raw<-read.csv(file,header=T,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
    ######CG Additions 1/13/2016 
    ###To account for changes in axion update-- takes out well info 
    data.raw<-data.raw[-which(data.raw$Electrode==""),]
    data.raw<-data.raw[1:(nrow(data.raw)-40),]
    data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
    data.raw[,3]<-as.numeric(as.character(data.raw[,3]))
    #######End of CG Additions
    
    
    #remove NA
    ind.want<-which(!is.na(data.raw[,1]) )
    if (length( ind.want )>0){
      data.raw2<-data.frame(
        elect<-data.raw[ ind.want ,"Electrode"],
        timestamps<-data.raw[ ind.want ,"Time..s."],
        stringsAsFactors = FALSE
      )
 
      # if the recording lasts less than 720 seconds, flag it
      data.raw2 <- data.raw2[order(data.raw2$timestamps),]
      last_time <- tail(data.raw2$timestamps, n=1)
      if (last_time < (900 - 3*60)) {
        stop(paste0("\n",file," only goes to ",last_time," seconds\n"))
      }
      # if the recording went over 900 seconds, remove extra time
      else if (last_time > 900.0) {
        # get the index of the first time stamp that is over 900 seconds
        drop_index <- which(data.raw2$timestamps > 900.0)[1]
        data.raw2 <- data.raw2[1:(drop_index-1),]
        print(paste0("file went to ",last_time," seconds, dropped all points after 900.0s"))
      }
      
      # order data frame by electrode
      data.raw2<-data.raw2[order(data.raw2$elect), ]
      
      spikes<-split(data.raw2$timestamps, data.raw2$elect)
    } else {
      spikes<-NULL
    }
    
    spikes
  }



axion.spkList.to.h5<-function(key, spkListFile, chem.info, debug=T, remake_all=T ){
  #function to convert spike list to h5 file
  
  #remove _spike_list
  key<-unlist( strsplit(key, split="_spike_list") )
  
  # # correct for issues with ( ) in file names
  # h5file <- gsub("\\(|\\)", "_", sprintf("%s/%s.h5", h5.dir, key))
  # if ( substring(basename(h5file), nchar(basename(h5file))-3, nchar(basename(h5file))-3)=="_" ){
  #   h5file<-paste( dirname(h5file) ,"/" ,
  #     unlist( strsplit(basename(h5file), split="_.h5") ) ,
  #                 ".h5", sep="")
  # }
  
  # what if we just allow ()? let's give it a try
  h5file <- sprintf("%s/%s.h5", h5.dir, key)
  
  # if the h5file has already been created, skip it
  if(file.exists(h5file) & !remake_all) {
    return(h5file)
  }
  
  wildcard <- gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", sprintf("^%s.*csv$", key)))

  #f is a list of all files
  f <- spkListFile

  #get spikes
  spikes.sep <- lapply(f, spkList2list)
  
  short.filenames <- gsub("_spike_list.csv", "", basename(f))
  summary.table <- t(sapply(spikes.sep, sjemea::axion.spikesum2) )
  rownames(summary.table) <- short.filenames
  ma <- do.call("rbind", lapply(spikes.sep, sjemea::axion.spikestodf))
  #s2 is a list with all the channels and spikes under each channel
  s2 <- split(ma$time, ma$elec)
  numelec <- length(s2)
  total.spikes <- sum(sapply(s2, length))
  time.ranges <- sapply(s2, range)
  time.min <- min(time.ranges[1, ])
  time.max <- max(time.ranges[2, ])
  #printf formats the text and variables to output ready formats
  #cat contatenates files and then prints them
  cat(printf("Total number of spikes: %d\n", total.spikes))
  cat(printf("Unique number of electrodes: %d\n", numelec))
  cat(printf("Time range [%.3f %.3f] (seconds)\n", time.min, 
             time.max))
  print(summary.table)
  
  map.to.h5.dh(s2, chem.info, h5file)
  
  if (debug) {
    d <- as.data.frame(summary.table)
    d2 <- data.frame(file = rownames(summary.table), d, 
                     stringsAsFactors = FALSE)
    h5write(d2, path.expand(h5file), "summary.table")
  }
  
  
  h5file
  
  
}










