# spike_list_functions.R
# Diana Hall
# May 5, 2015
# purpose: functions to run the pipeline with spike list files
# one electrode and one timestampe per row

spkList2list <-function (file) {
    
    data.raw<-suppressWarnings( fread(input=file,header=T,select=c(3,4,5), colClasses = c("NULL", "NULL", "numeric","character","numeric"), 
                                      fill = FALSE, check.names = TRUE) )
    # fread will stop when it encounters an empty line
    # Will return a warning if there is text after the empty line
    # e.g. 'Well infomration', as in later versions of Axion
    # but we want to drop the Well informatin regardless, so the warning can be ignored
    
    #remove NA
    ind.want<-which(!is.na(data.raw[,1]) )
    if (length( ind.want )>0){
      data.raw <- data.raw[ind.want, list(elect=Electrode, timestamps = Time..s.)]
      data.raw <- data.raw[order(elect)]
      spikes<-split(data.raw$timestamps, data.raw$elect)
      
    } else {
      spikes<-NULL
    }
    
    spikes
  }



axion.spkList.to.h5<-function(key, spkListFile, chem.info, debug=T ){
  #function to convert spike list to h5 file
  
  #remove _spike_list
  key<-unlist( strsplit(key, split="_spike_list") )
  
  # Replace any ( ) with "_" in the file name (but not in h5.dir)
  key <- gsub("\\(|\\)", "_", key)
  h5file <- sprintf("%s/%s.h5", h5.dir, key)
  
  # if h5file ends in "_.h5", remove the "_"
  h5file <- sub("_.h5", ".h5", h5file)

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
  cat(printf("Total number of spikes: %d\n", total.spikes))
  cat(printf("Unique number of electrodes: %d\n", numelec))
  cat(printf("Time range [%.3f %.3f] (seconds)\n", time.min, 
             time.max))
  print(summary.table)
  
  map.to.h5.dh(s2, chem.info, h5file)
  
  # add the date and plate data to the h5 file
  h5write(chem.info$Experiment.Date, h5file, "/Experiment.Date")
  h5write(chem.info$Plate.SN, h5file, "/Plate.SN")
  
  if (debug) {
    d <- as.data.frame(summary.table)
    d2 <- data.frame(file = rownames(summary.table), d, 
                     stringsAsFactors = FALSE)
    h5write(d2, path.expand(h5file), "summary.table")
  }
  
  
  h5file
  
  
}










