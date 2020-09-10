# spike_list_functions.R
# Diana Hall
# May 5, 2015
# purpose: functions to run the pipeline with spike list files
# one electrode and one timestampe per row



spkList2list <-function (file) {
    
    
    # data.raw<-read.csv(file,header=T,colClasses=c("NULL", "NULL", NA, NA, NA))
    data.raw<-read.csv(file,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
    
    # using this instead of header=T, because sometimes the colnames are not in the first row in the spike list file
    header_row <- which(data.raw[,1] == "Time (s)")
    colnames(data.raw) <- data.raw[header_row,]
    data.raw <- data.raw[-which(data.raw[,1] == "Time (s)"),]
    
    ######CG Additions 1/13/2016 
    ###To account for changes in axion update-- takes out well info 
    data.raw <- data.raw[data.raw$Electrode != "",] # works even if no rows in Electrode are == ""
    data.raw<-data.raw[1:(nrow(data.raw)-40),]
    data.raw[,1]<-as.numeric(as.character(data.raw[,1]))
    data.raw[,3]<-as.numeric(as.character(data.raw[,3]))
    #######End of CG Additions
    
    #remove NA
    ind.want<-which(!is.na(data.raw[,1]) )
    if (length( ind.want )>0){
      data.raw2<-data.frame(
        elect<-data.raw[ ind.want ,"Electrode"],
        timestamps<-data.raw[ ind.want ,"Time (s)"],
        stringsAsFactors = FALSE
      )
 
      data.raw2 <- data.raw2[order(data.raw2$timestamps),]
      
      # Check if the recording was offset from original file start time (and so max_time can be 900 + offset)
      check_info <- read.csv(file, header = FALSE, nrows = 70, colClasses = c("character","character","NULL","NULL","NULL"))
      offset_string <- check_info[grepl("Recording Offset from File Start", check_info$V1),"V2"]
      if (length(offset_string) > 0 && !is.na(offset_string)) {
        if (!grepl("m",offset_string)) offset_string <- paste0("0m",offset_string)
        offset_values <- strsplit(offset_string, split = "[ms]")[[1]] # offset_string is e.g. "27m0s" or "0m38s"
        offset_seconds <- as.numeric(offset_values[1])*60 + as.numeric(offset_values[2]) # minutes*60 + seconds
      } else {
        offset_seconds <- 0.0
      }
      max_time <- 900.00 + offset_seconds
      
      # this still feels tenuous...
      # what if the tag phrase "Recording Offset from File Start" change, even slightly, with diff versions of axion?
      # what if diff versions change the format of _m_s?
      # what if... what if?
      
      # if the recording is over 3 minutes short of max_time, flag it
      last_time <- tail(data.raw2$timestamps, n=1)
      if (last_time < (max_time - 3*60)) {
        cat(paste0("\n",file," only goes to ",last_time," seconds\n"))
        cat("Continue with this spike list file anyways? (Only do this if you know why the recording is significantly less than 900sec\n")
        resp <- readline(prompt = "(y/n): ")
        if (!(resp %in% c("y","Y"))) {
          stop("Update spike list file selection")
        }
      }
      # remove any points after max_time
      data.raw2 <- data.raw2[data.raw2$timestamps <= max_time,]
      
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










