# current sript: 537 lines
# target number of lines: 300 or less

# this function has been edited to reference local.corr.all.ont.ae.filter (not in meadq package)
# to calculate the correlation coefficient 12/13/2019

create_burst_ont_Data <-
  function(h5Files,  save.rdata=F, add.silent.wells=T , AEfile = F){
    
    # get list of plates from h5File names
    plates <- unique( sapply(strsplit(basename(h5Files), split="_"), function(x) x[3]) )
    num.plates <- length(plates)
    
    for (cur.plate in 1:num.plates){
      
      # initialize write.header - want new file for new plate
      write.header = T
      
      # initialize s object. s will hold all of the data for cur.plate
      s=list()
      
      # get the h5Files that match the current plate, order by DIV
      h5Files.plate <- h5Files[grepl(pattern = paste0("_",plates[cur.plate],"_"), x = basename(h5Files))]
      div.list <- sapply(strsplit(basename(h5Files.plate),split="_"), function(x) x[4])
      h5Files.plate = h5Files.plate[order(as.numeric(div.list))]
      
      for (cur.file in 1:length( h5Files.plate ) ){
        
        # ------------------------------------------------
        # Calculate activity features
        # ------------------------------------------------
        
        # initialize s[[cur.file]], gather spike data per well, and calculate meanfiringrate by well
        s[[cur.file]]=h5.read.spikes( h5Files.plate[cur.file] ) # sjemea
        
        # get meta-data
        g <- c()
        g<-h5read(path.expand( h5Files.plate[cur.file] ), name = "/")
        if ( is.element( "treatment", tolower( names(g) ) ) ){
          names(g)<-tolower(names(g))
          names(g)[names(g)=="scount"]<-"sCount"
          s[[cur.file]]$cw = substring(s[[cur.file]]$channel,1,2 )
          s[[cur.file]]$treatment = g$treatment         
          names(s[[cur.file]]$treatment) = g$well
          s[[cur.file]]$dose = g$dose
          names(s[[cur.file]]$dose) = g$well
          s[[cur.file]]$sCount = g$sCount
          names(s[[cur.file]]$sCount) = g$names
          s[[cur.file]]$units = g$units
          names(s[[cur.file]]$units) = g$well # appears to list all the wells, regardless of whether or not they spiked
          s[[cur.file]]$well = g$well 
          s[[cur.file]]$summary.table = g$summary.table
          s[[cur.file]]$array = g$array     
          s[[cur.file]]$DIV = strsplit( basename(s[[cur.file]]$file), split="_")[[1]][4]
        } else{
          print ('Error: meta-data not available in .h5 file');
          
        }
        
        # calculate features related to bursting
        allb <- list()
        allb <- lapply(s[[cur.file]]$spikes, mi.find.bursts) # sjemea
        s[[cur.file]]$allb <- allb  
        s[[cur.file]]$bs<-calc.burst.summary( s[[cur.file]]  )
        s[[cur.file]]$isi <- calc.all.isi(s[[cur.file]], allb) # sjemea
        
        # calculate features related to network spikes
        nspikes.old <- calculate.network.spikes(s[[cur.file]])
        s[[cur.file]]$ns.all <- nspikes.old$ns.all
        # nspikes.old is modified/updated with the following function:
        nspikes <- summary.network.spikes.dh(s[[cur.file]],nspikes.old, ns.E = 2, sur)
        s[[cur.file]]$ns.all <- nspikes$ns.all
        
        # calculate local correlation feature
        ae.index.v<- unlist( split((s[[cur.file]]$meanfiringrate*60>=5), f=s[[cur.file]]$cw ) )
        # ae.index.v is a vector of the electrodes that fired at least once, grouped by well, with value T/F indiciating if it is an AE
        s[[cur.file]]$local.cor <- local.corr.all.ont.ae.filter(s, t.p=cur.file, ae.index.v)

        
        # ----------------------------------
        # Gather well indexign vectors

        all.well.names <- s[[cur.file]]$well
        # s[[cur.file]]$cw - vector of the well names, with 1 entry per spiking electrode
        
        # not amazing, but this will work
        
        # vector of well names, with 1 entry corresponding to each electrode that spiked at least once
        # (only have mfr, etc data for these electrodes)
        well.names.of.active.channels <- subset(s[[cur.file]]$cw, ae.index.v)
        
        # list the wells that have at least one AE
        well.names <- unique( well.names.of.active.channels )
        num.wells <-  length( unique (well.names) )
        well.indices <- which(is.element(s[[cur.file]]$well, well.names ))
        
        # ABE filter stuff
        abe.index<- unlist( split((s[[cur.file]]$bs$bursts.per.min>=0.5), f=s[[cur.file]]$cw ) )
        # Restrict all ABE to also be AEs (there actually was an instance when this was not true!)
        abe.index = abe.index & ae.index.v
        
        # we need the wells that have at least one ABE
        well.names.of.abe.channels <- subset(s[[cur.file]]$cw, abe.index)
        well.names.abe<-unique( subset(s[[cur.file]]$cw, abe.index) )
        # well.indices.abe = which(is.element(s[[cur.file]]$well, well.names.abe ))
        
        # ----------------------------------
        # Calculate all features
        
        features <- list(
          # endpoints that are averaged by electrode
          ## endpoints filtered by AE
          meanfiringrate = tapply(X = subset(s[[cur.file]]$meanfiringrate, ae.index.v),
                                  INDEX = well.names.of.active.channels, FUN = mean, na.rm = T),
          
          burst.per.min = tapply(X = subset(s[[cur.file]]$bs$bursts.per.min, ae.index.v),
                                 INDEX = well.names.of.active.channels, FUN = mean, na.rm = T),
          
          ## endpoints filterd by ABE
          mean.isis = tapply(X = subset(s[[cur.file]]$bs$mean.isi, abe.index),
                             INDEX = well.names.of.abe.channels, FUN = mean, na.rm = T),
          
          per.spikes.in.burst = tapply(X = subset(s[[cur.file]]$bs$per.spikes.in.burst, abe.index),
                                       INDEX = well.names.of.abe.channels, FUN = mean, na.rm = T),
          
          mean.dur = tapply(X = subset(s[[cur.file]]$bs$mean.dur, abe.index),
                            INDEX = well.names.of.abe.channels, FUN = mean, na.rm = T),
          
          mean.IBIs = tapply(X = subset(s[[cur.file]]$bs$mean.IBIs, abe.index),
                             INDEX = well.names.of.abe.channels, FUN = mean, na.rm = T),
          
          # endpoints that are sums per well
          # was [well.names] list of wells with at least one AE, or at least one spikeing electrode?
          # these are numeric lists, so all ready to go into data table, with keep.rownmaes = "well"
          
          # NOTE; I removed filter to only allow wells with at least one AE
          
          nAE = unlist( lapply( by( s[[cur.file]]$meanfiringrate*60, s[[cur.file]]$cw, 
                                    function(x) x>=5), sum ) ),
          
          nABE= unlist( lapply( 
            by( s[[cur.file]]$bs$bursts.per.min, s[[cur.file]]$cw, 
                function(x) x>=0.5), sum ) ),
          
          ns.n = sapply(s[[cur.file]]$ns.all, function(x) x$brief['n']),
          
          # network spike endpoints. These also numeric
          ns.peak.m = sapply(s[[cur.file]]$ns.all, function(x) x$brief['peak.m']),
          
          ns.durn.m = sapply(s[[cur.file]]$ns.all,function(x) x$brief['durn.m']),
          
          ns.percent.of.spikes.in.ns = sapply(s[[cur.file]]$ns.all, function(x) x$brief['percent.of.spikes.in.ns']),
          
          ns.mean.insis = sapply(s[[cur.file]]$ns.all, function(x) x$brief['mean.insis']),
          
          ns.durn.sd = sapply(s[[cur.file]]$ns.all, function(x) x$brief['durn.sd']),
          
          ns.mean.spikes.in.ns = sapply(s[[cur.file]]$ns.all, function(x) x$brief['mean.spikes.in.ns']),
          
          # somthign seems a bit off here, with all the warnings, and oddly 47 wells
          r=sapply(s[[cur.file]]$local.cor, mean, na.rm=T)
          
        )
      
        # create dt and add in ID columns to dt
        dt <- data.table()
        dt[, well := all.well.names]
        filename.split <- strsplit(basename(s[[cur.file]]$file), split="_")[[1]]
        dt[, date := filename.split[2] ]
        dt[, Plate.SN := filename.split[3] ]
        dt[, DIV := filename.split[4] ] # wait, is s[[cur.file]]$DIV a thing??
        dt[, trt := s[[cur.file]]$treatment]
        dt[, dose := s[[cur.file]]$dose]
        dt[, units := s[[cur.file]]$units]
        dt[, file.name := basename(s[[cur.file]]$file)]
        
        # ------------------ first idea
        # group endpoints by similarities in how calculate
        # Features for which well-level values are calculated as an average of channel values at this step
        # (other might be avearged too, but in previous scripts)
        
        # features.to.average <- list(
        #   meanfiringrate.by.channel = subset(s[[cur.file]]$meanfiringrate, ae.index.v),
        #   burst.per.min.by.channel = subset(s[[cur.file]]$bs$bursts.per.min, ae.index.v),
        #   
        #   # not sure these are working right. Plus, need ABE filter.
        #   # then well.names.spiking.electrode no longer applies...
        #   per.spikes.in.burst.by.channel = subset(s[[cur.file]]$bs$per.spikes.in.burst, ae.index.v),
        #   
        #   mean.dur.by.channel = subset( s[[cur.file]]$bs$mean.dur, ae.index.v),
        #   
        #   mean.IBIs.by.channel = subset( s[[cur.file]]$bs$mean.IBIs, ae.index.v)
        # )
        # 
        # for (i in 1:length(features.to.average)) {
        #   feati <- features.to.average[[i]]
        #   endpoint <- tapply(X = feati, INDEX = well.names.of.active.channels, FUN = mean, na.rm = T)
        #   wellnames <- names(endpoint)
        #   # needs to be numeric, instead of array. This drops the well names
        #   # add data to dt. Holes will be filled with Na
        #   dt <- merge(dt, data.table(endpoint = as.numeric(endpoint), well = wellnames), all = TRUE)
        #   feature_name <- sub("\\.by\\.channel", "", names(features.to.average)[i])
        #   setnames(dt, old = "endpoint", new = feature_name)
        # }
        #--------------- end first idea. too many different things, I think
        # and it is kinda nice seeing each endpoint explicitly stated
        
        
        # --------------- resume current idea
        
        # add the features to the data table
        for (i in 1:length(features)) {
          featurei <- features[[i]]
          wellnames <- names(featurei)
          wellnames <- regmatches(wellnames, regexpr("[A-F]{1}[1-8]{1}", wellnames)) # extract just the well part (some are e.g. A1.peak.m)
          dt <- merge(dt, data.table(featurei = as.numeric(featurei), well = wellnames), all = TRUE)
          feature_name <- names(features)[i]
          setnames(dt, old = "featurei", new = feature_name)
        }
        
        # if num.wells<0, will that cause probs now? (or abe.index)
        
        #change NA to 0 for endpoints that may be zero
        may.be.zero<-c("nAE", "nABE", "meanfiringrate", "burst.per.min",
                       "per.spikes.in.burst", 
                       "ns.percent.of.spikes.in.ns","ns.n", 
                       "per.spikes.in.burst", "r")
        for(cur.var in may.be.zero ){
          na_rows <- which(is.na(dt[,..cur.var]))
          set(dt, i = na_rows, j = cur.var, value = 0)
        }
        
        
        # write data to .csv file
        write.table(  dt, 
                      file= paste( paste( csv.filename.ABEfilt,
                                          strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                          plates[cur.plate],sep="_"), ".csv", sep="" ),
                      sep=",", col.names = write.header, append = !write.header, row.names=F )
        write.header=F
      } #end loop through h5Files in current plate
      
      
      if (save.rdata ){
        save(s, file = paste(h5.dir ,  paste("/s",
                                             strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                             plates[cur.plate] , 
                                             paste('thru',s[[cur.file]]$DIV, ".Rdata", sep="") ,
                                             sep="_" ), sep="")    ) 
      }
      
      print(paste('-----------------       Done with file # ', cur.file, sep= "") )
      
    } # end of loop through plates

  }
