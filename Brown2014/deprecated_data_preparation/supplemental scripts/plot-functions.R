# functions for comparing endpoints over DIV from 2 different cultures
# or possibly, two different plates, trt, etc

getPreparedData <- function(path_to_dat) {
  data.files <- list.files(path = path_to_dat, full.names = T)
  alldat <- list()
  for (file in data.files) {
    dat  <- fread(file)
    alldat <- rbind(alldat, dat)
  }
  return(alldat)
}


compareControlsByEndpoint <- function(run1.data, run2.data = NULL, endpoint, plot_type = "p", single_plot = TRUE, offsetDIV=TRUE) {
  
  usecols <- c("date","Plate.SN","well","DIV","dose","trt",endpoint)
  plotcols <- c("DIV", endpoint)
  
  # order by DIV
  run1.data <- run1.data[order(DIV)]
  run2.data <- run2.data[order(DIV)]
  
  # plot endpoint control wells data from run 1
  plotdata <- run1.data[dose == 0, ..usecols]
  date1 <- unique(plotdata$date)
  wells <- sort(unique(plotdata$well))
  xrange <- range(c(run1.data$DIV, run2.data$DIV))
  if (offsetDIV) xrange[2] <- xrange[2] + 0.5
  plot(plotdata[well == wells[1], ..plotcols], pch = "", xlim = xrange, ylim = range(c(run1.data[,..endpoint], run2.data[, ..endpoint]), na.rm=T))
  
  for (plate in unique(plotdata$Plate.SN)) {
    for (welli in wells) {
      points(plotdata[Plate.SN == plate & well == welli, ..plotcols], type = plot_type, cex=1.25)
    }
  }
  
  # add in data from run2
  plotdata <- run2.data[dose == 0, ..usecols]
  date2 <- unique(plotdata$date)
  # offset x val's, so easier to distinguish points
  if (offsetDIV) plotdata[, DIV := DIV + 0.25]
  wells <- sort(unique(plotdata$well))
  
  for (plate in unique(plotdata$Plate.SN)) {
    for (welli in wells) {
      points(plotdata[Plate.SN == plate & well == welli, ..plotcols], type = plot_type, col = "blue", cex=1.25)
    }
  }
  
  # guess distinguishing ID for run1 and run2 for legend
  if (all(date1 != date2)) {
    run1.id <- date1
    run2.id <- date2
  } else {
    run1.id <- paste0(unique(run1.data$Plate.SN), collapse=",")
    run2.id <- paste0(unique(run2.data$Plate.SN), collapse=",")
  }
  
  
  if (single_plot) {
    legend(x = "topleft", legend = c(run1.id, run2.id), col = c(1,4), pch = 19, cex = 1)
    title(main = paste0(endpoint, " - Comparison of Control Wells\nin ", run1.id, " and ", run2.id))
  } else {
    mtext(text = c(paste0("Comparison of Control Wells in\n", run1.id, " and\n"),paste0("\n\n",run2.id)), col = c(1,4),side = 3, outer = TRUE, cex = 1)
  }
  
}

