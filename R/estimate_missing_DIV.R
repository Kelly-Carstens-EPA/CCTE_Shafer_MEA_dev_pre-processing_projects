# Plate 20140730 MW1007-104 is missing DIV 9 recording
# After discussion with Tim 4/28/2020, we have decided to add approximate a DIV 9
# value based on the median of that value in the other 2 plates in this culture that tested the same compounds
# We are doing this so that the AUC value will not be relativly smaller or larger without the DIV 9 value
# see graphs in figs folder

# Note that I use median(x, na.rm=T). So if a parameter is na for one of the reference plates, the values from the other reference plate will be used
# If the values are NA in both plates, median(x, na.rm=T) will return NA. This will be set to zero regardless before AUC calculation

library(data.table)
estimate_missing_DIV <- function(dat, date_platei, add.DIV)
{
  
  if (!("date_plate" %in% names(dat))) dat[, date_plate := paste(date, Plate.SN, sep = "_")]
  trts <- dat[date_plate == date_platei & dose != 0, unique(trt)]
  cplates <- dat[date == sub("_.*$","",date_platei) & date_plate != date_platei & trt %in% trts, unique(date_plate)]
  cat("Estimating values for DIV",add.DIV,"on",date_platei,"from the plates",cplates,"\n")
  
  # get the columns for the endpoints
  id.cols <- c("date_plate","date","Plate.SN","DIV","well","trt","dose","units","file.name")
  usecols <- setdiff(names(dat), id.cols)
  
  # set trt name of control wells to all the same name, so that the median of these wells will be found together
  dat[, trt2 := trt]
  dat[dose == 0, trt2 := "Control"]
  
  # get a template of the ID data for platei from first DIV
  plate.dat.template <- dat[date_plate == date_platei & DIV == unique(DIV)[1], .SD, .SDcols = c(id.cols,"trt2")]
  
  # make all columns numeric (get an error with integers sometimes if not)
  dat[, c(usecols) := lapply(usecols, function(x) as.numeric(get(x)))]
  
  # calculate the median parameter value for each trt and dose
  # by trt2, with all control wells grouped
  add.dat <- dat[date_plate %in% cplates & DIV == add.DIV, lapply(.SD, function(x) median(x, na.rm = T)), by = c("trt2","dose","units"), .SDcols = usecols]
  add.dat <- merge(plate.dat.template, add.dat, by = c("trt2","dose","units"))
  add.dat[, `:=`(DIV = add.DIV, file.name = paste0("median_at_DIV",add.DIV,"_in_corresponding_wells_of_",paste0(cplates,collapse=",")))]
  
  # add these values back to dat
  dat <- rbind(dat, add.dat)
  dat[, trt2 := NULL]
  
  # quick visual verification of results in control wells
  plot(meanfiringrate ~ DIV, dat[date_plate == date_platei & dose == 0], pch = "", ylab = "Mean Firing Rate (Hz) in control wells",
       main = paste0("Verification of Estimation of DIV",add.DIV," in ",date_platei,"\n",
                     " from ",paste0(cplates,collapse=", ")),
       ylim = c(0, max(dat[date_plate == date_platei & dose == 0, meanfiringrate])*1.1))
  for (welli in dat[date_plate == date_platei & dose == 0, unique(well)]) {
    points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli][order(DIV)], type = "l")
    points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli][order(DIV)], pch = 19, col = "black")
    points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli & grepl("median_at",file.name)][order(DIV)], pch = 19, col = "blue")
  }
  legend(x = "topleft", legend = c("estimated DIV"), col = c("blue","gray70"), pch = c(19,19), bg = "transparent")
  
  return(dat)
}

# should I approximate the control wells as the median of all control wells, 
# or the median of control wells tested with the same trt label
# or the median of control wells from the same row?
# Same trt/row -> that doesn't make sense/what is tested in wells next to it should not have an effect
# possibly by well though, bc of the 

# verifications
# all_data[, unique(file.name)]
# date_platei <- "20190807_MW69-3715"
# trti <-  "12"
# plot(meanfiringrate ~ DIV, dat[date_plate == date_platei & trt == trti], pch = "", ylab = "Mean Firing Rate (Hz) in control wells",
#      main = paste0("Verification of Estimation of DIV",add.DIV," in ",date_platei,"\n",
#                    " from ",paste0(cplates,collapse=", ")),
#      ylim = c(0, max(dat[date_plate == date_platei & trt == trti, meanfiringrate])*1.1))
# for (welli in dat[date_plate == date_platei & trt == trti, unique(well)]) {
#   points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli][order(DIV)], type = "l")
#   points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli][order(DIV)], pch = 19, col = "black")
#   points(meanfiringrate ~ DIV, dat[date_plate == date_platei & well == welli & grepl("median_at",file.name)][order(DIV)], pch = 19, col = "blue")
# }

