# This script will read in the raw data values and calculated the 
# blank-corrected flurescense or optical density values
# Output is a long file with all the necessary columns for tcpl (except for treatment name instead of spid column)

# Notes
# this script sets any negative raw values to zero
# this script addes a date column (for integration with check_unique_apid.R)

###################################################################################
# USER INPUT
###################################################################################
# set the location for the output file
basepath <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016"

# set the name of the output file
filename = "test_brown_cyto.csv"

# input_file <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Cytotoxicity_data/20140205mw100753CultureAlamarBlue.xlsx"
input_files <- list.files(path = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Cytotoxicity_data/", full.names = T)
masterChemFile <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/MaestroExperimentLog.xlsx"

# if empty, you can enter these at the command line
plate <- ""
date <- ""

cyto_type <- "CTB" # CTB/AB, or LDH

newFile <- TRUE

###################################################################################
# END USER INPUT
###################################################################################

library(xlsx)
library(tcltk)
library(pracma)


###################### FUNCTIONS

returnindex <- function(value, mydata, exact = T) {
  
  #  assign the checkval function
  if (exact) {
    checkval <- function(value, i, j) {
      res <- strcmp(as.character(mydata[i,j]),as.character(value))
      return(res)
    }
  } else {
    checkval <- function(value, i, j) {
      res <- grepl(pattern = value, as.character(mydata[i,j]))
      return(res)
    }
  }
  
  # cycle thru each row and column
  for (i in 1:nrow(mydata)) {
    for (j in 1:ncol(mydata)) {
      if (is.na(mydata[i,j])) {
        next
      }
      if (checkval(value, i, j)) {
        return(c(i,j))
      }
    }
  }
  # print("could not find index in data frame")
  return(NULL)
}

###################### END FUNCTIONS

getCytoData <- function(input_file, cyto_type, plate, date)
{
dat <- read.xlsx(input_file, sheetIndex = 1, header=F, stringsAsFactors = FALSE)
corner <- returnindex("Raw Data",dat, exact=F)

colNames <- c("rowc",unlist(dat[(corner[1]+1), (corner[2]):(corner[2]+11)], use.names = F))
dat2 <- dat[(corner[1]+2):(corner[1]+9), (corner[2]-1):(corner[2]+11)]
if (any(is.na(dat2))) {
  stop("valuemap contains NAs")
}

colnames(dat2) <- colNames
sourcedata <- melt(as.data.table(dat2), id.vars = c("rowc"), variable.name = "coli", value.name = "raw_value", variable.factor = F)
# warning bc some col's read as numeric, some as char in file

# do blank-correction
sourcedata$raw_value <- as.numeric(sourcedata$raw_value)
mean_of_blanks <- sourcedata[rowc == "G" & coli %in% c(1,2,3), mean(raw_value)]
sourcedata[, blank_corrected_value := raw_value - mean_of_blanks]

# remove wells we don't need
sourcedata <- sourcedata[rowc %in% c("A","B","C","D","E","F") & coli %in% 1:8]

# if any blank-corrected values are negative, then we should set these to zero
if (any(sourcedata$blank_corrected_value < 0)) {
  sourcedata[, blank_corrected_value := ifelse(blank_corrected_value < 0, 0.0, blank_corrected_value)]
  print("some blank-corrected values are negative. Setting these to zero")
}

# clean up the file, add columns
setnames(sourcedata, old = "blank_corrected_value", new = "rval")
sourcedata$srcf <- basename(input_file)
sourcedata[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
sourcedata$coli <- as.numeric(sourcedata$coli)
sourcedata$apid <- plate
sourcedata$date <- date # optional, adding this in to differentiate between plates that were reused in separate culture dates
sourcedata$wllq <-  1 # for good data

if (cyto_type == "LDH") {
  sourcedata$acsn = "NHEERL_MEA_dev_LDH"
} else if (cyto_type == "AB" | cyto_type == "CTB") {
  sourcedata$acsn = "NHEERL_MEA_dev_AB"
} else {
  stop("invalid cyto_type entered")
}


# get treatment, conc data from master chem list
masterChemData <- read.xlsx(masterChemFile, stringsAsFactors = FALSE, sheetIndex = 1)
setDT(masterChemData)

# add in missing "MW" prefixes
masterChemData[, Plate.SN := ifelse(grepl("MW",Plate.SN),Plate.SN,paste0("MW",Plate.SN))]

# match treatment by rowi and coli
for (ci in unique(sourcedata$coli)) {
  for(ri in unique(sourcedata$rowi)) {
    # get corresponding values from master chem list
    welli <- paste0(intToUtf8(ri + utf8ToInt("A") - 1),ci)
    compound <- masterChemData[Well == welli & Experiment.Date == date & Plate.SN == plate, Treatment]
    dose <- masterChemData[Well == welli & Experiment.Date == date & Plate.SN == plate, Dose.]
    
    
    # replace in sourcedata
    sourcedata[coli == ci & rowi == ri, `:=`(treatment = compound, conc = dose)]
  }
}

sourcedata[, wllt := ifelse(conc == 0, "n","t")]

# reorder columns
sourcedata <- sourcedata[,c("treatment","apid","rowi","coli","wllt","wllq","conc","rval","srcf","acsn", "date")]
}


# cycle through all input files
for (i in 1:length(input_files)) {
  
  if (date == "") {
    date <- readline(prompt = paste0("Enter date for ",basename(input_files[i])," : "))
  }
  if (plate == "") {
    plate <- readline(prompt = paste0("Enter plate name for ",basename(input_files[i])," : "))
  }
  
  dat <- getCytoData(input_files[i], cyto_type, date, plate)
  
  # write the data to a table
  fwrite(dat, file = paste(basepath, filename, sep = "/"), append = (!floor(1/i) | !newFile))
}




