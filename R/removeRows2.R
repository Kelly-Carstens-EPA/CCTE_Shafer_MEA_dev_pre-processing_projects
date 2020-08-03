# Script that removes rows of unwanted data
library(tcltk)

inputFile = tk_choose.files(caption = "select AUC or cytotox file that needs to be trimmed")

###################################################################################
# USER INPUT
###################################################################################
# set the location for the output file
# basepath = ""
# or, use the following line put the output next to the existing data
basepath = dirname(inputFile)

# set the name of the output file
# outfile = paste("trimmed_", basename(inputFile),sep = "") # e.g., outfile = paste(basename(inputFile),"_trimmed.csv",sep = "")
# or, to overwrite the existing file, use the following line
outfile = basename(inputFile)

# For which plates will you be removing rows?
plates = list("MW1140-26", "MW1140-27", "MW1140-28")
# plates = list("MW1140-26", "MW1140-27", "MW1140-28")
# For which compound?
compoundName = "Valinomycin"
# Do you want to keep the plate data for only rows with compoundName (use TRUE),
# or do you want to keep all of the data EXCEPT the rows with compoundName (use FALSE)?
keepCompoundData = TRUE

# For the output log file Information for output log file, to keep record of the changes you have made
# Create a new log file, or appending to an existing one?
newLogFile = FALSE

###################################################################################
# END USER INPUT
###################################################################################

setwd(basepath)

removeRows = function(mydata, plates, compound, keepCompoundData) {

  nrowstart = nrow(mydata)
  
  # identify name of plate column
  platecol1 = "apid"
  platecol = platecol1
  
  if (keepCompoundData) {
    # mydata = mydata[all rows (except rows with given plates and not that compound) OR control well]
    mydata = mydata[(!((is.element(mydata[,platecol],plates))&(mydata$treatment != compound))) | (mydata$conc == 0),]
  } else {
    # mydata = mydata[all rows (except rows with given plates and that compound) OR control well]
    mydata = mydata[(!((is.element(mydata[,platecol],plates))&(mydata$treatment == compound))) | (mydata$conc == 0),]
  }
  
  nrowend = nrow(mydata)
  cat(nrowstart - nrowend, "rows removed\n")
  return(mydata)
}


mydata = read.csv(inputFile, stringsAsFactors = FALSE)
mydata = removeRows(mydata, plates, compoundName, keepCompoundData)
write.table(mydata, file=outfile, sep=",", append = F, col.names=T, row.names=F )

cat(outfile, "is ready\n")

# Creating log file data

inputFileName = basename(inputFile)
outputFileName = outfile
if (keepCompoundData) {
  actionDescription = paste("Removed data rows that are not for",compoundName, "on plates", plates,"\n",sep = " ")
} else {
  actionDescription = paste("Removed data rows for",compoundName, "on plates",plates, "\n",sep = " ")
}

sink(file = paste("trim_log_",inputFileName, ".txt", sep=""), append = newLogFile)
cat("Changes made on",as.character.Date(Sys.Date()),":\n")
cat("input file name",inputFileName,"\n")
cat("output file name",outputFileName,"\n")
cat("Action taken:",actionDescription,"\n\n")

sink()

cat("log file is ready\n")
