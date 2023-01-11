# This script will extract the blank-corrected flurescense values (instead of the percent of control)
# Script to process cytotoxicity data to prepare for tcpl
# Output is a long file with all the necessary columns for tcpl (except no spid, just treatment column)
# This script will extract the cytotoxicity data (LDH and Alamar Blue) from the input files
# Example input files:
# - "ON_20160720_MW1139-19_Summary.xlsx" - these contain data for 1 plate per sheet (LDH and Alamar Blue)
# - "20171011_ON G8_2 Calculations.xlsx" - these contain data for 3 plates per sheet (LDH and Alamar Blue)

# changes:
# this script sets any negative raw values to zero
# this script addes a date column (for integration with check_unique_apid.R)

###################################################################################
# USER INPUT
###################################################################################
# set the location for the output file
basepath <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output"

# set the name of the output file
filename = "Brown2016_cytotoxicity.csv"

# Do your individual input excel sheets contain data for one plates or three plates per sheet?
sheetdata = "one" # set to "one" or "three"

# what are the Alamar Blue and LDH sheets names in your input files?
# ABname = "Alamar Blue" # e.g. "CTB", "Alamar Blue"
# LDHname = "Total LDH" # e.g. "LDH", "Total LDH"

# If you are creating a new file or want to overwrite an existing file of the same name, set newFile = TRUE
# If you want to append data to an existing file with the same name, set newFile = FALSE
newFile = TRUE

# This script extracts the values under "Corrected Optical Denisty 490 nm"
###################################################################################
# END USER INPUT
###################################################################################

library(xlsx)
library(tcltk)
library(pracma)

###################### FUNCTIONS

# function to  look up index of value in a data frame
#  there probably is a more efficient, data table way to do this...
returnindex = function(value, mydata, repeat_num = 1) {
  times_found <- 0
  for (i in 1:nrow(mydata)) {
    for (j in 1:ncol(mydata)) {
      if (is.na(mydata[i,j])) {
        next
      }
      if (strcmp(as.character(mydata[i,j]),as.character(value))) {
	      times_found <- times_found + 1
        if (times_found == repeat_num) return(c(i,j))
      }
    }
  }
  # print("could not find index in data frame")
  return(NULL)
}

# uses the list of possible tab names to get the AB or LDH data from excel sheet
findTabData <- function(sourcefile, assay = c("AB", "LDH")) {
  
  ABnames <- c("Alamar Blue", "AB", "CTB", "CellTiter Blue")
  LDHnames <- c("LDH", "Total LDH")
  
  tabNames <- switch(assay, 
         AB = ABnames,
         LDH = LDHnames)
  
  # get the source data, trying all common names
  i <- 1
  repeat {
    if (i > length(tabNames)) {
      # print(paste0("Could not find sheet for AB data in ", basename(sourcefile)))
      tabName <- readline(prompt = paste0("Enter name of tab in ", basename(sourcefile)," for Alamar Blue data: "))
    } 
    else {
      tabName <- tabNames[i]
    }
    
    my_data <-  tryCatch({
      # The code you want run
      read.xlsx(sourcefile, sheetName = tabName, header = FALSE, stringsAsFactors = FALSE)
    }, error = function(err) {
      # Is executed if error encountered
      NULL
    })
    
    if (!is.null(my_data)) {
      break
    }
    i <- i+1
  }
  return(my_data)
}


# use this function if the data from each plate comes from a separate file or sheet
createCytoTable = function(sourcefile,firstround) {
  
  AB_data <- findTabData(sourcefile, "AB")
  
  # --------------- removing LDH, because we don't have that for this data set
  # LDH_data <- findTabData(sourcefile, "LDH")
  
  # get source file name
  filenamesplit = strsplit(sourcefile, split = "/")
  srcname = tail(filenamesplit[[1]], n = 1)
  
  # look for plate name in file name (anything with -)
  namesplit = strsplit(srcname, split ="_")
  apid = namesplit[[1]][grep(pattern="-",namesplit[[1]])]
  # ------------------ adding in for this data set
  apid <- sub("Summary.xlsx","",apid)
  
  # get the date from file name:
  date = namesplit[[1]][grep(pattern="[0-9]{8}",namesplit[[1]])] # looks for 8-digit string in namesplit
  
  if (isempty(AB_data)) {
    print("AB data not found")
  } else {
    createCytoData(AB_data, "AB", firstround, apid, srcname, date)
  }
  # if(isempty(LDH_data)) {
  #   print("LDH data not found")
  # } else {
  #   createCytoData(LDH_data, "LDH", 0, apid, srcname, date)
  # }
  
}


# use this function when data for 3 plates are on same sheet
createCytoTable2 = function(sourcefile, firstround) {
  
  # get source file name
  srcname = basename(sourcefile)
  
  # get the date from filename
  namesplit = strsplit(srcname, split ="_")
  date = namesplit[[1]][grep(pattern="[0-9]{8}",namesplit[[1]])] # looks for 8-digit string in namesplit
  
  AB_data_all = findTabData(sourcefile, "AB")
  LDH_data_all = findTabData(sourcefile, "LDH")
  
  # split the data wherever there is a new occurrence of the word "Chemical" in the first column
  # Only want the first 3 - because these should correspond to the first 3 plates
  # Any occurences of "chemical" after that are probably other calculations
  plate_indicies = which(AB_data_all[,1] == "Chemical")[1:3]
  
  for (p in plate_indicies) {
    
    # get the plate slice of the data
    AB_data = AB_data_all[p:(p+9),]
    LDH_data = LDH_data_all[p:(p+9),]
    
    # get apid. Should be same for AB and LDH
    plateindex = returnindex("Plate", AB_data)
    ABapid = paste("MW",AB_data[plateindex[1],(plateindex[2]+1)], sep = "")
    plateindex = returnindex("Plate", LDH_data)
    LDHapid = paste("MW",LDH_data[plateindex[1],(plateindex[2]+1)], sep = "")
    
    if (isempty(AB_data)) {
      print("AB data not found")
    } else {
      createCytoData(AB_data, "AB", firstround, ABapid, srcname, date)
      firstround = FALSE
    }
    if(isempty(LDH_data)) {
      print("LDH data not found")
    } else {
      createCytoData(LDH_data, "LDH", firstround, LDHapid, srcname, date)
      firstround = FALSE
    }
  }
  
}

createCytoData = function(sourcedata,cyto_type,firstround = 0, apid = NULL, srcname = NULL, date = NULL) {
  
  cat(apid,cyto_type,"\n")
  
  # compound map
  compoundindex = returnindex("Chemical",sourcedata)
  chemrow = compoundindex[1] + 3
  chemcol = compoundindex[2] + 1
  compoundmap = sourcedata[chemrow:(chemrow+5),chemcol:(chemcol+7)]
  
  # concetrations
  concindex = returnindex("Concentration mM",sourcedata)
  concrow = concindex[1] + 3
  conccol = concindex[2] + 1
  concmap = sourcedata[concrow:(concrow+5),conccol:(conccol+7)]
  
  # Get desired values
  # if one-plate version, expected rval header is "Corrected for Blank"
  if (sheetdata == "one") {
    # valueindex = returnindex("Row",sourcedata)
    # # note that data start only 2 rows below, instead of 3
    # valuerow = valueindex[1] + 2
    # valuecol = valueindex[2] + 1
    # --------------- Change for working with these summary files, which do not have "corrected for blank" headings
    valueindex = returnindex("Row",sourcedata, repeat_num = 5)
    valuerow <- valueindex[1] + 1
    valuecol <- valueindex[2] + 1
    # -------------------------------------------------------------------------
  }
  if (sheetdata == "three") {
    tagPhrases = c("Corrected for Blank", "Corrected Optical Denisty 490 nm", "Corrected Fluorescence")
    valueindex = NULL
    i = 1
    while(is.null(valueindex) ) {
      if (i > length(tagPhrases)) {
        stop(paste("no corrected for blank data found for",apid,cyto_type))
      }
      valueindex = returnindex(tagPhrases[i], sourcedata)
      i = i+1
    }
    # assuming that data starts 3 rows below
    valuerow = valueindex[1] + 3
    valuecol = valueindex[2] + 1
    
  }
  valuemap = sourcedata[valuerow:(valuerow+5),valuecol:(valuecol+7)]
  if (any(is.na(valuemap))) {
    stop("valuemap contains NAs")
  }
  
  # if any blank-corrected values are negative, then we should set these to zero
  if (any(valuemap < 0)) {
    valuemap[valuemap < 0] = 0.0
    print("some blank-corrected values are negative. Setting these to zero")
  }
  
  # now create a data frame with all the desired values
  
  compounds = c()
  concentrations = c()
  values = c()
  rowi = c()
  coli = c()
  for (i in 1:nrow(compoundmap)) {
    for (j in 1:ncol(compoundmap)) {
      # compounds = c(compounds, as.character(compoundmap[i,j]))
      # concentrations = c(concentrations, as.character(concmap[i,j]))
      compounds = c(compounds, compoundmap[i,j])
      concentrations = c(concentrations, concmap[i,j])
      values = c(values, as.numeric(valuemap[i,j]))
      rowi = c(rowi, i)
      coli = c(coli, j)
    }
  }
  
  # create data frame
  sourcedata = data.frame("treatment" = compounds, "rowi" = rowi, "coli" = coli, "conc" = concentrations, "rval" = values, stringsAsFactors = FALSE)
  
  if (isempty(apid) | length(apid)>1) {
    print("Plate cannot be determined from file name")
    apid = readline("Enter plate sn: ")
  }
  
  # determine correct assay component source name
  if (cyto_type == "LDH") {
    sourcedata$acsn = "NHEERL_MEA_dev_LDH"
  } else if (cyto_type == "AB" | cyto_type == "CTB") {
    sourcedata$acsn = "NHEERL_MEA_dev_AB"
  } else {
    stop("invalid cyto_type used")
  }
  
  sourcedata$srcf = srcname
  sourcedata$apid = apid
  sourcedata$date = date # optional, adding this in to differentiate between plates that were reused in separate culture dates
  sourcedata$wllt = "t" # well type "t" for treated
  # For control wells, make well type "n" for neutral control
  sourcedata[sourcedata$conc == 0, "wllt"] = "n"
  sourcedata$wllq = 1 # for good data
  sourcedata$srcf = srcname
  
  # reorder columns
  sourcedata = sourcedata[,c("treatment","apid","rowi","coli","wllt","wllq","conc","rval","srcf","acsn", "date")]
  
  # if provided, replace the treatment names with the names in the master chemical lists
  if (length(masterChemFiles) != 0) {
    # Get the masterChemFile with the same plate number
    masterChemFile = grep(pattern = paste0("_",apid,"_"), masterChemFiles, value = T)
    if (length(masterChemFile) != 1) {
      stop(paste("master chem file match not found for",apid,sep = " "))
    }
    masterChemData = read.csv(masterChemFile, stringsAsFactors = FALSE)
    
    # matching up Treatment from masterChemData where Well matches with rowi and coli
    letterList = c("A", "B", "C", "D", "E", "F")
    for (l in 1:length(letterList)) {
      well_row_names = masterChemData[grep(pattern = letterList[l], masterChemData$Well),]
      for (c in unique(sourcedata$coli)) {
        correct_name = well_row_names[grep(pattern = c, well_row_names$Well), "Treatment"]
        sourcedata[sourcedata$rowi == l, "treatment"] = correct_name
      }
    }
  }
  
  # write the data to a table
  if (firstround) {
    write.table(sourcedata, file = paste(basepath, filename, sep = "/"), sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
  } else {
    write.table(sourcedata, file = paste(basepath, filename, sep = "/"), sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
}

###################### END FUNCTIONS


cytoFiles = tk_choose.files(caption = paste("Select all files containing cytotoxicity data with",sheetdata,"plate(s) per sheet",sep=" "))
masterChemFiles = tk_choose.files(caption = "(optional) Select all Master Chemical Lists Files fot these plates")

if (length(masterChemFiles) == 0) {
  print("no master chem list found, using treatment names from input data")
}

for (i in 1:length(cytoFiles)) {
  
  if (sheetdata == "one") {
    createCytoTable(cytoFiles[i], firstround = floor(1/i)&newFile)
  }
  else if (sheetdata == "three") {
    createCytoTable2(cytoFiles[i], firstround = floor(1/i)&newFile)
  } 
  else {
    print("invalid value for sheetdata")
  }

}

cat(filename,"is ready\n")
