# Script to process cytotoxicity data to prepare for tcpl
# Output is a long file with all the necessary columns for tcpl (except no spid, just treatment column)
# This script will extract the cytotoxicity data (LDH and Alamar Blue) from the input files
# Example input files:
# - "ON_20160720_MW1139-19_Summary.xlsx" - these contain data for 1 plate per sheet (LDH and Alamar Blue)
# - "20171011_ON G8_2 Calculations.xlsx" - these contain data for 3 plates per sheet (LDH and Alamar Blue)

###################################################################################
# USER INPUT
###################################################################################
# set the location for the output file
basepath = "C:/Users/Acarpe01/Documents/NTP tcpl prep/Intermediate Output/"
# set the name of the output file
outfile = "NTP_cytotoxicity_100.csv"

# Do your individual input excel sheets contain data for one plates or three plates per sheet?
sheetdata = "three" # set to "one" or "three"

# what are the Alamar Blue and LDH sheets names in your input files?
ABname = "Alamar Blue" # e.g. "CTB", "Alamar Blue"
LDHname = "LDH" # e.g. "LDH", "Total LDH"

# If you are creating a new file or want to overwrite an existing file of the same name, set newFile = TRUE
# If you want to append data to an existing file with the same name, set newFile = FALSE
newFile = FALSE

# This script extracts the values under "Percent of Control"
###################################################################################
# END USER INPUT
###################################################################################

setwd(basepath)

library(xlsx)
library(tcltk)
library(pracma)

########### FUNCTIONS

# function to  look up index of value in a data frame
#  there probably is a more efficient, data table way to do this...
returnindex = function(value, mydata) {
  for (i in 1:nrow(mydata)) {
    for (j in 1:ncol(mydata)) {
      if (is.na(mydata[i,j])) {
        next
      }
      if (strcmp(as.character(mydata[i,j]),as.character(value))) {
        return(c(i,j))
      }
    }
  }
  print("could not find index in data frame")
  return(NULL)
}


# use this function if the data from each plate comes from a separate file or sheet
createCytoTable = function(sourcefile,firstround) {
  
  AB_data = read.xlsx(sourcefile, sheetName = ABname, stringsAsFactors = FALSE)
  LDH_data = read.xlsx(sourcefile, sheetName = LDHname, stringsAsFactors = FALSE)
  
  # get source file name
  filenamesplit = strsplit(sourcefile, split = "/")
  srcname = tail(filenamesplit[[1]], n = 1)
  
  # look for plate name in file name (anything with -)
  namesplit = strsplit(srcname, split ="_")
  apid = namesplit[[1]][grep(pattern="-",namesplit[[1]])]
  
  if (isempty(AB_data)) {
    print("AB data not found")
  } else {
    createCytoData(AB_data, "AB", firstround, apid, srcname)
  }
  if(isempty(LDH_data)) {
    print("LDH data not found")
  } else {
    createCytoData(LDH_data, "LDH", 0, apid, srcname)
  }
  
}


# use this function when data for 3 plates are on same sheet
createCytoTable2 = function(sourcefile, firstround) {
  
  # get source file name
  filenamesplit = strsplit(sourcefile, split = "/")
  srcname = tail(filenamesplit[[1]], n = 1)
  
  AB_data_all = read.xlsx(sourcefile, sheetName = ABname, header = FALSE, stringsAsFactors = FALSE)
  LDH_data_all = read.xlsx(sourcefile, sheetName = LDHname, header = FALSE, stringsAsFactors = FALSE)
  
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
      createCytoData(AB_data, "AB", firstround, ABapid, srcname)
      firstround = FALSE
    }
    if(isempty(LDH_data)) {
      print("LDH data not found")
    } else {
      createCytoData(LDH_data, "LDH", firstround, LDHapid, srcname)
      firstround = FALSE
    }
  }
  
}

createCytoData = function(sourcedata,cyto_type,firstround = 0, apid = NULL, srcname = NULL) {
  
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
  
  # percent of control values
  valueindex = returnindex("Percent of Control",sourcedata)
  valuerow = valueindex[1] + 3
  valuecol = valueindex[2] + 1
  valuemap = sourcedata[valuerow:(valuerow+5),valuecol:(valuecol+7)]
  
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
      values = c(values, 100*as.numeric(valuemap[i,j]))
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
  sourcedata$wllt = "t" # well type "t" for treated
  # For control wells, make well type "n" for neutral control
  sourcedata[sourcedata$conc == 0, "wllt"] = "n"
  sourcedata$wllq = 1 # for good data
  sourcedata$srcf = srcname
  
  # reorder columns
  sourcedata = sourcedata[,c("treatment","apid","rowi","coli","wllt","wllq","conc","rval","srcf","acsn")]
  
  # if provided, replace the treatment names with the names in the master chemical lists
  if (length(masterChemFiles) == 0) {
    print("no master chem list selected")
    print("treatment names in input data sheets will be used")
  } else {
    # Get the masterChemFile with the same plate number
    masterChemFile = masterChemFiles[ grep(pattern = apid, masterChemFiles) ]
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
    write.table(sourcedata, file = outfile, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
  } else {
    write.table(sourcedata, file = outfile, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
  
}

###################### END FUNCTIONS


cytoFiles = tk_choose.files(caption = paste("Select all files containing cytotoxicity data with",sheetdata,"plates per sheet",sep=" "))
masterChemFiles = tk_choose.files(caption = "Select all Master Chemical Lists Files fot these plates")


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

cat(outfile,"is ready\n")
