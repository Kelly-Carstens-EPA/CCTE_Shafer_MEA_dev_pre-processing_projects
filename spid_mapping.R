# script to replace treatment names with spids
# this script may need to be edited for a given chemical set
# particularly the commented section around line 35

###################################################################################
# USER INPUT
###################################################################################
# set the location for the output file
basepath = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA - Test"

# set the name of the output file
filename = "DNT_NFA_Test_MEA_mc0_withspids.csv"

# input is the usually the output file from tcpl_MEA_dev_AUC.R
mc0_filename = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA - Test/DNT_NFA_Test_mc0.csv"
spidmap_filename = "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
# Name of sheet in spidmap_filename that contains the spid's
sheetName = "Chemical List"

# specify the column name of spids in the spid map file
spidCol = "NCCT ID"
# specify the column name in the spidmap file you will match to the treatment column in the mc0data 
mapMatchCol = "Chemical ID"
# specify the column name in the mc0 file that you will match to the mapMatchColName.
# This column will be replaced with SPIDs
mc0MatchCol = "treatment" # this will always be treatment, unless you are using something other than standard tcpl_MEA_dev_AUC.R output file mc0 file

###################################################################################
# END USER INPUT
###################################################################################

library(xlsx)

mc0data = read.csv(mc0_filename, stringsAsFactors = FALSE)
spidmap = read.xlsx(spidmap_filename, stringsAsFactors = FALSE, sheetName = sheetName, check.names = FALSE)

# If using chemical name as mapMatchCol, may need to fix slight differences in naming, such as
# mc0data[mc0data$treatment == "Diazonon", "treatment"] = "Diazinon"
# mc0data[mc0data$treatment == "Z-tetrachlorvinphos", "treatment"] = "Z-Tetrachlorvinphos"
# mc0data[mc0data$treatment == "Malaxon", "treatment"] = "Malaoxon"

chemlist = unique(mc0data[,mc0MatchCol])

for (chem in chemlist) {
  # don't need to replace control wells (don't have any spids for these)
  if (all(mc0data[mc0data[,mc0MatchCol]== chem, "conc"] == 0)) {
    print(paste("not changing",chem,"control data rows",sep = " "))
    next
  }
  
  correct_spid = spidmap[spidmap[,mapMatchCol] == chem, spidCol]
  
  # check for common potential errors with correct_spid
  if (length(correct_spid) != 1) {
    stop(paste("spid formatting incorrectly for ", chem, ": ", correct_spid, sep =""))
  } 
  else if (is.na(correct_spid)) {
    stop(paste("spid not found for",chem, sep = " "))
  }
  else {
    mc0data[mc0data[,mc0MatchCol] == chem, mc0MatchCol] = correct_spid
  }
}

# replace name of treatment column in mc0data with "spid"
names(mc0data)[names(mc0data) == mc0MatchCol] = "spid"

# checking that all of the spids in the spid map were used
extraSpids = 0
for (i in spidmap[,spidCol]) {
  if (!is.element(i, mc0data$spid)) {
    extraSpids = extraSpids + 1
    print(paste(i,"from spid map not used", sep = " "))
  }
}
cat(extraSpids,"unused from spid map\n")

write.table(mc0data, file = paste(basepath,"/",filename,sep = ""), sep= ",", append = FALSE, row.names = FALSE, col.names = TRUE)

cat(filename,"is ready\n")