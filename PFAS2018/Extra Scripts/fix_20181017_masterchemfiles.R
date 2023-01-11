# 10/14/2020
library(data.table)
# fix the master chem lists for 20181017, where col1 and col2 are mistakenly switched
# (according to the lab notebook, col 1 and 1 should only be switched for 1208-3)
plates <- paste0("MW",c("1207-43","1207-44","1208-1","1208-2","1208-4"))
main.dir <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/20181017 Culture PFAS Group 3-2"
masterChemFile_names <- paste0("20181017_",plates,"_MaestroExperimentLog_Ontogeny.csv")
masterChemFiles <- file.path(main.dir,plates,"csv Files", masterChemFile_names)

for (masterChemFile in masterChemFiles) {
  mdat <- read.csv(masterChemFile)
  setDT(mdat)
  mdat[grepl("1",Well), Dose := 0.03]
  mdat[grepl("2",Well), Dose := 0]
  write.csv(mdat, file = masterChemFile, row.names = FALSE)
}
