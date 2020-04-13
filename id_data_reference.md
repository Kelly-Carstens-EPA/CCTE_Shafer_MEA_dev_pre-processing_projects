# Documentation of how ID data is saved and pulled at each stage
I recognize this is currently very inconsistent. I hope writing it out will help me see how to make it better.

- h5_conversion.R
spikefilename = basename(spkListFiles[i])
  spikefilename_split = strsplit(spikefilename, split = "_")
  platename = spikefilename_split[[1]][ grep(pattern = "-", spikefilename_split[[1]]) ]
platename taken from the first tag that contains "-"
Then, matched to masterchemlog via
masterChemFile = grep(pattern = paste0("_",platename,"_"), masterChemFiles, value = T)
Get plate.chem.info from chem.info.3
Stores plate.chem.info in h5 file for each recording via map.to.h5.dh and some lines in spike_list_functions

- chem.info.3 (used in h5_conversion)
get Experiment.Date from Master Chem File column that matches pattern="xperiment{1}"
get Plate from Master Chem File column that matches pattern="[Pp]+late{1}"
plate.chem.info object is created from Master Chem File, stores Date, plate, well, trt, and dose
But DIV is NOT saved in plate.chem.info, because not saved in Master Chem File
  
- create_ont_csv.R
Does not read the plate and date

- create_burst_ont_Data.R
plates <-  unique( sapply(strsplit(basename(h5Files), split="_"), function(x) x[3]) )
date: unlist(strsplit(basename(s[[cur.file]]$file), split="_"))[2]
i.e., plates are referenced by the third tag in h5File name
date referenced by the second tag in h5File name
DIV referenced by 4th tag in h5File name

- MI_script_all.R
plate referenced by third tag in h5File name. Plate used to grep() for other h5Files from teh same plate
date retrieved by second tag in h5file name. Date only used for the purpose of naming the file

- spikeLoadRoutnines
DIV retrieved by the 4th tag in teh h5file name.
Plate, date, names (of compounds), dose, and well all retrived from h5file, e.g. `plate<-h5read(fileName[[j]], '/Plate.SN')`
spikeLoadRountines saves all of the "meta" data in the second half of the object `x`.

- nmi_wrapper
All ID data saved in "mutdata" object, created by spikeLoadRountines

- cytotoxprep_06.R
namesplit = strsplit(srcname, split ="_")
  apid = namesplit[[1]][grep(pattern="-",namesplit[[1]])]
  date = namesplit[[1]][grep(pattern="[0-9]{8}",namesplit[[1]])]
Plate found by the tag that contains "-" in the input Calculations file
Date found by the tag that contains an 8-digit string in the input Calculations file


