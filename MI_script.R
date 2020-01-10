require('pracma')
library('compiler')
require('h5')
require('Matrix')
require('gtools')

# use this script to calculate the normalized mutual information endpoint for one plate

h5files<-choose.files(caption = "Select h5 files for all DIVs from one plate for NMI analysis", multi = TRUE)

filepath = choose.dir(caption = "Choose folder where output should go")
filename = readline("Enter desired file name (Without file extension): ")

file_split<-split(h5files,1:length(h5files))

parsed_data<-list()
parsed_data<-load.spikedata_final(333,file_split)
print("spikeLoadRoutines complete")

MI_output<-list()
MI_output<-nmi_wrapper(parsed_data)

# any existing file with this name is destroyed
write.table(MI_output, paste(filepath, "/", filename, ".csv",sep=""), col.names=TRUE, append = FALSE, row.names=FALSE, sep=",")

cat(filename, " is complete\n")