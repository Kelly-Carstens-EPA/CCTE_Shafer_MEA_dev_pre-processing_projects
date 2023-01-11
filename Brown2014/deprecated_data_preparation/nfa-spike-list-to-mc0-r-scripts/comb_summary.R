# Default value of file_path will put the output in the same folder as the input

###################################################################################
# USER INPUT
###################################################################################
# Set the name of the output file

outfile = 'Brown2016_MI.csv' # e.g. [dataset]_prepared_data.csv

###################################################################################
# END USER INPUT
###################################################################################

library(tcltk)

comb_data<-function(file_path = NULL, outfile) {
  all_data<-tk_choose.files("Select files to combine")
  if (is.null(file_path)) {
    file_path = paste(dirname(all_data[1]), outfile, sep="/")
  }
  
  all_files<-list()
  for (i in 1:length(all_data)){
    all_files[[i]]<-read.table(file=all_data[[i]],header=T,sep=",", stringsAsFactors = F)
    if (i==1){
      test<-rbind(all_files[[1]])
    }
    if(i>1){
      test<-rbind(test,all_files[[i]])
    }
  }
  
  write.table(test,file=file_path,sep=",",row.names=F,col.names=T)
  cat(file_path,"is ready")
}

# Execute the function
comb_data(outfile = outfile)
