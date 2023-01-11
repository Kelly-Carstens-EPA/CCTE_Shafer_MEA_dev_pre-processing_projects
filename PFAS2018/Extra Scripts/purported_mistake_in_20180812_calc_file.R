# 11/18/2020
# I had the note to do: "!! Fix the Calc files for PFAS!! Seems to be wrong compounds for 20180812, even though master chem files correct"
# but, that culture doesn't exist
# I am looking for where the treatment names from teh Calc fiel and master chem files don't agree
cytodat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/PFAS2018/output/PFAS2018_cytotox.csv"))
auc <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/PFAS2018/output/PFAS2018_AUC.csv"))
auc[, `:=`(coli = as.integer(sub("[[:alpha:]]","",well)), 
           rowi = match(sub("[[:digit:]]","",well), LETTERS))]
long_auc <- melt(auc, id.vars = c('date','Plate.SN','well','treatment','dose','units','wllq','wllq_notes','coli','rowi'),
                 variable.name = "acsn", value.name = "rval", variable.factor = F)
auc_sum <- long_auc[, unique(.SD), .SDcols = c('date','Plate.SN','treatment','dose','units','coli','rowi')]
cyto_sum <- cytodat[, unique(.SD), .SDcols = c('date','Plate.SN','treatment','conc','coli','rowi')]

dat <- merge(cyto_sum, auc_sum, by = c("date","Plate.SN","rowi","coli"), suffixes = c(".c",".a"))

dat[treatment.a != treatment.c] # empty??
dat[treatment.c != treatment.a] # also empty
dat[dose != conc] # empty
dat[, length(unique(treatment.a))] # 78

dat[, .(paste0(sort(unique(date)),collapse=",")), by = .(treatment.a)] # every compound was tested only once

# I guess I either already updated it,
# or this was not a real issue
