# ------------------------------------------------------------------------ #
# Debugging issue with DIVs encountered with DNTFalseNegatives
# And patch fix for MI data
# April 13, 2022
# ------------------------------------------------------------------------# 

mi_data[, .N, by = .(DIV)]
# DIV   N
# 1: 05(000)(000).h5 144
# 2: 07(000)(000).h5  48
# 3:      09(000).h5 144
# 4:      12(000).h5 144
# 5:      07(000).h5  96
parameter_data[, .N, by = .(DIV)]
# DIV   N
# 1:            <NA> 383
# 2: 05(000)(000).h5 117
# 3: 07(000)(000).h5  10
# 4:      09(000).h5  24
# 5:      12(000).h5  18
# 6:      07(000).h5  24

# Okay, so how is the 

parameter_data[is.na(DIV), .N, by = .(file.name)]
# all 12 h5files...

View(parameter_data)

# Got an idea:
# take the DIV From the filename this way:
strsplit(basename('NFA_20210818_MW75-9205_05(000)(000).h5'), split="[_\\(\\)]")[[1]][4]

# Are there any h5 files where there are parentheses before the DIV, which would affect the tag location?

check.folders <- file.path(list.dirs(path = 'L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl', recursive = F), 'h5Files')
check.folders <- Filter(file.exists, check.folders)

# all.h5files.todate <- unlist(lapply(check.folders, list.files, pattern = '', full.names = T)) # this taking forever..
all.h5files.todate <- c()
for(folderi in check.folders) {
  cat(folderi, '\n')
  all.h5files.todate <- c(all.h5files.todate, list.files(path = folderi, pattern = ''))
}

all.divs.new.method <- c()
for(filei in all.h5files.todate) {
  all.divs.new.method <- c(all.divs.new.method, strsplit(filei, split="[_\\(\\)]")[[1]][4])
}

all.h5files.split <- strsplit(all.h5files.todate, split = "[_\\(\\)]")
all.h5files.new.divs <- lapply(all.h5files.split, function(spliti) spliti[4])
table(unlist(all.h5files.new.divs))
table(as.numeric(unlist(all.h5files.new.divs)))
#   5   6   7   8   9  12  13 
# 349   3 348   3 349 348   3 

# cool, all of these look like legit div!


# Patch fix for MI data with this updated method, so tha tI don't have to recalculate this 
mi_data_files <- list.files(path = file.path('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTFalseNegatives', "All_MI"), pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
for (i in 1:length(mi_data_files)) {
  mi_datai <- read.csv(mi_data_files[i], stringsAsFactors = F)
  # Do the fix
  setDT(mi_datai)
  mi_datai[, DIV := as.numeric(unlist(lapply(strsplit(file.name, split = "[_\\(\\)]"), function(spliti) spliti[4])))]
  write.csv(mi_datai, file = mi_data_files[i], row.names = F)
}
