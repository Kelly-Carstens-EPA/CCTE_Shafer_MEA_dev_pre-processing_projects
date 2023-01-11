# Plate 20140730 MW1007-104 is missing DIV 9 recording
# After discussion with Tim 4/28/2020, we have decided to add approximate a DIV 9
# value based on the mean of that value in the other 2 plates in this culture that tested the same compounds
# We are doing this so that the AUC value will not be relativly smaller or larger without the DIV 9 value
# see graphs in figs folder

# Note that I use mean(x, na.rm=T). So if a parameter is na for one of the reference plates, the values from the other reference plate will be used
# If the values are NA in both plates, mean(x, na.rm=T) will return NA. This will be set to zero regardless before AUC calculation

# I have switched to using the median instead of the mean, so this script is deprecated

library(data.table)
approximate_DIV_by_mean <- function(dat, platei, cplates, add.DIV)
{
  # get the columns for the endpoints
  id.cols <- c("date","Plate.SN","DIV","well","trt","dose","units","file.name")
  usecols <- setdiff(names(dat), id.cols)
  
  # get a template of the ID data for platei from first DIV
  plate.dat.template <- dat[Plate.SN == platei & DIV == unique(DIV)[1], ..id.cols]
  
  # calculate the mean parameter value for each trt and dose
  add.dat <- dat[Plate.SN %in% cplates & DIV == add.DIV, lapply(usecols, function(x) mean(get(x), na.rm = T)), by = c("trt","dose","units")]
  setnames(add.dat, old = paste0("V",1:length(usecols)), new = usecols)
  add.dat <- merge(plate.dat.template, add.dat, by = c("trt","dose","units"))
  add.dat[, `:=`(DIV = add.DIV, file.name = paste0("mean_of_DIV9_in_corresponding_wells_of_",cplates[1],",",cplates[2]))]
  return(add.dat)
}


# update the prepared data
pdat <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/prepared_data/Brown2016_all_prepared_data.csv")
outfile <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/not_using_comparison_only/Brown2016_all_prepared_data_added_DIV9_mean.csv"

platei <- "MW1007-104"

# find the corresponding plates, cplates
pdat[Plate.SN == platei, unique(date)]
trts <- pdat[Plate.SN == platei, unique(trt)]
# [1] "Glyphosate"           "Sodium Orthovanadate" "Bisindolymaleimide 1"
# I want to use the other plate(s) that used exactly this combo of trts, I think
pdat[, unique(trt), by = c("date","Plate.SN")]
# I honestly think I am just going to use all of the plates, since there is not plate from the same culture
cplates <- unique(pdat$Plate.SN)

# run the function, append the data, and save it
div9.dat <- approximate_DIV_by_mean(pdat, platei = platei, cplates = cplates, add.DIV = 9)
pdat <- rbind(pdat, div9.dat)
fwrite(pdat, file = outfile)


# update the MI data
midat <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/All_MI/Brown2016_MI.csv")
outfile2 <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/not_using_comparison_only/Brown2016_MI_added_DIV9_mean.csv"

# run the function, append the data, and save it
rm(div9.dat)
div9.dat <- approximate_DIV_by_mean(midat, platei = "MW1230-53", cplates = cplates, add.DIV = 9)
midat <- rbind(midat, div9.dat)
fwrite(midat, file = outfile2)

# see that things look good
plot(midat[, .(DIV, Mutual.Information)])
plot(pdat[, .(DIV, meanfiringrate)])
