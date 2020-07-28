## MEA neuronal ontogeny AUC calculations
## Adapted from Chris Frank - February-March 2016
## Adapated from Mahmoud 2019 by Amy Carpenter Nov 2019

##########################################################
# USER INPUTS
##########################################################
# Set directory for output location
basepath <-  "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/MEA_NFA_testing/burst_param_colreorder_confirmation"
# Set output file name
filename <- ""
# set the DIVs that should be included
use_divs <- c(5,7,9,12) # note that a point of DIV = 2 value = 0 will be added for every endpoint regardless
##########################################################
# END USER INPUTS
##########################################################

# get files with the 16 parameters for all culture dates
parameter_data_files <- choose.files(default = basepath, caption = "Select all files containing activity parameter values to calculate AUC")
# fet files with mi data for all culture dates
mi_data_files <- choose.files(default = basepath, caption = "Select all files containing MI data to calculate AUC")

# List of parameters for tcpl
assay_components = c("NHEERL_MEA_dev_firing_rate_mean",
                     "NHEERL_MEA_dev_burst_rate",
                     "NHEERL_MEA_dev_per_burst_interspike_interval",
                     "NHEERL_MEA_dev_per_burst_spike_percent",
                     "NHEERL_MEA_dev_burst_duration_mean",
                     "NHEERL_MEA_dev_interburst_interval_mean",
                     "NHEERL_MEA_dev_active_electrodes_number",
                     "NHEERL_MEA_dev_bursting_electrodes_number",
                     "NHEERL_MEA_dev_network_spike_number",
                     "NHEERL_MEA_dev_network_spike_peak",
                     "NHEERL_MEA_dev_spike_duration_mean",
                     "NHEERL_MEA_dev_per_network_spike_spike_percent",
                     "NHEERL_MEA_dev_per_network_spike_interspike_interval_mean",
                     "NHEERL_MEA_dev_network_spike_duration_std",
                     "NHEERL_MEA_dev_per_network_spike_spike_number_mean",
                     "NHEERL_MEA_dev_correlation_coefficient_mean",
                     "NHEERL_MEA_dev_mutual_information_norm")

# previous_columns = c("meanfiringrate_AUC",
# "burst.per.min_AUC","mean.isis_AUC","per.spikes.in.burst_AUC",
# "mean.dur_AUC","mean.IBIs_AUC","nAE_AUC","nABE_AUC","ns.n_AUC","ns.peak.m_AUC","ns.durn.m_AUC","ns.percent.of.spikes.in.ns_AUC","ns.mean.insis_AUC","ns.durn.sd_AUC","ns.mean.spikes.in.ns_AUC","r_AUC","mi_AUC")

options(digits = 6)

# read in the data
parameter_data <- c()
for (i in 1:length(parameter_data_files)) {
  parameter_datai <- read.csv(parameter_data_files[i])
  parameter_data <- rbind(parameter_data, parameter_datai)
}
mi_data <- c()
for (i in 1:length(mi_data_files)) {
  mi_datai <- read.csv(mi_data_files[i])
  mi_data <- rbind(mi_data, mi_datai)
}

parameter_data <- unique(parameter_data) # eliminate duplicate data
mi_data <- unique(mi_data) # eliminate duplicate data
all_data <- merge(parameter_data, mi_data, by = c("date","Plate.SN","DIV","well","trt","dose","units")) # merge two data frames on common columns (plateID, date, well, treatment, source file name, etc.)
if (nrow(all_data) < nrow(parameter_data) | nrow(all_data) < nrow(mi_data)) {
  stop(paste0("Some rows of parameter_data and mi_data don't match or are missing\n"))
}

# check if there are any DIVs other than use_divs
allDIV <- unique(all_data$DIV)
diffDIV <- setdiff(allDIV, use_divs)
if (length(diffDIV) > 0) {
  plates_with_diff_div <- unique(all_data[all_data$DIV %in% diffDIV, "Plate.SN"])
  warning(paste0("Recordings from DIV ",paste0(diffDIV,collapse = ","), " are found in ",paste0(plates_with_diff_div,collapse=","),"\nData from ",paste0(diffDIV,collapse = ",")," will be excluded."))
  all_data <- subset(all_data, DIV %in% use_divs)
}

# check if ea plate has a recording for each of use_divs
all_data$date_plate <- paste0(all_data$date, "_", all_data$Plate.SN)
date_plates <- unique(all_data$date_plate)
for (date_plate in date_plates) {
  plate_divs <- sort(unique(all_data[all_data$date_plate == date_plate, "DIV"]))
  missing_divs <- setdiff(use_divs, plate_divs)
  if (length(missing_divs) > 0) {
    warning(paste0("There is no data for ",sub("_"," ",date_plate), " DIV ",paste0(missing_divs,collapse=",")))
  }
}
all_data <- all_data[, !grepl("date_plate",names(all_data))]

# rename "Mutual.Information" column to "mi"
names(all_data)[names(all_data) == "Mutual.Information"] = "mi" # renaming this column

# Removing BIC data
#bis_rows <- grep("12_01_", all_data$file.name, fixed=TRUE) #index all Bicuculline-treated wells
#all_data <- all_data[- bis_rows,] #Remove all bic-treated wells

#Replace all NAs with zeros for AUC calculations - This may be undesirable for MEA parameters that are derived from other parameters.
all_data[is.na(all_data)] <- 0

all_data$dose <- sprintf("%.5f", all_data$dose)
## Split data frame by individual wells over time (interaction term speeds this up greatly for larger datasets)
all_data_split <- split(all_data, interaction(all_data[,"date"], all_data[,"Plate.SN"], all_data[,"well"], all_data[,"trt"], all_data[,"dose"], drop=TRUE)) # Split data into bins of single well across time (DIV

#*****************************************************************************
#*                              FUNCTION                                     *
#*****************************************************************************

## Function to calculate area under the curve (AUC) for each ontogeny parameter for each bin (each experiment)
calc_auc <- function(all_data_split, sqrt=FALSE) {
  require(pracma) #pracma package has trapz function that computes AUC based on trapezoidal geometry (no curve fitting)
  
  out <- lapply(1:length(all_data_split), function(i) {
    
    all_data_split[[i]] <- all_data_split[[i]][order(all_data_split[[i]][,"DIV"]),]  # Make sure order of rows follows DIV time
    
    date <- all_data_split[[i]]$date[1]
    plate.SN <- all_data_split[[i]]$Plate.SN[1]
    well <- all_data_split[[i]]$well[1]
    trt <- all_data_split[[i]]$trt[1]
    dose <- all_data_split[[i]]$dose[1]
    units <- all_data_split[[i]]$units[1]
    
    # calculating AUC for each parameter after adding div=2, param=0 data point to each AUC calculation with append() to represent no activity at first timepoint
    # first parameter in all_data_split is on col 9
    for (j in names(all_data_split[[i]][,9:ncol(all_data_split[[i]])])) {
      param_name <- paste(j, "_auc", sep="") # create auc variable name
      assign(param_name, round(trapz(append(all_data_split[[i]][,"DIV"], 2, after=0), append(all_data_split[[i]][,j], 0, after=0)),6), inherits=TRUE) # calculate auc, assign to variable name
    }
    
    # put vector of AUC values together
    c(date, as.character(plate.SN), as.character(well), as.character(trt), dose, as.character(units), meanfiringrate_auc, burst.per.min_auc, mean.isis_auc, per.spikes.in.burst_auc, 
      mean.dur_auc, mean.IBIs_auc, nAE_auc, nABE_auc, ns.n_auc, ns.peak.m_auc, ns.durn.m_auc, ns.percent.of.spikes.in.ns_auc, ns.mean.insis_auc, ns.durn.sd_auc, 
      ns.mean.spikes.in.ns_auc, r_auc, mi_auc)
  })
  
  ##FIX!!! - (me): not sure what needs to be fixed here
  sum_table <- as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE) # Re-form data frame
  names(sum_table) <- c("date","plate.SN","well","treatment","dose","units",assay_components)
  sum_table[,7:ncol(sum_table)] <- lapply(sum_table[,7:ncol(sum_table)], as.numeric) # Change output columns class to numeric
  # Not changing name to control yet, so can merge with cytotox wells in next script
  #sum_table[sum_table[,"dose"]==sprintf("%.5f",0),"treatment"] <- ControlTreatmentName # Convert all zero dose rows to say "Control" for treatment
  
  #Do we need to do this? (me)
  if (sqrt==TRUE){
    sum_table <- cbind(sum_table[,1:6], sqrt(sum_table[,7:25]))
  }
  
  sum_table
}

#*****************************************************************************
#*                             END FUNCTIONS                                 *
#*****************************************************************************

sum_table <- calc_auc(all_data_split = all_data_split)

write.csv(sum_table, paste(basepath, filename, sep = "/"), row.names = FALSE)

cat(filename,"is ready\n")
