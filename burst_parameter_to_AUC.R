## MEA neuronal ontogeny AUC calculations
## Adapted from Chris Frank - February-March 2016
## Adapated from Mahmoud 2019 by Amy Carpenter Nov 2019

##########################################################
# USER INPUTS
##########################################################
# Set directory for output location
basepath = "C:/Users/Acarpe01/Documents/ToxCast tcpl prep/"
setwd(basepath)
# Set output file name
filename = "AUC_TC_normalized_zero_center_up.csv"
# set file with the 16 parameters for all culture dates
parameter_data = read.csv("Published Source Data/All_TC.csv", sep = ",")
# set file with mi data for all culture dates
mi_data = read.csv("Published Source Data/All_MI_TC.csv", sep = ",")
# do you want to normalize the data in this script?
normalize = FALSE
# do you want to make the normalized response positive?
zero_center_positive_response = FALSE
##########################################################
# END USER INPUTS
##########################################################



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

# Removing BIC wells! yay!
#bis_rows <- grep("12_01_", all_data$file.name, fixed=TRUE) #index all Bicuculline-treated wells
#all_data <- all_data[- bis_rows,] #Remove all bic-treated wells

parameter_data[is.na(parameter_data)] <- 0 #Replace all NAs with zeros for AUC calculations - This may be undesirable for MEA parameters that are derived from other parameters.
parameter_data <- subset(parameter_data, DIV %in% c(5,7,9,12)) # Remove sparse DIV2 data
parameter_data <- unique(parameter_data) # eliminate duplicate data

## Read in mutual information parameter and attach to full dataset
mi_data <- subset(mi_data, DIV %in% c(5,7,9,12)) # Remove sparse DIV2 data
mi_data <- unique(mi_data) # eliminate duplicate data
all_data <- merge(parameter_data, mi_data, all.x=TRUE) # merge two data frames on common columns (plateID, date, well, treatment, source file name, etc.)

# switch the order of the columns file.name and Mutual.Information
L = length(names(all_data))
all_data = all_data[, c(1:(L-2), L, L-1)]
names(all_data)[names(all_data) == "Mutual.Information"] = "mi" # renaming this column
all_data[is.na(all_data$mi),"mi"] = 0 # make all mi na's = 0

#####
# May need to manually update all compound names, e.g.
# all_data[all_data[,"trt"]=="1-Methyl-4-phenylpyridinium iodide","trt"] <- "MPP+"


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
    # first parameter in all_data_split is on col 8. Last parameter is in second to last column. (Last column contains file.name)
    for (j in names(all_data_split[[i]][,8:(ncol(all_data_split[[i]])-1)])) {
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

## function to normalize by plate for each ontogeny parameter
auc_summary <- function(summary_table) {
  
  # Split data set by plate (date as well because plateIDs get reused)
  per_plate_split <- split(summary_table, interaction(summary_table[,"date"], summary_table[,"plate.SN"], drop=TRUE))
  
  # Normalize each plate by percent of control median and combine into single data frame
  norm_plates <- list() # initialize list
  
  # Loop through each plate
  for (i in per_plate_split) {
    
    if (TRUE) { # Can exclude any individual plates where untreated wells are off, e.g. i[,"plate.SN"][[1]]!="MW1008-41"
      
      # Loop through each ontogeny parameter
      for (j in names(summary_table[,7:ncol(summary_table)])) {
        cntrls <- (subset(i, dose==sprintf("%.5f",0))[,j]) # Get vector of control values for that parameter on that plate
        i[,j] <- (i[,j] / median(na.omit(cntrls)))*100 # Divide all values on that plate by controls median
      }
      
    } else { # For plates singled-out above, use culture median instead of plate median
      
      for (j in names(summary_table[,7:ncol(summary_table)])) {
        cntrls <- (subset(summary_table, dose==sprintf("%.5f",0) & date==i[,"date"][[1]])[,j]) # identify same-date control wells
        i[,j] <- (i[,j] / median(na.omit(cntrls)))*100  # Divide by same-culture control wells median     
      }
    }
    
    norm_plates[[length(norm_plates)+1]] <- i  # Add to growing list of normalized plates
  }
  
  rm(i,j) 
  norm_plates <- do.call(rbind, norm_plates) #Re-form one table of values
  
  #print(norm_plates) #checkpoint for normalization
  norm_plates
}

set_postive_response <- function(norm_plates) {
  # # how amy did this (incorrectly) previously
  # # zero - center
  # norm_plates[,7:ncol(norm_plates)] <- norm_plates[,7:ncol(norm_plates)]-100
  # # Take the absolute value
  # norm_plates[,7:ncol(norm_plates)] <- abs(norm_plates[,7:ncol(norm_plates)])
  
  # in previous scripts, e.g. AUC_analysis_Revised_SD.R, where set_direction = "up":
  #norm_plates[,7:ncol(norm_plates)] <- 100-(norm_plates[,7:ncol(norm_plates)]-100)
  # Here, I add 100, instead of 200, to make it zero-centered
  norm_plates[,7:ncol(norm_plates)] <- 100-(norm_plates[,7:ncol(norm_plates)])
  norm_plates
}

#*****************************************************************************
#*                             END FUNCTIONS                                 *
#*****************************************************************************

sum_table <- calc_auc(all_data_split = all_data_split)

if (normalize) {
  sum_table <- auc_summary(sum_table)
  # can only use set_postive_response for normalized data
  if (zero_center_positive_response) {
    sum_table <- set_postive_response(sum_table)
  }
}

write.csv(sum_table, filename, row.names = FALSE)

cat(filename,"is ready\n")
