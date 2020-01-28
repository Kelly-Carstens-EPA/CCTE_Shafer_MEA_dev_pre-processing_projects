# Microelectrode Array Network Formation Assay Spike List Pe-Processing Scripts

## to do:
* define bursts, active electrodes?
* go over some details for some endpoints
* See Diana Hall's notes on references for all endpoints, in make DIV?
* fix local.corr script location in image
* read over all

## Purpose of these scripts

These scripts are designed to process the raw data for the microelectrode array network formation concentration-response assay.

Briefly, cortical cells are grown on 48-well microelectrode-containing plates. On each plate, 6 compounds are tested at 7 concentrations, plus 6 control wells. Eachgroup of compounds and concentrations is replicated on 3 plates. The electrical activity of the neurons are recorded as the network develops, on days 5, 7, 9, and 12. The recording provide a lot of information concerning the general activity, organization, and connectivity of the neurons in each well. These scripts calculate several parameters from the recordings in order to describe these 3 aspects of the developing neuronal networks.

We want to graph the dose response for each compound and each endpoint, calculate an EC50 value, and determine if the compound is positive hit. These scripts transform the raw the recording data into a long file format. Then, the data can be process with the functions in the ToxCast Pipeline to fit the dose response curves and determine the hit calls for each compound.

## How to use these scripts

For a step-by-step guide, see the document *Step-by-Step_Guide.docx*. Below is a diagram showing the general flow of the process and the scripts used at each step. Raw data files are shown in blue, intermediate output files are in purple, and scripts are in orange.
![spikelist_to_mc0_overview](/images/SpikeList_to_mc0_overview.png)

## Narrative of the process

### Raw data: spike list files
The inital raw data is the recordings of the activity in each well, called spike list files. These files are outputted by an Axion Maestro amplifier and software interfaces. There will be a spike list file for each plate and for each day of recording, days 5, 7, 9, and 12. The spike list files are csv files with columns
| ----------- | ----------- | ----------- |
| Time (s) | Electrode | Amplitude(mV) |
| ----------- | ----------- | ----------- |,
where the first column records the time of each spike, the second column records the ID of the electrode that spiked, and the third column records the amplitude of the spike. Each recording spans ~900 seconds (15 minutes).

### h5 files
The scripts `h5_conversion.R` and `spike_list_functions.R` convert the raw data into the Hierarchical Data Format .h5. This file type is designed to handle large amounts of data. One h5 file is created for each spike list file.

### 16 parameters values

(note to self: even if I did rename these, i would still want to have this chart clearly defined somewhere to document the renaming in the code. so not a waste)
The scripts `create_burst_ont_Data.R`, `local.corr.all.ont.ae.filter.R`, and `create_ont_csv.R` calculate 16 parameters from the raw spike list data. The table below summarizes the 16 parameters.
| ----------- | ----------- | ----------- | ----------- |
| Name | Description | name in MEA DEV scripts | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Mean Firing Rate | # spikes per second, averaged for active electrodes in well | meanfiringrate | NHEERL_MEA_dev_firing_rate_mean |
| Burst Rate | Bursts per Minute on AE | burst.per.min | NHEERL_MEA_dev_burst_rate |
| Interspike Interval | Inter spike interval (s) within a burst, averaged on ABE | mean.isis | NHEERL_MEA_dev_per_burst_interspike_interval |
| Percent of Spikes in Burst | % of spikes in bursts, averaged on ABE | per.spikes.in.burst | NHEERL_MEA_dev_per_burst_spike_percent |
| Mean Burst Duration | Mean burst duration (s), averaged on ABE | mean.dur | NHEERL_MEA_dev_burst_duration_mean |
| Mean Interburst interval | Mean interval between bursts (s), averaged on ABE | mean.IBIs | NHEERL_MEA_dev_interburst_interval_mean |
| Number of Active Electrodes | # of electrodes where mean firing rate >= 5 spikes/min | nAE | NHEERL_MEA_dev_active_electrodes_number |
| Number of Actively Bursting Electrodes | # of electrodes where burst rate >= 0.5 bursts/min | nABE | NHEERL_MEA_dev_bursting_electrodes_number |
| Number of Network Spikes | # of network spikes in well over 15 minute recording | ns.n | NHEERL_MEA_dev_network_spike_number |
**desp blah bc don't understand | Network Spike Peak | # of spikes (or electrodes, or ae) at the peak of network spike | ns.peak.m | NHEERL_MEA_dev_network_spike_peak |
| Network Spike Duration | mean of the duration of network spikes in mean (s) | ns.durn.m | NHEERL_MEA_dev_spike_duration_mean |
| Percent of Spikes in Network Spike | % of spikes (in all electrodes? all ae?) that are part of network spikes (or, for each ae, percent in ns, then average those %'s) | ns.percent.of.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_percent |
** confirm, units etc| Interspike Interval in Network Spikes | mean interspike interval of spikes in a network spike (s) | ns.mean.insis |
NHEERL_MEA_dev_per_network_spike_interspike_interval_mean |
| Standard Deviation of Network Spike Duration | standard deviation of network spike duration | ns.durn.sd | NHEERL_MEA_dev_network_spike_duration_std |
| Mean Number of Spikes in Network Spikes | number of spikes in newtork spike, averaged on ae?| ns.mean.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_number_mean |
| Mean correlation | spike time tiling coefficient, averaged on AE | r | NHEERL_MEA_dev_correlation_coefficient_mean |

The scripts currently also calcuate to other parameters: cv.time and cv.network. It was reported that there was an issue with the parameters, so these parameter values are removed from the data in burst_parameter_to_AUC.R.

These scripts were created by Diana Hall in 2013, most functions can be found in her meadq package on GitHub. A few alterations were made to her scripts, namely control over ABE filter, calculating the mean correlation only on active electrodes, and some streamlining. Therefore, edited versions of the 3 scripts mentioned above are included and should be sourced in order to overwrite the other versions from the meadq package, which you will download before you begin.

One csv file containing these parameter values will be created for each plate. Value for each DIV recording will be in separate rows. These files will be combined into one csv file for all the plates using comb.summary.R (or your own use of rbind) before calculating the area under the curve.

### Mutual Information

The mutual information parameter was created by x. See x paper for more info

The scripts `spikeLoadRoutines.R`, `nmi_wrapper.R`, `nmi2_final.R`, and `MI_script_all.R` contain the functions used to calculated the mutual information.

The calculation of the mutual information is much more computationally intensive. Therefore, the calcuation of this parameter is done separatley from the rest of the parameters. The script MI_script_all.R is designed to calculate the mutual information for all plates, so that the task can be done overnight, or remotely. One csv file will be created for 3 plates in each culture date. These files will be combined into one csv file for all the plates using comb.summary.R (or your own use of rbind) before calculating the area under the curve.

As above, here are the names used:
| ----------- | ----------- | ----------- | ----------- |
| Name | Description | name in MEA DEV scripts | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Normalized Mutual Information | concurrently measures synchrony and activity, see Ball 2016 for more info | mi | NHEERL_MEA_dev_mutual_information_norm |

### Area Under the Curve

We want to quatify the alterations to development from DIV 0 - 12 that a compound might cause compared to controls. If we were to plot the value of a given parameter and a given well over time, we would expect to see something like this:

In order to "sum up" the overall change in a parameter value, we calculate the trapezoidal area under the curve. This value is what will be compared between treated and control wells.

The script `burst_parameter_to_AUC.R` uses the `trapz` function from the pracma package to calculate the trapezoidal area under the curve for each parameter.

A couple of notes:
- In some recordings, especially early on, there are no bursts or network spikes in a well. Therefore, many parameters that measure some aspect of bursts or network spikes are NA in these wells. In order to calculate the area under the curve, these NA values are set to 0. Below is a summary of the parameters that might be NA. For the first 4, it makes sense to set these to 0. But for the latter 4, it might be counterintutive to set the NA instances to 0. For example, if there are not bursts in a well, the interspike interval (wihtin a burst) would be NA. However, if we set this instance to 0, that implies that the spikes within bursts are infinitely close together. 
Or, if there are no network spikes, the Standard Deviation of Network Spike Duration is NA. If we set this instance to 0, we imply that the the standard deviation is 0, i.e. that the duration of the network spikes is perfectly consistent (which we might expect of a very well developed, organized, consistent network).
Perhaps these latter 4 should be calculated differently, or excluded entitrely from the AUC analysis.
| ----------- | ----------- | ----------- | ----------- |
| Name | Description | name in MEA DEV scripts | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Mean Burst Duration | Mean burst duration (s), averaged on ABE | mean.dur | NHEERL_MEA_dev_burst_duration_mean |
| Network Spike Peak | # of spikes (or electrodes, or ae) at the peak of network spike | ns.peak.m | NHEERL_MEA_dev_network_spike_peak |
| Network Spike Duration | mean of the duration of network spikes in mean (s) | ns.durn.m | NHEERL_MEA_dev_spike_duration_mean |
| Mean Number of Spikes in Network Spikes | number of spikes in newtork spike, averaged on ae?| ns.mean.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_number_mean |

| Interspike Interval | Inter spike interval (s) within a burst, averaged on ABE | mean.isis | NHEERL_MEA_dev_per_burst_interspike_interval |
| Mean Interburst interval | Mean interval between bursts (s), averaged on ABE | mean.IBIs | NHEERL_MEA_dev_interburst_interval_mean |
| Interspike Interval in Network Spikes | mean interspike interval of spikes in a network spike (s) | ns.mean.insis |
NHEERL_MEA_dev_per_network_spike_interspike_interval_mean |
| Standard Deviation of Network Spike Duration | standard deviation of network spike duration | ns.durn.sd | NHEERL_MEA_dev_network_spike_duration_std |


- Historically, activity in the wells was also recorded on DIV 2. However, there was usually very little activity, which sometimes caused errors in the calculations of parameters because there was so little data. To solve this, someone decided to set all DIV 2 parameter values to 0 in burst_parameter_to_AUC.R. Now, even though activity is no longer recorded on DIV 2, we still calculate the area under the curve with the first point at (2, 0). (versus starting at DIV 5, or DIV 0)

### Cytotoxicity Data

There are 2 measures of cytotoxicity - the lactate dehyrogenase (LDH) and the cell titer blue (as called Alamar blue, AB). Both measure teh change in some byproduct. Those, both measure the percent survival in each well. The script `cytotox_prep05_rawValues.R` extracts the blank-corrected fluorescense or optical density values from each well and formats the data in the long file format. Any negative raw values are set to zero in this script.

### Format all into mc0 file

The script `tcpl_MEA_dev_AUC.R` formats all of the AUC and cytotoxicity data into one long file with the columns needed for an mc0 file in the TosCast Pipeline. 
This script will also
- Set wllt to 't' for all treated wells and 'n' for all control wells (where conc = 0)
- Set the treatment column to the corresponding vehicle control for control wells
- Check for any recycled plate ID's within the current data set, and with previous data sets. (coming in re-org scripts). Any re-used plate ID is renamed in order to ensure a unique plate ID for each culture date

### Adjust as needed

For some datasets, some adjustment may need to be made. For example, you may need to
- remove rows
- set wllq = 0. 

### Replace treatment column with sample ID

The script `spid_mapping.R` maps the treatment names to the corresponding sample ID, provided in another file. Because the chemical names in the data are derived from the names used in the Master Chemical Lists, sometimes the names do not match up. Best judgement for renmaing and consulting the lab notebook should be used. 
In the future, the chemical names in the Master Chemical Lists may be replaced with CASN's in order to clean up this process.

Now the file should be ready to be processed with the ToxCast Pipeline.

