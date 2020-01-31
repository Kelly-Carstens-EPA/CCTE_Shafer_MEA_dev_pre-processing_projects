# Microelectrode Array Network Formation Assay Pre-Processing Scripts

## to do:
* read over all, when eyes not glazed over...
* merge with master! (will probs need to repalce image links)

## Purpose

These scripts are designed to process the raw data for the microelectrode array network formation concentration-response assay.

Briefly, cortical cells are grown on 48-well microelectrode-containing plates. On each plate, 6 compounds are tested at 7 concentrations, plus 6 control wells. Each group of compounds and concentrations is replicated on 3 plates. The test compounds are added to the wells on days 0, 5, and 9. The electrical activity of the neurons are recorded as the network develops, on days 5, 7, 9, and 12. The recording provide a lot of information concerning the general activity, organization, and connectivity of the neurons in each well. These scripts calculate several parameters from the recordings in order to describe these 3 aspects of the developing neuronal networks.

We want to graph the dose response for each compound and each endpoint, calculate an EC50 value, and determine if the compound is positive hit. These scripts transform the raw the recording data into a long file format. Then, the data can be process with the functions in the ToxCast Pipeline to fit the dose response curves and determine the hit calls for each compound.

## Acknowledgements

These script rely  heavily on functions in the packages `sjemea` and `meadq`, available on github. The scripts in these packages are availabe on GitHub and contain most of the functions used to calculate the parameter values in these scripts. 

## How to use these scripts

For a step-by-step guide, see the document *Step-by-Step_Guide.docx*. Checkout this [diagram](https://ncct-bitbucket.epa.gov/projects/NSLTM/repos/nfa-spike-list-to-mc0-r-scripts/browse/images/SpikeList_to_mc0_overview.png?at=refs%2Fheads%2Fre-org) to visualize the steps. Raw data files are shown in blue, intermediate output files are in purple, and scripts are in orange.

## Narrative of the process

### Raw data: spike list files
The inital raw data is the recordings of the activity in each well, called spike list files. These files are outputted by an Axion Maestro amplifier and software interfaces. There will be a spike list file for each plate and for each day of recording, days in vitro (DIV) 5, 7, 9, and 12. The spike list files are csv files with columns

| Time (s) | Electrode | Amplitude(mV) |
| ----------- | ----------- | ----------- |
where the first column records the time of each spike, the second column records the ID of the electrode that spiked, and the third column records the amplitude of the spike. Each recording spans ~900 seconds (15 minutes).

### h5 files
The scripts `h5_conversion.R` and `spike_list_functions.R` convert the raw data into the Hierarchical Data Format .h5. This file type is designed to handle large amounts of data. One h5 file is created for each spike list file.

### 16 parameters values

The scripts `create_burst_ont_Data.R`, `local.corr.all.ont.ae.filter.R`, and `create_ont_csv.R` calculate 16 parameters from the spike list files. The table below summarizes the 16 parameters.

Note that these are all well-level values. 

A "burst" on an electrode is a set of spikes in rapid succession. Using the max-interval method, a set of spikes must satisfy these conditions in order to be considered a burst:
- The number of spikes in the burst >= 5 spikes
- The duration of the burst >= 0.05s
- The time in between the first 2 spikes in the burst <= 0.1s
- The time in between the last 2 spikes in the burst <= 0.25s
- The amount of time in between bursts >= 0.8s
(Taken from [Brown *et al*., 2016](https://academic.oup.com/toxsci/article/154/1/126/2422066))

A "network spike" is a group of spikes that occur on across several electrodes at the same time. The definition for a network spike was inspired by [Eytan & Marom, 2006](https://www.jneurosci.org/content/26/33/8465). In this code, a network spike is calculated by:

The entire recording is divided into 0.05 s time bins. A network spike occurs when at least 5 electrodes fire within a time bin. The peak of a network spike is the maximum number of electrodes involved in a spike. The duration of a network spike is the length of time between when half of the peak number of electrodes spiked before and after the time at the peak. See *parameter_calcuation_notes.md* for more details.

| Name | Description | Abbreviation | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Number of Active Electrodes (AE) | # of electrodes where mean firing rate >= 5 spikes/min | nAE | NHEERL_MEA_dev_active_electrodes_number |
| Mean Firing Rate | # spikes per second, averaged over all AE in each well | meanfiringrate | NHEERL_MEA_dev_firing_rate_mean |
| Burst Rate | # bursts per minute, averaged over all AE in each well | burst.per.min | NHEERL_MEA_dev_burst_rate |
| Number of Actively Bursting Electrodes (ABE) | # of electrodes where burst rate >= 0.5 bursts/min | nABE | NHEERL_MEA_dev_bursting_electrodes_number |
| Mean Burst Duration | mean duration of bursts (s), averaged over all ABE in each well | mean.dur | NHEERL_MEA_dev_burst_duration_mean |
| Mean Interburst Interval | mean interval between bursts (s), averaged over all ABE in each well | mean.IBIs | NHEERL_MEA_dev_interburst_interval_mean |
| Interspike Interval in a Burst | mean interspike interval (s) within a burst, averaged over all ABE in each well | mean.isis | NHEERL_MEA_dev_per_burst_interspike_interval |
| Percent of Spikes in Burst | # of spikes within burst divided by total spike count, averaged over all ABE in each well | per.spikes.in.burst | NHEERL_MEA_dev_per_burst_spike_percent |
| Number of Network Spikes | # of network spikes in each well during the 15 minute recording | ns.n | NHEERL_MEA_dev_network_spike_number |
| Network Spike Peak | max # of electrodes particpating in a network spike, averaged over all network spikes during recording | ns.peak.m | NHEERL_MEA_dev_network_spike_peak |
| Mean Network Spike Duration | mean duration (s) of all network spikes in each well | ns.durn.m | NHEERL_MEA_dev_spike_duration_mean |
| Standard Deviation of Network Spike Duration | standard deviation of duration of all network spikes in well | ns.durn.sd | NHEERL_MEA_dev_network_spike_duration_std |
| Mean correlation | mean Pearson correlations between pairs of AE, averaged on AE | r | NHEERL_MEA_dev_correlation_coefficient_mean |
| Percent of Spikes in Network Spike | total # of spikes from an electrode in network spikes * / total # of spikes in well during recording | ns.percent.of.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_percent |
| Mean Number of Spikes in Network Spikes |  total # of spikes from an electrode in network spikes * / # of network spikes | ns.mean.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_number_mean |
| Interspike Interval in Network Spikes | mean time between peaks of consecutive network spikes (s) | ns.mean.insis | NHEERL_MEA_dev_per_network_spike_interspike_interval_mean |

\* I have concerns about how the number of spikes in network spikes is calculated. See *parameter_calculation_notes.md*.

One csv file containing these parameter values will be created for each plate. Value for each DIV recording will be in separate rows. These files will be combined into one csv file for all the plates using `comb.summary.R` (or your own implementation of `rbind`) before calculating the area under the curve.

Notes:
- The scripts currently calculate 2 additional parameters: cv.time and cv.network. However, there may be an issue with the way these are calculated. Therefore, these parameter values are removed from the data in `burst_parameter_to_AUC.R`.

- These scripts were created by Diana Hall in ~2014. A few alterations were made to her scripts, such as exclusively filtering for actively bursting electrodes for relevant parameters, calculating the mean correlation only on active electrodes, and generally streamlinging the process. Therefore, edited versions of the 3 scripts mentioned above are included in this repo and should be sourced in order to mask the previous functions from the meadq package, which you will download before you begin.

### Mutual Information

The mutual information is a robust parameter that desribes both the global synchrony and level of activity in a network. See *A multivariate extension of mutual information for growing neural networks* [here](https://www.sciencedirect.com/science/article/abs/pii/S0893608017301612?via%3Dihub) by K. Ball et al. for more information.

The scripts `spikeLoadRoutines.R`, `nmi_wrapper.R`, and `nmi2_final.R` contain the functions used to calculate the mutual information for this assay.

The calculation of the mutual information is computationally intensive. Therefore, this parameter is calculated separatley from the rest of the parameters. The script `MI_script_all.R` is designed to calculate the mutual information for all plates, so that the task could be done overnight or remotely for the entire dataset. One csv file will be created for all 3 replicate plates in each culture date. These files will be combined into one csv file for all the plates using `comb.summary.R` (or your own implementation of `rbind`) before calculating the area under the curve.

As above, here are the names used for this parameter

| Name | Description | Abbreviation | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Normalized Mutual Information | concurrently measures synchrony and activity of the neural network | mi | NHEERL_MEA_dev_mutual_information_norm |

### Area Under the Curve

We want to quatify the alterations to development from DIV 0 - 12 that a compound might cause compared to controls. See this [example](https://ncct-bitbucket.epa.gov/projects/NSLTM/repos/nfa-spike-list-to-mc0-r-scripts/browse/images/meanfiringrate_development_example.jpeg?at=refs%2Fheads%2Fre-org) of the development of the mean firing rate in a given well over time.

In order to "sum up" the overall change in a parameter value, we calculate the trapezoidal area under the curve. This value will be used to compare the overall increase or decrease of a parameter in treated wells and control wells.

The script `burst_parameter_to_AUC.R` uses the `trapz` function from the pracma package to calculate the trapezoidal area under the curve (AUC) for each parameter. One csv file will contain the AUC values for all plates and parameters.

Notes:
- When there are no bursts or network spikes in a well, many parameters that measure some aspect of bursts or network spikes are NA. In order to calculate the area under the curve, these NA values are set to 0. Below is a summary of the parameters that are sometimes NA. For some of these, setting NA instances to zero might be counterintuitive, particularly for the latter four in the list. For example, if there are no network spikes, the Standard Deviation of Network Spike Duration is NA. If we set the standard deviation to 0 in this instance, it implies that the duration of the network spikes is extremely consistent (which we might expect of a very well developed, organized, network). More analysis will be done on these parameters. Perhaps these latter 4 should be calculated differently, or excluded entitrely from the AUC analysis.

| Name | Description | Abbreviation | TCPL acsn |
| ----------- | ----------- | ----------- | ----------- |
| Mean Burst Duration | Mean burst duration (s), averaged on ABE | mean.dur | NHEERL_MEA_dev_burst_duration_mean |
| Network Spike Peak | # of spikes (or electrodes, or ae) at the peak of network spike | ns.peak.m | NHEERL_MEA_dev_network_spike_peak |
| Network Spike Duration | mean of the duration of network spikes in mean (s) | ns.durn.m | NHEERL_MEA_dev_spike_duration_mean |
| Mean Number of Spikes in Network Spikes | number of spikes in newtork spike, averaged on ae?| ns.mean.spikes.in.ns | NHEERL_MEA_dev_per_network_spike_spike_number_mean |
| Interspike Interval | Inter spike interval (s) within a burst, averaged on ABE | mean.isis | NHEERL_MEA_dev_per_burst_interspike_interval |
| Mean Interburst interval | Mean interval between bursts (s), averaged on ABE | mean.IBIs | NHEERL_MEA_dev_interburst_interval_mean |
| Interspike Interval in Network Spikes | mean interspike interval of spikes in a network spike (s) | ns.mean.insis | NHEERL_MEA_dev_per_network_spike_interspike_interval_mean |
| Standard Deviation of Network Spike Duration | standard deviation of network spike duration | ns.durn.sd | NHEERL_MEA_dev_network_spike_duration_std |

- Historically, activity was recorded on DIV 2. There was usually very little activity in these recordings. At some point, it was decided that all DIV 2 parameter values should be set to 0 before calculating the AUC in `burst_parameter_to_AUC.R`. Now, even though activity is no longer recorded on DIV 2, we still calculate the area under the curve with the first point at (2, 0) (instead of just starting at DIV 5).

### Cytotoxicity data

After the cells are grown on the plates for 12 days, two assays are used to assess to the cell viability - CellTiter-Blue and Lactate Dehydrogenase.

The CellTiter-Blue assay (also called Alamar Blue) measures the amount of reagent metabolized by living cells in each well. First, reagent is added to each well. Then, some media from each well is transferred to an opaque 96-well plate. The fluorescense of resazurin, a metabolite of the reagent, is measured. Three blank wells that contain only reagent are used as a baseline fluorescense values. The average of the fluorescence in the three blank-corrected wells is substracted from the raw fluorescence values in the remaining 48 wells. (See [Brown *et al.* (2016)](https://academic.oup.com/toxsci/article/154/1/126/2422066) for more information). The blank-corrected values in the treated wells will be normalized to the median value in control wells in the ToxCast Pipeline. 

The total lactate dehydrogenase (LDH) assay is also used to quantify the number of living cells in treated wells versus control wells. First, all of the media is removed from each well in order to remove any LDH already released from dying cells. Then, a lysis solution is added to lyse all living cells. Next, the lysis solution is transfered to another plate with a solution containing tetrazolium salt. The LDH in the solution is allowed to transfrom the the tetrazolium salt in red formazan for 30 minutes until a stop solution is added. The optical density of red formazan is measured. The amount of red formazan produced reflects the amount of LDH released from the cells that were living at the start of the assay. Three blank wells containing only lysis solution are used as a baseline optical density value. This information was synthesized from [Frank *et al.* (2017)](https://academic.oup.com/toxsci/article/160/1/121/4083261), the product description for [CytoTox 96 Non-Radioactive Cytotoxicity Assay](https://www.promega.com/products/cell-health-assays/cell-viability-and-cytotoxicity-assays/cytotox-96-non_radioactive-cytotoxicity-assay/?catNum=G1780), and from the LDH Assay Summary file *NHEERL_MWP_LDHo_dn_final.docx*. The blank-corrected values in the treated wells will be normalized to the median value in control wells in the ToxCast Pipeline.

For both assays, the script `cytotox_prep06.R` extracts these blank-corrected values from the excel sheets created by the lab technicians. Any negative values are set to zero.

### Format all data into long file

The script `tcpl_MEA_dev_AUC.R` formats all of the AUC and cytotoxicity data into one long file with the columns needed for an mc0 file in the TosCast Pipeline. (except with "treatment" column instead of "spid".)
This script will also:
- Set wllt to 't' for all treated wells and 'n' for all control wells (where conc = 0)
- Set the treatment column to the corresponding vehicle control for control wells
- Check for any recycled plate ID's within the current data set and within previously pipelined data sets. (coming in re-org branch).<br>Any re-used plate ID will be renamed to ensure unique plate IDs for each culture date

### Make adjustments to the data set if needed

Now that all of the cytotoxicity and ontogeny data is combined, you can make adjustments to the entire data set if needed. For example,
- If there was an issue with any wells (e.g. contamination, misdose, etc.), set wllq = 0 for that plate's wells. These data rows will be removed in level 2 in the ToxCast Pipeline.
- Remove any values that should not be included (e.g. unregistered compounds, or rows of plates that were already pipelined with another data set).
These tasks are not currently integrated into any scripts in this repository.

### Replace treatment column with sample ID

The script `spid_mapping.R` maps the treatment names to the corresponding sample IDs. Because the chemical names in the data are derived from the names used in the Master Chemical Lists, sometimes the names do not match up. Best judgement, and consulting the lab notebook should be used to rename the compounds as needed. In the future, the chemical names in the Master Chemical Lists may be replaced with CASN's in order to clean up this process.

Now the file should be ready to be processed with the ToxCast Pipeline.

