# Feature Calculation Documentation

The document provides details on how each feature (sometimes called parameter) is calculated, and which scripts perform the calculations. The are the features are divided into 5 categories in this document:

* Spike features
* Burst features
* Network Spike features
* Additional connectivity features
* Cytotoxicity endpoints

The functions used to calculated the features are called in the script `create_burst_ont_Data.R`. The results are stored in are object named `s`. Then, the well-level values are calculated in `create_burst_ont_Data.R` using the appropriate filters. Details for each feature are described below.

The functions rely on several parameters to calculate the features. The default parameters values set in `chgv_parameters.R` (in `meadq`):

| Name | Value | Description | Notes |
| ----------- | ----------- | ----------- | ----------- |
| beg.isi | 0.1 | max ISI for 2 spikes at beg of burst | find in `mi.par$beg.isi` |
| end.isi | 0.25 | max ISI for 2 spikes at end of burst | find in `mi.par$end.isi` |
| min.ibi | 0.8 | min time (s) between bursts (else consecutive bursts are merged) | find in `mi.par$min.ibi` |
| min.spikes | 5 | min # spikes in burst (else burst not counted) | find in `mi.par$min.spikes` |
| min.durn | 0.05 | min time (s) of burst (else burst not counted) | find in `mi.par$min.durn` |
| ns.T | 0.05 | time length of bins (s) | used to find network spikes |
| ns.N | 4 | Greater than ns.N electrodes must spike in one time bin to count as a network spike | used to find network spikes |
| sur | 100 | # of time bins before and after a ns peak in which `find.halfmax` will look for the beg and end of the ns | used to find network spikes |
| elec.min.rate | 0 | Min electrode firing rate |  |
| elec.max.rate | 1000 | Max electrode firing rate | |
| well.min.rate | 0 | Min firing rate in well | |

# Spike Features

## Mean Firing Rate
location in `s` object: `s[[cur.file]]$meanfiringrate`<br>
function called in `create_ont_burst_Data.R`: `h5.read.spikes` (`sjemea`), which calls `construct.s` (`meadq`)

How it's calculated:<br>
the mean firing rate for each electrode is calculated as total # of spikes / total time (s).
The well-level value is found by the average of the mean firing rates from active electrodes.

Note:
The function `construct.s` is in both the meadq and sjema package. The only difference appears to be a patch fix in the meadq version - if there were no spikes on an electrode, the meanfiringrate = nspikes = list().

## Number of Active Electrodes (AE)
location in `s` object: not stored in `s`<br>
function called in `create_ont_burst_Data.R`: n/a

How it's calculated:<br>
An electrode is an active electrode if its mean firing rate is at least 5 spikes per minute (`s[[cur.file]]$meanfiringrate*60>=5`).
The well-level value is the sum of the number of active electrodes in the well.

# Burst Features
Useful abbreviations:<br>
IBI = interburst interval<br>
mean.isis = mean interspike interval within a burst

A "burst" is a set of spikes that occur in rapid succession. The function `mi.find.bursts` (found in `maxinterval.R` (`sjemea`), called by `create_burst_ont_Data.R`) finds bursts using the following algorithm:<br>
Phase 1: find any set of spikes where the first pair of spikes is less than 0.1 s apart, and the last 2 spikes are less than or equal to 0.25 s apart<br>
Phase 2: any bursts with IBI < 0.8 are merged together<br>
Phase 3: any bursts with < 5 spikes or < min.durn are removed<br>

`mi.find.bursts` returns a table of the form:

| beg | end | IBI | len | durn | mean.isis | SI |
| ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- |
| 19 | 25 | NA | 7 | 0.10640 | 0.01773333 | 1 |
| 28 | 33 | 5.42920 | 6 | 0.09480 | 0.01896000 | 1 |
| 38 | 46 | 6.89864 | 9 | 0.13504 | 0.01688000 | 1 |
| 72 | 78 | 27.27240 | 7 | 0.12040 | 0.02006667 | 1 |
| 92 | 98 | 25.09456 | 7 | 0.06904 | 0.01150667 | 1 |

The columns beg and end signify the index of the first and last spike in the burst, respectively.
The column IBI is the time from the last spike of previous burst to the first spike of the current burst
The column len is the number of spikes in the burst
The column durn is the length of time of the burst (in seconds)
The column mean.isis for a burst is calculated by: durn of given burst / (# spikes in burst - 1)
The column SI is the "surpise index.". This feature is only relevant for the Poisson Surprise method of burst detection, which is not used here

All of the burst features are derived from the data in this table.

The above method has some unintended side effects, namely:

* Burst creation - If two sets of 4 or fewer spikes labelled as a 'bursts' in phase 1 are within 0.8 s of each other, they will be merged into 1 burst in phase 2. Then this set of spikes can pass through phase 3 as a valid burst
* Burst extension - If a set of 4 or fewer spikes labelled as a 'burst' found in phase 1 is within 0.8 s of a valid burst, the bursts will be merged together. Thus the duration, # of spikes, and interspike interval in the valid burst are extended
* Burst merging - If two valid bursts are less than 0.8 seconds apart, these bursts will be merged into 1 burst in phase 3

## Burst Rate
location in `s` object: ``s[[cur.file]]$bs$bursts.per.min`<br>
function called in `create_ont_burst_Data.R`: `calc.burst.summary` (`sjemea`)

How it's calculated:<br>
The number of bursts on an electrode is found by the # rows of in its burst table (see table above)
The bursts per min is calculated by ( # bursts / duration of the recording ) \* 60.
The well-level value is found by the average bursts per minute of the active elctrodes in each well

## Number of Actively Bursting Electrodes (ABE)
location in `s` object: not stored in `s`<br>
function called in `create_ont_burst_Data.R`: n/a

How it's calculated:<br>
An electrode is an actively bursting electrode if on average it has at least 1 burst every 2 minutes (`(s[[cur.file]]\$bs\$bursts.per.min >= 0.5`).
The well-level value is the sum of the number of actively bursting electrodes in the well.

## Mean Burst Duration
location in `s` object: `s[[cur.file]]$bs$mean.dur`<br>
function called in `create_ont_burst_Data.R`: `calc.burst.summary` (`sjemea`)

How it's calculated:<br>
The duration of the each burst is found as described above in the burst day (spikes[data[,"beg"]] - spikes[data[,"end"]] - i.e., the time at the index of the first spike, to the time at the index of the last spike in the burst.)
The `mean.dur` for each electrode is calculated as the average "durn" from the burst table.
The well-level value is found by the average of the mean durations of bursts from actively bursting elctrodes.

## Mean Interburst Interval
location in `s` object: `s[[cur.file]]$bs$mean.IBIs`<br>
function called in `create_ont_burst_Data.R`: `calc.burst.summary` which calls `calc.all.ibi`, which calls `calc.ibi` (`sjemea`)

How it's calculated:<br>
The IBI for each set of bursts is recalculated (not sure why they didn't use the IBI data from the burst summary table):
The index of the "end" spikes is calculated as the index of the "beg" spike plus the "len" (number of spikes in the burst) minus 1.
 The interburst intervals are calculated by spikes[start.spikes] - spikes[end.spikes] (spikes stores the time of each spike)
The IBI for each electrode is calculated as the average of its inter-burst intervals.
In the burst summary table above, the IBI column is the time from the last spike of the previous burst to the first spike of the current burst.
The well-level value is found by the average of the mean interburst intervals from actively bursting elctrodes.

## Interspike Interval in a Burst
location in `s` object: `s[[cur.file]]$bs$mean.isi`<br>
function called in `create_ont_burst_Data.R`: `calc.burst.summary`, which calls `calc.all.isi` (`sjemea`)

How it's calculated:<br>
`calc.all.isi` - returns a vector of the time between all spikes that were in bursts for each electrode<br>
`calc.burst.summary` - averages the interspike intervals within bursts for each electrode. Result stored in res$mean.isis<br>
The well-level value is found by the average of the mean inter spike interval in bursts from actively bursting elctrodes.

## Percent of Spikes in Burst
location in `s` object: `s[[cur.file]]$bs$per.spikes.in.burst`<br>
function called in `create_ont_burst_Data.R`: `calc.burst.summary` 

How it's calculated:<br>
For each electrode, sum up total number of spikes in burst from burst table (the "len" column).
Then divide by total number of spikes on that electrode.
The well-level value is found by the average percents of spikes in bursts from actively bursting elctrodes.

# Network Spike Features
A network spike occurs when several electrodes fire at the same time. Network spikes are indicative of network connectivity and synapse formation. (Note that a *network burst* is different - it adds the requirement that the network spike also qualfy as a burst).

Creating the network spike table:<br>
The function`compute.ns` (located in the script `networkspikes.R` (sjemea), called in `calculate.network.spikes`) calls the functions needed to calculate the network spikes and stores the results in a table. Each row corresponds to a network spike. "time" is the time at the peak of the network spike. The index is the time-bin index of the network spike.

| time | index | peak.val | durn |
| ----------- | ----------- | ----------- | ----------- |
10.10 | 203 | 14 | 0.2233333
38.25 | 766 | 13 | 0.2354167
57.25 | 1146 | 13 | 0.2729167
101.45 | 2030 | 14 | 0.2466667
113.10 | 2263 | 14 | 0.2033333
144.85 | 2898 | 14 | 0.2416667
162.30 | 3247 | 12 | 0.3000000
179.30 | 3587 | 13 | 0.2475000
190.60 | 3813 | 11 | 0.3166667
217.75 | 4356 | 14 | 0.2250000
227.60 | 4553 | 14 | 0.2066667
256.75 | 5136 | 13 | 0.2437500

How it's calculated:<br>
Divide the entire recording into time bins of length ns.T = 0.05. (`spikes.to.count2`)<br>
Count the number of spiking electrodes in each time bin (`C_ns_count_activity`, found in `sjemea.c`). The # of spiking electrodes in each time bin is called 'count'.<br>
Cycle though each time bin. When you find a time bin with count > 0, set peak = count at that time bin. This marks the start of a potential network spike. If the count is larger at the next time bin, replace peak with the new count. Continue until you get to a time bin with count of 0. Thus 'peak.val' is the largest value of 'count' in a set of time bins enclosed by 2 time bins of count=0.\* If 'peak.val' is greater than ns.N = 4, then that set of time bins is determined to be a network spike. Otherwise, 'peak.val' is not recorded in the network spike summary table. (`find.peaks`)<br>
Calculate the duration of each network spike by the length of time from  (`mean.ns`, which calls `find.halfmax`)

\* Thought: Should the number of spikes really have to get back to 0 for it to be a completed network spike? Like, what if there is just some background electrode... So, we could be undercounting the number of network spikes. - actually,  I think this is okay. Would we really want to split a network spike into 2? depends...

Minor bug: In `networkspikes.R` (sjemea) in the function `mean.ns`, any network spike occuring within 100*ns.T of the beginning or end of the recording are removed (ns.T = 0.05 seconds). I doubt many network spikes occur within 5 seconds of the beginning or end though, and the beginning and end of the recording are arbitrary endpoints anyhow.

Note: `summary.newtworks.spikes.dh` gets ns.N and ns.T from `s` for each well individiually. So theoretically those parameters could be tweaked via `s`.

## Number of Network Spikes
location in `s` object: `s[[cur.file]]$ns.all$[well_name]$brief['n']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh`

How it's calculated:<br>
The number of network spikes in each well is the number of rows in the network spike summary table. (`nrow(ns$measures)` in `summary.network.spikes.dh`)

## Network Spike Peak
location in `s` object: `s[[cur.file]]$ns.all[well_name]$brief['peak.m']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

How it's calculated:<br>
The mean number of electrodes that particpate in a network spikes in each well is calculated as the mean of 'peak.val' from the network spike summary table (`mean(ns$measures[, "peak.val"])`).

## Mean Network Spike Duration
location in `s` object: `s[[cur.file]]$ns.all[well_name]$brief['durn.m']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

How it's calculated:<br>
For a given network spike, let 'peak.t' be the time bin of 'peak.val'. Then, find the time bins before and after 'peak.t' where the 'count' is equal to half of 'peak.val' (`find.halfmax`).
The duration of each netwrok spikes is the difference between the indicies of the start and end time bins times the length of a time bin (ns.T = 0.05 s) (`mean.ns`).
Find the mean of the durations of all network spikes in each well (`mean(ns$measures[, "durn"], na.rm = TRUE)` in `summary.network.spikes.dh`).

## Standard Deviation of Network Spike Duration
location in `s` object: `s[[cur.file]]$ns.all[well_name]$brief['durn.sd']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

How it's calculated:<br>
For a given network spike, let 'peak.t' be the time bin of 'peak.val'. Then, find the time bins before and after 'peak.t' where the 'count' is equal to half of 'peak.val' (`find.halfmax`).
The duration of each netwrok spikes is the difference between the indicies of the start and end time bins times the length of a time bin (ns.T = 0.05 s) (`mean.ns`).
Fine the standard deviation of the durations of all network spikes in each well (`sd(ns$measures[, "durn"], na.rm = TRUE)` in `summary.network.spikes.dh`).

## Percent of Spikes in Network Spikes
location in `s` object: `s[[cur.file]]$ns.all$brief['percent.of.spikes.in.ns']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

We want to find:
total # of spikes in network spikes / total # of spikes from any electrode in the well.

How it's calculated:<br> 
The number of spikes in network spikes for a given electrode is calculated as the total number of spikes from any electrode that occur within 1 time bin (0.05 s) of the peak of a network spike. If an electrode only spikes once during the peak of a network spike (i.e. less than ns.E = 2 spikes during time bin at peak), then it is not counted as a spike in the network spike. The total number of spikes in network spikes is the sum of the number of spikes in network spikes from each electrode in the well. This total is divided by the the total number of spikes throughout the recording in the well. (`100 * sum(en.map)/sum(e$nspikes[indexes])`)

Proposed change:
I think we should use the duration of the network spike bin (perhaps as determined with the halfmax approach), instead of just 1 time bin at the peak.
The network spike duration, using halfmax approach, is usualy ~0.15 - 0.35 seconds, so a typical network spike would cover 3-7 time bins of length ns.T = 0.05 s.

## Mean Number of Spikes in Network Spikes
location in `s` object: `s[[cur.file]]$ns.all$brief['mean.spikes.in.ns']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

We want to find:
total # of spikes in network spikes / total number of network spikes

How it's calculated:<br>
The total number of spikes in network spikes is calculated as in 'Percent of Spikes in Network Spikes'. This total is divided by the total number of network spikes in the well (which is found by the number of rows of the network spike summary table). (`sum(en.map)/nrow(ns$measures)`)

Proposed change:
same as described in 'Percent of Spikes in Network Spikes'

## Inter-Network Spike Interval
location in `s` object: `s[[cur.file]]$ns.all$brief['mean.insis']`<br>
function called in `create_ont_burst_Data.R`: `summary.network.spikes.dh.R`

How it's calculated:<br>
Using the network spike summary table, calculate the difference in time between each consecutive pair of network spikes. Then take the average of the differences (`mean(diff(ns$measures[, "time"]))`). Note that the "time"" is the time at the peak of the network spike.

# Additional connectivity features

## Mean correlation
location in `s` object: `s[[cur.file]]$local.cor`<br>
function called in `create_ont_burst_Data.R`: `local.corr.all.ont.ae.filter`

How it's calculated:<br>
A "spike train" is created for each electrode. The spike train is a list that records the activity at every millisecond in the recording, with a 1 if there was a spike and a 0 if not.<br>
The pairwise correlation between electrodes is calculated as the Pearson correlation between the spike trains.<br>
The correlation coefficient for a given electrode is the mean of its pairwise correlations with every other active electrode in the well.<br>
The well-level value is found by the average of the mean correlations for each active electrode.

## Mutual Information
The mutual information is calculated separately from the rest of the features.
scripts: `spikeLoadRoutines.R`, `nmi_wrapper.R`, and `nmi_final2.R`. The script `MI_script_all.R` calls the functions in those scripts.

Amy has not yet read through the code to thoroughly understand how the mutual information is calculated. The mutual information is a robust parameter that desribes both the global synchrony and level of activity in a network. See *A multivariate extension of mutual information for growing neural networks* [here](https://www.sciencedirect.com/science/article/abs/pii/S0893608017301612?via%3Dihub) by K. Ball et al. for more information.

# cytotoxicity endpoints

## Alamar Blue
script: `cytotox_prep06.R`

The blank-corrected fluorescense values from each well are collected from the input data sheet. If any value is negative, it is set to 0. The blank-corrected values and well, plate, treatment, and dose data are stored in output csv file.

## LDH
script: `cytotox_prep06.R`

The blank-corrected optical-density values from each well are collected from the input data sheet. If any value is negative, it is set to 0. The blank-corrected values and well, plate, treatment, and dose data are stored in output csv file.
