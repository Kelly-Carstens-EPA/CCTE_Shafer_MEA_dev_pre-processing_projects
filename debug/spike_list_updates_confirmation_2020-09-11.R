# confirming updates to spike_list_functions re checking offset
source('spike_list_functions.R')
remake_all <- TRUE
# analysis <- c("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-21/Spike List Files/ON_20180801_MW1207-21_05_00(000)_spike_list.csv",
#               "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-21/Spike List Files/ON_20180801_MW1207-21_07_00(000)_spike_list.csv",
#               "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 8/70-2417/Spike List Files/Original/NFA_20191016_MW70-2417_5_00(000)(000)_spike_list.csv",
#               "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/NFA_20191016_MW70-2417_5_00(000)(000)_testoffset_spike_list.csv"
# )
# confirmed that this throws and error for the last file, where I tweaked the tagphrase but it is offset!
analysis <- c("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-21/Spike List Files/ON_20180801_MW1207-21_05_00(000)_spike_list.csv",
              "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-21/Spike List Files/ON_20180801_MW1207-21_07_00(000)_spike_list.csv",
              "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 8/70-2417/Spike List Files/Original/NFA_20191016_MW70-2417_5_00(000)(000)_spike_list.csv"
)
masterChemFiles <- c("L:/Lab/NHEERL_MEA/Project PFAS 2018/20180801 Culture PFAS Group_1/MW1207-21/csv Files/20180801_MW1207-21_MaestroExperimentLog_Ontogeny.csv",
                     "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 8/70-2417/csv Files/20191016_MW70-2417_MaestroExperimentLog_Ontogeny.csv")
# removing usual line to get analysis files from h5_conversion.R
source('h5_conversion.R')

# all done!

# confirm these files are identical:
dat_u <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files/ON_20180801_MW1207-21_05_00(000).h5", name = "/")
dat_o <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/original_h5Files/ON_20180801_MW1207-21_05_00(000).h5", name = "/")
all.equal(dat_u, dat_o)
# [1] "Component “summary.table”: Component “nelectrodes”: Mean relative difference: 0.14"
dat_u$summary.table
dat_o$summary.table

dat_u <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files/ON_20180801_MW1207-21_07_00(000).h5", name = "/")
dat_o <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/original_h5Files/ON_20180801_MW1207-21_07_00(000).h5", name = "/")
all.equal(dat_u, dat_o)
# [1] "Component “summary.table”: Component “nelectrodes”: Mean relative difference: 0.0311284"
dat_u$summary.table
# file nelectrodes nspikes time.min time.max
# 1 ON_20180801_MW1207-21_07_00(000)         514  137022  0.00168 899.9856
dat_o$summary.table
# file nelectrodes nspikes time.min time.max
# 1 ON_20180801_MW1207-21_07_00(000)         530  137022  0.00168 899.9856
length(dat_u$spikes)
length(dat_u$channels) # 514
length(dat_o$channels) # 514... woah!!
# hmm, did I perhaps make these h5 files before I made the updates to split?

names(dat_o)
all.equal(dat_u$spikes, dat_o$spikes) # TRUE

# now checkign the offset file that should be different
dat_u <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files/NFA_20191016_MW70-2417_5_00(000)(000).h5", name = "/")
dat_o <- h5read("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/h5Files/NFA_20191016_MW70-2417_5_00_000__000.h5", name = "/")
all.equal(dat_u, dat_o)
dat_u$summary.table
# file nelectrodes nspikes time.min time.max
# 1 NFA_20191016_MW70-2417_5_00(000)(000)         125   12520 1620.874 2519.981
dat_o$summary.table
# file nelectrodes nspikes time.min time.max
# 1 NFA_20191016_MW70-2417_5_00(000)(000)         139   12532 1620.874 2520.732

# check if the spike below a certain level again
all.equal(dat_u$spikes, dat_o$spikes[dat_o$spikes <= max(dat_u$spikes)])
# TRUE!