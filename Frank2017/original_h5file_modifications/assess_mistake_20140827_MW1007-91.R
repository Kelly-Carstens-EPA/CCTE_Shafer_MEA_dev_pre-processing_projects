library(rhdf5)
# Backstory:
# I accidentally seem to have modified this h5File on Oct 13, 2020
# L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91/h5Files/ON_20140827_MW1007-91_05_00_001.h5
# as I was developing mymethod to modify the h5files directly, instead of needing to find the spike list files
# (see investigate_original_h5Files.R)
# However, when I attempted to modify the file, I got an error saying that I could not 
# ( I think because you can't just overwrite an h5file without unlinking)

# my real fear:
# that I modified h5_dat somehow as I was playing with this function
# So, I don't want to use unintentionally modified source data!
# But, clearly, when I (accidentally) tried to modify the h5File on the L drive,
# I got this message:

# during a test, i ran:  h5write(nspikes, h5File, "/sCount")
# Error in H5Dwrite(h5dataset, obj, h5spaceMem = h5spaceMem, h5spaceFile = h5spaceFile) : 
#   HDF5. Dataset. Write failed.

# even though it says that I modified this file, I believe that this occured because
# I opened the file while attempting to overwrite it
# (then that attempt was aborted, as seen by the error message)
# I think it says that I modified the file merely because I opened it and closed it in R
h5_dat <- h5read("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91/h5Files/ON_20140827_MW1007-91_05_00_001.h5", name = "/")
first_spike_time <- min(h5_dat$spikes)
last_spike_time <- max(h5_dat$spikes)
last_spike_time # 1204.485
first_spike_time + 900.00 # 1199.51
# So, at the very least it is evident that I did not modify the spikes with my recording-chopping in this original file

# Let's just confirm that the sCount is still accurate, since that is what I attempted to overwrite
spikes_org <- h5_dat$spikes
names(spikes_org) <- rep(h5_dat$channels, times = h5_dat$sCount)
spikes_org <-split(spikes_org, f = names(spikes_org))

# Calculate the updated values wanted
# from sjemea::map.to.h5 (which is called by meadq::map.to.h5.dh)
nspikes <- sapply(spikes_org, length)
all.equal(h5_dat$sCount, nspikes)
# 1] "names for current but not for target"                              "Attributes: < names for target but not for current >"             
# [3] "Attributes: < Length mismatch: comparison on first 0 components >" "target is array, current is numeric"                              
head(nspikes)
# A1_11 A1_12 A1_21 A1_22 A1_24 A1_31 
# 47     6    90   517  3583   175 
head(h5_dat$sCount)
# [1]   47    6   90  517 3583  175


# what does sCount usually look like?
h5_dat2 <- h5read("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 2/20140827 Ontogeny/MW 1007-91/h5Files/ON_20140827_MW1007-91_07_00_001.h5", name = "/")
head(h5_dat2$sCount) #  71  163   11  102  262 3465
# ah, so the names are probably stripped when you write to h5File

length(h5_dat$sCount) == length(nspikes) # TRUE!
names(nspikes) <- NULL
all.equal(h5_dat$sCount, as.array(nspikes)) # TRUE!!

# great! I think I did not actually modify the h5File at all, and it is safe to use.