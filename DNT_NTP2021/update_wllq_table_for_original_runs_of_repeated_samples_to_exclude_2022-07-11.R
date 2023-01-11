# ------------------------------------------------------------------------ #
# Updating wllq table based on samples that were repeated because of well 
# quality issues
# July 11, 2022
# ------------------------------------------------------------------------ #
library(data.table)
library(openxlsx)

# Load current well quality table
wllq.tb <- as.data.table(read.csv('DNT_NTP2021/wells_with_well_quality_zero.csv'))

# Load rescreen recommendations table
rescreen.tb <- as.data.table(read.xlsx('L:/Lab/NHEERL_MEA/Project - DNT_NTP_2021/DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-05-26.xlsx', sheet = 2))

# Load current data (to get dates for when things tested)
load('DNT_NTP2021/output/DNT_NTP2021_preliminary_longfile.RData')
cat(description)
# Saving a preliminary version of DNT NTP 2021
# So that we can evalute which chemicals need to be repeated.
# What is left to do:
# - Confirm that all concentrations have been corrected to the exact conc consistently
# - convert all concentration units to uM
# - Assign sample ids/blind codes
# Date Ran: May 17 2022

# Determine necessary info for the samples that are being rescreened because of noise (and so we dont' want to keep the original run)
spids.rescreened.noise <- rescreen.tb[grepl('several endpoints appear noisy',rescreen_note) | grepl('Sample has >= 4 noisy endpoints and platewise median of controls < 2SD below mean of all NFA data for key general activity endpoints',rescreen_note), spid]
length(spids.rescreened.noise) # 11

# Is the spid the same as the treatment col from this version of dat?
setdiff(rescreen.tb$spid, dat$treatment) # empty, so all are present
dat[is.na(treatment)] # empty -> so treatment is always dfeined

dat[treatment %in% spids.rescreened.noise, .N, by = .(culture_date, treatment)]
# cool, all from the same culture (20211110)

# I remember that this culture was performed when the hot water was shut off.
# Were any other spid tested in this culture but not rescreened?
dat[culture_date == '20211110', setdiff(treatment, spids.rescreened.noise)]
# "DMSO"     "7126 C11"
rescreen.tb[spid %in% c("7126 C11"), .(spid, wllq_notes, rescreen, rescreen_note)]
# spid                                         wllq_notes rescreen                     rescreen_note
# 1: 7126 C11 7126 C11 will be repeated on a lower concentration        1 rescreen at lower concentrations;
# oh, this one will be rescreened at a lower concentration regardess!!
# But the question remains - should we include or exclude this data?
# Looking at the dose-response curves (see L:\Lab\NHEERL_MEA\Carpenter_Amy\pre-process_mea_nfa_for_tcpl\DNT_NTP2021\check_for_chem_to_rescreen\figs)
# It looks like the was no noise for this sample because there was basically 0 activity!
# But what is interesting is that this chemical isn't 100% cytotoxic until the 4th conc tested!
# So if we only use the new rescreen data, is will only go up to the current lowest conc tested, we may not see cytotoxicity
# So can we trust the data from this culture where it seemed the temperature in the building was affecting the results?

# Let's look at curves for other samples tested at this time...
spids.rescreened.noise
# Looking at the first 3 samples, there was definitely activity (i.e., points near 0, where controls would have been... oh wait.
# The controls would have been affected as well!)

# Other question - wasn't the water off for quite a while? Were not other cultures affected as well?

# Looking at email records...
# Began Sept 28, 2021, was extended to Oct 18, 2021
# Then extended indefinitely.. until
# Hot water began to slowly reheat on Dec 29, 2021!

# Dates of the cultures for this data set:
# 20210915 - 20220413
# Cultures that would have been started or had any recordings in the Sept 28, 2021 - Dec 29, 2021 window:
# 20210929
# 20211027
# 20211110
# 20211124
# 20211208

# Wow, so we can't even say that the 20211110 culture was the only culture during this window,
# or definitely that is was in the coldest months when the hot water would have had the strongest effects

# I feel like I need to assess the impact of the hot water shutoff on the activity of the control wells
# If it is noticably lower or noisy during cultures when the hot water was shut off AND the temperature was lower, 
# then that would be a serious issue
# BUT, based on the reviews I did today... I'm not sure this is the best use of my time.

# For the moment - let's just update the wllq based on where we saw:
# "Sample has >= 4 noisy endpoints and platewise median of controls < 2SD below mean of all NFA data for key general activity"
# won't try to use the hot water shutoff as an explanation for the activity
# But I think I will add an additional wllq_note for other affected cultures.


# Add wllq_note for cultures during hot water shutoff ---------------------

dat[as.numeric(culture_date) + 12 >= 20210928 & as.numeric(culture_date) <= 20211229, .N, by = .(culture_date)][order(culture_date)]


add.wllq.notes <- dat[as.numeric(culture_date) + 12 >= 20210928 & as.numeric(culture_date) <= 20211229, unique(.SD), .SDcols = c('culture_date','apid')]
add.wllq.notes[, Plate.SN := sub('^.*_','',apid)]
add.wllq.notes[, `:=`(DIV = 'all',
                      well = 'all',
                      wllq = 1, # not changing the wllq for these cases
                      wllq_notes = 'Hot water shut off in building',
                      affected_endpoints = 'mea,CTB,LDH')]
setnames(add.wllq.notes, old = 'culture_date', new='date')
add.wllq.notes[, apid:= NULL]
add.wllq.notes[, date:=as.integer(date)]

# Any affected dates already present?
wllq.tb[date %in% add.wllq.notes$date]
# one case.. I will merge at the end

wllq.tb <- rbind(wllq.tb, add.wllq.notes)



# Update wllq for samples repeated because of noise ----------------------------------------

spids.rescreened.noise <- rescreen.tb[grepl('several endpoints appear noisy',rescreen_note) | grepl('Sample has >= 4 noisy endpoints and platewise median of controls < 2SD below mean of all NFA data for key general activity endpoints',rescreen_note), spid]
dat[, row_char:= LETTERS[rowi]]
dat[, well := paste0(row_char, coli)]
update.wllq.tb <- dat[treatment %in% spids.rescreened.noise, unique(.SD), .SDcols = c('culture_date','apid','well','treatment')]
update.wllq.tb[, `:=`(DIV = 'all',
                      wllq = 0, # don't want to use this data, using repeats only
                      wllq_notes = paste0('(',treatment,') Sample has >= 4 noisy endpoints and platewise median of controls < 2SD below mean of all NFA data for key general activity endpoints'),
                      affected_endpoints = 'mea,CTB,LDH')]
update.wllq.tb[, treatment := NULL]
update.wllq.tb[, Plate.SN := sub('^.*_','',apid)]
setnames(update.wllq.tb, old = 'culture_date', new='date')
update.wllq.tb[, apid:= NULL]
update.wllq.tb[, date:=as.integer(date)]

wllq.tb <- rbind(wllq.tb, update.wllq.tb)


# The optional repeat sample: ---------------------------------------------

# Looks like 9163 B6 was not rescreened. (This is the sampel that looks like it had 1 (out of 3) cytotoxic replicates).
# at this time, I think it is better to leave in all 3 replicates - because I think anyoene how knows anything will look at the curve at say - ah ha,
# it looks liek one of the replciates was cytotoxic, but the other 2 weren't! 
# (See tpclfit2 test dose-response curves - even with the wonky replicate, was still able to fit a curve for some endpoints)
# And regardless, I'm erroring on the side of beign more permissive, which is what we want for a screening battery



# Save updated wllq_notes table -------------------------------------------

View(wllq.tb)

# Note: it's okay if there are multiple wllq notes per well
# the scripts will collapse this and take the minimum well quality that applies to each well and merge the wllq notes

write.csv(wllq.tb, file = 'DNT_NTP2021/wells_with_well_quality_zero.csv', row.names= F)
