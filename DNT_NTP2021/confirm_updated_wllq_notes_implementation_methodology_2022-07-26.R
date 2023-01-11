# ------------------------------------------------------------------------ #
# Verification of updated wllq notes implementation
# compared to previous method
# July 26, 2022
# ------------------------------------------------------------------------ #

# Load data
load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
# cat(description)
# DNTP MEA NFA prepared data
# Date Ran: July 26 2022

# Just confirming this lines up...
dat[, wllq := NULL]
dat[, wllq_notes := NULL]

dat[, wllq := 1]
dat[, wllq_notes := '']

## OLD method -----------------------

## ** implement wllq notes found in readme's by treatment!! 
# (maybe do this after finalize conc's/spids? We'll see...)
dat[, culture_date := sub('_.*$','',apid)]

dat[culture_date == '20210915' & treatment == "7126 A3", 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),
                             '7126 A3 prrecipitates in media (need to be repeated)'))]

dat[culture_date == '20210915' & treatment == '7126 A12',
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 A12 precipitates in media, need to be repeated on lower concentration"))]
dat[culture_date == '20210915' & treatment == '7126 A9', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 A9 turns the media yellow - may be acidic"))]

dat[culture_date == '20210929' & treatment == '7126 B2', 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),
                             "Chemical 7126 B2 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well\nwill be repeated with lower concentration"))]
dat[culture_date == '20210929' & treatment == '7126 B11', 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"Chemical 7126 B11 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well"))]
dat[culture_date == '20210929' & treatment == '7126 B12', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')), "Chemical 7126 B12 is yellow in color."))]

dat[culture_date == '20211027' & treatment == '7126 H10', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 H10 will be repeated on a lower concentration"))]

dat[culture_date == '20211110' & treatment == '7126 C11', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 C11 will be repeated on a lower concentration"))]


dat[culture_date == '20211124' & treatment %in% c('7126 D4','7126 D5'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"Chemical 7126 D4 and 7126 D5 will be repeated at a lower concentration."))]

dat[culture_date == '20211124' & treatment %in% c('7126 D7'),
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 D7 need to be repeated on a lower concentration"))]

dat[culture_date == '20211208' & treatment %in% c('7126 E3'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 E3 may be needed to be repeated on a lower concentration "))]
dat[culture_date == '20220119' & treatment %in% c('7126 F1','7126 F2'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F1, 7126 F2 need to be repeated on a lower concentration "))]
dat[culture_date == '20220119' & treatment %in% c('7126 F8'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F8 need to be repeated on a lower concentration"))]
dat[culture_date == '20220119' & treatment %in% c('7126 F12'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F12 may be needed to be repeated on a lower concentration"))]

dat[culture_date == '20220223' & treatment %in% c('7126 G3'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 G3 may need to be repeated at a lower concentration."))]

dat[culture_date == '20220316' & treatment %in% c('7126 H7'), 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 H7 needs to be repeated (dosing error) "))]


# # consistent implementation of what's wllq 0, what's jsut a note?
# View(dat[wllq_notes != '', .N, by = .(wllq, wllq_notes)][order(wllq_notes, wllq)])
# View(dat[wllq_notes != '', .N, by = .(treatment, wllq, wllq_notes)][order(wllq_notes, wllq)])

# general rule: preciptate -> wllq == 0
# dosing error, cell debris -> wllq == 0
# need to repeat at lower conc -> note, but wllq still == 1
# note about color of chemical/solution -> note, but wllq still == 1

# Do some checks to confirm I entered all readme ntoes (soem counts?)
# confirmed that all notes in readme's have been implemented.

# See if wllq aligns
dat[, .N, by = .(wllq_merged, wllq)]
#    wllq_merged wllq      N
# 1:           1    1 259334
# 2:    evaluate    1   6523
# 3:    evaluate    0   7308
# 4:           0    1  20199
# 5:           0    0   1827
dat[wllq == 1 & wllq_merged == 'evaluate', .N, by = .(wllq_notes, wllq_notes_merged)]
# this is fine
dat[wllq == 0 & wllq_merged == 'evaluate', .N, by = .(wllq_notes, wllq_notes_merged)]
# these are mostly notes regarding precipitate, will evaluate instead of just remove whre this is th eonly data we have
dat[wllq == 1 & wllq_merged == 0, .N, by = .(wllq_notes, wllq_notes_merged)]
# yep, I think what i"m doing now is more robust. Main case is setting wllq to 0 for noisy samples that were repeated
