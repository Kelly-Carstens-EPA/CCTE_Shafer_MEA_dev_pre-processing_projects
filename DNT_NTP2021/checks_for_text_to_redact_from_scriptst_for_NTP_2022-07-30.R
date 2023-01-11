files <- list.files(path = 'DNT_NTP2021/scripts_for_NTP', pattern = '\\.R', full.names = TRUE)

scripts.txt <- lapply(files, scan, what = character(), sep = '\n')


# Check for any references to files on the L drive
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(x, fixed = 'L:')))))
# 5  8 10 11
files[c( 5,  8, 10, 11)]
# [1] "DNT_NTP2021/scripts_for_NTP/DNT_NTP2021_create_table_of_samples_to_rescreen_2022-07-27.R"                   
# [2] "DNT_NTP2021/scripts_for_NTP/preliminary_run_me_DNT_NTP2021_2022-05-17.R"                                    
# [3] "DNT_NTP2021/scripts_for_NTP/run_me_DNT_NTP2021.R"                                                           
# [4] "DNT_NTP2021/scripts_for_NTP/update_wllq_table_for_original_runs_of_repeated_samples_to_exclude_2022-07-11.R"

# made adjustments, recheck
scripts.txt <- lapply(files, scan, what = character(), sep = '\n')
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(x, fixed = 'L:')))))
# 8
# files 8  -> I know there is a commented out section wehre i reference RMySQL::MySQL() -> this is fine

# check for any passwords
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(tolower(x), fixed = ***REMOVED***)))))
# 8
# ah -> I just have this in the comments in file 8
# dbConnect(drv = RMySQL::MySQL(), user = "", pass = "", dbname='',host = "")
# this is okay
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(tolower(x), fixed = 'user')))))
files[c(8, 10)]
# "DNT_NTP2021/scripts_for_NTP/preliminary_run_me_DNT_NTP2021_2022-05-17.R" "DNT_NTP2021/scripts_for_NTP/run_me_DNT_NTP2021.R" 
# Same line causing this flag for file 8
# And in both , I have the "USER INPUT" section headings - > this is okay



# Update August 1, 2022 ---------------------------------------------------

files <- list.files(path = 'DNT_NTP2021/scripts_for_NTP', pattern = '\\.R', full.names = TRUE)

scripts.txt <- lapply(files, scan, what = character(), sep = '\n')


# Check for any references to files on the L drive
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(x, fixed = 'L:')))))
# 5  8 10
files[c( 5,  8, 10)]
# [1] "DNT_NTP2021/scripts_for_NTP/DNT_NTP2021_create_table_of_samples_to_rescreen_2022-08-01.R"
# [2] "DNT_NTP2021/scripts_for_NTP/preliminary_run_me_DNT_NTP2021_2022-05-17.R"                 
# [3] "DNT_NTP2021/scripts_for_NTP/run_me_DNT_NTP2021.R"  

# made adjustments, recheck
scripts.txt <- lapply(files, scan, what = character(), sep = '\n')
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(x, fixed = 'L:')))))
files[c(8)]
# "DNT_NTP2021/scripts_for_NTP/preliminary_run_me_DNT_NTP2021_2022-05-17.R"
# files 8  -> I know there is a commented out section wehre i reference RMySQL::MySQL() -> this is fine

# check for any passwords
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(tolower(x), fixed = ***REMOVED***)))))
# 8
# ah -> I just have this in the comments in file 8
# dbConnect(drv = RMySQL::MySQL(), user = "", pass = "", dbname='',host = "")
# this is okay
which(unlist(lapply(scripts.txt, function(x) any(stri_detect(tolower(x), fixed = 'user')))))
files[c(8, 10)]
# "DNT_NTP2021/scripts_for_NTP/preliminary_run_me_DNT_NTP2021_2022-05-17.R" "DNT_NTP2021/scripts_for_NTP/run_me_DNT_NTP2021.R" 
# Same line causing this flag for file 8
# And in both , I have the "USER INPUT" section headings - > this is okay
