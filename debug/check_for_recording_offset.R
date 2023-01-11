# Let's make a fun to check every h5file ever if it starts signficantly before 0!
# okay, so really, the deal is that I need to check the toxcast file, to determine if seline needs the updated files
h5file <- h5Files[1]

h5Files <- choose.files(caption = "Select h5 files to analyze")
h5Files <- c(h5Files, choose.files())

min_times <- sapply(h5Files, function(h5File) h5read(h5File, name = "summary.table")$time.min)

# selected all OP's prev's pipelined
range(min_times) # 0.00008 0.11880
summary(min_times)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000080 0.000560 0.001800 0.006188 0.003360 0.118800 
stripchart(min_times, vertical = T, method = "jitter", pch = 1)

min_times[min_times > 0.04] # only1, and it is DIV5
# so it def does not look like there is any file offset, just sometimes a pause before the first recorded spike
# but there could be an offset for some random file out htere...
