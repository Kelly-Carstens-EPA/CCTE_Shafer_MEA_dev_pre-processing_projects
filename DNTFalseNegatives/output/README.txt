The version of DNTFalseNegatives longfile that was saved on 2022-04-25 was used to calculate the preliminary hit calls for the 6-ppd and 6-ppd quinone data on April 28, 2022.

For this version of the data, I had set the wllq to 0 where a compound was noted to have precipitated out of solution (see deprecated wllq notes table). However, during this analysis, we realized that we should NOT sent the wllq to 0 where precipitate was observed. I set the wllq to 1 where the precipitate was the only wllq note for that analysis. However, there are 2 wells that were contaminated on DIV12 that also had precipitate. Since the wllq was 0 because of the precipitate, the script did not calculate an estimated value for DIV12 for these 2 wells (none of the removed contaminated wells had tested 6-ppd or 6-ppd quinone).

Now, I have updated the wllq notes table so that the wllq will not be set to 0 for these 2 wells. I am re-running the run_me from the step of calculating the AUC, then will manually add the wllq notes for the precipitate in the run_me (but not udpate the wllq). That way, I will have updated data and methods for whenever we pipeline this data.

- Amy F. Carpenter, April 29, 2022 