# Content: get the subset of the original data for demo usage.
# usage: manually.
########################################################################################
flags1 = which(NY_SID_2009_HFdata_refined[,died == 1])
flags2 = sample(which(!flags),3000)
NY_SID_2009_HFdata_refined_subsetDemo <- NY_SID_2009_HFdata_refined[c(flags1,flags2)]
# > dim(NY_SID_2009_HFdata_refined)
# [1] 64580   717
# > dim(NY_SID_2009_HFdata_refined_subsetDemo)
# [1] 5805  717
