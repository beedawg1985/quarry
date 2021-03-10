setwd('/home/barneyharris/projects/quarry')
source(paste0(getwd(),'/rscript/general_functions.R'))

# load('data/intpols.RDS')

# results <- list()
# results_offset_nonoise <- analyseDat(filePattern = 'march_offset_nonoise_nodiffs')
# results_offset_50noise <- analyseDat(filePattern = 'intdat_march_offset_wnoise')
# results$`diffDat_feb28_nodiffs` <- analyseDat(filePattern = 'diffDat_feb28_nodiffs')
# results_nooffset_50noise <- analyseDat(filePattern = 'diffDat_feb28_nodiffs')

# with bicubic
results <- list()
results$`intdat_march5_offset_w50noise_all` <- 
  analyseDat(filePattern = 'intdat_march5_offset_w50noise_all')

results$`intdat_march6_offset_nonoise_all` <- 
  analyseDat(filePattern = 'intdat_march6_offset_nonoise_all')

results$`intdat_march6_nooffset_w50noise_all` <- 
  analyseDat(filePattern = 'intdat_march6_nooffset_w50noise_all')


# save(results,file='/media/mal/working_files/quarry/results_march5-6.RDS')




