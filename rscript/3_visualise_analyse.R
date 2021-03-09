setwd('/home/barneyharris/projects/quarry')
source(paste0(getwd(),'/rscript/general_functions.R'))

load('data/intpols.RDS')

results <- list()
results_offset_nonoise <- analyseDat(filePattern = 'march_offset_nonoise_nodiffs')
results_offset_50noise <- analyseDat(filePattern = 'intdat_march_offset_wnoise')
results$`diffDat_feb28_nodiffs` <- analyseDat(filePattern = 'diffDat_feb28_nodiffs')
results_nooffset_50noise <- analyseDat(filePattern = 'diffDat_feb28_nodiffs')

# with bicubic
results$`intdat_march5_offset_w50noise_all` <- 
  analyseDat(filePattern = 'intdat_march5_offset_w50noise_all')
results <- list()
results[[sessionTag]] <- analyseDat(filePattern=sessionTag)
results[[sessionTag]]$plots$errPlots$compareDiff


save(results_offset,results,file='/media/mal/working_files/quarry/results.RData')


