

# alternative helper function extracts relevant data from compareInt() output
# x <- 1
# paramsCv <- do.call(rbind,lapply(1:length(rasData), function(x) {
#   allRmse <- rasData %>% map_df('rmses') %>% 
#     dplyr::rename(run = run_no)
#   
#   params <- rasData[[x]]$orig.maps %>% 
#     map(~.x$param) %>% discard(is.null)
#   
#   params.j <- params %>% 
#     map(~.x %>% left_join(allRmse,
#                           on=c('run','intpol_fid','int_method'))) %>% 
#     melt(id.vars = c("run", "intpol_fid", "int_method", "trainingErr.r", "testErr.r", 
#                      "compareDiff", "testErr.ex.r", "testErr.inc.r", "compareDiff.inc.r", 
#                      "compareDiff.ex.r")) %>% 
#     mutate(L1 = NULL)
# }))

cvPlots <- map2(paramsCv,names(paramsCv),.f = function(df, y) {
  df.m <- df %>% 
    melt(id.vars = c("run_no", "intpol_fid", "int_method", "trainingErr.r", "testErr.r", 
                     "compareDiff", "testErr.ex.r", "testErr.inc.r", "compareDiff.inc.r", 
                     "compareDiff.ex.r"))
  ggplot(df.m) + 
    geom_point(aes(x = value,
                   y = testErr.ex.r)) + 
    facet_wrap(variable ~ intpol_fid,
               scales='free',
               ncol = length(unique(df.m$intpol_fid))) + 
    ggtitle(y)
})
cvPlots$`Nearest Neighbor`
cvPlots$`GRASS Regularized Splines Tension`
# facet_grid_paginate(intpol_fid + variable ~ int_method,
#                     ncol=4,
#                     nrow=6,
#                     page=1,
#                     scales='free')
a
# visualize results
vizData <- function(paramsCv) {
  
  # names of sites' fids in data
  fids <- unique(paramsCv[[1]]$intpol_fid)
  
  fids %>% map(.f = function(x) {
    pcv.m <- paramsCv$`Nearest Neighbor` %>% dplyr::filter(intpol_fid == x) %>% 
      # melt(measure.vars = c('nmaxVals','nminVals'))
      melt(id.vars=c("run_no", "intpol_fid", "int_method", "trainingErr.r", "testErr.r", 
                     "compareDiff", "testErr.ex.r", "testErr.inc.r", "compareDiff.inc.r", 
                     "compareDiff.ex.r"))
    
    ggplot(pcv.m) + 
      geom_point(aes(x = value,
                     y = testErr.ex.r)) + 
      facet_wrap(~variable, scales='free')
  })
  
  
  
  
  vars <- names(rasData[[1]]$diff.maps[[1]][[1]])
  averageRas <- lapply(vars, function(v) {
    meanRas <- lapply(dat$diff.maps, function(n) {
      allRuns <- n %>%
        map(v)
      do.call(mean, allRuns)
    })
    stack(meanRas)
  })
  names(averageRas) <- vars
  plot(averageRas$testErr.r)
  require(reshape)
  require(ggforce)
  
  allRmse <- rasData %>% map_df('rmses')
  rmse.m <- melt(allRmse,id.vars=c('int_method','run_no','intpol_fid'))
  rmse.m$int_method_wrapped <-  sapply(lapply(rmse.m$int_method, strwrap, width=20),
                                       paste, collapse="\n")
  
  ggplot(rmse.m %>% dplyr::filter(intpol_fid %in% c(2))) +
    geom_boxplot(aes(factor(int_method_wrapped),value)) +
    labs(x = 'Interpolation method',
         y = 'RMSE') +
    facet_grid(rows=vars(variable),scales = 'free') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ggtitle(paste0('RMSE for polygon fid:',pol$fid))
  
  
  
}
# 
# x <- dat$rmses
# 
# plot(dat$orig.maps$`Triangular Irregular Surface`$ras[[1]])
# plot(dat$diff.maps$`Triangular Irregular Surface`[[1]]$compareDiff.inc.r)
# sa <- terrain(dat$orig.maps$`Triangular Irregular Surface`$ras[[1]],opt=c('slope', 'aspect'))
# hs <- hillShade(slope=sa$slope,aspect=sa$aspect)
# plot(hs)
# plot(dat$diff.maps$`Triangular Irregular Surface`[[1]]$compareDiff.inc.r)
# 
# ggplot(dat$rmses) + 
#   geom_boxplot(aes(factor(int_method),compareDiff.inc.r))
# # visualise outputs
# rmses <- dat$rmses
# dat$orig.maps$`Nearest Neighbor`$param
# interpRas <- lapply(1:5, function(x) {
#   rasPrep <- cutHole(int,x)
#   maskIntDiff(rasPrep)
# })
# 
# # process previous run outputs
# # intFiles <- list.files('raster/',pattern='raslist_int*',full.names = T)
# # interpRas <- lapply(intFiles, function(x) mget(load(x)))
# 
# # load previous run
# # save(interpRas, file='data/interpRas_default_settings.RData')
# # load(file='data/interpRas_default_settings.RData')
# # 
# # load('data/cvdevGrid.ras3.RData')
# # load('data/cvdevGrid.ras5.RData')
# # load('data/cvdevGrid.ras10.RData')
# diffStats <- function(st) {
#   vals <- as.vector(st[[1]])
#   vals <- vals[!is.na(vals)]
#   rmse <- sqrt(mean(vals^2))
#   vol <- sum(abs(vals))
#   return(list(rmse = rmse,
#        vol_diff = vol))
# }
# #  
# # interRasEx <- interpRas %>% map(~ pluck(., 'rasterListClip')) # if loaded from files
# interRasEx <- interpRas
# 
# 
# rmses <- do.call(rbind, lapply(1:length(interRasEx), function(x) {
#   df <- interRasEx[[x]] %>%
#     map(c('fill','diff')) %>% map(diffStats) %>%
#     as.data.frame() %>% mutate(int_row = x)
#    }))
# 
# rmses.m <- reshape2::melt(rmses,id.vars='int_row') %>%
#   filter(str_detect(variable,'rmse')) %>%
#   mutate_if(is.factor,as.character)
# 
# rmses.m3 <- rmses.m %>% left_join(rmses.m2, by=c('int_row','variable'))
# # 
# #
# univar <- function(x) {
#   data.frame(mean = mean(x),
#        sd = sd(x),
#        median = median(x),
#        max = max(x),
#        min = min(x))
# }
# 
# rmses.m.grp <- rmses.m %>%
#   dplyr::group_by(variable) %>%
#   dplyr::summarise(univar(value))
# 
# # ggplot(rmses.m) + 
# #   geom_bar(aes(x = factor(int_row),
# #                y = value,
# #                fill = variable),
# #            stat='identity',
# #            position = 'dodge')
# # 
# # rmses.mean <- rmses %>% map_df(mean) %>% t
# # 
# # which(int$include_id == 31)
# # plot(rmses$inc_id,rmses$Nearest.Neighbor.rmse)
# # a[[1]]
# # plot(interpRas[[1]]$rasterListClip$`GRASS Bilinear Splines`$fill$diff)
# # a <-  map(rasterListClip,'fill')
# # 
# # filldiffsRMSE <- a %>% map(c('fill','diff')) %>%
# #   map(diffStats)
# # 
# # filldiffsRMSE <- rasterListClip %>% map(c('fill','diff')) %>%
# #     map(diffStats)
# 
# #  
# #   # plotting function
# #   # pol_overlay <- pol
# #   # raster_name <- 'Nearest Neighbor'
# #   # raster_object <- rasterListClip$`Nearest Neighbor`$surf$int
# #   plot_my_stars <- function(raster_object, raster_name, pol_overlay){
# #     print(raster_name)
# #     sf <- st_as_sf(raster_object, as_points = TRUE, merge = FALSE)
# #     df <- cbind(z = sf[,1], st_coordinates(sf)) %>% 
# #       as.data.frame()
# #     colnames(df) <- c("Z","X", "Y")
# #     
# #     ggplot() +
# #       geom_raster(data=df, aes(x = X, y = Y, fill = Z)) +
# #       geom_sf(data=pol,
# #               aes(color='Interpolated area'),
# #               color='white',
# #               fill=NA) + 
# #       ggtitle(label = raster_name) +
# #       scale_fill_viridis(option = "C") +
# #       theme_bw() +
# #       theme(
# #         axis.text = element_blank(),
# #         axis.title = element_blank(),
# #         axis.ticks = element_blank()
# #       )
# #   }
# #   
# #   a_rasPlot <- plot_my_stars(a_ras, 'Original Elevation',pol)
# #   plotlistDiffs[[1]]
# #   plotlistDiffs <- lapply(names(rasterListClip), function(x) {
# #     plot_my_stars(rasterListClip[[x]]$surf$diff, x, pol)
# #   })
# #   
# #   # Note that the next trick only works because of library(patchwork)
# #   patchPlot <- function(listOfPlots,maxplots=6) {
# #     
# #     a <- (listOfPlots[[1]] + listOfPlots[[2]]) /
# #       (listOfPlots[[3]] + listOfPlots[[4]]) /
# #       (listOfPlots[[5]] + listOfPlots[[6]]) / 
# #          (listOfPlots[[7]] + listOfPlots[[8]]) / 
# #       (listOfPlots[[9]] + listOfPlots[[10]])
# #     return(a)
# #   }
# #   
# #   aa <- patchPlot(plotlistDiffs)
# #   aa
# # 
# #   diffSurfaces <- lapply(1:nrow(int), function(x) {
# #     maskIntDiff(int[x,])
# #   })
# # 
# # }
# # 
# # plot(diffSurfaces[[1]]$layer.1)
# # require(lidR)
# # 
# 
# ## cross validation
# 
require(reshape2)
# generate CV grids ----

# rasPrep <- cutHole(int,3,
#                    buffer = 50,
#                    buffHole = 10, # how much to increase hole by? (for CV testing)
#                    offsetPol = T) # which site ID to test CV on ?
# 
# 
# 
# cvPers <- seq(0.1,0.9,by=0.1)
# x <- 0.9
# rasIntCV <- lapply(cvPers,
#                    function(x) maskIntDiff(rasPrep, cvSampPer = x))
# 
# names(rasIntCV) <- cvPers
# rmses <- rasIntCV %>% purrr::map_df(function(x) { x %>% map(list('cv','diffHole.rmse')) }) %>%
#   mutate(training_per = cvPers) %>% melt(., id.vars = 'training_per')
# ggplot(rmses) +
#   geom_line(aes(training_per,value,color=variable))
# 
# plot(rmses.df)
# rasInt <- maskIntDiff(rasPrep,0.1) # which site ID to test CV on ?
# 
# cvdevGrid.ras3.5 <- crossValidateSplines(rasPrep$hole$hole.st.elev,
#                                   tensionVals = seq(0.05,0.1,by=0.01),
#                                   smoothVals = 20,
#                                   npminVals = seq(100,160,by=20))
# save(cvdevGrid.ras3.5, file='data/cvdevGrid.ras3.5.RData')
# 
# # repeat for additonal sites, IDs 5, 10, etc.
# rasPrep <- cutHole(int,12) # which site ID to test CV on ?
# cvdevGrid.ras12 <- crossValidate(rasPrep$hole$hole.st.elev)
# save(cvdevGrid.ras12, file='data/cvdevGrid.ras12.RData')
# 
# siteID <- 12
# 
# rasPrep <- cutHole(int,siteID) # which site ID to test CV on ?
# cvdevGrid.ras <- crossValidate(rasPrep$hole$hole.st.elev)
# save(cvdevGrid.ras, file=paste0('data/cvdevGrid.ras',siteID,'.RData'))
# 
# 
# # analyse CV grids ----
# 
# analyseCV <- function(siteID) {
# 
#   for (f in list.files('data',pattern=paste0('ras',siteID),full.names = T)) load(f)
#   cvdevAll <- bind_rows(mget(objects(pattern='cvdevGrid')))
#   bestRun <- cvdevAll[which.min(cvdevAll$rmse),]
#   p <- grid.arrange(
#     ggplot(cvdevAll %>% group_by(tensionVal) %>% slice(which.min(rmse))) +
#     geom_line(aes(x = tensionVal,
#                    y = rmse)),
#     ggplot(cvdevAll %>% group_by(smoothVal) %>% slice(which.min(rmse))) +
#       geom_line(aes(x = smoothVal,
#                      y = rmse)),
#     ggplot(cvdevAll %>% group_by(npminVal) %>% slice(which.min(rmse))) +
#       geom_line(aes(x = npminVal,
#                      y = rmse))
#   )
#   return(list(minRmse = bestRun,
#          plots = p))
# }
# cutHole(int, 3)
# rasPrep <- cutHole(int, 12)
# 
# fit_GSPLINE(rasPrep$hole$hole.r$elev,int[12,])
# site3cv <- analyseCV(3)
# site12cv <- analyseCV(12)
# 
# site3cv$minRmse
# site12cv$minRmse
# 
# 
# site3cv$minRmse
# # save(cvdevGrid, file=paste0(getwd(),'/data/cvdevGrid_default.RData'))
# # load('data/cvdevGrid_default.RData')
# load('data/cvdevGrid.ras10.3.RData')
# cvdevGrid <- cvdevGrid.ras10.3
# 
# #
# ggplot(cvdevGrid) +
#   geom_point(aes(x = smoothVal,
#                  y = er_mean_abs))
# 
# ggplot(cvdevGrid) +
#   geom_point(aes(x = npminVal,
#                  y = er_mean_abs))
# 