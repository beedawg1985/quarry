#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if (length(args)>0) setwd(args[1])
source(paste0(getwd(),'/rscript/general_functions.R'))


load('data/intpols.RDS')
# cycle through polygons, cookie cutting and interpolating hole, 
# finally producing difference map

# save(prepData, file='data/prepdata_noise_batch2.RDS')
cvGrids <- 
  list(
    nn = data.frame(expand.grid(
      nmaxVals = seq(1,80,by=2),
      nminVals = seq(1,50,by=2)
    )),
    idw = data.frame(expand.grid(
      nmaxVals = seq(1,80,by=5),
      nminVals = seq(1,50,by=5),
      idpVals = seq(0.1,5,by=0.25)
    )),
    ok = data.frame(expand.grid(
      nmaxVals = seq(1,80,by=5),
      nminVals = seq(1,50,by=5)
    )),
    gspline = data.frame(expand.grid(
      tensionVals = c(seq(0.0001,0.1,by=0.005),seq(0.1,3,by=0.1)),
      smoothVals = seq(1,50,by=5),
      nminVals = seq(11,121,by=25)
    )),
    gbicubic = data.frame(expand.grid(
      stepVals = seq(1,30,by=2),
      lamVals = c(seq(0.0001,0.1,by=0.005),seq(0.1,3,by=0.1))
    ))
  )


# x <- 1
# 1:nrow(int)
# x <- 6
# offset=T
prepareData <- function(x,offset=F,smpper=0.5) {
  print(paste0('row number...',x))
  pol <- int[x,]
  # load time series rasters from storage, using extent of polygon
  tiles <- loadLidar(pol,offsetPol = offset)
  
  # fold the data into test / training, using polygon and / or random noise
  foldA <- processTiles(tiles$a, # data with which to construction int models
                        cutpoly = tiles$pol, # polygon to exclude data from training
                        sampleRasPer = smpper # percent of data to keep; noise removes remainder
  )
  return(list(pol = pol,
              tiles = tiles,
              foldA = foldA))
}

# pd <- prepData[[1]]
runInterpolations <- local(function(pd,tag = 'testout',
                                    outputDir = '/media/mal/working_files/quarry/',
                                    tcv = T,
                                    cvg = cvGrids,
                                    workerToDb,
                                    grassDB = "/home/barneyharris/user_quarry_data/grassdb/quarry/q1") {
  
  pid <- Sys.getpid() # gets pid of thread
  mapSetNum <- workerToDb[workerToDb$worker_id==pid,'grass_id']
  grassDB <- paste0('/home/barneyharris/user_quarry_data/grassdb/quarry/q',mapSetNum)
  
  
  print('interpolating...')
  # interpolate test data using a variety of methods
  intA <- interpolateRas(pd$foldA$train,
                         pd$foldA$all,
                         # maskPoly = pd$pol, # if using non-offset poly
                         maskPoly = pd$tiles$pol, # if using offset poly
                         paramData=cvg,
                         testCV = tcv, 
                         outputTag = tag,
                         gdb=grassDB)
  print('analysing...')
  dat <- compareInt(intRasters=intA,
                    foldedRas=pd$foldA,
                    tiledRas=pd$tiles)
  
  # dat$diff.maps <- NULL
  gc()
  frem <- list.files('raster',pattern=paste0('gspline_int_intfid_',pd$tiles$pol$fid),
                     full.names = T)
  file.remove(frem)
  
  save(dat,
       file=paste0(outputDir,'intdat_',tag,'_polfid',pd$pol$fid,'.RDS'))
  return(paste0(outputDir,'intdat_',tag,'_polfid',pd$pol$fid,'.RDS'))
})

plan("multisession", workers = 2)
pids <- unique(unlist(future.apply::future_lapply(1:10, function(x) Sys.getpid())))
# generate correspondance table so each worker has a GRASS mapset
w2db <- data.frame(worker_id = pids,
                         grass_id = 1:length(pids))
# run number 1 ----
st <- Sys.time()
sessionTag <- 'march8_offset_w50noise'

prepData <- map(c(6,9),          # int polygon row numbers
                       .f=prepareData,
                       offset=T,    # offset the polygon?
                       smpper=0.5  # percentage noise
                       )

datOut <- future_map(prepData, 
                     ~runInterpolations(pd = .x, 
                                        tcv = F, # check tcv = F for full run
                                        workerToDb = w2db,
                                        tag=sessionTag,
                                        cvg = cvGrids),
                     .options=furrr_options(packages=c('stars','raster'),
                                            seed=T),
                     .progress=T)

biOut <- lapply(prepData,interpolateRasBicubic,
                cvg=cvGrids,
                testCV=F, # check testCV = F for full run
                tag=sessionTag)

bindCubic(sessionTag)
print(Sys.time() - st)

# run number 2 ----
plan("multisession", workers = 5)
pids <- unique(unlist(future.apply::future_lapply(1:10, function(x) Sys.getpid())))
# generate correspondance table so each worker has a GRASS mapset
w2db <- data.frame(worker_id = pids,
                   grass_id = 1:length(pids))

st <- Sys.time()
sessionTag <- 'march8_nooffset_w50noise'
prepData <- map(c(6,7,9,11,13),          # int polygon row numbers
                       prepData,
                       offset=F,    # offset the polygon?
                       smpper=0.5,  # percentage noise
                       )

datOut <- future_map(prepData, 
                     ~runInterpolations(pd = .x, 
                                        tcv = F,
                                        workerToDb = w2db,
                                        tag=sessionTag,
                                        cvg = cvGrids),
                     .options=furrr_options(packages=c('stars','raster'),
                                            seed=T),
                     .progress=T)

biOut <- lapply(prepData,interpolateRasBicubic,
                cvg=cvGrids,
                testCV=F,
                tag=sessionTag)

bindCubic(sessionTag)
print(Sys.time() - st)

# run number 3 ----
st <- Sys.time()
sessionTag <- 'march8_offset_nonoise'
prepData <- map(c(6,7,9,11,13),          # int polygon row numbers
                       prepData,
                       offset=T,    # offset the polygon?
                       smpper=0,  # percentage noise
                       )

datOut <- future_map(prepData, 
                     ~runInterpolations(pd = .x, 
                                        tcv = F,
                                        workerToDb = w2db,
                                        tag=sessionTag,
                                        cvg = cvGrids),
                     .options=furrr_options(packages=c('stars','raster'),
                                            seed=T),
                     .progress=T)

biOut <- lapply(prepData,interpolateRasBicubic,
                cvg=cvGrids,
                testCV=F,
                tag=sessionTag)

bindCubic(sessionTag)
print(Sys.time() - st)


# prepData <- map(1:5,          # int polygon row numbers
#                 .f=prepareData,
#                 offset=T,    # offset the polygon?
#                 smpper=0.5  # percentage noise
# )
