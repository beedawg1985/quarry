setwd('/home/barneyharris/projects/quarry')
source(paste0(getwd(),'/rscript/general_functions.R'))

conPostgres()
# import manually categorised data
inc <- st_read('data/includes.gpkg') %>% 
  # filter(interpolate == T) %>% 
  mutate_if(is.factor,as.character)
# buffer selected polygons
inc.buff <- inc %>% st_buffer(250) %>% 
  mutate(a_loc = paste(req,a,sep='_'))
# write buffered polygons
st_write(inc.buff,'data/inc_buff.shp',
         delete_dsn=T)

# manually trace / mark out features for interpolation 
# layer name: 'interpolation'

# import interpolation polygon
conPostgres()
int <- st_read(con,c('quarry','interpolate')) %>% 
  left_join(inc %>% st_drop_geometry(), by=c('include_id'='pkey')) %>% 
  mutate(a_locs = paste0(userDataDir,'/',layer,
                         '/diffs/',req,'_',a,
                         '.tif'),
         b_locs = paste0(userDataDir,'/',layer,
                         '/diffs/',req,'_',b,
                         '.tif'))

# cycle through polygons, cookie cutting and interpolating hole, 
# finally producing difference map

# x <- 1
# 1:nrow(int)
prepData <- lapply(1:5,function(x) {
  pol <- int[x,]
  # load time series rasters from storage, using extent of polygon
  tiles <- loadLidar(pol,offsetPol = T)
  # fold the data into test / training, using polygon and / or random noise
  foldA <- processTiles(tiles$a, # data with which to construction int models
                        cutpoly = tiles$pol, # polygon to exclude data from training
                        sampleRasPer = 0 # percent of data to keep; noise removes remainder
  )
  return(list(pol = pol,
              tiles = tiles,
              foldA = foldA))
})

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
      tensionVals = seq(0.01,3,by=0.2),
      smoothVals = seq(1,50,by=5),
      nminVals = seq(41,140,by=5)
    ))
  )
# pd <- prepData[[1]]
st <- Sys.time()
runInterpolations <- local(function(pd,tag = 'testout',
                        outputDir = '/media/mal/working_files/quarry/',
                        cv = cvGrids) {
  # interpolate test data using a variety of methods
  intA <- interpolateRas(pd$foldA$train,
                         pd$foldA$all,
                         # maskPoly = pd$pol, # if using non-offset poly
                         maskPoly = pd$tiles$pol, # if using offset poly
                         paramData=cv,
                         testCV = F,
                         outputTag = tag)
  
  dat <- compareInt(intRasters=intA,
                foldedRas=pd$foldA,
                compareRas=pd$tiles$b,
                # maskPoly=pd$pol
                maskPoly=pd$tiles$pol
                )
  
  save(dat,
       file=paste0(outputDir,'diffDat_',tag,'_polfid',pd$pol$fid,'.RDS'))
  return(paste0(outputDir,'diffDat_',tag,'_polfid',pd$pol$fid,'.RDS'))
})
require(furrr)

future::plan('multisession',workers=5)
datOut <- furrr::future_map(prepData, 
                         .options=furrr_options(packages=c('stars','raster'),
                                                seed=T),
                         ~runInterpolations(pd = .x, tag='march_offset_nonoise'))
print(Sys.time() - st)



