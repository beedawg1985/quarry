require(RSelenium)
# require(pdftools)
require(mapview)
require(stringr)
require(stars)
require(sf)
require(leaflet)
require(dplyr)
require(gdalUtils)
require(raster)

conPostgres <- function() {
  
  # install.packages("RPostgreSQL")
  require("RPostgreSQL")
  
  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  pw <- {
    'Only/Pass/Week/Track/89!!'
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <<- dbConnect(drv, dbname = 'mal',
                    host = "localhost", port = 5432,
                    user = 'barneyharris', password = pw)
  rm(pw) # removes the password
}

st_write_withPkey <- function(con,sf,schema='',tname,pkeycol='pkey') {
  # sf <- iw
  st_write(sf,con,c(schema,tname))
  if (schema != '') {
    schema <- paste0(schema,'.')
  }
  dbSendQuery(con,paste0('ALTER TABLE ',schema,tname,
                         ' ADD COLUMN pkey SERIAL PRIMARY KEY;'))
}

clearTempTables <- function(con) {
  lapply(dbListTables(con)[str_detect(dbListTables(con),'_temp$')],
         dbRemoveTable, conn=con)
}

# useful print paste function
pp <- function(...) {
  print(paste0(list(...),collapse=''))
}
'%!in%' <- function(x,y)!('%in%'(x,y))


userDataDir <<- '/home/barneyharris/user_quarry_data'
setwd("/home/barneyharris/projects/quarry")

# import OS 5k grid polygons for England
grid5k <- st_read('data/osgb/OSGB_Grid_5km.shp') %>% 
  st_transform(4326) %>% dplyr::filter(ENGLAND == 't')
grid50k <- st_read('data/osgb/OSGB_Grid_50km.shp')
# get grid letters
osgbLetters <- 
  unique(unlist(lapply(grid5k$TILE_NAME, substr,1,2)))

# import directory of mines and quarries
load(file='osgbLetters.RData')
load(file='data/engQuarries.RData')
# britPit <- pdftools::pdf_data('DirectoryOfMinesAndQuarries2014.pdf')
# extract pages with addresses
# mineAdress <- britPit[32:150]
# 
# # function for extracting grid references from address
# getGridRefs <- function(mineAdressPage) {
#   tileRefs <- which(mineAdressPage$text %in% osgbLetters)
#   unlist(lapply(tileRefs, function(x) {
#     paste(mineAdressPage$text[x:(x+2)],collapse='')
#   }))
# }
# 
# # run grid ref extraction function
# gridRefs <- unlist(lapply(mineAdress,getGridRefs))
# # validate grid refs, remove erros
# gridRefsVal <- gridRefs[str_detect(gridRefs,
#            '^([STNHOstnho][A-Za-z]\\s?)(\\d{5}\\s?\\d{5}|\\d{4}\\s?\\d{4}|\\d{3}\\s?\\d{3}|\\d{2}\\s?\\d{2}|\\d{1}\\s?\\d{1})$')]
# # write to text file
# writeLines(gridRefsVal, 'gridRefs.txt')
# # convert grid refs to easting northings
# quarryCoor <- osg_parse(gridRefsVal, coord_system = 'BNG')
# # build sf data frame of points
# coorDf <- data.frame(x = quarryCoor$easting,
#                      y = quarryCoor$northing,
#                      grid_ref = gridRefsVal)
# coorSf <- st_as_sf(coorDf,
#               coords = c('x','y'),
#               crs = st_crs(27700))
# # select only those quarries in england
# engQuarries <- st_join(coorSf,st_transform(grid5k,27700),
#                        left=F)
# leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
#   addCircleMarkers(data=st_transform(engQuarries,4326))
# # save(engQuarries,file='data/engQuarries.RData')
# load('data/engQuarries.RData')

# create square buffers around quarry locations,
# dissolve any overlapping and calc areas
qBuff <- engQuarries %>% 
  st_buffer(600, endCapStyle="SQUARE") %>% 
  st_simplify(500, preserveTopology=T) %>% 
  st_join(grid50k, left = F) %>% 
  mutate(tile_name_char = as.character(TILE_NAME.x),
         tile50k_name_char = as.character(TILE_NAME.y))

qBuff.sum <- qBuff %>% 
  group_by(tile50k_name_char) %>%
  summarise(geometry = st_union(geometry),
            all_n = sum(n())) %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  arrange(tile50k_name_char)
  
qGrps <- split.data.frame(qBuff.sum, 
                               qBuff.sum$tile50k_name_char)

# bufferedPoly <- qGrps$SUSW
# bufferedPoly <- qGrps$STNE
# max request limit = 625455396.147
bufferedPoly <- qGrps$SPSE
getLidar2 <- function(bufferedPoly,overwrite=T) {
  # define chrome options
  eCaps <- list(chromeOptions = list(
    args = c('--disable-gpu', 
             '--headless',
             '--window-size=1280,800')
  ))
  rD <- rsDriver(browser = "chrome",
                 chromever = "81.0.4044.138",
                 extraCapabilities = eCaps,
                 port = 
                   as.integer(base::sample(seq(32768,65535, by=1),1)))
  remDr <- rD[["client"]]
  
  browseQuery <- function(remDr,bufferedPoly) {
    bufferedPoly <- st_union(bufferedPoly) %>% st_sf
    st_write(bufferedPoly, dsn=paste0('data/temp.shp'),
             delete_dsn=T)
    simplePoly <- paste0(getwd(),'/data/temp.shp')
    simplePolyZip <- paste0(getwd(),'/data/temp.zip')
    simplePolyFiles <- paste0(getwd(),'/data/temp*')
    # zip shapefiles into archive
    system(paste0('zip ',simplePolyZip,' ',simplePolyFiles))
    
    # navigate to the EA lidar portal
    remDr$navigate("https://environment.data.gov.uk/DefraDataDownload/?Mode=survey")
    # upload zipped shape file
    suppressMessages({
      try(remDr$findElement("id", "fileid"),TRUE)
    })
    while (remDr$status == 7) {
      Sys.sleep(2)
      print('waiting for portal to load...')
      suppressMessages({
        try(remDr$findElement("id", "fileid"),silent=TRUE)
      })
    }
    webElem <- remDr$findElement("id", "fileid")
    webElem$sendKeysToElement(list(simplePolyZip))
    print('uploading shape file...')
    # wait for upload to complete (2 seconds)
    Sys.sleep(5)
    # find 'Get Tiles' button
    getTiles <- remDr$findElement(using = 'css selector', ".grid-item-container")
    # click 'Get Tiles' button
    getTiles$clickElement()
    # sys.sleep ?
    suppressMessages({
      try(remDr$findElement(using = 'css selector', '.data-ready-container'),TRUE)
    })
    
    i <- 0
    while (remDr$status == 7) {
      Sys.sleep(5)
      print(paste0('waiting for tiles to be returned...',
                   round(Sys.time() - startTime,2),
                   ' seconds elapsed'))
      suppressMessages({
        try(remDr$findElement(using = 'css selector', '.data-ready-container'),TRUE)
      })
      i <- i + 1
      if (i > 12) {
        print('error with shape file...')
        return('shapefile error')
      }
    }
    print('tiles returned!')
  } # / browseQuery
  browseResult <- browseQuery(remDr,bufferedPoly)
  if (browseResult == 'shapefile error') {
    print(browseResult)
    return(browseResult)
  }
  print('searching available tiles...')
  # select products DTMs
  desiredProds <- c("LIDAR Tiles DTM")
  prodElem <- remDr$findElement(using = 'css selector', '#productSelect')
  prodList <- unique(prodElem$selectTag()$text)
  prodsIndex <- which(prodList %in% desiredProds)
  
  xP <- paste0('//*[@id="productSelect"]/option[',prodsIndex,']')
  webElem <- remDr$findElement(using = 'xpath', 
                               value = xP)
  webElem$clickElement()
  # check which year available
  yrElem <- remDr$findElement(using = 'css selector', '#yearSelect')
  yrList <- unique(yrElem$selectTag()$text)
  
  # cycle through years, selecting 1m res and recording tiles names
  # x <- 1
  tileList <- lapply(1:length(yrList), function(x) {
    yr <- yrList[x]
    xP <- paste0('//*[@id="yearSelect"]/option[',x,']')
    webElem <- remDr$findElement(using = 'xpath', 
                                 value = xP)
    webElem$clickElement()
    # now cycle through res
    resElem <- remDr$findElement(using = 'css selector', '#resolutionSelect')
    resVec <- unique(resElem$selectTag()$text)
    # pick only 1m
    if (length(which(resVec == 'DTM 1M')) == 0) {
      return(NULL) } else { r <- which(resVec == 'DTM 1M') }

    resElem$clickElement() # open drop down
    xP <- paste0('//*[@id="resolutionSelect"]/option[',r,']')
    webElem <- remDr$findElement(using = 'xpath', 
                                 value = xP)
    webElem$clickElement() # select 1m res
    tileLinks <- remDr$findElement(using = 'css selector', '.data-ready-container')
    tileLinks.a <- tileLinks$findChildElements('tag', 'a')
    tiles <- unlist(lapply(tileLinks.a, function(x) x$getElementAttribute('href')))
    
    return(tiles)
  })
  # name list by years
  names(tileList) <- yrList
  # remove nulls (years with no 1m res)
  tileList[unlist(lapply(tileList,is.null))] <- NULL
  
  # extract tile names from download URLs
  # x <- names(tileList)[1]
  tileNames <- lapply(names(tileList), function(x) {
    unlist(lapply(str_split(tileList[[x]], 
                            paste0(x,'-')),function(y) substr(y[2],1,6)))
  })
  names(tileNames) <- names(tileList)
  # convert data to tile names with lists of years
  tilesYears <- lapply(unique(unlist(tileNames)), function(tile) {
    allYears <- lapply(names(tileNames), function(year) {
      if (tile %in% tileNames[[year]]) year
    })
    allYears[unlist(lapply(allYears,is.null))] <- NULL
    return(unlist(allYears))
  })
  names(tilesYears) <- unique(unlist(tileNames))
  
  # minimun number of years survey 3, remove the rest
  minSurvey <- 2
  tilesYears[unlist(lapply(tilesYears, function(x) length(x) < minSurvey))] <- NULL
  if (length(tilesYears) == 0) {
    er <- 'no tiles with sequential surveys found...'
    print(er)
    return(er)
  }
  allLinks <- as.character(unlist(tileList))
  dlLinks <- allLinks[str_detect(allLinks,paste(names(tilesYears),collapse = '|'))]
  
  # output URLs as list for Wget
  fileName <- paste0(bufferedPoly$tile50k_name_char,'_list.txt')
  write.table(dlLinks,
              file=paste0('wget/',fileName),
              quote = F,row.names=F,col.names = F)
  print(paste0('written download list to ... wget/',fileName))
  
  # close selenium
  remDr$close()
  rD$server$stop()
  gc()
  
  # create folder structure
  folderPath <- paste0(userDataDir,'/',bufferedPoly$tile50k_name_char)
  if (!dir.exists(folderPath)) {
    dir.create(folderPath)
    lapply(unique(unlist(tilesYears)),function(x) dir.create(paste0(folderPath,'/',x)))
  }
  
  # overwrite=T
  if (overwrite) {
    system(paste0('rm ',folderPath,' -R'))
    dir.create(folderPath)
    lapply(unique(unlist(tilesYears)),function(x) dir.create(paste0(folderPath,'/',x)))
  }
  
  # download and uncompress EA lidar with magic!
  # x <- 1
  system(paste0('cat ',getwd(),'/',paste0('wget/',fileName),' | parallel --gnu ',
                shQuote(paste0('wget {} -P ',folderPath))))
  
  # extract to yearly folders 
  yrs <- unique(unlist(tilesYears))
  # x <- yrs[7]
  lapply(yrs, function(x) {
    zips <- paste0(folderPath,'/',
                   list.files(folderPath)[grep(paste0("*(",x,").*zip$"),list.files(folderPath))])
    if (length(zips) > 1) { 
      lapply(zips, function(y) {
        system(paste0("unzip -n ",y,
                    ' -d ',folderPath,'/',x,'/'))
        })
      }
  })
  
  return(folderPath)
}
# bufferedPoly <- qGrps$STSW
# folderPath <- fp
# folderName <- 'SPSE'
processLidar <- function(qGrps,folderName,removeDls=T) {
  print(paste0('processing folder...',folderName))
  folderPath <- paste0(userDataDir,'/',folderName)
  bufferedPoly <- qGrps[[basename(folderPath)]]
  # check which raster tiles overlap with one another...
  # list all files
  f <- list.files(folderPath,pattern='*.tif$',
                  full.names = T,
                  recursive = T)
  # remove any tifs from diffs path
  f <- f[!str_detect(f,'diffs')]
  # create index 
  indexLoc <- paste0(folderPath,'/',basename(folderPath),'_tindex.shp')
  if (file.exists(indexLoc)) {
    system(paste0('rm ',str_replace(indexLoc,'.shp','*'))) }
  suppressWarnings({gdaltindex(indexLoc,
                               f)})
  # build tile index of rasters, group by geometry and
  # discard any tiles which do not overlap more than
  # minimum survey value
  reqSng <- bufferedPoly %>% st_cast('POLYGON') %>% 
    mutate(req_id = 1:nrow(.))
  
  tIndex <- st_read(indexLoc,
                    crs=27700,quiet = T,
                    stringsAsFactors = F) %>% 
    mutate(tile_id = 1:nrow(.),
           wkt = st_as_text(geometry)) %>% 
    group_by(wkt) %>% 
    add_tally() %>% 
    dplyr::filter(n >= minSurvey) %>% 
    mutate(year = basename(dirname(location))) %>% 
    st_join(reqSng, left = F) # of the remaining polygons, keep 
    # only those which intersect with with quarries request polygon
  
  if (nrow(tIndex) == 0) return('no rasters intersecting with quarries...')
  
  # generate VRT lists, grouping rasters first by request polygon ID,
  # then year of survey
  toVrt <- tIndex %>% group_by(req_id,year) %>% 
    summarise(locs = list(location)) %>% 
    mutate(year = as.numeric(year))
  # create dir for vrts
  if (!dir.exists(paste0(folderPath,'/vrts'))) {
    dir.create(paste0(folderPath,'/vrts'))
  }
  # generate vrts
  x <- 1
  st <- lapply(1:nrow(toVrt), function(x) {
    vrt <- paste0(folderPath,'/vrts/',
                  'req',toVrt[x,]$req_id,'_',
                  toVrt[x,]$year,'.vrt')
    print(vrt)
    gdalbuildvrt(unlist(toVrt[x,]$locs),
                 output.vrt = vrt
                 # ,tr=c(1,1)
                 ,a_srs=st_crs(bufferedPoly,parameters=T)$proj4string
                 )
    r <- raster(vrt)
    crs(r) <- st_crs(bufferedPoly,parameters=T)$proj4string
    return(r)
    })
  
  names(st) <- paste0('req',toVrt$req_id,'_',
                      toVrt$year)
  
  # now subtract earliest survey from latest survey
  if (!dir.exists(paste0(folderPath,'/diffs'))) {
    dir.create(paste0(folderPath,'/diffs'))
  }
  
  co <- c('TILED=YES','COMPRESS=DEFLATE')
  print('generating difference rasters...')
  x <- 12
  diffsResult <- lapply(unique(toVrt$req_id), function(x) {
    print(paste0('processing request poly... ',x))
    df <- toVrt %>% as.data.frame() %>% 
      filter(req_id == x)
    maxYear <- paste0('req',x,'_',max(df$year))
    minYear <- paste0('req',x,'_',min(df$year))
    diffRas <- tryCatch({st[[maxYear]] - st[[minYear]]},
             error = function(x) { return('error in rasters..different res?') })
    if (!is.Raster(diffRas)) return(diffRas)
    
    if (!is.na(raster::maxValue(diffRas)) && raster::maxValue(diffRas) > 2) {
      diffOut <- paste0(folderPath,'/diffs/',
                        'req',x,'_',max(df$year),
                        '_',min(df$year),'_diff.tif')
      writeRaster(diffRas, diffOut, options=co, overwrite=T)
      maxOut <- paste0(folderPath,'/diffs/',
                     'req',x,'_',max(df$year),'.tif')
      minOut <- paste0(folderPath,'/diffs/',
                     'req',x,'_',min(df$year),'.tif')
      writeRaster(st[[maxYear]], maxOut, options=co, overwrite=T)
      writeRaster(st[[minYear]], minOut, options=co, overwrite=T)
      gdaladdo(diffOut, levels=c('8','16','32','64','128'))
      gdaladdo(maxOut, levels=c('8','16','32','64','128'))
      gdaladdo(minOut, levels=c('8','16','32','64','128'))
      return('succesfully produced differences rasters')
    } else return('no differences over 2 metres')
  })
  
  # remove original
  if (removeDls) {
    system(paste0('rm ',folderPath,'/*.zip'))
    yearDirs <- setdiff(list.dirs(folderPath),folderPath)
    system(paste0('rm ',paste(yearDirs[!str_detect(yearDirs,'diffs')],
                              collapse=' '),' -R'))
  }
  # plot(r)
  return(diffsResult)
}

# fname <- basename(list.dirs(userDataDir,recursive = F))[2]
fname <- 'NXSE'

minDiff <- 0.5
processDiffs <- function(fname,minDiff=0.5,
                         minArea=500,
                         maxArea=10000,
                         refresh=F) {
  pp('processing folder...',fname)
  folderPath <- paste0(userDataDir,'/',fname)
  diffPath <- paste0(folderPath,'/diffs')
  # load rasters
  diffs <- lapply(list.files(diffPath,full.names = T,pattern='*diff.tif'),raster)
  if (length(diffs) == 0) return('no difference rasters in folder..')
  # calc function to remove cells less than n difference
  pp('processing difference rasters with minimum difference of... ',minDiff)
  # rCalc <- function(x) { x[dplyr::between(x,-minDiff,minDiff)] <- NA; return(x) }
  rCalc <- function(x) { x[x < minDiff] <- NA; return(x) }
  diffsCalc <- lapply(diffs, calc, rCalc)
  
  # sieve rasters, removing clumps smaller than n
  # x <- diffsCalc[[1]]
  pp('sieving rasters to leave only areas larger than ',minArea,' sq. m')
  pp('sieving rasters to leave only areas smaller than ',maxArea,' sq. m')
  diffsSieve <- lapply(diffsCalc, function(x) {
    clump <- raster::clump(x)
    clumpDf <- as.data.frame(freq(clump))
    # put these into a vector of clump ID's to be removed
    excludeID <- clumpDf$value[which(clumpDf$count < minArea)]
    excludeID <- c(excludeID,clumpDf$value[which(clumpDf$count > maxArea)])
    clump.sieve <- clump
    clump.sieve[clump %in% excludeID] <- NA
    # plot(clump.sieve)
    return(clump.sieve)
  })
  
  # vectorize and add to gpkg
  pp('adding polygons to vector layer...')
  pols <- do.call(rbind,lapply(1:length(diffsSieve), function(x) {
    pol <- st_simplify(st_as_sf(st_as_stars(diffsSieve[[x]], crs=st_crs(27700)),
                                as_points = F, merge=T,10,
                                crs=27700)) %>% 
      mutate(ras = basename(diffs[[x]]@file@name) )
    st_crs(pol) <- st_crs(27700)
    return(pol)
    }))
  
  if (nrow(pols) > 0) {
  st_write(pols,paste0('data/aoi_poly_wfiles.gpkg'),
                      layer=fname,
           append = F)
  }
  
  # require(ggplot2)
  # a <- as.data.frame(diffs[[1]],xy=T)
  # b <- as.data.frame(diffsSieve[[1]],xy=T)
  # plot(diffsSieve[[1]])
  # ggplot() +
  #   # geom_raster(data = a, aes(x = x, y = y, fill=layer)) + 
  #   geom_raster(data = b, aes(x = x, y = y, fill=clumps)) + 
  #   coord_quickmap()
  
}


# run funcs ----
toDo <- setdiff(names(qGrps),names(liPaths))
# g <- 'SWSE'
for (g in names(qGrps[toDo]) ) {
  liPaths[[g]] <- getLidar2(qGrps[[g]])
}

# proResults <- list()
liDirs <- basename(list.dirs(userDataDir,recursive = F))
toProc <- setdiff(liDirs,names(proResults))
for (d in toProc) {
  proResults[[d]] <- processLidar(qGrps, d, removeDls = F)
}

allDiffDir <- list.files(userDataDir,recursive = T,pattern='*diffs$',
           include.dirs = T, full.names = T)
# non-empty directories
allDiffDir <- setdiff(basename(dirname(allDiffDir[which(lengths(lapply(allDiffDir, list.files)) > 0)])),basename(userDataDir))
# toVec <- allDiffDir
toVec <- setdiff(allDiffDir[2:length(allDiffDir)],st_layers('data/aoi_poly.gpkg')$name)
for (r in toVec) {
  processDiffs(r)
}

# write AOI poly to DB
l <- st_layers('data/aoi_poly_wfiles.gpkg')$name

aoi <- lapply(l, function(x) {
  g <- st_read('data/aoi_poly_wfiles.gpkg',layer=x,crs=27700)
  if (nrow(g) > 0) g %>% st_sf
        }) %>% bind_rows()

uk <- st_read('data/ne_10m_coast_uk.shp') %>% 
  st_transform(27700) %>% 
  st_cast('POLYGON')

aoi.filt <- aoi %>%
  # filter(st_area(geometry) > units::as_units(200, "m^2")) %>% 
  # filter(st_area(geometry) < units::as_units(5000, "m^2")) %>% 
  st_within(uk) %>% 
  # dplyr::select(geometry) %>% 
  mutate(include = NA)
plot(aoi.filt$geometry)
st_write(aoi.filt, 'data/aoi_filt.shp')
st_write_withPkey(con,aoi,schema='quarry','aoi_all')
st_write_withPkey(con,aoi.filt,schema='quarry','aoi_300_5000')
st_write_withPkey(con,aoi.filt,schema='quarry','aoi')
# st_write(aoi.filt,con,c('quarry','aoi'))


allDiffs <- list.files(allDiffDir,full.names = T,pattern='*diff.tif')
mergedDiffDir <- paste0(userDataDir,'/diffs')
file.copy(allDiffs, 
          paste0(mergedDiffDir,'/',basename(allDiffs)))
diffs <- list.files(mergedDiffDir,full.names = T,
                    pattern='*tif')
lapply(diffs, function(x) {
  gdal_translate(src_dataset = x,
          dst_dataset =str_replace(x,'.tif','_rep.tif'),
           a_srs='http://spatialreference.org/ref/epsg/27700/',
          overwrite=T)
})
repDiffs <- list.files(paste0(mergedDiffDir,'/rep'),full.names = T,
                    pattern='*tif')
lapply(repDiffs,gdaladdo,levels=c(2,4,8,16,32,64))

# create / update mosaics in GeoServer
x <- paste0(mergedDiffDir,'/rep')

# delete pre-existing mosaic index files
f <- list.files(x,full.names = T)
fRm <- f[!str_detect(f,'tif')]
lapply(fRm,function(x) try(system(paste0('rm -f ', x))))
# add index properties file
inProp <- c('PropertyCollectors=ResolutionExtractorSPI(resolution)',
            'Schema=*the_geom:Polygon,location:String,resolution:String',
            'Caching=false',
            'AbsolutePath=false')
write.table(inProp,
            file=paste0(x,'/indexer.properties'),
            quote = F,
            row.names=F,col.names = F)
# add new mosaic
system2("curl",
        paste0('-v -u ',
               shQuote('admin:geoserver'),
               ' -XPUT -H "Content-type: text/plain" ',
               '-d ',paste0('"file://',x,'"'),' ',
               '"http://localhost:8080/geoserver/rest/workspaces/quarry/coveragestores/',
               basename(x),'/external.imagemosaic"'))

# edit GeoServer coverage file to set resolution sorting
configFolder <- paste0('/usr/share/geoserver/data_dir/workspaces/quarry/',basename(x),
                       '/')
sudoPass <- 'Only/Pass/Week/Track/89!!'
# set permissions
system(paste0('sudo -kS setfacl -R --set-file=/usr/share/geoserver/data_dir/workspaces/mal/permissions.acl ',
              configFolder)
       , input = sudoPass
       )

# restart geoserver
system("sudo -kS service geoserver stop", input = sudoPass)
system("sudo -kS service geoserver start", input = sudoPass)
# 
# plot(coorSf)
# st_write(quar, dsn='quarries.shp')
# leaflet() %>% 
#   leaflet::addProviderTiles("Esri.WorldImagery") %>% 
#   addPolygons(data = st_transform(pointDfs$`23`,4326))
  
  

