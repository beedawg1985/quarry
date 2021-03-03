
require(ggforce)
require(reshape2)
require(OGC)
require(RSelenium)
require(e1071)
require(caret)
require(randomForest)
# require(pdftools)
require(mapview)
require(stringr)
require(stars)
require(sf)
require(leaflet)
require(dplyr)
require(gdalUtils)
require(raster)
require(RSAGA)
require(Rsagacmd)
require(automap)
library(fields) # Thin Plate Spline
library(interp) # Triangulation
library(mgcv)   # Spatial GAM
library(patchwork)
library(viridis)
require(ggplot2)
require(purrr)
require(furrr)
require(gridExtra)
require(doParallel)
require(future.apply)


userDataDir <<- '/home/barneyharris/user_quarry_data'
gdb <- paste0(userDataDir,'/grassdb/quarry/PERMANENT/')
setwd("/home/barneyharris/projects/quarry")
par(mfrow=c(1,1), mar=c(1.1, 1.1, 1.1, 1.1)) 

conPostgres <- function() {
  
  # install.packages("RPostgreSQL")
  require("RPostgreSQL")
  
  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  load('private/pw.RData')
  
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

genSections <- function(con,uidNum,secLength,
                        secDensity=sectionDensity,
                        xDensity=1) {
  print('generating section lines...')
  # original earthwork geometry lines
  ewLinesWgs <- st_read(con, c('rs','earthwork_geometry')) %>% 
    filter(uid == uidNum) %>% st_transform(4326)
  ewLines <- st_read(con, c('rs','earthwork_geometry')) %>% 
    filter(uid == uidNum) %>% st_cast('LINESTRING')
  ewBuff <- st_read(con, c('rs','earthwork_geometry')) %>% 
    filter(uid == uidNum) %>% st_buffer(secLength) %>% 
    st_union()
  # export for raster processing
  ewBuff %>% 
    st_write('/srv/mal_data/rmal/shapefiles/ewbuff_temp.gpkg',
             delete_dsn=T)
  
  leaflet() %>% addTiles() %>% 
    addPolylines(data=st_transform(ewLines,4326))
  # convert lines to points, numbered in sequence
  ewPoints <- st_line_sample(ewLines, density=secDensity,
                             type='regular') %>%
    st_as_sf() %>% st_cast('POINT') %>% mutate(cat = row_number()) %>% 
    arrange_at('cat') %>% st_transform(4326)
  
  # calculate bearings and difference between bearings between points
  ewPointsSp <- ewPoints %>% as_Spatial()
  ewBearings <- bearing(ewPointsSp)
  ewPoints$bearings <- ewBearings
  ewPoints$diff <- diff(c(ewPoints$bearings,NA),lag=1)
  # create directionless bearing for purposes of comparison with objects
  ewPoints$pos_bearing <- (ewPoints$bearings + 180) %% 180
  
  ewPointsOsgb <- st_as_sf(ewPoints) %>% st_transform(27700)
  # generate section lines
  s1 <- as.data.frame(destPoint(ewPointsSp, ewBearings-90, 
                                (secLength/2) ))
  s1Sf <- st_as_sf(s1[complete.cases(s1),],
                   coords=c('lon','lat'), crs=4326) %>% 
    mutate(id = row_number(),
           s = 's1')
  
  st_write(ewPoints, con, 'secpoints_temp')
  print('written section points to db as... secpoints_temp')
  # st_write(s1Sf, con, 'ewpoints_s1_temp')
  s2 <- as.data.frame(destPoint(ewPointsSp, ewBearings+90, 
                                (secLength/2) ))
  s2Sf <- st_as_sf(s2[complete.cases(s2),],
                   coords=c('lon','lat'),
                   crs=4326) %>% 
    mutate(id = row_number(),
           s = 's2')
  
  sectionLines <- rbind(s1Sf,s2Sf) %>% group_by(id) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("LINESTRING")
  
  # convert section lines into SP points
  sectionLinesGB <- st_transform(sectionLines, 27700)
  st_write(sectionLinesGB, con, 'seclines_temp')
  print('written section lines to db as... seclines_temp')
  
  sectionP <- st_line_sample(sectionLinesGB, density=xDensity)
  # st_write(sectionP, con, 'sectionpoints_temp')
  sectionPoints <- lapply(sectionP, function(x) {
    co <- st_coordinates(x)
    sp::SpatialPoints(co,CRS(st_crs(27700)$proj4string))
  })
  
  allPoints <- do.call(rbind,sectionPoints)
  
  
  out <- list(lines = sectionLines,
              points = sectionPoints,
              pointsDf = allPoints,
              sf_points = sectionP,
              ew_points = ewPointsOsgb,
              ew_buff = ewBuff)
  return(out)
}

# functions for 1_process_quarry_locations ----

# bufferedPoly <- qGrps$SUSW
# bufferedPoly <- qGrps$STNE
# max request limit = 625455396.147
# x <- 1
# bufferedPoly <- int[x,]
# whichProd <- "LIDAR Point Cloud"
# whichYears <- c(int[x,]$a,int[x,]$b)
# bufferedPoly <- sfFilt
getLidar2 <- function(bufferedPoly,
                      whichProd="LIDAR Tiles DTM",
                      whichYears,
                      minSurvey = 2,
                      userDataDirRoot='tmp',
                      overwrite=T) {
  
  if (is.null(bufferedPoly$tile50k_name_char)) {
    bufferedPoly$tile50k_name_char <- bufferedPoly$layer
  }
  
  # define chrome options
  eCaps <- list(chromeOptions = list(
    args = c(
      '--disable-gpu'
      ,'--headless',
      '--window-size=1280,800'
    )
  ))
  rD <- rsDriver(browser = "chrome",
                 chromever = "81.0.4044.138",
                 extraCapabilities = eCaps,
                 port =
                   as.integer(base::sample(seq(32768,65535, by=1),1)))
  # rD <- RSelenium::rsDriver(
  #   browser = "firefox",
  #   extraCapabilities = list(
  #     "moz:firefoxOptions" = list(
  #       args = list('--headless')
  #     )
  #   ),
  #   port = 
  #     as.integer(base::sample(seq(32768,65535, by=1),1))
  # )
  
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
      print(paste0('waiting for tiles to be returned...'))
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
  l <- leaflet() %>% addTiles() %>% 
    addPolygons(data=st_transform(bufferedPoly,4326))
  print(l)
  print('searching available tiles...')
  # select products DTMs
  desiredProds <- whichProd
  # desiredProds <- "LIDAR Tiles DTM"
  # desiredProds <- "LIDAR Point Cloud"
  prodElem <- remDr$findElement(using = 'css selector', '#productSelect')
  prodList <- unique(prodElem$selectTag()$text)
  prodsIndex <- which(prodList %in% desiredProds)
  
  xP <- paste0('//*[@id="productSelect"]/option[',prodsIndex,']')
  webElem <- remDr$findElement(using = 'xpath', 
                               value = xP)
  webElem$clickElement()
  webElem$getElementText()
  # check which year available
  yrElem <- remDr$findElement(using = 'css selector', '#yearSelect')
  yrList <- unique(yrElem$selectTag()$text)
  
  if (desiredProds == "LIDAR Tiles DTM") { 
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
    
  }
  
  if (desiredProds == "LIDAR Point Cloud") {
    # yr <- "2011"
    tileList <- lapply(whichYears, function(yr) {
      x <- which(yrList==yr)
      if (length(x) == 0) {
        print(paste0('year ',yr,' not available as LAZ'))
        return(NULL)
      } 
      xP <- paste0('//*[@id="yearSelect"]/option[',x,']')
      webElem <- remDr$findElement(using = 'xpath', 
                                   value = xP)
      webElem$clickElement()
      
      tileLinks <- remDr$findElement(using = 'css selector', '.data-ready-container')
      tileLinks.a <- tileLinks$findChildElements('tag', 'a')
      tiles <- unlist(lapply(tileLinks.a, function(x) x$getElementAttribute('href')))
      
      return(tiles)
    })
    
    # name list by years
    names(tileList) <- whichYears
    tileList[unlist(lapply(tileList, is.null))] <- NULL
  }
  
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
  if (minSurvey > 0) {
    tilesYears[unlist(lapply(tilesYears, function(x) length(x) < minSurvey))] <- NULL
    if (length(tilesYears) == 0) {
      er <- 'no tiles with sequential surveys found...'
      print(er)
      return(er)
    }
  }
  
  allLinks <- as.character(unlist(tileList))
  dlLinks <- allLinks[str_detect(allLinks,paste(names(tilesYears),collapse = '|'))]
  
  # output URLs as list for Wget
  fileName <- paste0(unique(bufferedPoly$tile50k_name_char,'_list.txt'))
  
  write.table(dlLinks,
              file=paste0('wget/',fileName),
              quote = F,row.names=F,col.names = F)
  print(paste0('written download list to ... wget/',fileName))
  
  # close selenium
  remDr$close()
  rD$server$stop()
  gc()
  
  # create folder structure
  folderPath <- paste0(userDataDir,'/',userDataDirRoot,'/',unique(bufferedPoly$tile50k_name_char))
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
  # x <- yrs[2]
  lapply(yrs, function(x) {
    zips <- paste0(folderPath,'/',
                   list.files(folderPath)[grep(paste0("*(",x,").*zip$"),list.files(folderPath))])
    if (length(zips) > 0) { 
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
# folderName <- 'SONE'
# bufferedPoly <- int[x,]
# whichProd <- "LIDAR Point Cloud"
# whichYears <- c(int[x,]$a,int[x,]$b)
processLidar <- function(polyGrps=qGrps,folderName,removeDls=T) {
  print(paste0('processing folder...',folderName))
  folderPath <- paste0(userDataDir,'/',folderName)
  bufferedPoly <- polyGrps[[basename(folderPath)]]
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


# sfPoly <- int[1,]
processLaz <- function(sfPoly) {
  require(lidR)
  print(paste0('processing folder...',sfPoly$layer))
  folderPath <- paste0(userDataDir,'/laz/',sfPoly$layer)
  lazDirs <- list.dirs(list.dirs(folderPath,
                                 recursive = F,
                                 full.names =T),
                       recursive = F,
                       full.names = T)
  
  lapply(lazDirs, function(x) {
    lazPath <- paste0(x,'/')
    ctg <- readLAScatalog(lazPath)
    las_check(ctg)
    sfBuff <- sfPoly %>% st_buffer(50)
    sfDiff <- st_difference(sfBuff,sfPoly)
    
    processPoly <- function(sf, tag) {
      st_write(sf,paste0('data/lidar_poly_',tag,'.shp'),
               delete_dsn=T)
      ctgRoi <- shapefile(paste0('data/lidar_poly_',tag,'.shp'))
      roi <- clip_roi(ctg, ctgRoi)
      
      ras <- grid_terrain(roi, algorithm = tin())
      crs(ras) <- st_crs(27700,parameters=T)$proj4string
      outRoot <- str_replace_all(str_replace(lazPath,paste0(userDataDir,'/laz/'),''),
                                 '/','_')
      outName <- paste0(userDataDir,'/int/',outRoot,sfPoly$req,
                        '_',tag)
      writeRaster(ras,filename = paste0(outName,'.tif'),
                  overwrite=T)
      rasHs <- hillShade(terrain(ras,opt="slope"),
                         terrain(rasDiff,opt="aspect"),
                         filename=paste0(outName,'_hs.tif'),
                         overwrite=T)
    }
    
    try(processPoly(sfBuff,'buff'))
    try(processPoly(sfDiff,'diff'))
    
  })
  
}

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

# functions for 2_interpolation and analyses  ----

# buffFid <- 5
generateCoVars <- function(buffFid) {
  
  demFolder <- file.path(userDataDir,'edina','terrain-5-dtm_3791534')
  intBuff <- st_read('data/interpolation_polygons_500m_buff.kml') %>% 
    st_transform(27700)
  
  gdaltindex(index_file = paste0(demFolder,'/tindex.shp'),
             gdal_file = list.files(demFolder,pattern='*.asc$',
                                    full.names = T,
                                    recursive = T))
  tIndex <- st_read(paste0(demFolder,'/tindex.shp'),
                    crs=27700,quiet = T,
                    stringsAsFactors = F)
  # x <- 5
  callGrass <- function(x) {
    ras <- tIndex %>% st_join(intBuff[intBuff$fid==x,],left=F) %>% 
      dplyr::select(location)
    gdalbuildvrt(ras$location,'raster/temp_vrt.vrt',overwrite = T)
    r <- raster('raster/temp_vrt.vrt')
    
    rAg <- aggregate(r, 4)
    writeRaster(rAg, 'raster/temp_ag.tif',overwrite=T)
    # grass python script cam be editted in RStudio!
    pyLoc <- paste0(getwd(),'/python/processDEM.py')
    
    intID <- paste0(x,'_fid')
    
    # geomorphon search radius (in metres)
    geosearch <- '500'
    # run grass via system command, executing above python script
    system2('grass',
            paste(shQuote(gdb),
                  shQuote('--exec'),
                  shQuote(pyLoc),
                  shQuote(file.path(getwd(),'raster/temp_ag.tif')),
                  shQuote(file.path(userDataDir,'coarse')),
                  shQuote(intID),
                  shQuote(geosearch)
            )
            # ,stderr = paste0(getwd(),'/logs/pyfunc_err.txt') # comment out to see output
            ,stdout = paste0(getwd(),'/logs/processDEM_stdout.txt') # comment out to see output
    )
    
    grassOut <- read.table('logs/processDEM_stdout.txt',sep='\n',
                           stringsAsFactors = F) %>% slice_tail()
    grassOut <- str_split(grassOut$V1,', ')[[1]]
    grassFiles <- paste0(userDataDir,'/','coarse/',grassOut,'_',intID,'.tif')
    
    grassRas <- lapply(grassFiles, raster)
    
    grassStars <- read_stars(unlist(grassFiles))
    return(list(ras = grassRas,
                st = grassStars))
    
  } # per interpolation polygon
  
  cov <- callGrass(buffFid)
  return(cov)
}

# dummy data generator
dummyData <- function() {
  s <- 10
  xy <- do.call(rbind,lapply(-s:s, function(x) { data.frame(x = x, y=-s:s) } ))
  xy$z <- sqrt(abs((xy$x*xy$x)+(xy$y*xy$y)))
  
  # df <- xy2
  foursUp <- function(df) {
    df1 <- df
    df2 <- df
    df3 <- df
    # plus x and y the same
    df1[,'x'] <- df1[,'x'] + s*2
    # plus x and minus y
    df2[,'x'] <- df2[,'x'] + s*2
    df2[,'y'] <- df2[,'y'] - s*2
    # x the same and minus y
    df3[,'y'] <- df3[,'y'] - s*2
    bind_rows(df,df1,df2,df3)
  }
  
  xy2 <- foursUp(xy)
  xy3 <- foursUp(xy2)
  xy4 <- foursUp(xy3)
  xyRas <- rasterFromXYZ(xy4)
  plot(xyRas)
  writeRaster(xyRas,'raster/dummy.tif',overwrite=T)
  xyStars <- read_stars('raster/dummy.tif')
  smpMask <- st_as_sf(xy4[sample(1:nrow(xy4),3),],
                      coords = c('x','y')) %>% 
    mutate(id = 1) %>% 
    group_by(id) %>% st_union() %>% st_cast("LINESTRING") %>% 
    smoothr::smooth('chaikin') %>% st_buffer(s/2) %>% st_sf
  
  a_ras <- xyStars
  b_ras <- xyStars
  pol <- smpMask
  st_crs(pol) <- st_crs(27700)
  st_crs(a_ras) <- st_crs(27700)
  st_crs(b_ras) <-st_crs(27700)
  plot(a_ras)
  plot(pol, add=T)
}

# hillshade function
st_hillshade <- function(st_ras) {
  slope.aspect <- terrain(as(st_ras, "Raster"),opt=c('slope','aspect'))
  hs <- hillShade(slope.aspect$slope,slope.aspect$aspect)
  writeRaster(hs,'raster/hs.tif',overwrite=T)
  hsSt <- read_stars('raster/hs.tif')
  return(hsSt)
}

# st_ras <- a_ras
st_slopeaspect <- function(st_ras, formatOut='Stars') {
  st_r <- as(st_ras, "Raster")
  slope.aspect <- terrain(st_r,opt=c('slope','aspect'))
  st_r <- mask(st_r,slope.aspect$slope)
  writeRaster(st_r,'raster/elev.tif',overwrite=T)
  
  writeRaster(slope.aspect$slope,'raster/slope.tif',overwrite=T)
  writeRaster(slope.aspect$aspect,'raster/aspect.tif',overwrite=T)
  
  if (formatOut == 'Stars') {
    output <- read_stars(c('raster/elev.tif','raster/slope.tif','raster/aspect.tif'))
  } else output <- brick(st_r,slope.aspect$slope,slope.aspect$aspect)
  return(output)
}
# pol <- int[1,]
loadLidar <- function(pol, 
                      bufferDem=50, # how much DEM around hole to include?
                      offsetPol=F) {
  # buffer around interpolation polygon to select ras LIDAR tiles
  b <- bufferDem
  
  # offset by buffer amount
  if (offsetPol) {
    pol_bbox <- st_geometry(pol) %>% st_bbox()
    pol_width <- pol_bbox$xmax-pol_bbox$xmin
    st_geometry(pol) <- st_geometry(pol) - (b + pol_width)
    st_crs(pol) <- st_crs(27700)
  }
  
  # load rasters into memory, and clip by polygon
  # refCol <- 'a'
  loadRas <- function(pol,b,refCol) {
    tIndex <- st_read(paste0(userDataDir,'/dtms/',pol$layer,'/',
                             pol %>% st_drop_geometry %>% 
                               dplyr::select(all_of(refCol)))) 
    tiles <- pol %>% st_join(tIndex) %>% mutate_if(is.factor,as.character)
    if (length(tiles$location) > 1) {
      t <- lapply(tiles$location, raster)
      t$fun <- mean
      r <- do.call(mosaic, t)
      rStars <- st_as_stars(r)
    } else rStars <- read_stars(tiles$location)
    st_crs(rStars) <- st_crs(pol)
    polBuff <- pol %>% st_buffer(b)
    clippedRas <- rStars[polBuff]
    write_stars(clippedRas, paste0('raster/',refCol,'.tif'))
    # plot(clippedRas)
    return(clippedRas)
  }
  
  # load LIDAR rasters as stars objects
  
  a_ras <- loadRas(pol, b, 'a') # latest survey
  b_ras <- loadRas(pol, b, 'b') # earliest survey
  
  gridExtra::grid.arrange(
    ggplot() + 
      geom_stars(data=a_ras) + 
      geom_sf(data=pol,fill=NA,color='white') + 
      coord_sf(datum = sf::st_crs(27700)) + 
      theme_bw() + 
      theme(legend.position = 'none',
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) + 
      ggtitle(paste0('DEM A: ',pol$a)),
    ggplot() + 
      geom_stars(data=b_ras) + 
      geom_sf(data=pol,fill=NA,color='white') + 
      coord_sf(datum = sf::st_crs(27700)) + 
      theme_bw() + 
      theme(legend.position = 'none',
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) + 
      ggtitle(paste0('DEM B: ',pol$b)),
    nrow=1
  )
  
  return(list(a = a_ras,
              b = b_ras,
              pol = pol))
}


# ras <- tiles$a
# cutpoly <- tiles$pol
# sampleRasPer <- 0.8
processTiles <- function(ras, 
                         cutpoly=NULL, # cut hole with polygon
                         increaseHole=0, # adjust size of hole from polygon within DEM
                         sampleRasPer=0, # randomly select percent of raster e.g. 0.1
                         addOGC=T,
                         addSlopeAspect=T) {
  
  # generate slope aspect as raster brick
  ras.sa.r <- st_slopeaspect(ras,formatOut ='Raster')
  #
  # # generate oblique coordinates
  ras.ogc.r <- makeOGC(as(ras, "Raster"), 8)
  ras.r <- stack(ras.sa.r,ras.ogc.r)
  
  # ras.r <- as(ras, "Raster")
  names(ras.r)[1] <- 'elev'
  
  # cookie cut interplation mask polygon A raster and convert
  # resulting raster to SF points for interpolation
  if (!is.null(cutpoly)) {
    if (increaseHole > 0) cutpoly <- cutpoly %>% st_buffer(increaseHole)
    
    # convert hole polygon into raster
    pol.r <- fasterize::fasterize(cutpoly, raster = raster(ras.r))
    # cookie cut
    train.r <- mask(ras.r,pol.r,inverse=T)
    test.r <- mask(ras.r,pol.r)
    
    # generate distance stat for hole cells, add to stack
    # ras.r <- stack(ras.r,mask(raster::distance(ras.r$elev),pol.r))
    # names(ras.r)[which(names(ras.r)=='layer')] <- 'hole_distance'
  }
  
  if (sampleRasPer > 0) {
    # stars implementation - slow
    ras.sp <- st_as_sf(ras,as_points = T,merge=F) %>% 
      as_Spatial()
    ras.sp.s <- 
      ras.sp[sample(1:nrow(ras.sp),
                    ceiling(nrow(ras.sp)*sampleRasPer),
                    replace = F),]
    
    ras.r.s <- dropLayer(rasterize(ras.sp.s, as(ras, 'Raster')),'ID')
    # incorporate cut poly hole, if produced
    if (!is.null(cutpoly)) ras.r.s <- mask(ras.r.s,train.r)
    train.r <- mask(ras.r,ras.r.s)
    test.r <- mask(ras.r,ras.r.s,inverse=T)
  }
  
  out <- lapply(list(train = train.r,
                     test = test.r,
                     all = ras.r),
                function(x) {
                  ras.st <- st_as_stars(x)
                  st_crs(ras.st) <- st_crs(27700)
                  return(ras.st)
                })
  
  convertStars <- function(r) {
    r.sf <-  st_as_sf(r, merge=F, as_points = T) %>%
      dplyr::rename(elev = 1) %>% na.omit() # simple feature points
    
    # r.sf <- 
    #   st_as_sf(raster::rasterToPoints(as(r, 'Raster'),spatial=T)) %>% 
    #   dplyr::rename(elev = 1) %>% na.omit() # simple feature points
    
    st_crs(r.sf) <- st_crs(27700)
    r.df <-  st_drop_geometry(r.sf) %>% 
      cbind(st_coordinates(r.sf)) # data.frame
    
    r.sp <-  as(r.sf, "Spatial") # sp points
    r.sp@proj4string <- CRS('+init=epsg:27700')
    r.ras = as(r, "Raster") # raster
    crs(r.ras) <- CRS('+init=epsg:27700')
    return(list(sf = r.sf,
                df = r.df,
                sp = r.sp,
                ras = r.ras))
  }
  # convert data
  
  
  out$train <- convertStars(out$train)
  out$all <- convertStars(out$all)
  
  return(out)
}

# mode = 1: process prepared raster as is and compare interpolated
# surface to both B raster and A raster (itself)

# trainingRas <- foldA$train
# testRas <- foldA$all
# cv <- T
# paramData <- cvGrids
# pd <- prepData[[1]]
# trainingRas <- pd$foldA$train
# testRas <- pd$foldA$all
# maskPoly = pd$pol
interpolateRas <- function(trainingData, testData, maskPoly, paramData,
                           testCV = T,
                           outputDir = '/media/mal/working_files/quarry/',
                           outputTag='testnew') {
  
  # interpolation models ----
  
  # no parameters needed to be trialled for RF and TIN
  # random forest model, uses buffer distances----
  fit_RF <- function(trainingSp, testSp, tag='',subSample=NULL,
                     classInt=0.1) {
    # save crs from training data
    spCrs <- crs(trainingSp)
    
    # sample hole points
    if (!is.null(subSample)) {
      trainingSp <- trainingSp[sample(nrow(trainingSp),subSample),]
    }
    
    intTemplateCopy <- testSp
    # convert full a_ras to grid of locations to predict elev distances against
    gridPix <- as(testSp, "SpatialPixelsDataFrame")
    
    # split elevation into classes to reduce comp time
    classesElev <- cut(trainingSp$elev,
                       breaks=seq(min(trainingSp$elev),
                                  max(trainingSp$elev)),
                       length=classInt)
    print('calculating buffer distances...')
    grid.dist0 <- GSIF::buffer.dist(trainingSp["elev"], 
                                    gridPix[1], 
                                    # as.factor(1:nrow(trainingSp)) # all elevs
                                    classesElev # groups elev values
    )
    grid.dist0@data <- cbind(grid.dist0@data,gridPix@data)
    
    # hole.ogc <- OGC::makeOGC(hole.r,4)
    # plot(hole.ogc)
    ov.elev <- over(trainingSp["elev"], grid.dist0)
    rm.elev <- cbind(trainingSp@data, ov.elev)
    dn0 <- paste(setdiff(names(rm.elev),c('elev','slope','aspect')),
                 collapse="+")
    fm0 <- as.formula(paste("elev ~ ", dn0))
    m.elev <- ranger::ranger(fm0, rm.elev, quantreg=TRUE, num.trees=150, seed=1)
    elev.rfd <- predict(m.elev, grid.dist0@data, type="quantiles")$predictions
    
    intTemplateCopy$pred_elev <- elev.rfd[,2]
    
    outList <- list(rfSp = rasterFromXYZ(cbind(intTemplateCopy@coords,
                                               intTemplateCopy$pred_elev),
                                         crs=spCrs))
    
    return(outList)
  }
  interp_RF <- fit_RF(trainingSp = trainingData$sp,
                      testData$sp,tag='classed')
  
  # Triangular Irregular Surface----
  fit_TIN <- interp::interp( # using {interp}
    x = trainingData$df$X,     # the function actually accepts coordinate vectors
    y = trainingData$df$Y,
    z = trainingData$df$elev,
    xo = testData$df$X,# here we already define the target grid
    yo = testData$df$Y,
    output = "points"
  ) %>% bind_cols()
  
  interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = crs(trainingData$sf))
  
  # parameterised interpolation models ----
  
  # process CV grids to remove bad combinations (nmin vals > nmax vals)
  # and add run number, add polygon id
  # df <- paramData$nn
  paramData.c <- 
    paramData %>% 
    map2(.y = names(paramData), 
         .f = function(df, y) {
           if (any(str_detect(names(df),'nmaxVals')) && 
               any(str_detect(names(df),'nminVals'))) {
             df <- df %>% filter(nminVals < nmaxVals) }
           df <- df %>% 
             mutate(run_no = 1:nrow(.),
                    intpol_fid = maskPoly$fid,
                    int_method = y)
           if (testCV) df <- df[1:5,]
           return(df)
         })
  
  interp_NNs <- 
    lapply(paramData.c$nn$run_no, 
           function(x) {
             pdata <- paramData.c$nn %>% 
               filter(run_no == x)
             print(pdata)
             # Nearest neighbour
             fit_NN <- gstat::gstat( # using package {gstat} 
               formula = elev ~ 1,    
               data = trainingData$sp,
               nmax = pdata$nmaxVals,
               nmin = pdata$nminVals,# Number of neighboring observations used for the fit
             )
             interp_NN <-  raster::interpolate(testData$ras[[1]], fit_NN)
           })
  
  interp_IDWs <- 
    lapply(paramData.c$idw$run_no,
           function(x) {
             pdata <- paramData.c$idw %>% 
               filter(run_no == x)
             
             # Inverse Distance Weighting
             fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
               formula = elev ~ 1,
               data = trainingData$sp,
               nmax = pdata$nmaxVals,
               nmin = pdata$nminVals,
               set = list(idp = pdata$idpVals)) # inverse distance power
             
             interp_IDW <- raster::interpolate(testData$ras[[1]], fit_IDW)
           })
  
  ## ordinary kriging
  v_mod_ok <- automap::autofitVariogram(elev~1,input_data =
                                          trainingData$sp)
  
  # not happy running with future_lapply!
  interp_OKs <- 
    lapply(paramData.c$ok$run_no,
           function(x) {
             pdata <- paramData.c$ok %>% 
               filter(run_no == x)
             # print(OK_grid[x,])
             fit_OK = gstat(formula = elev ~ 1, model = v_mod_ok$var_model, 
                            data = trainingData$sp,
                            nmax=pdata$nmaxVals,
                            nmin=pdata$nminVals)
             interp_OK <- raster::interpolate(testData$ras[[1]], fit_OK)
           })
  
  # grass-based splines model ----
  # training.sf <- trainingData$sf
  # test.r <- testData$ras
  
  # write raster as gdal for GRASS
  writeRaster(testData$ras[[1]],
              paste0('raster/intout_',maskPoly$fid,'_ras_clip.tif'),
              overwrite=T)
  
  st_write(trainingData$sf[,'elev'], 
           paste0('vector/intout_',maskPoly$fid,'_training.gpkg'),
           delete_dsn=T)
  
  # x <- 1
  interp_GSPLINEs <- lapply(paramData.c$gspline$run_no, function(x) {
    pdata <- paramData.c$gspline %>% 
      filter(run_no == x)
    system2('grass',
            paste(shQuote('--tmp-location'),
                  shQuote('EPSG:27700'),
                  shQuote('--exec'),
                  shQuote('/home/barneyharris/projects/quarry/python/GRASS_vrst.py'),
                  shQuote(paste0(getwd(),'/vector/intout_',maskPoly$fid,'_training.gpkg')),
                  shQuote(pdata$smoothVals),
                  shQuote(pdata$tensionVals),
                  shQuote(pdata$nminVals),
                  shQuote(paste0(getwd(),'/raster/intout_',maskPoly$fid,'_ras_clip.tif')),
                  shQuote(x),
                  shQuote(pdata$intpol_fid)
            ),
            stderr = paste0(getwd(),'/logs/grass_vrst_errout_',
                            pdata$intpol_fid,'.txt')
    )
    r <- raster(paste0('raster/gspline_int_intfid_',pdata$intpol_fid,
                       '_runnum_',x,'.tif'))
    # file.remove(paste0('raster/gspline_int_intfid_',pdata$intpol_fid,
    #                    '_runnum_',x,'.tif'))
    return(r)
  })
  
  # output parameters as melted df
  paramsCv <- paramData.c %>% 
    map_df(~reshape2::melt(.x,
                           id.vars=c('run_no','intpol_fid',
                                     'int_method')))
  # gen list
  rasterlist <- list(
    "Nearest Neighbor" = interp_NNs
    ,"Inverse Distance Weighted" = interp_IDWs
    ,"Ordinary Kriging" = interp_OKs
    ,"Triangular Irregular Surface" = list(interp_TIN)
    ,"Random Forest SP" = list(interp_RF$rfSp)
    ,"GRASS Regularized Splines Tension" = interp_GSPLINEs
  ) %>% 
    map( ~map(.x, .f = function(r) {
      mask(crop(extend(r,testData$ras[[1]]),testData$ras[[1]]),
           testData$ras[[1]])
    }))
  
  print(paste0('completed pol id...',maskPoly$fid))
  out <- list(ras = rasterlist,
              params.m = paramsCv)
  
  # save(out,
  #         file=paste0(outputDir,'intA_',outputTag,'_polfid',maskPoly$fid,'.RDS'))
  return(out)
}



# intRasters <- intA
# foldedRas <- foldA
# compareRas <- tiles$b
# maskPoly <- pol

compareInt <- function(intRasters, # list of interpolated rasters
                       foldedRas, # the test/training rasters
                       compareRas, # another raster to compare int surfaces with
                       maskPoly=NULL # a polygon to exclude from testErr calculations
                       ) {
  
  # # convert all to raster format, if required
  # foldedRas.r <- lapply(foldedRas, function(x) {
  #   x.r <- as(x, 'Raster')
  #   x.out <- x.r[[1]]
  # }) 
  
  if (class(compareRas)=='stars') {
    compareRas.r <- as(compareRas, 'Raster')
    compareRas.r <- compareRas.r[[1]]
  }
  
  frtest <- as(foldedRas$test,'Raster')
  foldedRas$test <- frtest$layer.1
  
  # training error is difference between original training data and modelled training data
  # test error is difference between original test data and modelled test data
  
  # test data below is essentially the original raster 
  # ep <- pd$pol
  # fr <- foldedRas
  # cr <- compareRas.r
  compareEach <- function(raslist,fr,cr,ep=NULL) {
    
    # raslist <- intRasters$ras$`Nearest Neighbor`
    # interpolated <- raslist[[1]]
    compFunction <- function(interpolated) {
      
      trainingErr.r <- mask(interpolated,fr$train$ras$layer.1) - 
        fr$train$ras$layer.1
      testErr.r <- mask(interpolated,fr$test) - fr$test
      compareDiff <- interpolated - cr
      
      out <- list(
        trainingErr.r = trainingErr.r,
        testErr.r = testErr.r,
        compareDiff = compareDiff)
      
      if (!is.null(ep)) {
        ep.r <- fasterize::fasterize(ep, fr$test)
        out$testErr.ex.r <- mask(interpolated,fr$test) - 
          mask(fr$test,ep.r,inverse=T)
        out$testErr.inc.r <- mask(interpolated,fr$test) - 
          mask(fr$test,ep.r)
        out$compareDiff.inc.r <- interpolated - mask(cr,ep.r)
        out$compareDiff.ex.r <- interpolated - mask(cr,ep.r,inverse=T)
      }
      
      return(out)
    }
    
    updatedRas <- raslist %>% map(~compFunction(.x))
    # raslist$ras.comp <- updatedRas
    # return(raslist)
  }
  
  diffMaps <- intRasters$ras %>% 
    map(~compareEach(.x, 
                     fr = foldedRas,
                     cr = compareRas.r,
                     ep = maskPoly))
  
  
  # diffMaps$`Nearest Neighbor`$compare$compareDiff
  # plot(stack(diffMaps$`Triangular Irregular Surface`))
  # r <- diffMaps$`Ordinary Kriging`$trainingErr.r
  calcRMSEfromRas <- function(r) sqrt(cellStats(r^2,mean))
  
  diffRMSEs <- lapply(names(diffMaps), function(x) {
    diffRMSEs <- diffMaps[[x]] %>% map_df(map, calcRMSEfromRas) %>% 
      mutate(int_method = x,
             run_no = 1:nrow(.))
  }) %>% bind_rows()
  
  # add pol fid if pol provided
  if (!is.null(maskPoly)) diffRMSEs <- diffRMSEs %>% 
    mutate(intpol_fid = maskPoly$fid)
  
  return(list(orig.maps = intRasters,
              diff.maps = diffMaps,
              rmses = diffRMSEs))
}

visData <- function(dat) {
  rmses.df <- dat$rmses
  
  # first plots of CV data (if it exists)
  if (nrow(dat$orig.maps$`Nearest Neighbor`$param) > 1) {
    params <- dat$orig.maps %>% map(pluck,'param')
    params[unlist(lapply(params,function(x) length(x) == 0))] <- NULL
    flatten(params)
    params %>% map_df(flatten)
    rmses.df
  }
  
  dat$orig.maps %>% map_df('param') 
  
  rmsesByMethod <- split(dat$rmses, dat$rmses$int_method)
  rmsesByMethod %>% map(left_join, .x, )
  
  
}

# cross validation check
# ras <- interpRas[[3]]$rasterListClip$`Nearest Neighbor`$surf$orig
# tensionVals <- 10
# smoothVals <-  0.5
# npminVals <-  50
# ras <- rasPrep$hole$hole.st.elev
crossValidateSplines <- function(ras, tensionVals = seq(0.01,0.1,by=0.01),
                                 smoothVals = seq(10,30,by=5),
                                 npminVals = seq(60,140,by=20),
                                 tag='sys.time') {
  
  if (tag == 'sys.time') {
    tag <- paste0('t_',format(Sys.time(),'%d_%m_%Y_%H_%M_%S')) }
  
  rasSf <- ras %>% 
    st_as_sf(points=TRUE,merge=FALSE) %>% 
    st_centroid() %>% 
    mutate(cat = 1:nrow(.)) %>% 
    dplyr::rename(elev = 1)
  
  # rasSf <- rasSf %>% # test
  #   head(1000)
  
  st_write(rasSf,'vector/op_points.gpkg',delete_dsn=T)
  # ras <- rasterFromXYZ(cbind(rasSf %>% st_coordinates(),rasSf$layer)) # test
  
  maskLoc <- 'raster/op_raster.tif'
  # writeRaster(ras,maskLoc,overwrite=T) # test
  writeRaster(as(ras,"Raster"),
              file=maskLoc,
              overwrite=T)
  
  pointsLoc <- paste0(getwd(),'/vector/op_points.gpkg')
  
  # optimise these parameters
  
  # starter params
  # tensionVals <- seq(10,200,by=10)
  # smoothVals <- seq(0.1,1,by=0.1)
  # npminVals <- seq(200,400,by=100)
  # best run with the above was
  # cvdevGrid[116,]
  #         er_mean     er_mean_abs run smoothVal tensionVal npminVal
  # 116 -1.7246e-05   0.0159536   204       0.4         10      300
  
  pGrid <- expand.grid(smoothVal = smoothVals,
                       tensionVal = tensionVals,
                       npminVal = npminVals) %>% 
    mutate(run = 1:nrow(.))
  
  plan(multisession, gc = TRUE, workers = 4) ## Run in parallel on local computer
  # x <- 1
  
  future.apply::future_lapply(1:nrow(pGrid), function(x) {
    
    system2('grass',
            paste(shQuote('--tmp-location'),
                  shQuote('EPSG:27700'),
                  shQuote('--exec'),
                  shQuote('/home/barneyharris/projects/quarry/python/GRASSoptimise.py'),
                  shQuote(pointsLoc),
                  shQuote(pGrid[x,'smoothVal'] ),
                  shQuote(pGrid[x,'tensionVal']),
                  shQuote(pGrid[x,'npminVal']),
                  shQuote(x),
                  shQuote(maskLoc),
                  shQuote(tag)
            ),
            stderr = paste0(getwd(),'/logs/grassOptimise_errout.txt')
    )
  })
  
  # x <- list.files('cvdev',full.names = T)[1]
  # x <- "cvdev/cvdev_1_t_16_12_2020_13_30_44.txt"
  cvdevs <- do.call(rbind,lapply(list.files('cvdev',
                                            pattern=tag,
                                            full.names = T), function(x) {
                                              runNum <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x))
                                              d <- read.csv(x, stringsAsFactors = F, header=F)
                                              vals <- lapply(d$V1, str_split_fixed, '=', 2) %>% map(2)
                                              d$vals <- as.numeric(unlist(vals))
                                              # data.frame(var = c('mean','mean_abs'),
                                              #            val = c(d[8,2],d[9,2]),
                                              #            run = runNum)
                                              data.frame(mae = d[9,2],
                                                         rmse = sqrt(d[24,2]),
                                                         run = runNum)
                                            }))
  
  cvdevGrid <- left_join(cvdevs,pGrid)
  
  
  return(cvdevGrid)
}





filePattern <- 'diffDat_feb28_nodiffs'
filePattern <- 'diffDat_march_offset_nonoise_nodiffs'

analyseDat <- function(dirLoc = '/media/mal/working_files/quarry',
                       filePattern) { 
  
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  fs <- list.files(dirLoc,full.names = T,
                  pattern=paste(filePattern,'*'))
  print('loading raster files...')

  # remove diffs from rdata 
  # for (f in fs) {
  #   load(f) # loads 'dat' file
  #   dat$diff.maps <- NULL
  #   gc()
  #   save(dat, file=str_replace(f,filePattern,paste0(filePattern,'_nodiffs')))
  #   rm(dat)
  #   gc()
  # }
  rasData <- lapply(fs, loadRData)
  
  # name correlations
  nCor <- data.frame(long_name = c("Nearest Neighbor",
                                   "Inverse Distance Weighted",
                                   "Ordinary Kriging",
                                   "GRASS Regularized Splines Tension"),
                     short_name = c('nn','idw','ok','gspline'))
  
  rmsesAll <- rasData %>% 
    map_df( ~.x$rmses)
  
  # summary plots
  
  # group vars
  gVars = c('run_no','intpol_fid','int_method')
  # err vars
  errVars <- setdiff(names(rmsesAll),gVars)
  
  errPlots <- errVars %>% map(.f = function(v) {
    rmsesAll2 <- rmsesAll
    rmsesAll2$intpol_fid <- 
      as.character(rmsesAll2$intpol_fid)
    ggplot(rmsesAll2) + 
      geom_boxplot(aes_string(x = 'intpol_fid',
                              y = v)) + 
      facet_wrap(~int_method) + 
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  }) %>% setNames(errVars)
  
  # variable plots
  allData <- rasData %>% 
    map_df( ~.x$orig.maps$params.m) %>% 
    left_join(nCor, by=c('int_method' = 'short_name')) %>% 
    mutate(int_method = long_name) %>% 
    dplyr::select(-long_name) %>% 
    left_join(rmsesAll, by=c('run_no','intpol_fid','int_method'))
  
  # plots showing how cross validation parameters result in differing error
  # values
  varPlots <- errVars %>% map(.f = function(erv) {
    varPlots <- unique(allData$int_method) %>% 
      map(.f = function(i) {
        ggplot(allData %>% dplyr::filter(int_method == i)) +
          geom_smooth(aes(x = value,
                          y = get(erv) )) + 
          facet_wrap_paginate(intpol_fid ~ variable,
                              scales = 'free',
                              # ncol = 2, # n variables
                              nrow = 4, # n sites
                              page = 1) + 
          ggtitle(i) + 
          ylab(erv) + 
          xlab('Parameter value')
        
      }) %>% setNames(unique(allData$int_method))
  }) %>% setNames(errVars)
  
  # plots showing relaitionship between test error values 
  # and earlier surface values
  rmsesAll.m <- rmsesAll %>% 
    reshape2::melt(id.vars = c(gVars, 'testErr.ex.r'))
  crossPlots <- unique(allData$int_method) %>% 
    map(.f = function(i) {
      l <- list(compareDiff.ex.r = 
                  ggplot(rmsesAll.m %>% 
                           dplyr::filter(int_method == i) %>% 
                           dplyr::filter(str_detect(variable,
                                                    'compareDiff.ex.r|compareDiff.inc.r'))
                         ) +
                  geom_smooth(aes(x = testErr.ex.r,
                                  y =  value)) + 
                  facet_wrap_paginate(~ intpol_fid + variable,
                                      scales = 'free',
                                      # ncol = 2, # n variables
                                      nrow = 4, # n sites
                                      page = 1) + 
                  ggtitle(i))
      
      
    }) %>% setNames(unique(allData$int_method))
  
  
  # find best runs per error value
  bestRuns <- errVars %>% 
    map_df(.f = function(x) {
      a <- rmsesAll %>% 
        group_by(intpol_fid,int_method) %>% 
        slice_min(order_by=get(x)) %>% 
        dplyr::select(-setdiff(errVars,x)) %>% 
        dplyr::rename('error_value' = x) %>% 
        mutate(error_value = as.numeric(error_value),
               error_var = x)
    }) %>%
    dplyr::filter(error_var != 'trainingErr.r') %>% 
    dplyr::filter(error_var != 'testErr.inc.r') %>% 
    left_join(allData[,c(gVars,'variable','value')] %>% 
                filter(variable == 'nmaxVals')) %>% 
    group_by(intpol_fid,int_method, error_var) %>% 
    slice_min(order_by = 'nmaxVals') %>% # where there are ties select run with 
                                         # lowest nmax for reduced computation
    mutate(bid = 1:nrow(.)) %>% 
    dplyr::select(-c('variable', 'value'))
  
  # isoloate parameters for best runs
  bestRuns.params <- bestRuns %>% left_join(allData) %>% 
    dplyr::select(c(gVars,'bid','variable','value'))
  
  errBars <- ggplot(bestRuns) + 
    geom_col(aes(x = intpol_fid,
                 y = error_value,
                 fill = int_method),
             position='dodge'
    ) + 
    facet_wrap(~ error_var) + 
    theme(legend.position = 'bottom')
  
  
  # extract best rasters
  # convert raster data names to reflect real polygon fids
  names(rasData) <- rasData %>% 
    map(~unique(.x$rmses$intpol_fid)) %>% 
    unlist(.)
  
  # extract rasters to flat list
  bestRas <- 1:nrow(bestRuns) %>% 
    map(.f = function(x) {
      df <- bestRuns[x,]
      rasData[[as.character(df$intpol_fid)]]$orig.maps$ras[[df$int_method]][[df$run_no]]
    }) %>% setNames(as.character(1:nrow(bestRuns)))
  # split list into logical groupings & # extract original interpolated rasters, 
  # by map with lowest error per error measure
  
  bestRas.l <- split.data.frame(bestRuns, bestRuns$error_var) %>% 
    map(~split(.x, .x$intpol_fid)) %>% map(~map(.x, .f = function(b) {
      list(ras = bestRas[b$bid],
           params = b %>% left_join(bestRuns.params))
    }) 
    # %>% setNames(b$int_method)
    )
  
  # plot best ras
  # errRas <- bestRas.l$compareDiff
  # fidRas <- errRas$`2`
  bestRasPlots <- bestRas.l %>% map(.f= function(errRas) {
    map(errRas, .f = function(fidRas) {
      
      allRasdf <- map2_df(fidRas$ras, .y = names(fidRas$ras), .f = function(r, rn) {
        df <- as.data.frame(rasterToPoints(r)) %>% 
          dplyr::rename(elev = 3) %>% 
          mutate(bid = as.numeric(rn) ) %>% 
          left_join(bestRuns, by='bid')
      })
      df.l <- fidRas$params %>% na.omit() %>% 
        dplyr::group_by(int_method, run_no, bid) %>% 
        dplyr::summarise(param_line = paste0(variable,': ',value,collapse='\n'))
      
      ggplot() +  
        geom_raster(data=allRasdf, aes(x=x, y=y, fill=elev)) + 
        geom_label(data=df.l, label.size=0.1, size=3,
                  aes(x=min(allRasdf$x),
                      y=min(allRasdf$y),
                      label=param_line),
                  nudge_x=(max(allRasdf$x) - min(allRasdf$x))*0.2,
                  nudge_y=(max(allRasdf$y) - min(allRasdf$y))*0.2) +
        scale_fill_viridis() +
        coord_equal() +
        theme(legend.position="none") + 
        facet_wrap(~ int_method) + 
        labs(title = paste0('Site: ',unique(allRasdf$intpol_fid)),
             subtitle = paste0('Lowest error maps for ',unique(allRasdf$error_var))
             )
        
    })
  })
  l <- list(bestRas.l = bestRas.l,
            bestRuns = bestRuns,
            bestRuns.params = bestRuns.params,
            allData = allData,
            plots = list(bestRasPlots = bestRasPlots,
                         errPlots = errPlots,
                         varPlots = varPlots,
                         crossPlots = crossPlots))
  return(l)
}

