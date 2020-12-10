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
x <- 1
bufferedPoly <- int[x,]
whichProd <- "LIDAR Point Cloud"
whichYears <- c(int[x,]$a,int[x,]$b)
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
  # rD <- rsDriver(browser = "chrome",
  #                chromever = "81.0.4044.138",
  #                extraCapabilities = eCaps,
  #                port = 
  #                  as.integer(base::sample(seq(32768,65535, by=1),1)))
  rD <- RSelenium::rsDriver(
    browser = "firefox",
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list('--headless')
      )
    ),
    port = 
      as.integer(base::sample(seq(32768,65535, by=1),1))
  )
  
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
folderName <- 'SONE'
bufferedPoly <- int[x,]
whichProd <- "LIDAR Point Cloud"
whichYears <- c(int[x,]$a,int[x,]$b)
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
buffFid <- 5
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


allDiffs <- list.files(allDiffDir,full.names = T,pattern='*diff.tif')
mergedDiffDir <- paste0(userDataDir,'/diffs')

createMosaic(filelist = allDiffs,
             trgdir = mergedDiffDir)

# manually examine polygons and difference maps to identify 
# suitable areas for the analyses
includes <- lapply(st_layers('data/aoi_poly_wfiles_inc.gpkg')$name,
                   function(x) {
                     p <- st_read('data/aoi_poly_wfiles_inc.gpkg',
                                  layer=x) %>% 
                       mutate(layer = x)
                     if (any(names(p) %in% 'include')) {
                       out <- p[p$include=='TRUE',]
                       if (nrow(out) > 0) out else NULL
                     } else NULL
                   }) %>% bind_rows() %>% 
  mutate_if(is.factor, as.character)
# populate difference columns
includes[,c('req','a','b')] <- 
  str_split_fixed(includes$ras,'_',4)[,1:3]
includes <- includes %>% 
  mutate(a_locs = paste0(userDataDir,'/',layer,
                         '/diffs/',req,'_',a,
                         '.tif'),
         b_locs = paste0(userDataDir,'/',layer,
                         '/diffs/',req,'_',b,
                         '.tif'))

conPostgres()
st_write_withPkey(con,includes,schema='quarry',tname='includes')

# manually assess which of the polygons is suitable for interpolation
# point cloud processing ----

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

# output for course DEM data download
st_write(int %>% st_buffer(500), 'data/interpolation_polygons_500m_buff.kml')

# below code is for LAZ clouds, but not every tile has corresponding LAZ clouds :(

# cycle through polygons, requesting LIDAR point clouds
# lazDirs <- setdiff(unique(int$layer),
#         list.dirs(file.path(userDataDir,'laz'),
#           recursive = F))
# lapply(lazDirs, function(x) {
#   sfFilt <- int[int$layer == x,]
#   yrs <- int[int$layer == x,c('a','b')] %>% 
#     st_drop_geometry() %>% 
#     unlist() %>% as.character() %>% 
#     unique()
#   getLidar2(bufferedPoly = sfFilt,
#             whichProd="LIDAR Point Cloud",
#             whichYears=yrs,
#             minSurvey = 0,
#             overwrite=T)
# })

# x <- 1
# lapply(1:nrow(int), function(x) {
#   lazGrps <- int[x,]
#   processLaz(int[x,])
#   
# })

# get dtms of areas for interpolation ----
# cycle through polygons grouped by tile ref, 
# requesting DTMs, then generate spatial index
dtmDirs <- setdiff(unique(int$layer),
                   basename(list.dirs(file.path(userDataDir,'dtms'),
                                      recursive = F)))
x <- dtmDirs[1]
lapply(dtmDirs, function(x) {
  dir.create(file.path(userDataDir,'dtms',x))
  sfFilt <- int[int$layer == x,]
  yrs <- int[int$layer == x,c('a','b')] %>% 
    st_drop_geometry() %>% 
    unlist() %>% as.character() %>% 
    unique()
  getLidar2(bufferedPoly = sfFilt,
            whichProd="LIDAR Tiles DTM",
            whichYears=yrs,
            minSurvey = 0,
            userDataDirRoot = 'dtms',
            overwrite=T)
  
  # cycle through each year folder and remove any tiles not intersecting with
  # polygons
  # yearFolder <- "/home/barneyharris/user_quarry_data/dtms/SONE/2011"
  lapply(paste0(userDataDir,'/dtms/',x,'/',yrs), function(yearFolder) {
    gdaltindex(index_file = paste0(yearFolder,'/tindex.shp'),
               gdal_file = list.files(yearFolder,pattern='*.tif$',
                                      full.names = T))
    
    tIndex <- st_read(paste0(yearFolder,'/tindex.shp'),
                      crs=27700,quiet = T,
                      stringsAsFactors = F) %>% 
      st_join(sfFilt, left = F) # of the remaining polygons, keep 
    # only those which intersect with with quarries request polygon
    
    # clean up, by removing non-intersecting tiles and .zip archive
    toRm <- setdiff(unique(list.files(yearFolder,pattern='*.tif$',full.names = T)),
                    unique(tIndex$location))
    # x <- toRm[1]
    if (length(toRm) > 0) {
      lapply(toRm, function(x) {
        file.remove(list.files(yearFolder, pattern=str_replace(basename(x),'.tif','*'),
                               full.names = T)) })
    }
  })
  
  file.remove(list.files(paste0(userDataDir,'/dtms/',x),pattern='*.zip$',
                         full.names = T))
})

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

# train rf model
# cycle through polygons, cookie cutting and interpolating hole, 
# finally producing difference map
pol <- int[2,]
maskIntDiff <- function(pol) {
  
  # load rasters into memory, and clip by polygon
  # refCol <- 'a'
  loadRas <- function(pol,buffer=10,refCol) {
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
    polBuff <- pol %>% st_buffer(buffer)
    clippedRas <- rStars[polBuff]
    write_stars(clippedRas, paste0('raster/',refCol,'.tif'))
    plot(clippedRas)
    return(clippedRas)
  }
  
  # buffer around interpolation polygon to select ras LIDAR tiles
  b <- 100
  
  # load LIDAR rasters as stars objects
  a_ras <- loadRas(pol, b, 'a') # latest survey
  b_ras <- loadRas(pol, b, 'b') # earliest survey
  
  prepareRas <- function(a_ras, b_ras, pol) {
    # write b_ras to disk as template for interpolation
    write_stars(b_ras, 'raster/b_ras.tif',overwrite=T)
    write_stars(a_ras, 'raster/a_ras.tif',overwrite=T)
    
    # cookie cut interplation mask polygon from A raster and convert
    # resulting raster to SF points for interpolation
    invMask <- a_ras %>% st_bbox %>% st_as_sfc %>% 
      st_difference(pol)
    hole <- a_ras[invMask]
    names(hole) <- 'elev'
    hole.sf <- st_as_sf(hole, as_points = TRUE, merge = FALSE)
    hole.df <- as.data.frame(cbind(elev = hole.sf$elev,st_coordinates(hole.sf)))
    crs_raster_format <- st_crs(hole.sf,parameters=T)$proj4
    hole.r <- rasterFromXYZ(hole.df[,c(2,3,1)], crs=crs_raster_format)
    
    # convert a_ras to sf points
    a_rasSf <- st_as_sf(a_ras, as_points = TRUE, merge = FALSE) %>% 
      dplyr::rename(elev = 1)
    a_rasSp <- as_Spatial(a_rasSf)
    
    return(list(hole = list(hole.st = hole,
                     hole.sf = hole.sf,
                     hole.df = hole.df,
                     hole.r = hole.r),
         ras = list(a_ras = a_ras,
                    a_rasSf = a_rasSf,
                    a_rasSp = a_rasSp,
                    b_ras = b_ras),
         crs = crs_raster_format
         ))
  }
  
  rasPrep <- prepareRas(a_ras,b_ras,pol)
  
  getCoarseCov <- function(rasPrep, 
                           pol, 
                           covars=c("aspect","geo_azi","geo_extend",
                                    "geo_intensity","slope")) {
    if (!is.null(pol$fid)) {
      # load rasters of 30m-res , co-variate data
      # check for pre-existing coarse rasters
      cR <- list.files(file.path(userDataDir,'coarse'),
                 pattern=paste0(pol$fid,'_fid'))
      if (length(cR) > 0) {
      cov <- read_stars(list.files(file.path(userDataDir,'coarse'),
                                   pattern=paste0(pol$fid,'_fid'),
                 full.names = T))
      } else cov <- generateCoVars(pol$fid)$st
      
      pp('keeping the following co vars...',
            paste(covars,collapse=' '))
      cov <- cov[str_detect(names(cov),paste(covars,collapse='|'))]
      # sample / clip coarse data by whole a_ras 
      print('sampling course raster...')
      covSamp <- st_extract(cov, rasPrep$ras$a_rasSf)
    } else {
      covSamp <- rasPrep$ras$a_ras
    }
    
    covOGC <- makeOGC(raster('raster/a_ras.tif') , 8)
    covOGCdf <- st_as_stars(covOGC) %>% st_as_sf() %>% 
      st_drop_geometry()
    
    a_rasJoin <- rasPrep$ras$a_rasSf
    
    a_rasJoin[,names(covOGCsf)[1:8]] <- 
      covOGCdf[,names(covOGCsf)[1:8]]
    
    # join back to original points to retrieve high-res elev values
    covSampSf <- st_as_sf(covSamp) %>% 
      st_join(a_rasJoin, st_equals)
    # lose any points falling within hole
    covHoleSf <- st_difference(covSampSf,pol) %>% 
      dplyr::select(names(covSampSf))
    # convert both to SP
    covSampSp <- covSampSf %>% as_Spatial()
    covHoleSp <- covHoleSf %>% as_Spatial()
    
    return(list(covSamp = covSamp,
                covSampSf = covSampSf,
                covSampSp = covSampSp,
                covHoleSf = covHoleSf,
                covHoleSp = covHoleSp)
           )
  }
  
  coarseDat <- getCoarseCov(rasPrep,pol)
  
  # random forest interpolation functions (inputs as SpatialPoints)
  # intTemplate <- coarseDat$covSampSp
  # holeObj <- coarseDat$covHoleSp
  # intTemplate <- rasPrep$ras$a_rasSp
  # holeObj <- as_Spatial(rasPrep$hole$hole.sf)
  fit_RF <- function(holeObj, intTemplate, tag='',subSample=NULL,
                     classInt=0.1) {
    # sample hole points
    if (!is.null(subSample)) {
      holeObj <- holeObj[sample(nrow(holeObj),subSample),]
    }
  
    intTemplateCopy <- intTemplate
    # convert full a_ras to grid of locations to predict elev distances against
    gridPix <- as(intTemplate, "SpatialPixelsDataFrame")
    
    # split elevation into classes to reduce comp time
    classesElev <- cut(holeObj$elev,
                       breaks=seq(min(holeObj$elev),
                                  max(holeObj$elev)),
                                  length=classInt)
    print('calculating buffer distances...')
    grid.dist0 <- GSIF::buffer.dist(holeObj["elev"], 
                                    gridPix[1], 
                                    # as.factor(1:nrow(holeObj)) # all elevs
                                    classesElev # groups elev values
                                    )
    
    # hole.ogc <- OGC::makeOGC(hole.r,4)
    # plot(hole.ogc)
    dn0 <- paste(names(grid.dist0), collapse="+")
    fm0 <- as.formula(paste("elev ~ ", dn0))
    ov.elev <- over(holeObj["elev"], grid.dist0)
    rm.elev <- cbind(holeObj@data["elev"], ov.elev)
    m.elev <- ranger(fm0, rm.elev, quantreg=TRUE, num.trees=150, seed=1)
    elev.rfd <- predict(m.elev, grid.dist0@data, type="quantiles")$predictions
    
    intTemplateCopy$pred_elev <- elev.rfd[,2]
    outList <- list(rfSp = rasterFromXYZ(cbind(intTemplateCopy@coords,
                                               intTemplateCopy$pred_elev)))
    
    
    # now with covariates
    
    if (length(names(intTemplate)) > 2) { 
      
      coVars <- setdiff(names(intTemplate),'elev')
      print(paste('using covariates...',paste(coVars,collapse=', '),collapse=' '))
      coVarsFm <- paste(coVars,collapse=' + ')
      # coVars <- paste(names(intTemplate)[c(1,7)],collapse=' + ')
      gridPix.spc = GSIF::spc(gridPix, as.formula(paste0("~ ",coVarsFm)))
      fm1 <- as.formula(paste("elev ~ ", dn0, " + ",
                              paste(names(gridPix.spc@predicted), collapse = "+")))
      
      ov.elev1 <- over(holeObj["elev"], gridPix.spc@predicted)
      rm.elev1 <- do.call(cbind, list(holeObj@data["elev"], ov.elev, ov.elev1))
      
      m1.elev <- ranger(fm1, rm.elev1, importance="impurity",
                        quantreg=TRUE, num.trees=150, seed=1)
      
      grid.dist1 <- grid.dist0
      grid.dist1@data <- cbind(grid.dist1@data,gridPix.spc@predicted@data)
      
      elev.rfd_cov <- predict(m1.elev, grid.dist1@data, type="quantiles")$predictions
      
      intTemplateCopy$co_pred_elev <- elev.rfd_cov[,2]
      outList$rfSpCov <- 
        rasterFromXYZ(cbind(intTemplateCopy@coords,
                            intTemplateCopy$co_pred_elev))
      
      }
    # plot(interp_RF_p)
    # plot(interp_RF_cp)
    # writeRaster(interp_RF_p,paste0(file='interp_RF_p_',tag,'.tif'),overwrite=T)
    # writeRaster(interp_RF_cp,file=paste0(file='interp_RF_cp_',tag,'.tif'),overwrite=T)
    
    return(outList)
  }
  
  # Nearest neighbour
  fit_NN <- gstat::gstat( # using package {gstat} 
    formula = elev ~ 1,    # The column `NH4` is what we are interested in
    data = as(rasPrep$hole$hole.sf, "Spatial"), # using {sf} and converting to {sp}, which is expected
    nmax = 20, nmin = 3 # Number of neighboring observations used for the fit
  )
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = elev ~ 1,
    data = as(rasPrep$hole$hole.sf, "Spatial"),
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  
  # Generalized Additive Model
  fit_GAM <- mgcv::gam( # using {mgcv}
    elev ~ s(X, Y),      # here come our X/Y/Z data - straightforward enough
    data = rasPrep$hole$hole.df     # specify in which object the data is stored
  )
  
  # Triangular Irregular Surface
  fit_TIN <- interp::interp( # using {interp}
    x = rasPrep$hole$hole.df$X,           # the function actually accepts coordinate vectors
    y = rasPrep$hole$hole.df$Y,
    z = rasPrep$hole$hole.df$elev,
    xo = rasPrep$ras$a_rasSp@coords[,1],     # here we already define the target grid
    yo = rasPrep$ras$a_rasSp@coords[,2],
    output = "points"
  ) %>% bind_cols()
  
  
  ## ordinary kriging
  v_mod_ok <- automap::autofitVariogram(elev~1,input_data =
                                          as(rasPrep$hole$hole.sf, 'Spatial'))
  fit_OK = gstat(formula = elev ~ 1, model = v_mod_ok$var_model, 
                 data = rasPrep$hole$hole.sf,
            nmax = 10)
  
  # grass spline implementations
  
  fit_GSPLINE <- function(hole) {
    write_stars(hole,'raster/ras_clip.tif',
                overwrite=T)
    system(paste0('grass ',gdb,' --exec ','r.in.gdal ',
                  'input=',paste0(getwd(),'/raster/ras_clip.tif'),
                  ' output=ras_clip -o --overwrite'))
    system(paste0('grass ',gdb,' --exec ','g.region ',
                  'raster=ras_clip'))
    
    intRasters <- lapply(c('bilinear','bicubic','rst'), function(intMethod) {
      system(paste0('grass ',gdb,' --exec ','r.fillnulls ',
                    "input=ras_clip method='",intMethod,"' ",
                    'output=ras_int ',
                    'npmin=500 ',
                    'tension=80 ',
                    ' --overwrite'))
      system(paste0('grass ',gdb,' --exec ','r.out.gdal ',
                    'input=ras_int ',
                    'output=',paste0(getwd(),'/raster/ras_int_',
                                     intMethod,'.tif'),
                    ' --overwrite'))
      # intMethod <- 'bilinear'
      int_ras <- raster(paste0(getwd(),'/raster/ras_int_',
                               intMethod,'.tif'))
      
      # diff_ras <- int_ras - b_ras
      int_ras <- mask(int_ras,raster('raster/b_ras.tif'))
      
      # names(diff_ras) <- paste0('int_',x$fid,'_',intMethod)
      return(int_ras)
    })
    names(intRasters) <- c('bilinear','bicubic','rst')
    
    return(intRasters)
  }
  
  # call functions / extract rasters
  
  interp_NN <- interpolate(raster('raster/b_ras.tif'), fit_NN)
  interp_IDW <- interpolate(raster('raster/b_ras.tif'), fit_IDW)
  interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = rasPrep$crs)
  interp_GAM <- rasPrep$ras$a_rasSp@coords %>% as.data.frame() %>% 
    mutate(X = coords.x1,
           Y = coords.x2) %>% 
    mutate(Z = predict(fit_GAM, .)) %>% 
    dplyr::select(X,Y,Z) %>% 
    rasterFromXYZ(crs = rasPrep$crs)
  interp_OK <- interpolate(raster('raster/b_ras.tif'), fit_OK)
  interp_RF <- fit_RF(coarseDat$covHoleSp,
                      coarseDat$covSampSp,tag='classed')
  interp_GSPLINE <- fit_GSPLINE(rasPrep$hole$hole.st)
  
  # gen list
  rasterlist <- list(
    "Nearest Neighbor" = interp_NN, 
    "Inverse Distance Weighted" = interp_IDW, 
    "Kriging" = interp_OK, 
    "Triangular Irregular Surface" = interp_TIN, 
    "Generalized Additive Model" = interp_GAM,
    "Random Forest SP" = interp_RF$rfSp,
    "Random Forest SPCov" = interp_RF$rfSpCov,
    "GRASS Bilinear Splines" = interp_GSPLINE$bilinear,
    "GRASS Bicubic Splines" = interp_GSPLINE$bicubic,
    "GRASS Regularized Splines Tension" = interp_GSPLINE$rst
  )
  
  # process rasters, clipping, difference and hillshades
  x <- rasterlist[[1]]
  rasterListClip <- lapply(rasterlist, function(x) {
  print(x)
    # hillshade function
    st_hillshade <- function(st_ras) {
      slope.aspect <- terrain(as(st_ras, "Raster"),opt=c('slope','aspect'))
      hs <- hillShade(slope.aspect$slope,slope.aspect$aspect)
      writeRaster(hs,'raster/hs.tif',overwrite=T)
      hsSt <- read_stars('raster/hs.tif')
      return(hsSt)
    }
    # masked areas only
    intFill <- st_set_crs(st_as_stars(x),st_crs(pol))[pol]
    origFill <- b_ras[pol]
    diffFill <- origFill-intFill
    
    
    # now full surfaces (masked area + surroundings, clipped by buffer)
    intSurf <- st_set_crs(st_as_stars(x),st_crs(pol))[pol] %>% 
      st_mosaic(rasPrep$hole$hole.st)
    intSurf <- intSurf[pol %>% st_buffer(b)]
    origSurf <- b_ras[pol %>% st_buffer(b)]
    # plot(intSurf)
    # plot(origSurf)
    
    # now difference maps 
    diffSurf <- origSurf-intSurf
    intSurfHs <- st_hillshade(intSurf)
    origSurfHs <- st_hillshade(origSurf)
    
    # now hole maps
    # invPol <- st_difference(pol %>% st_buffer(b),pol)
    # origHole <- origSurf[invPol]
    # intHole <- st_set_crs(st_as_stars(x),st_crs(pol))[invPol]
    # diffHole <- origHole - intHole
    
    return(list(fill = list(int = intFill,
                            orig = origFill,
                            diff = diffFill),
                surf = list(int = intSurf,
                            orig = origSurf,
                            diff = diffSurf,
                            intHS = intSurfHs,
                            origHS = origSurfHs)
                # ,hole = list(int = intHole,
                #             diff = diffHole
                #             )
                ))
    })
  names(rasterListClip) <- names(rasterlist)
  
  
  diffStats <- function(st) {
    vals <- as.vector(st[[1]])
    vals <- vals[!is.na(vals)]
    rmse <- sqrt(mean(vals^2))
    vol <- sum(abs(vals))
    return(list(rmse = rmse,
         vol_diff = vol))
  }
  
  filldiffsRMSE <- rasterListClip %>% map(c('fill','diff')) %>% 
    map(diffStats)
  holediffsRMSE <- rasterListClip %>% map(c('hole','diff')) %>% 
    map(diffStats)
  
  plot(rasterListClip$`GRASS Bicubic Splines`$fill$int)
  plot(rasterListClip$`GRASS Bicubic Splines`$fill$orig)
  # plotting function
  # pol_overlay <- pol
  # raster_name <- 'Nearest Neighbor'
  # raster_object <- rasterListClip$`Nearest Neighbor`$surf$int
  plot_my_stars <- function(raster_object, raster_name, pol_overlay){
    print(raster_name)
    sf <- st_as_sf(raster_object, as_points = TRUE, merge = FALSE)
    df <- cbind(z = sf[,1], st_coordinates(sf)) %>% 
      as.data.frame()
    colnames(df) <- c("Z","X", "Y")
    
    ggplot() +
      geom_raster(data=df, aes(x = X, y = Y, fill = Z)) +
      geom_sf(data=pol,
              aes(color='Interpolated area'),
              color='white',
              fill=NA) + 
      ggtitle(label = raster_name) +
      scale_fill_viridis(option = "C") +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
  }
  
  a_rasPlot <- plot_my_stars(a_ras, 'Original Elevation',pol)
  plotlistDiffs[[1]]
  plotlistDiffs <- lapply(names(rasterListClip), function(x) {
    plot_my_stars(rasterListClip[[x]]$surf$diff, x, pol)
  })
  
  # Note that the next trick only works because of library(patchwork)
  patchPlot <- function(listOfPlots,maxplots=6) {
    
    a <- (listOfPlots[[1]] + listOfPlots[[2]]) /
      (listOfPlots[[3]] + listOfPlots[[4]]) /
      (listOfPlots[[5]] + listOfPlots[[6]]) / 
         (listOfPlots[[7]] + listOfPlots[[8]]) / 
      (listOfPlots[[9]] + listOfPlots[[10]])
    return(a)
  }
  
  aa <- patchPlot(plotlistDiffs)
  aa

  diffSurfaces <- lapply(1:nrow(int), function(x) {
    maskIntDiff(int[x,])
  })

}

plot(diffSurfaces[[1]]$layer.1)
require(lidR)



