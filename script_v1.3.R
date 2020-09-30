require(RSelenium)
# require(pdftools)
require(mapview)
require(stringr)
require(rnrfa)
require(sf)
require(leaflet)
require(dplyr)
require(gdalUtils)
require(velox)
require(raster)


# import OS 5k grid polygons for England
grid5k <- st_read('data/osgb/OSGB_Grid_5km.shp') %>% 
  st_transform(4326) %>% dplyr::filter(ENGLAND == 't')
grid50k <- st_read('data/osgb/OSGB_Grid_50km.shp') %>% 
  dplyr::filter(ENGLAND == 't')
# get grid letters
osgbLetters <- 
  unique(unlist(lapply(grid5k$TILE_NAME, substr,1,2)))

# import directory of mines and quarries
britPit <- pdftools::pdf_data('DirectoryOfMinesAndQuarries2014.pdf')
# extract pages with addresses
mineAdress <- britPit[32:150]

# function for extracting grid references from address
getGridRefs <- function(mineAdressPage) {
  tileRefs <- which(mineAdressPage$text %in% osgbLetters)
  unlist(lapply(tileRefs, function(x) {
    paste(mineAdressPage$text[x:(x+2)],collapse='')
  }))
}

# run grid ref extraction function
gridRefs <- unlist(lapply(mineAdress,getGridRefs))
# validate grid refs, remove erros
gridRefsVal <- gridRefs[str_detect(gridRefs,
           '^([STNHOstnho][A-Za-z]\\s?)(\\d{5}\\s?\\d{5}|\\d{4}\\s?\\d{4}|\\d{3}\\s?\\d{3}|\\d{2}\\s?\\d{2}|\\d{1}\\s?\\d{1})$')]
# write to text file
writeLines(gridRefsVal, 'gridRefs.txt')
# convert grid refs to easting northings
quarryCoor <- osg_parse(gridRefsVal, coord_system = 'BNG')
# build sf data frame of points
coorDf <- data.frame(x = quarryCoor$easting,
                     y = quarryCoor$northing,
                     grid_ref = gridRefsVal)
coorSf <- st_as_sf(coorDf,
              coords = c('x','y'),
              crs = st_crs(27700))
# select only those quarries in england
engQuarries <- st_join(coorSf,st_transform(grid5k,27700),
                       left=F)
leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(data=st_transform(engQuarries,4326))
# save(engQuarries,file='data/engQuarries.RData')
load('data/engQuarries.RData')

# create square buffers around quarry locations,
# dissolve any overlapping and calc areas
qBuff <- engQuarries %>% 
  st_buffer(600, endCapStyle="SQUARE") %>% 
  st_simplify(500, preserveTopology=T) %>% 
  st_join(grid50k, left = F) %>% 
  mutate(tile_name_char = as.character(TILE_NAME.x),
         tile50k_name_char = as.character(TILE_NAME.y))

plot(qBuff$geometry)

qBuff.sum <- qBuff %>% 
  group_by(tile50k_name_char) %>%
  summarise(geometry = st_union(geometry),
            all_n = sum(n())) %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  arrange(tile50k_name_char)
  
qGrps <- split.data.frame(qBuff.sum, 
                               qBuff.sum$tile50k_name_char)
qGrps
bufferedPoly <- qGrps$SUSW
# max request limit = 625455396.147
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
    
    startTime <- Sys.time()
    while (remDr$status == 7) {
      Sys.sleep(5)
      print(paste0('waiting for tiles to be returned...',
                   round(Sys.time() - startTime,2),
                   ' seconds elapsed'))
      suppressMessages({
        try(remDr$findElement(using = 'css selector', '.data-ready-container'),TRUE)
      })
    }
    print('tiles returned!')
  } # / browseQuery
  browseQuery(remDr,bufferedPoly)
  
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
  x <- 1
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
  
  # x <- names(tileList)[1]
  tileNames <- lapply(names(tileList), function(x) {
    unlist(lapply(str_split(tileList[[x]], 
                            paste0(x,'-')),function(y) substr(y[2],1,6)))
  })
  names(tileNames) <- names(tileList)
  
  tilesYears <- lapply(unique(unlist(tileNames)), function(tile) {
    allYears <- lapply(names(tileNames), function(year) {
      if (tile %in% tileNames[[year]]) year
    })
    allYears[unlist(lapply(allYears,is.null))] <- NULL
    return(unlist(allYears))
  })
  names(tilesYears) <- unique(unlist(tileNames))
  
  # minimun number of years survey 3
  minSurvey <- 3
  tilesYears[unlist(lapply(tilesYears, function(x) length(x) < minSurvey))] <- NULL
  
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
  folderPath <- paste0('/home/barneyharris/user_quarry_data/',bufferedPoly$tile50k_name_char)
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
  # write request 
  # st_write(bufferedPoly,paste0(folderPath,'/',basename(folderPath),'_request.shp'))
  
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
  
  # check which raster tiles overlap with one another
  f <- list.files(folderPath,pattern='*.tif$',
                  full.names = T,
                  recursive = T)
  
  indexLoc <- paste0(folderPath,'/',basename(folderPath),'_tindex.shp')
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
                 output.vrt = 
                   vrt)
    r <- raster(vrt)
    return(r)
    })
  
  names(st) <- paste0('req',toVrt$req_id,'_',
                      toVrt$year)
  # now subtract earliest survey from latest survey
  if (!dir.exists(paste0(folderPath,'/diffs'))) {
    dir.create(paste0(folderPath,'/diffs'))
  }
  
  co <- c('TILED=YES','COMPRESS=DEFLATE')
  diffs <- lapply(unique(toVrt$req_id), function(x) {
    df <- toVrt %>% as.data.frame() %>% 
      filter(req_id == x)
    diff <- st[[paste0('req',x,'_',max(df$year))]] - 
      st[[paste0('req',x,'_',min(df$year))]]
    if(raster::maxValue(diff) > 2) {
      diffOut <- paste0(folderPath,'/diffs/',
                        'req',x,'_',max(df$year),
                        '_',min(df$year),'.tif')
      writeRaster(diff, diffOut, options=co, overwrite=T)
      gdaladdo(diffOut, levels=c('8','16','32','64','128'))
      return(diff)
    } else return(NULL)
  })
  

  # plot(r)
  return(countyPath)
}


st_write(pointDfs$`24`,dsn='test.shp',
         delete_dsn=T)

test <- coorSf[tileIntersClean[[78]],] %>% 
  st_buffer(600, endCapStyle="SQUARE") %>% 
  st_simplify(500, preserveTopology=T) %>% 
  st_write(dsn='test.shp',delete_dsn=T)

test <- coorSf[tileIntersClean[[78]],] %>% 
  st_buffer(600, endCapStyle="SQUARE") %>% 
  st_simplify(500, preserveTopology=T)
plot(test)
st_area(st_union(test) )
# %>% 
#   st_write(dsn='test.shp',delete_dsn=T)

plot(coorSf)
st_write(quar, dsn='quarries.shp')
leaflet() %>% 
  leaflet::addProviderTiles("Esri.WorldImagery") %>% 
  addPolygons(data = st_transform(pointDfs$`23`,4326))
  
  

