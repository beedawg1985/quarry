require(RSelenium)
require(pdftools)
require(stringr)
require(rnrfa)
require(sf)
require(leaflet)
require(dplyr)


# import OS 5k grid polygons for England
grid5k <- st_read('data/osgb/OSGB_Grid_5km.shp') %>% 
  st_transform(4326) %>% dplyr::filter(ENGLAND == 't')
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

enQuarriesBuff <- engQuarries[1:50,] %>% 
  st_buffer(600, endCapStyle="SQUARE") %>% 
  st_simplify(500, preserveTopology=T) %>% 
  st_union() %>% st_sf


getLidar2 <- function(bufferedPoly,overwrite=T) {
  # define chrome options
  eCaps <- list(chromeOptions = list(
    args = c('--disable-gpu', 
             # '--headless',
             '--window-size=1280,800')
  ))
  rD <- rsDriver(browser = "chrome",
                 chromever = "81.0.4044.138",
                 extraCapabilities = eCaps,
                 port = 
                   as.integer(base::sample(seq(32768,65535, by=1),1)))
  remDr <- rD[["client"]]
  
  browseQuery <- function(remDr,bufferedPoly) {
    st_write(bufferedPoly, dsn=paste0(getwd(),'/temp.shp'),
             delete_dsn=T)
    simplePoly <- paste0(getwd(),'/temp.shp')
    simplePolyZip <- paste0(getwd(),'/temp.zip')
    simplePolyFiles <- paste0(getwd(),'/temp*')
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
  x <- 1
  zipList <- lapply(1:length(yrList), function(x) {
    yr <- yrList[x]
    xP <- paste0('//*[@id="yearSelect"]/option[',x,']')
    webElem <- remDr$findElement(using = 'xpath', 
                                 value = xP)
    webElem$clickElement()
    # now cycle through res
    resElem <- remDr$findElement(using = 'css selector', '#resolutionSelect')
    resVec <- unique(resElem$selectTag()$text)
    ziplist <- list()
    # r <- 1
    for (r in 1:length(resVec)) {
      resElem$clickElement()
      xP <- paste0('//*[@id="resolutionSelect"]/option[',r,']')
      webElem <- remDr$findElement(using = 'xpath', 
                                   value = xP)
      webElem$clickElement()
      tileLinks <- remDr$findElement(using = 'css selector', '.data-ready-container')
      tileLinks.a <- tileLinks$findChildElements('tag', 'a')
      ziplist[[paste0(tolower(str_replace(resVec[r],' ','-')),
                     '-',yr)]] <- 
        unlist(lapply(tileLinks.a, function(x) x$getElementAttribute('href')))
    }
    return(ziplist)
  })

  # output URLs as list for Wget
  # x <- names(zipPaths)[1]
  listFiles <- unlist(lapply(zipList, function(x) {
    resNames <- names(zipPaths[[x]])
    fileOut <- c()
    # r <- resNames[1]
    for (r in resNames) {
      write.table(zipPaths[[x]][[r]],
                  file=paste0(x,'_',r,'_list.txt'),quote = F,
                  row.names=F,col.names = F)
      print(paste0(x,'_',r,'_list.txt'))
      fileOut <- c(fileOut,paste0(x,'_',r,'_list.txt'))
    }
    return(fileOut)
  }))
  
  # close selenium
  remDr$close()
  rD$server$stop()
  gc()
  
  
  # create folder structure
  folders <- paste0('/',str_replace(listFiles,'_list.txt',''))
  countyID <- str_split_fixed(bufferedPoly,'_',2)[1]
  countyPath <- paste0('/srv/mal_data/external_data/ea_lidar/',countyID)
  if (!dir.exists(countyPath)) {
    dir.create(countyPath)
    lapply(folders, function(x) {
      dir.create(paste0(countyPath,x))
    })
  }
  # overwrite=T
  if (overwrite) {
    system(paste0('rm ',countyPath,' -R'))
    dir.create(countyPath)
    lapply(folders, function(x) {
      dir.create(paste0(countyPath,x))
    })
  }
  
  resRootPaths <- paste0(countyPath,folders)
  
  # download and uncompress EA lidar with magic!
  # x <- 1
  lapply(1:length(resRootPaths), function(x) {
    system(paste0('cat ',getwd(),'/',listFiles[x],' | parallel --gnu ',
                  shQuote(paste0('wget {} -P ',resRootPaths[x]))))
    # system(paste0('wget -i ',getwd(),'/',listFiles[x],
    #               ' -P ',resRootPaths[x]))
    system(paste0("unzip ","'",resRootPaths[x],"/*.zip'",
                  ' -d ',resRootPaths[x],'/')) 
  })
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
  
  

