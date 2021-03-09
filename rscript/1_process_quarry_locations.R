source('rscript/general_functions.R')

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

qBuff.sum <- qBuff %>% 
  group_by(tile50k_name_char) %>%
  summarise(geometry = st_union(geometry),
            all_n = sum(n())) %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  arrange(tile50k_name_char)

qGrps <- split.data.frame(qBuff.sum, 
                          qBuff.sum$tile50k_name_char)


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

# these rasters are added to a geoServer Mosaic for inspection
allDiffs <- list.files(allDiffDir,full.names = T,pattern='*diff.tif')
mergedDiffDir <- paste0(userDataDir,'/diffs')

createMosaic(filelist = allDiffs,
             trgdir = mergedDiffDir)

# now a new vector file is manually created: 'data/aoi_poly_wfiles_inc.gpkg'
# this contains all the vectors from 'data/aoi_poly.gpkg' with a boolean field 
# describing whether the area should be included

# toVec <- allDiffDir
toVec <- setdiff(allDiffDir[2:length(allDiffDir)],st_layers('data/aoi_poly.gpkg')$name)
for (r in toVec) {
  processDiffs(r)
}

# manually examine polygons and difference maps to identify 
# suitable areas for the analyses
createIncludes <- function() {
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
  print('written included to DB...')
}
# run above function
createIncludes()



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

# redownload LiDAR DTMs of areas for interpolation 
# cycle through polygons grouped by tile ref, 
# requesting DTMs, then generate spatial index
dtmDirs <- setdiff(unique(int$layer),
                   basename(list.dirs(file.path(userDataDir,'dtms'),
                                      recursive = F)))
# x <- 'SJNE'
# x <- 'SYNE'
x <- 'NYNW'
# dtmDirs <- unique(int$layer)
lapply(dtmDirs, function(x) {
  dir.create(file.path(userDataDir,'dtms',x))
  sfFilt <- int[int$layer == x,]
  plot(sfFilt$geom)
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
    # toRm <- setdiff(unique(list.files(yearFolder,pattern='*.tif$',full.names = T)),
    #                 unique(tIndex$location))
    # # x <- toRm[1]
    # if (length(toRm) > 0) {
    #   lapply(toRm, function(x) {
    #     file.remove(list.files(yearFolder, pattern=str_replace(basename(x),'.tif','*'),
    #                            full.names = T)) })
    # }
  })
  
  file.remove(list.files(paste0(userDataDir,'/dtms/',x),pattern='*.zip$',
                         full.names = T))
})

