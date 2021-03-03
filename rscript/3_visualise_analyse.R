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

results_offset <- analyseDat(filePattern = 'march_offset_nonoise_nodiffs')
results <- analyseDat(filePattern = 'diffDat_feb28_nodiffs')

save(results_offset,results,file='/media/mal/working_files/quarry/results.RData')


