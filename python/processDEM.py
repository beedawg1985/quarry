#!/usr/bin/env python
import sys
import grass.script as grass
import os
import subprocess
import ntpath
import datetime
from osgeo import gdal
import json


def my_print(text):
    currentDT = datetime.datetime.now()
    sys.stdout.write('\n')
    sys.stdout.write(str(currentDT))
    sys.stdout.write('\n' + str(text))
    sys.stdout.flush()

def tifOut(rasterIn, basepath, basename):
    # output masked section of original map
    my_print('exporting ' + rasterIn + '...')
    grass.run_command('g.region',
    raster=rasterIn)
    grass.run_command('g.region',
    zoom=rasterIn)
    outputName = basepath + '/' + \
                  rasterIn + '_' + basename + '.tif'
    grass.run_command('r.out.gdal',\
                  input=rasterIn,\
                  format='GTiff',\
                  createopt='compress=DEFLATE,'+ \
                  'TILED=YES,BLOCKXSIZE=512,BLOCKYSIZE=512',\
                  output=outputName,\
                  overwrite=True,
                  flags='f')

def processDEM(demloc, basepath, basename, geosearch):
    grass.run_command('r.in.gdal', \
                      overwrite=True,\
                      input=demloc,\
                      output='dem')
    grass.run_command('g.region', \
                      raster='dem')
    # slope and aspect 
    my_print('slope and aspect...')
    grass.run_command('r.slope.aspect', \
                      overwrite=True,\
                      elevation='dem',\
                      slope='slope',
                      aspect='aspect',
                      flags='a')
    
    my_print('generating geomorphons...' + \
    ' using search range...' + geosearch + 'm')
    grass.run_command('r.geomorphon', \
                      overwrite=True,\
                      elevation='dem',\
                      forms='geo_forms',\
                      ternary='geo_ternary',\
                      intensity='geo_intensity',\
                      elongation='geo_elon',\
                      azimuth='geo_azi',\
                      extend='geo_extend',\
                      width='geo_width',\
                      search=geosearch,\
                      flags='m')
    
    allOut = ['slope','aspect','geo_forms','geo_ternary','geo_intensity',
    'geo_azi','geo_extend']
    # 'geo_elon' and 'geo_width' excluded due to NAs
    
    my_print('exporting slope, aspect and geomorphon output maps... ')            
    for g in allOut:
        tifOut(g, basepath, basename)
    
    return(my_print(', '.join(allOut)))


processDEM(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4])
