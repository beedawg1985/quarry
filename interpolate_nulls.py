#!/usr/bin/env python
import sys
import grass.script as grass
import os
import ntpath
import datetime
from osgeo import gdal
# execfile("/srv/mal_data/rmal/python/gen_functions.py")

def my_print(text):
    currentDT = datetime.datetime.now()
    sys.stdout.write('\n')
    sys.stdout.write(str(currentDT))
    sys.stdout.write('\n' + str(text))
    sys.stdout.flush()


def interpolate(demclipped, maskelev, outdir):
  grass.run_command('r.in.gdal', \
                    input=demclipped,\
                    output='clipped',\
                    flags='o',\
                    overwrite=True)
  grass.run_command('r.fillnulls',\
                    input='clipped',\
                    output='int',\
                    method='bilinear',
                    overwrite=True)
  grass.run_command('r.out.gdal',\
                      input='int',\
                      format='GTiff',\
                      type='Float32',\
                      createopt='compress=LZW,'+ \
                      'TILED=YES,BLOCKXSIZE=512,BLOCKYSIZE=512',\
                      output=outdir + '/int_output.tif',\
                      overwrite=True,
                      flags='f')


interpolate(sys.argv[1],sys.argv[2],sys.argv[3])
# interpolate('/Users/user/fuse2_dir/projects/quarry/raster/ras_clipped_rep.tif',
#             '/Users/user/fuse2_dir/projects/quarry/raster/ras_clip_rep.tif',
#             '/Users/user/fuse2_dir/projects/quarry/raster')
 
