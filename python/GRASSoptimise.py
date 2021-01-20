#!/usr/bin/env python
import csv
import sys
import os
import datetime
import grass.script as grass
import multiprocessing as multi
import itertools

def my_print(text):
    currentDT = datetime.datetime.now()
    sys.stdout.write('\n')
    sys.stdout.write(str(currentDT))
    sys.stdout.write('\n' + str(text))
    sys.stdout.flush()

def runInt(pointsLoc,smoothVal,tensionVal,npminVal,itNumber,maskLoc,tag):
  my_print('importing points...')
  grass.run_command("v.in.ogr", overwrite=True, input=pointsLoc, \
                    output='points')
  my_print('importing masking raster...')
  grass.run_command("r.in.gdal", overwrite=True, input=maskLoc, \
                    output='opMask', flags='o')
  grass.run_command("g.region", raster='opMask')
  my_print('running v.surf.rst...')
  grass.run_command("v.surf.rst", input='points',\
                    # segmax='30',\
                    zcolumn='elev',\
                    mask='opMask',\
                    cvdev='cvdev_' + itNumber + 'it',\
                    tension=smoothVal,\
                    smooth=tensionVal,\
                    npmin=npminVal,\
                    flags='c',\
                    overwrite=True)
  # vinfo = grass.read_command("v.info",map='cvdev_' + itNumber + 'it')
  # my_print(vinfo)
  grass.run_command("v.db.addcolumn", \
                    map='cvdev_' + itNumber + 'it',\
                    col="flt1_sq double")
  grass.run_command("v.db.update",\
                      map='cvdev_' + itNumber + 'it',\
                    col="flt1_sq", \
                    qcol="flt1 * flt1")
  vstat_err = grass.read_command("v.univar",map='cvdev_' + itNumber + 'it',\
                              type='point',\
                              column='flt1',
                              flags='g')
  vstat_sqerr = grass.read_command("v.univar",map='cvdev_' + itNumber + 'it',\
                              type='point',\
                              column='flt1_sq',
                              flags='g')
  # my_print(vstat)
  f = open('/home/barneyharris/projects/quarry/cvdev/cvdev_' + itNumber + '_' + tag + '.txt', "a")
  f.write(vstat_err)
  f.write('\n')
  f.write(vstat_sqerr)
  f.close()
  return
   
# parse arguments
pointsLoc = sys.argv[1]
smoothVal = sys.argv[2]
tensionVal = sys.argv[3]
npminVal = sys.argv[4]
itNumber = sys.argv[5]
maskLoc = sys.argv[6]
tag = sys.argv[7]

runInt(pointsLoc,smoothVal,tensionVal,npminVal,itNumber,maskLoc,tag)
