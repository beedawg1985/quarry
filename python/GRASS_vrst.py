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

def runInt(pointsLoc,smoothVal,tensionVal,npminVal,maskLoc,runNo,polfid):
  my_print('importing points...')
  grass.run_command("v.in.ogr", overwrite=True, input=pointsLoc, \
                    output='points',\
                    flags='o')
  my_print('importing masking raster...')
  grass.run_command("r.in.gdal", overwrite=True, input=maskLoc, \
                    output='opMask', flags='o')
  grass.run_command("g.region", raster='opMask')
  my_print('running v.surf.rst...')
  grass.run_command("v.surf.rst", input='points',\
                    segmax='40',\
                    zcolumn='elev',\
                    mask='opMask',\
                    tension=smoothVal,\
                    smooth=tensionVal,\
                    npmin=npminVal,\
                    elevation='int',\
                    overwrite=True)
  grass.run_command("r.out.gdal",\
                    input='int',\
                    output='/home/barneyharris/projects/quarry/raster/gspline_int_intfid_' + \
                    polfid + '_runnum_' + runNo + '.tif',
                    overwrite=True)
  return
   
# parse arguments
pointsLoc = sys.argv[1]
smoothVal = sys.argv[2]
tensionVal = sys.argv[3]
npminVal = sys.argv[4]
maskLoc = sys.argv[5]
runNo = sys.argv[6]
polfid = sys.argv[7]

runInt(pointsLoc,smoothVal,tensionVal,npminVal,maskLoc,runNo,polfid)
