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

def runInt(smoothVal,tensionVal,npminVal,runNo,polfid):
  my_print('running v.surf.rst...')
  grass.run_command("v.surf.rst", input='points',\
                    segmax='10',\
                    zcolumn='elev',\
                    tension=smoothVal,\
                    smooth=tensionVal,\
                    npmin=npminVal,\
                    mask='testMask',\
                    elevation='int',\
                    overwrite=True)
  grass.run_command("r.out.gdal",\
                    input='int',\
                    output='/home/barneyharris/projects/quarry/raster/gspline_int_intfid_' + \
                    polfid + '_runnum_' + runNo + '.tif',
                    overwrite=True)
  return
   
# parse arguments
smoothVal = sys.argv[1]
tensionVal = sys.argv[2]
npminVal = sys.argv[3]
runNo = sys.argv[4]
polfid = sys.argv[5]

runInt(smoothVal,tensionVal,npminVal,runNo,polfid)
