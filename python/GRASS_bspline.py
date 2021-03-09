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

def runInt(rasLoc,stepVal,lamVal,runNo,polfid):
  my_print('importing training raster...')
  grass.run_command("r.in.gdal", overwrite=True, input=rasLoc, \
                    output='ras', flags='o')
  grass.run_command("g.region", raster='ras')
  my_print('running r.resamp.bspline...')
  grass.run_command("r.resamp.bspline", input='ras',\
                    ew_step=stepVal,\
                    ns_step=stepVal,\
                    method='bicubic',\
                    lambda_=lamVal,\
                    output='int',\
                    overwrite=True,\
                    flags='n')
  grass.run_command("r.out.gdal",\
                    input='int',\
                    output='/home/barneyharris/projects/quarry/raster/gbicubic_int_intfid_' + \
                    polfid + '_runnum_' + runNo + '.tif',
                    overwrite=True)
  return
   
# parse arguments
rasLoc = sys.argv[1]
stepVal = sys.argv[2]
lamVal = sys.argv[3]
runNo = sys.argv[4]
polfid = sys.argv[5]

runInt(rasLoc,stepVal,lamVal,runNo,polfid)


                    
