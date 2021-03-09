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

def runInt(stepVal,lamVal,runNo,polfid):
  my_print('importing training raster...')
  my_print('running v.surf.bspline...')
  grass.run_command("v.surf.bspline", input='points',\
                    column='elev',\
                    ew_step=stepVal,\
                    ns_step=stepVal,\
                    method='bicubic',\
                    mask='testMask',\
                    lambda_i=lamVal,\
                    raster_output='int',\
                    overwrite=True)
  grass.run_command("r.out.gdal",\
                    input='int',\
                    output='/home/barneyharris/projects/quarry/raster/gbicubic_int_intfid_' + \
                    polfid + '_runnum_' + runNo + '.tif',
                    overwrite=True)
  return
   
# parse arguments
stepVal = sys.argv[1]
lamVal = sys.argv[2]
runNo = sys.argv[3]
polfid = sys.argv[4]

runInt(stepVal,lamVal,runNo,polfid)


                    
