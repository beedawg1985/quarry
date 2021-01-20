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

def multiWorkerFunc(jobs, msg, func, sysargs, funclabel, stat='', mask_val='', name=''):
    my_print(msg)
    workers = multi.cpu_count()
    # Check if workers are already being used
    if workers is 1 and "WORKERS" in os.environ:
        workers = int(os.environ["WORKERS"])
    if workers < 1:
        workers = 1
    
    # Initialize process dictionary
    proc = {}
  
    for i in range(jobs):
    # Insert job into dictionary to keep track of it
      proc[i] = func(i,sysargs['dem'], sysargs['xs'], 
                      sysargs['ys'], sysargs['vs1Dist'],
                      sysargs['vs2Dist'],
                      sysargs['targHeights'],
                      sysargs['obHeight'],
                      sysargs['envs'],
                      sysargs['smenvs'],
                      sysargs['lbBearings'],
                      funclabel,
                      stat,
                      sysargs['outdir'],
                      mask_val,
                      name)
      # If the workers are used up, wait for all of them from the last group to
      # finish.
      if (i != 0 and hasattr(proc[i], 'wait')):
        if i % workers is 0:
            for j in range(workers):
                proc[i - j].wait()
                  
    # Make sure all workers are finished.
    for i in range(jobs):
      if hasattr(proc[i], 'wait'):
        if proc[i].wait() is not 0:
          grass.fatal(_('Problem running analysis on evel_' + str(i) + '.'))
  
    return(proc) # end of multi thread function
    
# define each grass command as callable python function
def grassWrappers(i, points, xs, ys, vs1Dist, vs2Dist, targHeights,obHeight,envs,
                  smenvs, lbBearings,funclabel,stat,outdir,mask_val,name):
  
  func = grass.start_command("r.viewshed", overwrite=True, input=dem, \
                  output='vs_one_' + str(i), coordinate=str(xs[i]) + ',' + str(ys[i]), \
                  max_distance=vs1Dist, observer_elevation=0, \
                  target_elevation=0,
                  flags='c',
                  env=envs[i])
  return(func)

# number of long barrow coordinates provided
jobs = len(sysargs['ys'])
                    
multiWorkerFunc(jobs, 'calculating viewsheds 1...', grassWrappers, sysargs, 'vs1')

                      
