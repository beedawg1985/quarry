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


def addOverviews(file):
    my_print('Adding overviews...')
    InputImage = file
    Image = gdal.Open(InputImage, 1) # 0 = read-only, 1 = read-write.
    gdal.SetConfigOption('COMPRESS_OVERVIEW', 'DEFLATE')
    Image.BuildOverviews("AVERAGE", [2,4,8,16,32,64])
    del Image
    my_print('Overviews added.')


# splits rasters into contiguous clumps reducing raster extent and no-data
# no longer needed as same resolution products are now merged pre-processing
def clumpSplitSvfc(rasterIn, basepath, basename, tag):
    grass.run_command('g.region', \
    zoom=rasterIn,\
    res='50')
    grass.raster.mapcalc(str('tempras_bin = if(isnull({0}),null(),1)' 
    .format(rasterIn) ), overwrite=True)
    grass.run_command('r.clump',\
    overwrite=True,\
    input='tempras_bin',\
    output='tempras_clump',\
    minsize='10')
    clumps = int(grass.read_command('r.info',\
    map='tempras_clump',\
    flags='r').decode().split('\n')[1].split('=')[1])
    my_print('total clumps detected...' + str(clumps))
    for c in range(1,clumps+1):
    # for c in [1,3]:
      my_print('calculating sky view factor using multi for clump number...' + str(c))
      # create rast of single clump
      grass.run_command('g.region', \
      raster=rasterIn,
      res='50')
      grass.raster.mapcalc(str('clump_mask = if (tempras_clump == {0}, 1, null())' \
      .format(c) ), overwrite=True)
      # get res of original raster
      grass.run_command('g.region', \
      raster=rasterIn)
      # zoom to bounds of new, single clump raster map
      grass.run_command('g.region', \
      zoom='clump_mask')
      # output masked section of original map
      grass.run_command('r.mask',\
                      raster='clump_mask',
                      overwrite=True)
      try:
          grass.run_command('r.skyview.multi', \
                            overwrite=True,\
                            input=rasterIn,\
                            output='clump' + str(c) + '_tempras_svf',\
                            ndir='8',\
                            maxdistance='3',
                            colorized_output='clump' + str(c) + '_tempras_svfc',\
                            color_source='aspect')
      except:
          my_print('multi skyview failed...')
          my_print('calculating sky view factor using single thread...')
          grass.run_command('r.skyview', \
                            overwrite=True,\
                            input=rasterIn,\
                            output='clump' + str(c) + '_tempras_svf',\
                            ndir='8',\
                            maxdistance='3',
                            colorized_output='clump' + str(c) + '_tempras_svfc',\
                            color_source='aspect')
    allClump = grass.read_command('g.list',\
                                  type='rast',\
                                  pattern='^clump')
    grass.run_command('r.mask',\
                      overwrite=True,\
                      flags='r')
    grass.run_command('g.region', \
                      zoom=rasterIn)
    grass.run_command('r.mosaic',\
                      input=allClump,\
                      output='allclump_temp')
    tifOutOver('allclump_temp', basepath, basename, 'svfc', tag)
    return()
    
def groupSplitSvfc(rasterIn, basepath, basename, tag):
    vecGroups = grass.read_command('v.db.select',
                                  map='tempvec',
                                  columns='group_num')
    vecText = vecGroups.split('\n')[1:-1]
    vecGroups = list(set(vecText))
    # vecGroups = ['1','5', '3'] # testing
    my_print('total number of groups... ' + str(len(vecGroups)))
    for v in vecGroups:
      # copy just vectors within group to new map
      my_print('calculating skyview factor for group ...' + v)
      grass.run_command('v.extract',
                          input='tempvec',
                          where='group_num == ' + v,
                          output='tempvecgroup',
                          overwrite=True)
      grass.run_command('g.region',
                        vector='tempvecgroup')
      try:
          grass.run_command('r.skyview.multi', \
                            overwrite=True,\
                            input=rasterIn,\
                            output='vecgroup_' + v + '_svf',
                            ndir='8',\
                            maxdistance='3',
                            colorized_output='vecgroup_' + v + '_svfc',\
                            color_source='aspect')
      except:
          my_print('multi skyview failed...')
          my_print('calculating sky view factor using single thread...')
          grass.run_command('r.skyview', \
                            overwrite=True,\
                            input=rasterIn,\
                            output='vecgroup_' + v + '_svf',
                            ndir='8',\
                            maxdistance='3',
                            colorized_output='vecgroup_' + v + '_svfc',\
                            color_source='aspect')
    
    allClump = grass.read_command('g.list',\
                                  type='rast',\
                                  pattern='*svfc$').split('\n')[:-1]
    allSvfc = ','.join(allClump)
    my_print('filtered rasters')
    my_print(allSvfc)
    
    grass.run_command('r.mask',\
                      overwrite=True,\
                      flags='r')
    grass.run_command('g.region', \
                      rast=rasterIn)
    my_print('compiling mosaic of groups ...')
    grass.run_command('r.patch',\
                      input=allSvfc,\
                      output='allclump_temp',
                      overwrite=True)
    tifOutOver('allclump_temp', basepath, basename, 'svfc', tag)
    return()


def gdalSieve(sieveCells, basepath, basename, dtype, tag):
    my_print('sieving raster using ' + str(sieveCells) + ' cells')
    outputName = basepath + '/' + dtype + '/' +\
        basename[:-4] + '_'+ dtype + '_' + tag + '.tif'
    cmd = 'gdal_sieve.py -st '+ str(sieveCells) + ' -4 -of GTiff '+\
    outputName
    subprocess.call(cmd,shell=True)

def tifOutOver(rasterIn, basepath, basename, dtype, tag, \
btype='Float32', expandrgba=False):
    # output masked section of original map
    my_print('exporting ' + rasterIn + '...')
    grass.run_command('g.region',
    raster=rasterIn)
    grass.run_command('g.region',
    zoom=rasterIn)
    outputName = basepath + '/' + dtype + '/' +\
                  basename[:-4] + '_'+ dtype + '_' + tag + '.tif'
    grass.run_command('r.out.gdal',\
                  input=rasterIn,\
                  format='GTiff',\
                  type=btype,\
                  createopt='compress=LZW,'+ \
                  'TILED=YES,BLOCKXSIZE=512,BLOCKYSIZE=512',\
                  output=outputName,\
                  overwrite=True,
                  flags='f')
    # required for rgb pca shaded hillshade images
    if expandrgba == True:
        # outputName = 'my_tiff.tif'
        cmd = 'pct2rgb.py -of GTiff -rgba ' + \
        outputName + ' ' + outputName[:-4] + '_rgba.tif'
        my_print(cmd)
        os.system(cmd)
        cmd = 'gdal_translate '+ outputName[:-4] + '_rgba.tif ' + \
        outputName + ' -co compress=LZW -co TILED=YES '+\
        '-co BLOCKXSIZE=512 -co BLOCKYSIZE=512'
        my_print(cmd)
        os.system(cmd)
        cmd = 'rm '+ outputName[:-4] + '_rgba.tif '
        os.system(cmd)
    addOverviews(outputName)


def makeSLRM(demloc, buffloc, res, basepath, tag, whichp):
    my_print('Running slrm_py2_v2.5.py...')
    # parse which products into dictionary
    whichpd = json.loads(whichp)
    if "elev" not in whichpd:
      whichpd['elev'] = True
    if "lrms" not in whichpd:
      whichpd['lrms'] = True
    if "slope_aspect" not in whichpd:
      whichpd['slope_aspect'] = True
    if "spcahs" not in whichpd:
      whichpd['spcahs'] = True
    if "svfc" not in whichpd:
      whichpd['svfc'] = True
    my_print('listing which visualisation processes to be undertaken...'+'\n')
    for key, value in whichpd.items():
        print("Processing and outputting of {} is set to {}".format(key, value))
    
    pathName = ntpath.basename(ntpath.dirname(ntpath.dirname(demloc)))
    tifName = ntpath.basename(demloc)
    buffName = ntpath.basename(buffloc)[:-5]
    basename = pathName + '_' + tifName
    my_print('set projection...')
    grass.run_command('g.proj', \
                      epsg=27700,\
                      flags='c')
    # load data
    my_print('importing VRT... ' + demloc)
    grass.run_command('r.in.gdal', \
                      input=demloc,\
                      output='tempras',\
                      flags='o',\
                      overwrite=True)
    grass.run_command('g.region',\
                      raster='tempras')
    grass.run_command('r.info',\
                      map='tempras')
    # calc distances in map units
    dist = [6, 10, 18, 30]
    resFloat = float(grass.read_command('r.info',\
                       map='tempras',
                       flags='g').decode().split('\n')[5][6:])
    neighs = [i / resFloat for i in dist]
    neighsClean = [i + 1 if (i % 2) == 0 else i for i in neighs]
    neighsStr = [str(int(i)) for i in neighsClean]
    my_print('importing vector... ' + buffloc)
    grass.run_command('v.in.ogr', \
                      input=buffloc,\
                      # layer=buffName,\
                      output='tempvec',\
                      flags='o',\
                      overwrite=True)
    my_print('applying vector mask... ')
    grass.run_command('r.mask',\
    vector='tempvec',\
    overwrite=True)
    # output elev
    if whichpd['elev'] == True:
      my_print('exporting elevation maps... ')
      tifOutOver('tempras', basepath, basename, 'elev', tag)
    
    # local relief
    if whichpd['lrms'] == True:
      my_print('calculating local reliefs...')
      for i, neigh in enumerate(neighsStr):
          my_print('running local relief for... ' +
          res + ' res at neighbourhood distance ' + str(dist[i]) +\
          ' meters')
          grass.run_command('g.region',\
                            zoom='tempras')
          grass.run_command('r.local.relief', \
                            overwrite=True,\
                            input='tempras',\
                            output='tempras_lrm',\
                            neighborhood_size=neigh)
          grass.raster.mapcalc(str('rounded = round(tempras_lrm,0.0001)'),\
                            overwrite=True)
          grass.run_command('r.colors',\
                            map='rounded',\
                            color='grey',\
                            flags='e')
          grass.run_command('g.region',\
                            zoom='rounded')
          tifOutOver('rounded', basepath, basename, 
          'lrm_dist_' + str(dist[i]) + 'm', tag)
          
    if whichpd['spcahs'] == True:
      # shaded pca
      my_print('shaded pca...')
      grass.run_command('r.shaded.pca', \
                        overwrite=True,\
                        input='tempras',\
                        output='tempras_pca',
                        nprocs='6',
                        zscale='100',
                        nazimuths='16')
      my_print('exporting spcahs maps... ')
      tifOutOver('tempras_pca', basepath, basename, 'spcahs', tag, 'UInt16',
      expandrgba=True)
    
    if whichpd['slope_aspect'] == True:
      # slope and aspect 
      my_print('slope and aspect...')
      grass.run_command('r.slope.aspect', \
                        overwrite=True,\
                        elevation='tempras',\
                        slope='tempras_slope',
                        aspect='tempras_aspect')
      my_print('reclassifying aspect maps... ')
      grass.raster.mapcalc(str('reclass = if(tempras_aspect < 270, if(tempras_aspect != 0, 270 - tempras_aspect),' + \
        'if(tempras_aspect > 270 , 630 - tempras_aspect,0))'),\
        overwrite=True)
      grass.raster.mapcalc(str('bearing = if(reclass < 180, -(180-reclass), reclass-180 )'),\
      overwrite=True)
      my_print('exporting slope and aspect maps... ')
      tifOutOver('tempras_slope', basepath, basename, 'slope', tag)
      tifOutOver('bearing', basepath, basename, 'aspect', tag)
    
    # sky view factor
    if whichpd['svfc'] == True:
      grass.run_command('r.skyview.multi', \
                            overwrite=True,\
                            input='tempras',\
                            output='svf',
                            ndir='8',\
                            maxdistance='3',
                            colorized_output='svfc',\
                            color_source='aspect')
      tifOutOver('svfc', basepath, basename, 'svfc', tag)
    
    
    return(tag)


makeSLRM(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4], sys.argv[5],sys.argv[6])
