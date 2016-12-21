
# coding: utf-8

# In[13]:

import sys
import itertools
from math import sqrt
from operator import add
from os.path import join, isfile, dirname
import math
from pyspark import SparkConf, SparkContext
from pyspark.mllib.recommendation import ALS
from pyspark.mllib.linalg import Vectors
from pyspark.mllib.linalg import SparseVector
from pyspark.mllib.clustering import LDA, LDAModel
from sklearn.neighbors import NearestNeighbors
from pyspark.mllib.tree import RandomForest, RandomForestModel
from pyspark.mllib.util import MLUtils
from pyspark.mllib.regression import LabeledPoint
import textwrap
from datetime import date, datetime
import pyproj
from math import cos, sin
import matplotlib.pyplot as plt
from pyspark.mllib.clustering import GaussianMixture, GaussianMixtureModel
import numpy as np
from pyspark.mllib.stat import MultivariateGaussian
from pyspark.mllib.linalg import DenseMatrix
from pyspark.mllib.evaluation import BinaryClassificationMetrics
from datetime import datetime

#sc = SparkContext("local[2]", "randomforesttaxi")
#sc = SparkContext("spark://104.196.46.30:7077", "randomforesttaxi")
sc = SparkContext("yarn", "randomforesttaxii")
# In[2]:

moment = sys.argv[1] # 'finished' # 'before' 'during'

if len(sys.argv) != 3:
    print("usage: spark-submit apply-random-forest.py before 100")
    exit(0)

nrfTree = int (sys.argv[2])
print("nrfTree: " + str(nrfTree))

print("moment: " + moment)
#f = sc.textFile("file:///bgdt/yellow.2000000.csv") # file:// for local files (default hdfs)
#f = sc.textFile("gs://dataproc-97276f09-d220-4be8-9ace-dae0bcad2c57-us/test/yellow.1605.csv") # file:// for local files (default hdfs)
f = sc.textFile('gs://dataproc-97276f09-d220-4be8-9ace-dae0bcad2c57-us/test/yellow.2016.halfyear.csv')
#f = sc.textFile("gs://dataproc-97276f09-d220-4be8-9ace-dae0bcad2c57-us/test/yellow.20000.csv")
# 7575962 credit card trips
#f = sc.textFile("file:///bgdt/yellow.50000.csv")
#f = sc.textFile("file:///bgdt/yellow.csv")
header = f.first()
[ str(i) + ' ' + x for i, x in enumerate(header.split(",")) ]


# In[3]:

# def hour2ts(h):
#     if h in {}:
        
#     elif

dtf = "%Y-%m-%d %H:%M:%S"  # date format

def defineProj():
    return( pyproj.Proj("+init=epsg:2908 +ellps=WGS84 +datum=WGS84 +proj=utm") )

# theta, xoffset, yoffset are decided by seeing figures
def rotateManhattanY(latm, lonm, theta=-0.496, yoffset=4239000):
    lonm, latm = defineProj()(lonm, latm)
    return( cos(theta)*latm - sin(theta)*lonm - yoffset )

def rotateManhattanX(latm, lonm, theta=-0.496, xoffset=-1635000):
    lonm, latm = defineProj()(lonm, latm)
    return( sin(theta)*latm + cos(theta)*lonm - xoffset )

def inUWS(x, y):
    return(  955 < x and x < 2283 and
            9443 < y and y < 15003 )

def inUpperTimesSquare(x, y):
    return( 1500 < x and x < 4350 and
            8000 < y and y < 9500 )

def in14smallStreets(x, y):
    return( 2900 < x and x < 3100 and
            8125 < y and y < 9225 )

# translation bounds From: http://www.spatialreference.org/ref/epsg/2908/

univ = f.filter(lambda x: x != header).map(lambda l: l.split(",")).map(lambda c:
     {        'pt': c[ 1],
       'isWeekend': int(datetime.strptime(c[1],dtf).isoweekday()>5),
                    # isoweekday() returns Monday as 1, Sundey as 7
              'ph': datetime.strptime(c[1],dtf).hour//2,
              'dt': c[ 2],
             'len': (datetime.strptime(c[2],dtf) -
                     datetime.strptime(c[1],dtf)).seconds // 60,
                    # actual trip duration in minutes
             'dst': float(c[ 4]),
              'po': float(c[ 5]),
              'pa': float(c[ 6]),
              'do': float(c[ 9]),
              'da': float(c[10]),
               '$': c[11], # payment type: 1 is credit card (tip recorded)
            'fare': float(c[12]),
              'ex': float(c[13]),
             'tip': float(c[15]),
            'toll': float(c[16]),
      })\
.filter(lambda x: x['$'] == '1' )\
.filter(lambda x: x['len'] >= 2 and x['len'] <= 120 )\
.filter(lambda x: x['fare'] > 0 )\
.filter(lambda x:  40.4700 < x['pa'] and x['pa'] <  41.3100 and
                  -74.2700 < x['po'] and x['po'] < -71.7500 and
                   x['po'] != 0 and x['pa'] != 0 )\
.map(lambda c:
     { 'isWeekend': c['isWeekend'],
              'ph': c['ph'],
             'len': c['len'],
      'isHighHpay': int((c['tip']*60/c['len'])>=12),
             'dst': c['dst'],
              'po': c['po'],
              'pa': c['pa'],
              'px': rotateManhattanX(c['pa'], c['po']),
              'py': rotateManhattanY(c['pa'], c['po']),
              'do': c['do'],
              'da': c['da'],
            'fare': c['fare'],
              'ex': c['ex'],
            'toll': c['toll']
      })\
.cache()

# can insert before .cache()

# .filter(lambda x: inUWS(x['px'], x['py']) )\
# .filter(lambda x: inUpperTimesSquare(x['px'], x['py']) )\

univ.take(1)


# In[5]:

#exemode = sys.argv[1]
exemode = 'rf'
if exemode == 'rf': # random forest
    print("random forest")
    master = univ.filter(lambda x: inUpperTimesSquare(x['px'], x['py']) )
    #nrCards = univ.count()
    #print("%d credit card trips" % (nrCards) )
else:
    print("mixture gaussian model")
    master = univ.filter(lambda x: in14smallStreets(x['px'], x['py']) )
    print("  number of rows: " + str(master.count()))


# In[6]:

#preped = univ.map(lambda l: (l['dst'], l['tip']) )

if exemode == 'rf':
    if moment == 'before': 
        print("use before")
        preped = master.map(lambda l:
                          LabeledPoint(l['isHighHpay'],
                                       [l['isWeekend'], l['po'], l['pa'], l['ph'], l['ex']]))
    elif moment == 'during':
        print("use during")
        preped = master.map(lambda l:
                          LabeledPoint(l['isHighHpay'],
                                       [l['isWeekend'], l['po'], l['pa'], l['ph'], l['ex'],
                                        l['dst'], l['toll']
                                       ]))
    else:
        print("use finished")
        preped = master.map(lambda l:
                          LabeledPoint(l['isHighHpay'],
                                       [l['isWeekend'], l['po'], l['pa'], l['ph'], l['ex'],
                                        l['dst'], l['toll'],
                                        l['fare'], l['len'], l['do'], l['da']
                                       ]))
    preped.take(1)
    
    trRDD, vaRDD = preped.randomSplit([9,1], seed=0L)
    print("  training RDD:", trRDD.take(1))
    print("validation RDD:", vaRDD.map(lambda x: x.features ).take(1))


# In[7]:

if exemode == 'rf':
    model = RandomForest.trainClassifier(trRDD, numClasses=2,
                                         categoricalFeaturesInfo = {},
                                 numTrees=nrfTree, featureSubsetStrategy="sqrt",
                                impurity="gini", maxDepth=4, maxBins=32, seed = 1)
    # gini is faster
    model.save(sc, "myRandomForestModel" + str(datetime.now().strftime("%Y%m%d%H%M%S")) )
    print("model saved")
    preds = model.predict(vaRDD.map(lambda x: x.features ))
    labelsAndPredictions = vaRDD.map(lambda lp: lp.label).zip(preds)
    testErr = labelsAndPredictions.filter(lambda (v, p): v != p).count() / float(vaRDD.count())
    print('Test Accuracy = %f percent' % (100-round(testErr,7)*100))
    # print('Learned classification forest model:')
    # print(model.toDebugString()) # messy
    
    # Note: for unfiltered data,
    # 69 -before-> 72% -finish-> 75%
else:
    # we model 14 streets between 42st and 57st
    model = GaussianMixture.train(pickup_locationRDD, k=14, convergenceTol=1e-1, seed=1234)
    


# In[11]:

if exemode == 'rf':
    nActuNotHigh = labelsAndPredictions.filter(lambda lp: lp[0] == 0 ).count()
    nActuHigh    = labelsAndPredictions.filter(lambda lp: lp[0] == 1 ).count()
    nPredNotHigh = labelsAndPredictions.filter(lambda lp: lp[1] == 0 ).count()
    nPredHigh    = labelsAndPredictions.filter(lambda lp: lp[1] == 1 ).count()
    nA0P0 = labelsAndPredictions.filter(lambda lp: lp[0] == 0 and lp[1] == 0 ).count()
    nA1P1 = labelsAndPredictions.filter(lambda lp: lp[0] == 1 and lp[1] == 1 ).count()
    nA0P1 = labelsAndPredictions.filter(lambda lp: lp[0] == 0 and lp[1] == 1 ).count()
    nA1P0 = labelsAndPredictions.filter(lambda lp: lp[0] == 1 and lp[1] == 0 ).count()
else:
    print(model.gaussians)


# In[14]:

# Instantiate metrics object
metrics = BinaryClassificationMetrics(labelsAndPredictions)

# Area under precision-recall curve
print("Area under PR = %s" % metrics.areaUnderPR)

# Area under ROC curve
print("Area under ROC = %s" % metrics.areaUnderROC)


# In[20]:

print("Baseline : %f     " % (float(max(nActuNotHigh,nActuHigh)) / float(nActuNotHigh+nActuHigh)))
precision = float(nA1P1) /float(nA1P1 + nA0P1)         # TP / (TP + FP)
recall = float(nA1P1) / float(nA1P1 + nA1P0) # TP / (TP + FN)
F1 = 2 * float(nA1P1) / float(2 * nA1P1 + nA0P1 + nA1P0)    
print("precision: %.4f recall : %.4f  F1: %.4f" % (precision, recall, F1) )

