#Examples for Dimension reduction
#Joshua Hess

import pandas as pd
import numpy as np
#Import custom module
import DimensionReductionPlots as reduce

#Set home directory
home_dir = Path("/Users/reevesteammm2/Desktop")
os.chdir(home_dir)

dir = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction"
channels = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction/Columns to Keep.txt"
grp_file = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction/Group Assignment.txt"
test = MultiReadFCS(dir = dir, channels = channels,groups=grp_file,subsample=0.05)
redu = ReduceDimensions(test,method="umap",transform = "arcsinh")
#Set the channels that you want to plot with
channels = list(redu["TransformedData"].columns.values)
channels = channels[:4]
GridPlotDimRed(redu,channels = channels,grid_shape = (2,2))
GridPlotDimRed(redu,sampleID=True)
