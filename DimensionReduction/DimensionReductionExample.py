#Examples for Dimension reduction
#Joshua Hess

import pandas as pd
import numpy as np
#Import custom module
import DimensionReductionPlots as reduce

#Set home directory
home_dir = Path("/Users/reevesteammm2/Desktop")
os.chdir(home_dir)

#Where are your fcs files located
dir = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction"
#enter text file with channels to keep
channels = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction/Columns to Keep.txt"
#Enter file that indicates group assignment
grp_file = "/Volumes/viclab$/Reeves Team/Josh Hess/gists/CytofTools/DimensionReduction/Group Assignment.txt"
#Read fcs files
test = MultiReadFCS(dir = dir, channels = channels,groups=grp_file,subsample=0.05)
#Reduce data dimensionality (Optional to include transformation)
redu = ReduceDimensions(test,method="umap",transform = "arcsinh")
#Set the channels that you want to plot with
channels = list(redu["TransformedData"].columns.values)
channels = channels[:4]
#Plot the dimension reduction results
GridPlotDimRed(redu,channels = channels,grid_shape = (2,2))
GridPlotDimRed(redu,sampleID=True)
