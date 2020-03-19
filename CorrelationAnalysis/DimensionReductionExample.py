#Examples for Dimension reduction
#Joshua Hess

import pandas as pd
import numpy as np
#Import custom module
import DimensionReductionPlots as reduce

#Set home directory
Rs = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/R value.xlsx")
Ps = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/P value.xlsx")
clin_par = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/DimensionReduction/Columns to Keep.txt")

dict = reduce.ReadCorrelationResults(Rs,Ps,clin_par)
dict=reduce.ReduceDimensions(dict,method="phate")

reduce.GridPlotDimRed(dict,grid_shape=grid_shape,filename="TraumaCorrelationReduction.jpeg")
