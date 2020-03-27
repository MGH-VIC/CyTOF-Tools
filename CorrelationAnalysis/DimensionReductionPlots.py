#Functions for conducting dimension reduction and plotting
#Joshua Hess

from MulticoreTSNE import MulticoreTSNE as TSNE
import umap
import phate
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import FlowCytometryTools as fcs
from pathlib import Path
import os
import seaborn as sns
#Import custom modules
import utils





def ReadCorrelationResults(Rs,Ps,clin_par,group_data=None):
    """Function for reading correlation results from R and plotting tSNE"""

    #Create a dictionary to return
    return_dict = {}
    #Read each file
    rs = pd.read_excel(Rs)
    ps = pd.read_excel(Ps)
    #Check if cluster assignment
    if group_data is not None:
        #Get the clusters from clustering correlation matrix
        modules = pd.read_csv(group_data,sep="\t")
        #Rename the group column of group data to be that for Modules
        modules = modules.rename(columns={"group":"Module"})
        #Remove the prefix "group" in the column of modules
        modules["Module"] = modules['Module'].str.replace('group_', '')
        #Update the dictionary
        return_dict.update({"Modules":modules})
    #Rename the first column
    rs=rs.rename(columns = {'Unnamed: 0':'Cluster ID'})
    ps=ps.rename(columns = {'Unnamed: 0':'Cluster ID'})
    chans = pd.read_csv(clin_par)
    #Get the list of columms to keep
    keep_og = list(chans.Keep)
    keep = keep_og + ["Cluster ID"]
    #Subset the dataframe columns and then rows
    rs_keep = rs[keep]
    rs_keep = rs_keep.loc[~rs_keep["Cluster ID"].isin(keep)]
    ps_keep = ps[keep]
    ps_keep = ps_keep.loc[~ps_keep["Cluster ID"].isin(keep)]
    #Update the dictionary
    return_dict.update({"R":rs_keep,"P":ps_keep,"Columns":keep_og})
    #Return the dataframes
    return return_dict



def ReduceDimensions(dict,method="umap",transform = "arcsinh",**kwargs):
    """Function for reducing the dimensionality of data using tSNE or UMAP"""

    #Get the R value data
    data = dict["R"]
    #Add the column of sampleID to the return dictionary
    dict.update({"Cluster ID":data["Cluster ID"]})
    #Remove the sampleIDs for the dimension reduction
    data = data.drop(columns = ["Cluster ID"])

    #If running umap, run umap
    if method == "umap":
        obj = umap.UMAP(**kwargs).fit(data)
        embedding = obj.embedding_
        #Update the Dictionary
        dict.update({"umap":obj,"embedding":embedding})
    #If running phate, run phate
    elif method == "phate":
        obj = phate.PHATE(**kwargs).fit(data)
        embedding = obj.transform()
        #Update the Dictionary
        dict.update({"phate":obj,"embedding":embedding})
    #If running tsne, run tsne
    elif method == "tsne":
        obj = TSNE(**kwargs).fit(data)
        embedding = obj.embedding_
        #Update the Dictionary
        dict.update({"tsne":obj,"embedding":embedding})

    #Add the original data to your object
    dict.update({"Data":data})
    #Now return the dictionary
    return dict


def GridPlotDimRed(dict,filename,grid_shape,**kwargs):
    """Function for plotting grid of dimension reduction results
    grid_shape: tuple defining the dimensions of the grid for plotting"""

    #Get the number of channels
    channels = dict["Columns"]
    #Get the type of embedding
    if "phate" in dict.keys():
        x_name = "PHATE 1"
        y_name = "PHATE 2"
    elif "umap" in dict.keys():
        x_name = "UMAP 1"
        y_name = "UMAP 2"
    else:
        x_name = "t-SNE 1"
        y_name = "t-SNE 2"
    #Get a size for the figure (Results have shown 4 times size of grid is good)
    fig_size = (grid_shape[1]*8,grid_shape[0]*8)
    #Get the number of plots
    fig, axs = plt.subplots(grid_shape[0], grid_shape[1], figsize = fig_size)
    #Iterate through the channels and plot the results in the grid
    for chan, ax in zip(channels, axs.ravel()):
        #Get the pvalues
        pval = dict["P"][chan].values
        #Transform the pval
        pval_log = -np.log10(pval)
        #Replace inf with zeros if there is an issue
        pval_log[pval_log == np.inf]=0.1
        #Get the R value
        cm = dict["R"][chan]
        # Fewer bins will result in "coarser" colomap interpolation
        im = ax.scatter(dict["embedding"][:,0],dict["embedding"][:,1],c=cm,cmap='magma',s=pval_log*60,vmin=-1, vmax=1,**kwargs)
        ax.set_xlabel(x_name,labelpad=2.5,fontsize=16)
        ax.set_ylabel(y_name,labelpad=0.3,fontsize=16)
        ax.tick_params(labelsize=14)
        #Replace the periods in the channel name with a space
        ax.set_title(chan.replace("."," "))
        fig.colorbar(im, ax=ax)

        for area in [0.1, 1.0, 3.0]:
            ax.scatter([], [], c='k', alpha=0.3, s=area*60,label=str(area))
        ax.legend(scatterpoints=1, frameon=False, labelspacing=0.3, title='-log10(p-value)',loc="upper left",prop={'size': 15})
    #Save the plots
    plt.savefig(filename,dpi=400)

















#
