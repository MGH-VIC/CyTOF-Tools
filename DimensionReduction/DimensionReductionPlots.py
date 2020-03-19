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



def ReadFCS(im_path,channels,subsample=None):
    """Function for reading fcs files in python for use with dimension reduction
    This function will keep the channels found in the txt file 'channels'
    """

    #Create path object for reading the fcs file
    im_path = Path(im_path)
    #Get the stem of the path to use for the ID
    id = im_path.stem
    #Read the fcs file
    fcs_dat = fcs.FCMeasurement(ID=id, datafile=im_path)
    #Create a dataframe based on the data in fcs
    fcs_frame = fcs_dat.data
    #Create path object for the channels to keep
    chan_path = Path(channels)
    #Read the channels text file
    chans = pd.read_table(chan_path)
    #Construct a channel names list
    keep = list(chans.Keep)
    #Subset the dataframe
    fcs_frame = fcs_frame[keep]

    #Check for subsampling
    if subsample is not None:
        #If subsampling, subsample without replacement
        fcs_frame = fcs_frame.sample(frac=subsample,replace=False,axis=0)
    #Create a column with the sampleID
    fcs_frame["SampleID"] = id
    #Return the subsetted data
    return fcs_frame



def MultiReadFCS(dir,groups=None,**kwargs):
    """Function for reading a directory of fcs files for plotting dimension
    reduction"""

    #Create path object for the directory
    dir = Path(dir)
    #Make sure that the directory input is a direcory
    if dir.is_dir:
        #Get the list of files in this directory that end with .fcs
        fcs_files = utils.SearchDir(ending = ".fcs",dir=dir)
        #Set up a list to concatenate results
        all_frames = []
        #Iterate through each of the files in the list and run the ReadFCS function
        for file in fcs_files:
            #Print an update
            print("Reading " + str(file) + '...')
            #Run the function
            fcs_frame = ReadFCS(im_path = file,**kwargs)
            #Check for group assignments
            if groups is not None:
                #Create path object
                grp_file = Path(groups)
                #Read the group file
                grps = pd.read_table(grp_file)
                #Get the name of the current file
                f_name = file.name
                #Create a column for the group names and add this assignment to the column
                fcs_frame["GroupID"] = grps.loc[grps['SampleID'] == f_name, 'GroupID'].values[0]
            #Otherwise, create a nonexistent group name
            else:
                fcs_grame["GroupID"] = 0
            #Update the list of dataframes
            all_frames.append(fcs_frame)
            #Print an update
            print("Finished Reading " + str(file))
        #Concatenate all frames
        fcs_frames = pd.concat(all_frames,axis=0)
        #Reset the index
        fcs_frames.reset_index(drop=True, inplace=True)
    #Return the full dataframe
    return fcs_frames


def ReduceDimensions(data,method="umap",transform = "arcsinh",**kwargs):
    """Function for reducing the dimensionality of data using tSNE or UMAP"""

    #Create a dictionary to store the results in
    return_dict = {}
    #Add the column of sampleID to the return dictionary
    return_dict.update({"SampleID":data["SampleID"],"GroupID":data["GroupID"]})
    #Remove the sampleIDs for the dimension reduction
    data = data.drop(columns = ["SampleID","GroupID"])
    #Check to see if transforming the data
    if transform == "arcsinh":
        #Transform the data with arcsinh cofactor of 5
        dat = data/5
        dat = np.arcsinh(dat)
        #Add the original data to your object
        return_dict.update({"TransformedData":dat, "Data":data})
    else:
        dat = data
        #Add the original data to your object
        return_dict.update({"TransformedData":None, "Data":data})

    #If running umap, run umap
    if method == "umap":
        obj = umap.UMAP(**kwargs).fit(data)
        embedding = obj.embedding_
        #Update the Dictionary
        return_dict.update({"umap":obj,"embedding":embedding})
    #If running phate, run phate
    elif method == "phate":
        obj = phate.PHATE(**kwargs).fit(data)
        embedding = obj.transform()
        #Update the Dictionary
        return_dict.update({"phate":obj,"embedding":embedding})
    #If running tsne, run tsne
    elif method == "tsne":
        obj = TSNE(**kwargs).fit(data)
        embedding = obj.embedding_
        #Update the Dictionary
        return_dict.update({"tsne":obj,"embedding":embedding})

    #Add the original data to your object
    return_dict.update({"TransformedData":dat})
    #Now return the dictionary
    return return_dict




def GridPlotDimRed(results,grid_shape=None,channels=None,sampleID=False,groupID=False,**kwargs):
    """Function for plotting grid of dimension reduction results
    grid_shape: tuple defining the dimensions of the grid for plotting"""

    #Check if the sample ID is none...plot by channels
    if channels is not None:
        #Get a size for the figure (Results have shown 4 times size of grid is good)
        fig_size = (grid_shape[1]*4,grid_shape[0]*4)
        #Get the number of plots
        fig, axs = plt.subplots(grid_shape[0], grid_shape[1], figsize = fig_size)
        #Iterate through the channels and plot the results in the grid
        for chan, ax in zip(channels, axs.ravel()):
            #Check to see if there exists transformed data
            if results["TransformedData"] is not None:
                #Create the colormap
                cm = results["TransformedData"][chan]
            else:
                #Create colormap based on the untransformed data object
                cm = results["Data"][chan]
            # Fewer bins will result in "coarser" colomap interpolation
            im = ax.scatter(results["embedding"][:,0],results["embedding"][:,1],c=cm,cmap='magma',s=0.5,**kwargs)
            ax.set_title(chan)
            fig.colorbar(im, ax=ax)
    #Otherwise, plot by sample
    elif sampleID:
        #Get the sampleid
        cm = list(results["SampleID"].values)
        # Fewer bins will result in "coarser" colomap interpolation
        fig, ax = plt.subplots()
        fig.set_size_inches(8, 8)
        g = sns.scatterplot(results["embedding"][:,0],results["embedding"][:,1],hue=cm)
        _,hands = g.get_legend_handles_labels()
        g.legend(hands,loc="upper left",bbox_to_anchor=(-0.6, 1))
        plt.show()
    #Otherwise, plot by group
    elif groupID:
        #Get the sampleid
        cm = list(results["GroupID"].values)
        # Fewer bins will result in "coarser" colomap interpolation
        fig, ax = plt.subplots()
        fig.set_size_inches(8, 8)
        g = sns.scatterplot(results["embedding"][:,0],results["embedding"][:,1],hue=cm)
        _,hands = g.get_legend_handles_labels()
        g.legend(hands,loc="upper left",bbox_to_anchor=(-0.6, 1))
        plt.show()









#
