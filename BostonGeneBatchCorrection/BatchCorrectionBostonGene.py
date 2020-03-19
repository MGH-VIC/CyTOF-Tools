#Batch correction script from Boston Gene modified to suit VIC needs
#Joshua Hess

#Import modules
import pandas as pd
import fcsparser
import numpy as np
import pandas as pd
import os
from MulticoreTSNE import MulticoreTSNE as TSNEmc
import matplotlib.pyplot as plt
import seaborn as sns
from copy import deepcopy

#Import new modules - JH
from pathlib import Path
import utils
import fcswrite



#Function for loading fcs files
def fcs_load(fcs_path,columns_to_keep=None):
    """
    load FCS-file with rename Di to gene_name & arcsin/5 transformation
    return b(Dataframe) data_col_converter(pd.Series)
    :param fcs_path: path to .fcs file
    :param cofactor: arcsin coeff
    :return:
    """

    #Parse the fcs file
    a, b = fcsparser.parse(fcs_path, reformat_meta=True)
    #Get the channels of fcs frame
    data_col_converter = pd.Series(list(a['_channels_']['$PnS'].values), index=list(b.columns))
    #Get the marker after the '_' in the names if there is one
    data_col_converter = data_col_converter.apply(lambda x: str(x)).apply(lambda x: x[x.find('_') + 1:])
    #Get the data column name
    data_col_converter = data_col_converter.loc[~data_col_converter.apply(lambda x: str(x)).apply(
        lambda x: (len(x) == 0) | (x.startswith(tuple([str(i) for i in range(10)]))) | (x == 'None'))]
    #Rename the columns
    columns_rename = [x for x in b.columns if not (x.endswith('Di'))] + list(data_col_converter)
    #Concatenate the data
    b = pd.concat([b.loc[:, [not (x.endswith('Di')) for x in b.columns]], b.loc[:, data_col_converter.index]], axis=1)
    #Rename the columns
    b.columns = columns_rename

    #Check to see if manually removing columns from the fcs files
    if columns_to_keep is not None:
        #Create pathlib object
        columns_to_keep = Path(columns_to_keep)
        #Read the excel file
        cols_keep = pd.read_excel(columns_to_keep)
        #Get the single column we need
        cols_keep = cols_keep["Columns to Keep"].dropna()
        #Remove the metal from the name
        new_keep = [''.join(x.split('_',1)[-1]) for x in cols_keep]
        #Use the new names to extract the wanted columns
        b = b[b.columns.intersection(new_keep)]
    #Create an event number column
    if not 'Event #' in b.columns:
        #Create the column (add 1 because python is 0 indexed)
        b['Event #'] = b.index + 1
    #Fix the index of the dataframe to have the sample name included
    b.index = b.loc[:,'Event #'].apply(lambda x: os.path.basename(fcs_path).split('.fcs')[0]+"_"+str(int(x)))
    #Return the new data frame and the columns
    return b, data_col_converter


#Function for quantile
def quantiletranslate(channel, noise_threshold=20, quantile=0.95):
    """Function for calculating quantiles for each channel"""

    #Get the quantile for the channel
    channel_for_quantile = channel.loc[channel > noise_threshold]
    #if channel has no entry add na
    if channel_for_quantile.shape[0] < 1:
        result = np.nan
    else:
        #Apply the quantile to the dataset
        result = np.quantile(channel_for_quantile, quantile)
    #Return the results
    return result


def MultiFCSLoad(list_of_files,columns_to_keep=None):
    """Function for reading a list of datafiles for Boston Gene
    fcs load function"""

    #Create dictionaries to store the objects
    data = {}
    data_converter = {}
    #Iterate through the list of files
    for fcs_file in list_of_files:
        #Create os path object because Boston Gene uses it
        fcs_path = str(fcs_file)
        #Load this file
        dat, conv = fcs_load(fcs_file, columns_to_keep)
        #Update the data dictionary
        data.update({os.path.basename(fcs_path).split('.fcs')[0]:dat})
        #update the data converted dictionary
        data_converter.update({os.path.basename(fcs_path).split('.fcs')[0]:conv})
    #Return the two dictionaries
    return data, data_converter, list_of_files



def MultiNormalize(data,data_converter,list_of_files,cofactor=5):
    """Function for normalizing data from Boston Gene functions"""

    #Create a dictionary with untransformed data
    data_nontransformated = {}
    #Iterate through the samples
    for key in data.keys():
        #Update the dictionary
        data_nontransformated.update({key:deepcopy(data[key])})
    #Arcsinh transform the data with cofactor of 5
    for sample in data_nontransformated.keys():
        data_nontransformated[sample].loc[:, [x for x in data_nontransformated[sample].columns if x not in ['Time',\
         'Event_length', 'Center', 'Offset', 'Width', 'Residual','Event #']]] = np.arcsinh(
            data_nontransformated[sample].loc[:, [x for x in data_nontransformated[sample].columns if x not in ['Time', \
            'Event_length', 'Center', 'Offset', 'Width', 'Residual','Event #']]] / cofactor)

    #Now normalize the data contrary to the above dataset which is not transformed
    translate_table = pd.concat([data[sample].apply(lambda x: quantiletranslate(x)) for sample in data.keys()], axis=1)
    #Get the columns
    translate_table.columns = [str(x) for x in data.keys()]
    #Calculate the means
    translate_means= translate_table.T.mean()
    #Iterate through the translate table indices
    for x in list(translate_table.index):
        translate_table.loc[x, :] = translate_table.loc[x, :].fillna(translate_means.loc[x])
    #Create a list for the input list of markers
    genelist = list(data[list(data.keys())[0]].columns.difference(['Time',\
     'Event_length', 'Center', 'Offset', 'Width', 'Residual','Event #']))
    #merge the counts from the files in the dataset
    merge_counts = list(data.keys())
    #Iterate through each sample
    for sample in data.keys():
        print('Normalizing ',sample)
        for gene in genelist:
            data[sample].loc[:, gene] = (data[sample].loc[:,gene].clip(0,translate_table.loc[gene,sample]) / translate_table.loc[gene,sample]) * translate_means.loc[gene]
    #Arcsinh transform the datasets
    for sample in data.keys():
        data[sample].loc[:, [x for x in data[sample].columns if x not in ['Time', 'Event_length', 'Center', 'Offset', 'Width', 'Residual',
           'Event #']]] = np.arcsinh(
            data[sample].loc[:, [x for x in data[sample].columns if x not in ['Time', 'Event_length', 'Center', 'Offset', 'Width', 'Residual',
           'Event #']]] / cofactor)
    #print an update
    print('Finished Normalizing Directory')
    #Return the nontransformed and transformed data
    return data, data_nontransformated



def CommandBatchCorrection(folder_dir,output,columns_to_keep=None,cofactor=5,export_means=False):
    """Wrapper function for running the full Boston Gene
    batch correction pipeline"""

    #Create a pathlib object for the folder direectory
    folder_dir = Path(folder_dir[0])
    #retrieve a list of the fcs files in the directory
    list_of_files = utils.SearchDir(ending='.fcs',dir=folder_dir)
    #Load the fcs files
    data,data_converter,_ = MultiFCSLoad(list_of_files,columns_to_keep[0])
    #Normalize the data
    data, data_nontransformated = MultiNormalize(data,data_converter,list_of_files,cofactor)
    #Set the output directory
    output = Path(output[0])
    #Iterate through each file and export a new fcs file
    no_corr_means = []
    corr_means = []
    for key in data:
        #Get the key and create a new filename
        fcs_name = os.path.join(str(output),(key+"_BatchCorrected"+".fcs"))
        #Get the channel names
        chans = list(data[str(key)].columns.values)
        #Get the numpy array
        dat = data[str(key)].values
        #Write the fcs
        fcswrite.write_fcs(filename=fcs_name,chn_names=chans,data=dat)
        #Print Update
        print('Finished writing '+ str(fcs_name))
        #Check to see if exporting summary means
        if export_means:
            #Get the mean of each column in the corrected frame
            mean_corr = pd.DataFrame(data[key].mean(),columns=[str(key)])
            #Get the mean of each column in the non corrected frame
            no_corr_mean = pd.DataFrame(data_nontransformated[key].mean(),columns=[str(key)])
            #Update the lists
            no_corr_means.append(no_corr_mean)
            corr_means.append(mean_corr)
    #Check if there is data in the lists and export if yes
    if len(corr_means)==len(no_corr_means):
        #Check if empty
        if len(corr_means)>0:
            #Concatenate the list of dataframes
            corr_means = pd.concat(corr_means,axis=1)
            no_corr_means = pd.concat(no_corr_means,axis=1)
            #Export the two datatables to an excel file
            with pd.ExcelWriter(Path(os.path.join(str(output),'BatchCorrectionMeansComparison.xlsx'))) as writer:
                corr_means.to_excel(writer, sheet_name='BatchCorrected')
                no_corr_means.to_excel(writer, sheet_name='NotCorrected')
    #Print another update
    print('Finished Batch Correction')




















#
