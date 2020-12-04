#Community Detection Functions
#Joshua Hess

#Import modules
import umap
from sklearn.utils import check_random_state
import numpy as np
import igraph as ig
import pandas as pd
import networkx as nx
from scipy import sparse
import leidenalg
import louvain
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import minmax_scale
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sknetwork.hierarchy import LouvainHierarchy, BiLouvainHierarchy
from sknetwork.hierarchy import cut_straight, dasgupta_score, tree_sampling_divergence
from scipy.cluster.hierarchy import leaves_list, linkage
from sknetwork.clustering import modularity
from kneed import KneeLocator
from sklearn import metrics




def get_igraph_from_adjacency(adjacency, directed=None):
    """Get igraph graph from adjacency matrix. Function taken from scanpy utils.
    Scanpy also has options to cluster UMAP simplicial set using the leiden
    community detection algorithm.
    Here, the adjacency object is the simplicial set formed from UMAP
    Source: https://github.com/theislab/scanpy/blob/e33a2f338a4c63c794fe4bc5e03471d7c487cada/scanpy/_utils.py#L190
    """

    sources, targets = adjacency.nonzero()
    weights = adjacency[sources, targets]
    if isinstance(weights, np.matrix):
        weights = weights.A1
    g = ig.Graph(directed=directed)
    g.add_vertices(adjacency.shape[0])  # this adds adjacency.shape[0] vertices
    g.add_edges(list(zip(sources, targets)))
    try:
        g.es['weight'] = weights
    except:
        pass
    if g.vcount() != adjacency.shape[0]:
        logg.warning(
            f'The constructed graph has only {g.vcount()} nodes. '
            'Your adjacency matrix contained redundant nodes.'
        )
    return g


def RunInfomap(igraph_obj):
    """This function will perform the infomap algorithm on the connected
    graph returned by UMAP algorithm"""

    #run the infomap algorithm
    print('Running infomap clustering...')
    infmp = igraph_obj.community_infomap(edge_weights = igraph_obj.es['weight'])
    #Get group membership
    infmp_labels = np.asarray(infmp.membership)+1
    print('Finished')
    #return objects
    return infmp, infmp_labels



def RunLouvain(igraph_obj,**kwargs):
    """This function will perform the louvain clustering algorithm on the
    connected graph returned by the UMAP algorithm"""

    #Run louvain clustering
    print("Running louvain clustering...")
    louv = louvain.find_partition(igraph_obj, louvain.ModularityVertexPartition,weights = igraph_obj.es['weight'],**kwargs)
    #louv = igraph_obj.community_multilevel(weights = igraph_obj.es['weight'])
    #Get group membership
    louv_labels = np.asarray(louv.membership)+1
    print('Finished')
    #return objects
    return louv, louv_labels


def RunLeiden(igraph_obj,resolution=None,**kwargs):
    """This function will perform the leiden clustering algorithm on the
    connected graph returned by the UMAP algorithm"""

    #Run Leiden clustering
    print("Running leiden clustering...")
    #Check to see resolution
    if resolution is None:
        #Run modularity optimizer
        leid = leidenalg.find_partition(graph = igraph_obj,\
            partition_type = leidenalg.ModularityVertexPartition,\
            weights = igraph_obj.es['weight'],n_iterations = -1,**kwargs)
    #Otherwise use resolution parameter
    else:
        #Run clustering
        leid = leidenalg.find_partition(graph = igraph_obj,\
            partition_type = leidenalg.RBConfigurationVertexPartition,\
            weights = igraph_obj.es['weight'],n_iterations = -1,\
            resolution_parameter = resolution,**kwargs)
    #Get group membership
    leid_labels = np.asarray(leid.membership)+1
    print('Finished')
    #return objects
    return leid, leid_labels


def RunLeidenMultiplex(igraph1,igraph2):
    """This function will perform the leiden multiplex clustering algorithm on the
     graphs returned by the UMAP algorithm"""

    #Run Leiden clustering
    print("Running leiden clustering...")
    leid = leidenalg.find_partition_multiplex(graphs = [igraph1,igraph2],\
        partition_type = leidenalg.ModularityVertexPartition,n_iterations = -1)
    #Get group membership
    leid_labels = np.asarray(leid[0])+1
    #Extract modularity measure from clustering results
    modularity = np.asarray(leid[1])+1
    print('Finished')
    #return objects
    return leid, leid_labels, modularity


def RunRandomForestClassifier(feature_table,labels,test_set_size=0.3,num_estimators=1000,processes=-1):
    """This function will run a random forest classifier."""
    #Split data into dependent and ind. variables and split the dataframe
    x_train, x_test, y_train, y_test = train_test_split(feature_table,labels,test_size = 0.3)
    #Setup and run the Classifier
    print('Training Random Forest Classifier...')
    clf = RandomForestClassifier(n_estimators = num_estimators,n_jobs = processes).fit(x_train,y_train)
    #Predict classes for each pixel in the testing dataset
    print('Predicting for test set...')
    y_pred = clf.predict(x_test)
    #Get hold out accuracy
    hold_out_acc = metrics.accuracy_score(y_test,y_pred)
    print('Test Accuracy:',str(hold_out_acc))
    #Plot the variable importance
    features_imp = pd.Series(clf.feature_importances_,index = feature_table.columns).sort_values(ascending = False)
    #Get the probability of assignment for each class
    print('Predicting for full data...')
    full_dat_pred = clf.predict_proba(feature_table)
    print('Finished')
    random_forest = clf
    overall_imp = features_imp
    test_set = x_test
    #Return the objects
    return random_forest, overall_imp, full_dat_pred, test_set, hold_out_acc


#def GetRandomForestProbImages(self):
#    """This function will return the probability maps for your Random
#    Forest Classifier"""
#    #Get the random forest predictions for the full dataset
#    prd = np.copy(self.data.RFclassPred)
#    #Set all 0s to 0.1 to get a background in matplotlib
#    prd[prd ==0] = 0.1
#    #visualization of probabilities in the full Image as a heatmap
#    print('Exporting Images...')
#    for j in range(0,prd.shape[1]):
#        im = np.zeros((self.data.imzmldict["max count of pixels y"],\
#        data.data.imzmldict["max count of pixels x"]), dtype = np.float32)
#        for i, (x, y, z) in enumerate(self.data.coordinates):
#            im[y - 1, x - 1] = prd[i,j] #Python is 0 based so -1
#        plt.imshow(im,cmap = 'jet')
#        plt.imsave('RF_Cluster'+ str((j+1))+'.jpeg',im,cmap='jet',dpi=200)
#    print('Finished')



#def RFclusterFeatCont(self,top_k):
#    """This function will extract the median contribution for each feature for
#    each cluster in your random forest model and rank them by median"""
#    #Extract the contributions for eah prediction
#    print('Calculating Per Cluster Feature Importance...')
#    prediction, bias, contributions = ti.predict(self.data.RandomForestClassiferObj, self.data.RFtestSet)
#    #Create dictionary storing random forest predictions
#    rf_cont_full = {}
#    rf_cont_sub = {}
#    #Add the contributions to the dictionary as dataframes
#    for i in range(1,contributions.shape[2]):
#        #For each cluster, add the full contribution list
#        tmp=pd.DataFrame(contributions[:,:,i],columns=x_test.columns.values)
#        rf_cont_full['Cluster '+ str(i)] = tmp
#        #Rank the contributions by median
#        tmp_rank=pd.DataFrame(abs(rf_cont_full['Cluster '+ str(i)].median(axis=0)).sort_values(ascending = False))
#        #Extract the top k contributors by median rank in magnitude
#        tops = tmp.iloc[:top_k].index.tolist()
#        df = rf_cont_full['Cluster '+ str(i)]
#        df_tmp = df[df.columns[df.columns.isin(test)]]
#        final_cont = df_tmp.reindex(df_tmp.median(axis=0).sort_values().index, axis=1)
#        #Store the top k contributors in the final dictionary object
#        rf_cont_sub['Cluster '+ str(i)] = final_cont
#        #Return the dictionaries
#    self.data.RFperClusterCont = rf_cont_full
#    self.data.RFperClusterContTop = rf_cont_sub
#    print('Finished')
#    return self.data.RFperClusterCont,self.data.RFperClusterContTop











#
