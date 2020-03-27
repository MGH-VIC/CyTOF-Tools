#Examples for Dimension reduction
#Joshua Hess

import pandas as pd
import numpy as np
from pathlib import Path
import seaborn as sns
from sklearn.cluster import SpectralClustering
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
#Import custom module
import DimensionReductionPlots as reduce


#------------------Reducing correlation matrix dimension and exporting plots-----------
#Set home directory
group_data = None
#Path to R values
Rs = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/R value.xlsx")
#Path to p-values
Ps = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/P value.xlsx")
#Path to columns to keep
clin_par = Path("/Volumes/viclab$/Raju Paul/Josh Hess/Trauma Mendoza/Trauma Correlation/Live Cells (No Ref) 20000per k=22 c84 (Clusters 0.05)/DimensionReduction/Columns to Keep.txt")

dict = reduce.ReadCorrelationResults(Rs,Ps,clin_par,group_data)
dict=reduce.ReduceDimensions(dict,method="umap",metric="euclidean",n_components=2,random_state=22)
reduce.GridPlotDimRed(dict,filename="TraumaCorrelationReduction.jpeg",grid_shape=(7,5)) #Change grid shape depending on number of clinical parameters (Ex: (7,5)=35 panels)

#----------------Clustering the correlation matrix-------------------
pca = PCA(n_components=25,random_state=22).fit(dict['umap'].graph_.toarray())
#Get the explained variance ratio from the pca object
fig, ax = plt.subplots(figsize=(10,7)) #Change figure size if you need
ax.plot(np.cumsum(pca.explained_variance_ratio_),linewidth=4) #Change line width for the plot if you need
ax.set_xlabel('Number of Components',fontsize=15,labelpad=5)
ax.set_ylabel('Cumulative Explained Variance',fontsize=15,labelpad=5)
ax.tick_params(labelsize=15)
ax.set_title("Eigenvalue Decomposition of UMAP Affinities",fontsize=18)
plt.savefig("UMAP_SpectralPCA.jpeg",dpi=400)

#Use the elbow point of PCA for determining number of clusters for spectral embedding and clustering
clustering = SpectralClustering(n_clusters=8,affinity="precomputed",random_state=22).fit(dict['umap'].graph_)
colors = {1:"red",2:"blue",3:"green",4:"brown",5:"pink",6:"purple",7:"orange",8:"black"}
new_labs = pd.DataFrame(clustering.labels_+1,columns=["Module"])

#---------------Plot the dimensin reduction result with cluster labels---------
scatter_x = dict["embedding"][:,0]
scatter_y = dict["embedding"][:,1]
fig, ax = plt.subplots(figsize=(7,8))
for g in np.unique(new_labs['Module']):
    ix = np.where(new_labs['Module'] == g)
    im = ax.scatter(scatter_x[ix],scatter_y[ix],c=colors[g],label=g,s=60)
    ax.set_xlabel("UMAP 1",labelpad=2.5,fontsize=16)
    ax.set_ylabel("UMAP 2",labelpad=0.3,fontsize=16)
    ax.tick_params(labelsize=14)
    ax.set_title("Spectral Clustering and UMAP Embedding",fontsize=18)
ax.legend()
plt.savefig("UMAP_SpectralClustering.jpeg",dpi=400)


#---------Combine the cluster information with the clinical parameters to form heatmaps---------
module_means = pd.concat([new_labs,dict["R"]],axis=1)
module_means.columns = module_means.columns.str.replace(".", " ")
module_means.to_excel("ModuleFull.xlsx")
#Get the module stats
mod_means_stats = module_means.groupby('Module').describe()
#Export the module stats
mod_means_stats.to_excel("ModuleStats.xlsx")
#Get only the means to plot in a heatmap
mod_means = module_means.groupby('Module').mean()
#Plot heatmap of the means for each module
sns.clustermap(mod_means,square=False,col_cluster=False,vmin=-1,vmax=1,figsize=(16,16),cmap='coolwarm')
plt.savefig("ModuleMeans.jpeg",dpi=400,bbox_inches='tight',pad_inches = 0.5)

#Now plot correlation matrix with each cluster labelled by its color
module_means = module_means.set_index("Cluster ID")
fig, ax = plt.subplots(figsize=(20,20))
ax = sns.heatmap(module_means.loc[:,'Operative Invervention':'S2 Creat'],vmin=-1,vmax=1)
