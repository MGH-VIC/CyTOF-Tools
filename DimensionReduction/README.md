# Single-cell Dimension Reduction
Module for running dimension reduction on single-cell fcs files using UMAP, t-SNE, or PHATE with plotting a grid of heatmap embeddings based on clinical parameters. Also contains functions to plot by sampleID or groupID if chosen.
To run pipeline:
1) Modify and run DimensionReductionExample.py -- instructions should be in module
* Must have all your fcs files in a single directory
* You will need to have a columns to keep text file exactly as the one given in the repository that will parse the fcs files for only the parameters that you need
* Additional options for subsampling included.

