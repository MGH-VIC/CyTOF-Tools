# Batch Correction Boston Gene
Module for batch correction/normalization. Module requires as input a folder with fcs files which will be normalized and exported to new fcs files.
To run pipeline:
1) Open command line interface
2) Enter command arguments listed below

Template for command line arguments can be found in ExampleCommand.txt

**command arguments**:
* python /path/to/CommandBatchCorrection.py
* --folder_dir: Enter path to your fcs files (Ex: "/here/are/my/fcs/files")
* --output: Directory to export new fcs files (Ex: "/here/are/my/newfcs/files")
* --columns_to_keep: Path to excel file that indicates which columns to use for normalization (Ex: "path/Columns to Keep.xlsx")
* --cofactor: Cofactor to use for normalization (Ex: 5)
* --export_means: Export mean values for channels before and after normalization (include if yes, if not, exclude)
