#Functions for general purpose usage
#Joshua Hess
import os
from pathlib import Path
import re

def SearchDir(ending = ".txt",dir=None):
    """Function for searching only in current directory for files that end with
    the specified suffix

    Returns a list of full path file names"""

    #If directory is not specified, use the working directory
    if dir is None:
        tmp = Path('..')
        dir = tmp.cwd()
    #Search the directory only for files
    full_list = []
    for file in os.listdir(dir):
        if file.endswith(ending):
            full_list.append(Path(os.path.join(dir,file)))
    #Return the list
    return full_list
