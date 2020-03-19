#Functions for parsing command line arguments for Boston Gene batch correction
import argparse


def ParseInputBatchCorrection():
   """Function for parsing command line arguments for input to batch correction"""

#if __name__ == '__main__':
   parser = argparse.ArgumentParser()
   parser.add_argument('--folder_dir',nargs='*')
   parser.add_argument('--output',nargs='*')
   parser.add_argument('--columns_to_keep',nargs='*')
   parser.add_argument('--cofactor',type=int)
   parser.add_argument('--export_means', action='store_true',default = False)
   parser.add_argument('--no-export_means', dest='export_means', action='store_false')
   args = parser.parse_args()
   #Create a dictionary object to pass to the next function
   dict = {'folder_dir': args.folder_dir, 'output': args.output,\
    'columns_to_keep': args.columns_to_keep,\
      'cofactor':args.cofactor,'export_means':args.export_means}
   #Print the dictionary object
   print(dict)
   #Return the dictionary
   return dict
