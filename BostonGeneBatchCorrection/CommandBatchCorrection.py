#Script for parsing command line arguments and running ilastik prep functions
#Joshua Hess
import ParseInput
import BatchCorrectionBostonGene

#Parse the command line arguments
args = ParseInput.ParseInputBatchCorrection()

#Run the CommandBatchCorrection function
BatchCorrectionBostonGene.CommandBatchCorrection(**args)
