AERIAL IMAGE METRICS
27.3.2017
Kotivuori, Kukkonen,  Räty

###
###  Instructions for running the ai2metrics
###

#############################

Following files must be in the R's Working Directory:

   ai_metrics.R
   ai_function.R 

###
###  Example run
###

rm(list=ls())

setwd("D:\\ai2metrics") # Set the working directory R files
source("ai_function.R")                	# Import required functions

ai2metrics("leaf_off.txt",              # ALS data file
            2,				# Cutoff threshold
	    10,                        	# Minimum number of echoes
           "ai_metrics.txt"          	# Output file having ALS metrics
)


###
###  Descriptions for parameters
###
                      
ALS data file					Specify an ALS file (text file)
						Format: plot_cell_id; x; y; z; dz, i; echotype; flightline; terraclass; GPS-time   (delimeter: space)
						First seven columns must be in the abovementioned order, additional columns are optional


Cutoff threshold				Cuts off all echoes smaller or equal to the given threshold value.

Minimum number of echoes			Minimum number of echoes to compute metrics.

###
### OUTPUT
###

There is one output file. The number of columns depends on your settings.
All of the results are calculated from first + only echo category.

Output column names:



"plot_cell_id"					Plot or cell id

"R/G/B/Nmax"         				Maximum 
"R/G/B/Nmin"       				Minimum 
"R/G/B/Nstd"         				Standard deviation 
"R/G/B/Nmean"         				Mean


R = Red, G = Green, B = Blue & N = Near infrared



