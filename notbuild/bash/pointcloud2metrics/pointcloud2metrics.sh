#!/usr/bin/env bash

# This script is used to run als2metrics and ai2metrics functions via the
# unix terminal interface. The functions are written in R. 
# All arguments cannot be defined via this script, please refer to the R source (pcmetrics_run.R)
# The required R package will be downloaded from GitHub. The repo is not public, author token needed. Please contact the authors.

# Run scripts by Janne Raety, 
# The als2metrics and ai2metrics functions: Eetu Kotivuori, Mikko Kukkonen, Janne Raety & Petteri Packalen 2017

# 24022023

version()
{
cat << EOF

pointcloud2metrics.sh version "1.0"
	     
EOF
}


usage()
{
cat << EOF

The pointcloud2metrics.sh script provides a terminal-based interface 
for the use of als2metrics and ai2metrics functions.
The als2metrics function extracts metrics from 3D point clouds.
The ai2metrics function extracts spectral features from colorized 3D point clouds.
See the detailed input data specifications in the manuals of als2metrics/ai2metrics.

Note that, as a default, the installed packages will be stored in the TMP_R_LIBS repository.
Whether user want to use own package repository use "-p /Desired/Path/R_pkgs" argument that 
will override the .libPaths() definitions in R.

Date: 24022023


USAGE EXAMPLE: ./pointcloud2metrics.sh -i "./pointcloud.txt" -o 
"./pointcloud_metrics.txt" -c 0 -q "seq(0, .9, .1)" 

INPUT:

Point cloud format (.txt) id,x,y,z,dz,i,echotype,... (...,r, g, b, nir) - space delimited

ARGUMENTS:
	-i........ Input file (.txt). String.
	-o........ Output file (.txt). String.
	-c........ Height cut off (m). Double.
	-e........ Minimum number of echoes required for the 
		   computation of metrics. Default 10. Integer.
	-q........ Quantiles written following the R fashion, 
		   e.g. "seq(0, 1, 0.1)" or "c(0.2, 0.4, 0.6, 0.8)". 
		   Default: "seq(0.05, 0.95, 0.05)". String.
	-d........ The height values for height densities, following 
		   the R fashion. Default: "c(0.5, 2, 5, 10, 15, 20)". String.
	-s ....... Switch used to determine whether to run als2metrics (-a 0) or
		   ai2metrics (-aisw 1) or both (-a 2). Default: (-a 0). Integer.
	-g ....... Force the installation of the als2metrics package from 
		   the Git repo, integer 0 or 1. Default: 0. Integer.
	-t ....... Author token for the private Git repo. Required 
		   for the first installation, or when -g 1. String.
	-l........ Local path for the als2metrics package, tar.gz. 
	           Alternative to the git installation. 
		   Dependencies will be downloaded from CRAN. String. 
	-p........ Manually set repository for the R packages to be installed. 
		   Corresponds the lib argument of the install.packages() function. 
		   This is needed to override the default, specific for this run, 
		   storage path "./TMP_R_LIBS" defined as a default. String.
	    
HELP AND VERSION:
   	-h........ Show help
   	-v........ Show version		
	
EOF
}

# Defaults, they are also in the R code...
INPUT="-i -9999"
OUTPUT="-o -9999"
HCUT="-hcut 0"
# note: Q and D: the parameter name defined in the rscript call 
Q="seq(0.05, 0.95, 0.05)"
D="c(0.5, 2, 5, 10, 15, 20)"

AISW="-aisw 0"
GFORCE="-gforce 0"
TOKEN="-gtoken -9999"
MECHO="-mecho 10"
LOC_PATH="-localp -9999"
PKG_REPO="-prepo -9999"

while getopts 'hvi:o:c:q:d:s:g:t:e:l:p:' OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         v)
             version
             exit 1
             ;;
		 i)
			 INPUT="-i ${OPTARG}"
			 ;;
		 o)
			 OUTPUT="-o ${OPTARG}"
			 ;;
         c)
             HCUT="-hcut ${OPTARG}"
             ;;
         q)
             Q="${OPTARG}"
             ;;
         d)
             D="${OPTARG}"
             ;;
		 s)
             AISW="-aisw ${OPTARG}"
             ;;
		 g)
             GFORCE="-gforce ${OPTARG}"
             ;;
		 t)
             TOKEN="-gtoken ${OPTARG}"
             ;;
		 e)
             TOKEN="-mecho ${OPTARG}"
             ;;	 
		 l)
             LOC_PATH="-localp ${OPTARG}"
             ;;	 
		 p)
             PKG_REPO="-prepo ${OPTARG}"
             ;;	 
         ?)
             usage
             exit
             ;;
     esac
done

if [ $# == 0 ]; then
	
	usage
        exit 1

fi

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd)
#module load gdal
# Define R_LIBS_USER variable to store the package installations in a predetermined folder
export R_LIBS_USER="$parent_path/TMP_R_LIBS"
Rscript --verbose --vanilla "$parent_path/pcmetrics_run.R" $INPUT $OUTPUT $HCUT "-q" "$Q" "-d" "$D" $AISW $GFORCE $TOKEN $PKG_REPO $LOC_PATH
