#!/usr/bin/env bash
# Run debug run, compute both ALS and AI metrics (-s 2) from the colorized (RGBNiR in the last 4 columns) point cloud. Cut off 1.3 m.
# Fixed density metrics for the heights of 2 and 4 m. Quantiles: 0, 20%, 40%, 60%, 80%.

# HOX! If given argument '-t' als2metric is installed from the GIT.

TOKEN="-t -9999"
while getopts 't' OPTION
do
     case $OPTION in
         t)
             TOKEN="-t **TOKEN** -g 1"
             ;;
     esac
done

if [ "$TOKEN" != "-t -9999" ]
then
	/data/projects/3dnfi/sw/pointcloud2metrics/pointcloud2metrics.sh -i /data/projects/3dnfi/sw/pointcloud2metrics/debug/demoplots_dz.txt -o /data/projects/3dnfi/sw/pointcloud2metrics/debug/debug_aipc_metrics.txt -s 2 -c 1.3 -q "seq(.2,.8,.2)" -d "c(2,4)" $TOKEN 
else 
	/data/projects/3dnfi/sw/pointcloud2metrics/pointcloud2metrics.sh -i /data/projects/3dnfi/sw/pointcloud2metrics/debug/demoplots_dz.txt -o /data/projects/3dnfi/sw/pointcloud2metrics/debug/debug_aipc_metrics.txt -s 2 -c 1.3 -q "seq(.2,.8,.2)" -d "c(2,4)" 
fi
