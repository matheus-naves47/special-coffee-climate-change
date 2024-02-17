#!/bin/bash

directory="/home/matheus/Projects/special-coffee-climate-change/data/climate_grids/CMIP6/"

tasmin_files=$(find "$directory"/* -name "tasmin_*_conservative*")
tasmax_files=$(find "$directory"/* -name "tasmax_*_conservative*")
pr_files=$(find "$directory"/* -name "pr_*_conservative*")


for tasmin_file in $tasmin_files; do
	if [ -f "$tasmin_file" ]; then
		mv "$tasmin_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/"
	fi

done

for tasmax_file in $tasmax_files; do
	if [ -f "$tasmax_file" ]; then
		mv "$tasmax_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/"
	fi
done

for pr_file in $pr_files; do
	if [ -f "$pr_file" ]; then
	    mv "$pr_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/"
	fi
done

cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/conservative/tasmin_day_MM-ENSEMBLE_1961_2014.nc"
cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/conservative/tasmax_day_MM-ENSEMBLE_1961_2014.nc"
cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/conservative/pr_day_MM-ENSEMBLE_1961_2014.nc"
    
