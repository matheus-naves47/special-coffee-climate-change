#!/bin/bash

directory="/home/matheus/Projects/special-coffee-climate-change/data/climate_grids/CMIP6/"

read -rp 'Execute multimodel ensemble (y/n): ' mme_ans

if [ "$mme_ans" == "y" ]; then
	read -rp 'Enter remapping method (bilinear/conservative): ' remapping_method

	if [ "$remapping_method" == "conservative" ]; then

		# Do multimodel ensemble for all CMIP6 models using 1st order conservative remapping
		# List files for each variable
		tasmin_files=$(find "$directory"/* -name "tasmin_*_conservative*")
		tasmax_files=$(find "$directory"/* -name "tasmax_*_conservative*")
		pr_files=$(find "$directory"/* -name "pr_*_conservative*")

		# Move files to respective directories
		for tasmin_file in $tasmin_files; do
			if [ -f "$tasmin_file" ]; then
				mv "$tasmin_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/conservative/"
			fi

		done

		for tasmax_file in $tasmax_files; do
			if [ -f "$tasmax_file" ]; then
				mv "$tasmax_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/conservative/"
			fi
		done

		for pr_file in $pr_files; do
			if [ -f "$pr_file" ]; then
				mv "$pr_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/conservative/"
			fi
		done

		# Do ensemble mean for each variable
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/conservative/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/conservative/tasmin_day_MM-ENSEMBLE_1961_2014_conservative.nc"
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/conservative/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/conservative/tasmax_day_MM-ENSEMBLE_1961_2014_conservative.nc"
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/conservative/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/conservative/pr_day_MM-ENSEMBLE_1961_2014_conservative.nc"

	else
		# Do multimodel ensemble for all CMIP6 models using bilinear remapping

		# List files for each variable
		tasmin_files=$(find "$directory"/* -name "tasmin_*_bilinear*")
		tasmax_files=$(find "$directory"/* -name "tasmax_*_bilinear*")
		pr_files=$(find "$directory"/* -name "pr_*_bilinear*")


		# Move files to respective directories
		for tasmin_file in $tasmin_files; do
			if [ -f "$tasmin_file" ]; then
				mv "$tasmin_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/bilinear/"
			fi

		done

		for tasmax_file in $tasmax_files; do
			if [ -f "$tasmax_file" ]; then
				mv "$tasmax_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/bilinear/"
			fi
		done

		for pr_file in $pr_files; do
			if [ -f "$pr_file" ]; then
				mv "$pr_file" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/bilinear/"
			fi
		done

		# Do ensemble mean for each variable
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/bilinear/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmin/bilinear/tasmin_day_MM-ENSEMBLE_1961_2014_bilinear.nc"
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/bilinear/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/tasmax/bilinear/tasmax_day_MM-ENSEMBLE_1961_2014_bilinear.nc"
		cdo ensmean "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/bilinear/*.nc" "data/climate_grids/CMIP6/MM-ENSEMBLE/historical/pr/bilinear/pr_day_MM-ENSEMBLE_1961_2014_bilinear.nc"
	fi
fi
