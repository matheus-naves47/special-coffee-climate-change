#!/bin/bash

directory="/home/matheus/Projects/special-coffee-climate-change/data/climate_grids/CMIP6/"

read -rp 'Execute remap (y/n): ' remap_ans

if [ "$remap_ans" == "y" ]; then
  	read -rp 'Enter remapping method (bilinear/conservative): ' remapping_method

  	if [ "$remapping_method" == "conservative" ]; then
  		# Remap all CMIP6 models to BR-DWGD grid resolution using 1st order conservative remapping
		function conservative_remap() {
		local dir="$1"

		for file in "$dir"/*; do
			if [ -f "$file" ]; then
			cdo -remapcon,"data/climate_grids/BR-DWGD_grid.txt" "$file" "${file%.*}"_conservative.nc
			# echo "$file"
			fi

			if [ -d "$file" ]; then
			conservative_remap "$file"
			fi
		done
		}

		conservative_remap "$directory"
	else
		# Remap all CMIP6 models to BR-DWGD grid resolution using bilinear interpolation
		function bilinear_remap() {
		local dir="$1"

		for file in "$dir"/*; do
			if [ -f "$file" ]; then
			cdo -remapbil,"data/climate_grids/BR-DWGD_grid.txt" "$file" "${file%.*}"_bilinear.nc
			# echo "$file"
			fi

			if [ -d "$file" ]; then
			bilinear_remap "$file"
			fi
		done
		}

		bilinear_remap "$directory"
	fi
fi



read -rp 'Execute yearsplit (y/n): ' yearsplit_ans

if [ "$yearsplit_ans" == "y" ]; then

	read -rp 'Enter remapping method (bilinear/conservative): ' remapping_method
	
	if [ "$remapping_method" == "conservative" ]; then
		# Apply CDO's yearsplit function to all files remapped by 1st order conservative method
		function yearsplit_conservative() {
		local dir="$1"
		files=$(find "$dir"/* -name "*_conservative*")

		for file in $files; do
			if [ -f "$file" ]; then
			filename="$(basename "${file}")"
			mkdir "$(dirname "$file")"/conservative  # create directory to receive splityear output
			cdo splityear "$file" "$(dirname "$file")"/conservative/"${filename%%_historical*}"_
			rm "$file"
			fi

			if [ -d "$file" ]; then
			yearsplit_conservative "$file"
			fi
		done
		}

		yearsplit_conservative "$directory"

	else
		# Apply CDO's yearsplit function to all files remapped by bilinear method
		function yearsplit_bilinear() {
		local dir="$1"
		files=$(find "$dir"/* -name "*_bilinear*")

		for file in $files; do
			if [ -f "$file" ]; then
			filename="$(basename "${file}")"
			mkdir "$(dirname "$file")"/bilinear  # create directory to receive splityear output
			cdo splityear "$file" "$(dirname "$file")"/bilinear/"${filename%%_historical*}"_
			rm "$file"
			fi

			if [ -d "$file" ]; then
			yearsplit_bilinear "$file"
			fi
		done
		}

		yearsplit_bilinear "$directory"
	fi
fi



