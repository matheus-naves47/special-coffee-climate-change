#!/usr/bin/env bash
# Filename: cmip6-cdo-io.sh
cd data/climate_grids/CMIP6/HadGEM3-GC31-MM/hist/


# Initial operation

# Precipitation
cd pr/
cdo -f nc -copy "*.nc" pr_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc # copy all data to a single file
find . -name 'pr_day_HadGEM3-GC31-MM_historical_*.nc' -delete # Delete all other files
cdo -remapcon,../../../../BR_DWGD/pr_19610101_20200731_BR-DWGD.nc pr_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc pr_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc # Interpolate using conservative method to BR-DWGD resolution
rm pr_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc # Delete old file

# Minimum Temperature
cd ../tasmin
cdo -f nc -copy "*.nc" tasmin_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc
find . -name 'tasmin_day_HadGEM3-GC31-MM_historical_*.nc' -delete
cdo -remapcon,../../../../BR_DWGD/Tmin_19610101_20200731_BR-DWGD.nc tasmin_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc tasmin_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc 
rm tasmin_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc

# Maximum Temperature
cd ../tasmax
cdo -f nc -copy "*.nc" tasmax_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc
find . -name 'tasmax_day_HadGEM3-GC31-MM_historical_*.nc' -delete
cdo -remapcon,../../../../BR_DWGD/pr_19610101_20200731_BR-DWGD.nc tasmax_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc tasmax_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc
rm tasmax_day_HadGEM3-GC31-MM_hist_19600101-20141230.nc


# Splityear operation
cd ../pr
cdo splityear pr_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc pr_day_HadGEM3_

cd ../tasmin
cdo splityear tasmin_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc tasmin_day_HadGEM3_

cd ../tasmax
cdo splityear tasmax_day_HadGEM3-GC31-MM_hist_19600101-20141230_interpolated.nc tasmax_day_HadGEM3_