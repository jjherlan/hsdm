# 6.2 Performing Simple GIS Analyses in R
# 6.2.1 Introduction

require(sp)
#require(gdal)
require(raster)
require(dismo)
#require(maptools)
require(sf)
require(stars)
require(s2)
require(lwgeom)
require(tidyverse)
require(gstat)
require(spdep)
install.packages('spDataLarge',
                 repos='https://nowosad.github.io/drat/', type='source')
require(spatialreg)
require(spatstat)
require(tmap)
require(mapview)

# Performing Simple GIS Analyses in R

# Adjust resolution and extent of raster layer_scales
# Re-project and calculate them
# Stack and overlay them with point observation data (field samples)
# Generate contours from raster layers
# Use lines or polygons in combination with raster data
#  

# 6.2.2 Loading the Data and Initial Exploration


# bio3 = temperature isothermality
# bio7 = temperature annual range
# bio11 = mean temperature of coldest quarter
# bio12 = annual precipitation

bio3 <- raster("data/raster/bioclim/current/grd/bio3.grd")
bio7 <- raster("data/raster/bioclim/current/grd/bio7.grd")
bio11 <- raster("data/raster/bioclim/current/grd/bio11.grd")
bio12 <- raster("data/raster/bioclim/current/grd/bio12.grd")

# These grids are now loaded into R and 

# Evaluate the imported datasets, use GIS-type commands

bbox(bio7); ncol(bio7); nrow(bio7); res(bio7)

# 1/5 deg lat/long = 167 km spatial resolution at equator

# Load the GTOPO30 global DEM
# 30 arc seconds
# 1.85 km at equator

elev <- raster("data/raster/topo/GTOPO30.tif")
projection(elev) <- "+proj=longlat + datum=WGS84 + ellps=WGS84 + towgs84=0,0,0"

bbox(elev); ncol(elev); nrow(elev); res(elev)

# 6.2.3 Resampling, Spatial Alignment, and Indices




























