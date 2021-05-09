# Landsat debris change classification
# 2020-05-04
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# package req. "raster" "sp" "rgdal" "sf" "RGISTools" "fasterize"

# # # TEST
# other data (replace with function)
demL  <- raster::raster("/Users/mattolson/projects/Debris_snow_class/data/test/dem/srtm_148035.tif")
glac <- rgdal::readOGR("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
sf_glaciers <- sf::st_read("~/projects/Debris_snow_class/data/test/shp/Ls_148035_glaciers.shp")
#
# test 1 scene
PR = 148035
folder_path = "/Users/mattolson/data/Landsat/148035/L05_148035_1980_2011_C1"
folder_path = "/Users/mattolson/data/Landsat/148035/L07_148035_19992002_AugOct"
tile_list = list.files(folder_path, full.names = T)
tile_path = tile_list[2]
tile_name = paste0(PR,"_",gsub(paste0(".*",PR,"_(.+)_2.*"),'\\1',tile_path))
tile_name
# # #

# call all (create function to call all)
source('~/src/DebrisClass/LSread.R')
source('~/src/DebrisClass/LSmask.R')

##### single scene
# landsat_bands = LSread(tile_path) # L05
landsat_bands = LSread2(tile_path,LS7=TRUE) # 6 mins
# landsat_bands = LSread2(tile_path,LS8=TRUE)
# generate DEM
dem <- LSdem(landsat_bands) # 8 mins
# create masks & classification
LSmask(landsat_bands)
####
plot(raster(paste0(tmp_msks,"/dem.grd")))
plot(raster(paste0(tmp_msks,"/class0.grd")),col=ggplot2::alpha(c("deepskyblue3","firebrick"),0.5),add=T)

# multiple scenes
FolderLS(folder_path,PR, LS7 = TRUE)
# landsat scenes

# read in and unlink landsat_bands when done!

# clear all data
unlink(tmp_msks, recursive=TRUE)
unlink(tmpDir(), recursive=TRUE)
rm(list=ls())
gc()

