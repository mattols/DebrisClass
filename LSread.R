# read in Landsat data
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# filter T1 data
FolderLS <- function(folder_path){
  tile_list = list.files(folder_path, full.names = T)
  # LSread
  
  # save output
}

# Load tile (set for L07)
LSread <- function(tile_path, TM=TRUE, revertNA=TRUE, synchronise=TRUE){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[[:digit:]]",band_paths,value=TRUE)
  landsat_bands <- raster::brick(lapply(band_select, raster)) # faster | landsat_bands <- raster::stack(band_select)
  names(landsat_bands) = gsub(paste0(".*(B.+).TIF"),'\\1',band_select)
  # meta (for shadows?)
  metaData <<- RStoolbox::readMeta(band_paths[which(grepl("MTL",band_paths))])
  cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  # clean and synchronize
  if(revertNA){ # change all 0 values -> NA
    cat("\n synchronising... [estimated ~4.5 mins]\n")
    landsat_bands <- reclassify(landsat_bands, cbind(0, NA), right=FALSE) # ~1.6 mins
    # landsat_bands = mask(landsat_bands,landsat_bands!=0,maskvalue=0) # 3.9 mins
  }
  if(synchronise){
    landsat_bands <- synchroniseNA(landsat_bands) # 2.9 minslstk = synchroniseNA(landstk) # 2.9 mins
  }
  print(Sys.time() -strt)
  return(landsat_bands)
}

# # NEW LOAD BASED ON SENSOR
# if (LS7){
#   ls6band <- raster::raster(current_tile[which(grepl("B6.*2",current_tile))])
#   landsat_bands <- raster::stack(current_tile[which(grepl("B[1-5]",current_tile))])
#   ls6band <- raster::resample(ls6band,landsat_bands[[1]])
#   landsat_bands <- stack(landsat_bands,ls6band)
#   cat("Loading L07 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
#   }else{
#   if(LS8){
#     landsat_1 <- raster::stack(current_tile[which(grepl("B[1]",current_tile))])
#     landsat_bands <- raster::stack(current_tile[which(grepl("B[2-6]",current_tile))])
#     landsat_bands <- stack(landsat_1[[1]],landsat_bands,landsat_1[[2]])
#     cat("Loading L08 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
#   }else{
#     # must be L05
#     landsat_bands <- raster::stack(current_tile[which(grepl("B[[:digit:]]",current_tile))])
#     cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
#     
#   }
# metaData <<- RStoolbox::readMeta(current_tile[which(grepl("MTL",current_tile))])
# }

# can weight all images by highest TIR (Thermal values) = better quality

# synchronize function
# https://stackoverflow.com/questions/23909929/synchronise-na-among-layers-of-a-raster-stack
synchroniseNA <- function(x){
  if(canProcessInMemory(x, n = 2))
  {
    val <- getValues(x)
    NA.pos <- unique(which(is.na(val), arr.ind = T)[, 1])
    val[NA.pos, ] <- NA
    x <- setValues(x, val)
    return(x)
  } else
  {
    x <- mask(x, calc(x, fun = sum))
    return(x)
  }
}

# locate glacier tile ~3.1 mins
LGpoly <- function(r){
  # r is raster object 
  ghma <- rgdal::readOGR("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp")
  gh_t = spTransform(ghma, crs(r))
  gh_t2 <- rgeos::gBuffer(gh_t, byid=TRUE, width=0) # if "x[i, ] is invalid ERROR: Input geom 0 is invalid"
  gk = crop(gh_t2,r)
  return(gk)
}
# create glacier mask
LGpoly2 <- function(r){
  # r is raster object 
  # rgeos::gIsValid(sf::st_read("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp"))) # self-intersection
  library(dplyr)
  strt <- Sys.time()
  # sp2 
  raster::shapefile('/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp') %>%
    spTransform(raster::crs(r)) %>%
    rgeos::gBuffer(byid=TRUE, width=0) %>%
    raster::crop(r) %>%
    sf::st_as_sf() %>%
    fasterize::fasterize(r) %>%
    do.call(what = raster::writeRaster, args = list(.,filename=paste0(tmp_msks,"/glac2.grd"), overwrite=TRUE))
    #raster::writeRaster(.,filename=paste0(tmp_msks,"/glac2.grd"), overwrite=TRUE)
  
  print(Sys.time() -strt)
    
  ghma <- sf::st_read("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp") %>%
    sf::st_transform(raster::crs(r))
  # gh_t2 <- rgeos::gBuffer(gh_t, byid=TRUE, width=0) # if "x[i, ] is invalid ERROR: Input geom 0 is invalid"
  gk = sf::st_crop(gh_t,r)
  gk2 = sf::st_intersection(ghma,sf::st_geometry(r))
  return(gk)
}
