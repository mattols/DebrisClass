# read in Landsat data
# 2020-03-17
#
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# filter T1 data
FolderLS <- function(folder_path, PR, LS7=FALSE, LS8=FALSE){
  tile_list = list.files(folder_path, full.names = T)
  # mask to roi
  tile_roi = Ldt_tile(PR)
  for (t in tile_list){
    tile_name = paste0(PR,"_",gsub(paste0(".*",PR,"_(.+)_2.*"),'\\1',t))
    # t = tile_list[1]
    tmp_msks <<- paste0("~/data/Landsat/results/", "masks")
    if(!file.exists(tmp_msks)){
      dir.create(tmp_msks)
    }
    # LSread
    print(paste("Starting", match(t,tile_list),"of",length(tile_list)),quote=F)
    landsat_bands = LSread0(t)
    # generate DEM
    # LSdem(landsat_bands) # 8 mins
    # create masks & classification
    LSmask(landsat_bands,tile_roi, tile_name,TM=LS7)
    # output automatically saved
    unlink(tmpDir(), recursive=TRUE)
  }
}

# Load tile for bands 1-5 only
LSread0 <- function(tile_path,tile_roi){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[1-5]",band_paths,value=TRUE)
  landsat_bands <- raster::brick(lapply(band_select, raster))
  # landsat_bands <- mask(landsat_bands,tile_roi)
  print(Sys.time() -strt)
  return(landsat_bands)
}

# Load tile (set for L05)
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
LSread2 <- function(tile_path, LS7=FALSE, LS8=FALSE, revertNA=TRUE, synchronise=TRUE){
  # load bands and mtl
  strt <- Sys.time()
  band_paths = list.files(tile_path, full.names = T)
  band_select = grep("B[[:digit:]]",band_paths,value=TRUE)
  # insert L07 & L08 code
  if (LS7){
    ls6band <- raster::raster(band_select[which(grepl("B6.*2",band_select))])
    landsat_bands <- raster::stack(band_select[which(grepl("B[1-5]",band_select))])
    cat("\n ...resampling ETM+ Thermal Band (6)\n")
    ls6band <- raster::resample(ls6band,landsat_bands[[1]])
    landsat_bands <- raster::brick(as.list(landsat_bands,ls6band))
    rm(ls6band)
    cat("Loading L07 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
  }else{
    if(LS8){
      landsat_1 <- raster::stack(band_select[which(grepl("B[1]",band_select))])
      landsat_bands <- raster::stack(band_select[which(grepl("B[2-6]",band_select))])
      landsat_bands <- stack(landsat_1[[1]],landsat_bands,landsat_1[[2]])
      cat("Loading L08 lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
    }else{
      # must be L05
      landsat_bands <- raster::stack(band_select[which(grepl("B[[:digit:]]",band_select))])
      cat("Loading lv1 landsat bands - length:", nlayers(landsat_bands),"\n ",paste0("B",seq(1:7)),"\n")
    }
    metaData <<- RStoolbox::readMeta(band_select[which(grepl("MTL",band_select))])
  }
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

LSdem <- function(lstk){
  # rewrite srtm code (possibly use NASAdem)
  if(file.exists(paste0(tmp_msks,"/dem.grd"))){
    print("dem exists",q=F)
  }else{
    source('~/projects/Debris_snow_class/src/srtm_download1.R') # rewrite!
    dem0 <- get_srtm30(lstk[[2]], full_extent = TRUE, mask_to=TRUE) # call function
    dem = mask(crop(dem0,lstk),lstk)
    raster::writeRaster(dem,filename=paste0(tmp_msks,"/dem.grd"), overwrite=TRUE)
    # return(dem)
  }
}

Ldt_tile <- function(PR,dest_crs="+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs",centroid_only=FALSE){
  Ldesc = rgdal::readOGR("/Users/mattolson/projects/Debris_snow_class/data/test/LanTiles/WRS2_descending_0/WRS2_descending.shp")
  tile_find = which(Ldesc$PATH == substr(PR,1,3) & 
                      Ldesc$ROW == as.character(as.numeric(substr(PR,4,6))))
  Ldt = Ldesc[tile_find,]
  if(as.character(crs(Ldt))!=dest_crs){
    Ldt = sp::spTransform(Ldt,crs(dest_crs))  
    # Ldt = fasterize::fasterize()
  }
  if(centroid_only){
    Ldt_centroid = coordinates(Ldt)
    Ldt = SpatialPoints(Ldt_centroid,ccrs(dest_crs))
  }
  return(Ldt)
}
  
# # locate glacier tile ~3.1 mins
# LGpoly <- function(r){
#   # r is raster object 
#   ghma <- rgdal::readOGR("/Users/mattolson/tmp/rgi60_HMA/rgi60_HMA.shp")
#   gh_t = spTransform(ghma, crs(r))
#   gh_t2 <- rgeos::gBuffer(gh_t, byid=TRUE, width=0) # if "x[i, ] is invalid ERROR: Input geom 0 is invalid"
#   gk = crop(gh_t2,r)
#   return(gk)
# }
