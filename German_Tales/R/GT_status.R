### Get HiWi Progress and Stats for German Tales

# load current data, check progress and get full data
# check for unsure and WS

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
dat <- file.path(wd,"Data")
r <- "C:/Envimaster/DWA/German_Tales/R"

# load packages
require(raster)
require(mapview)
require(rgdal)

# source function script
source(file.path(r,"DWA_GT_status_functions.R"))

# check progress
bnd1 <-Check_progress(path = dat,bnd = 1,dup_check = T)
bnd2 <-Check_progress(path = dat,bnd = 2,dup_check = T)
bnd3 <-Check_progress(path = dat,bnd = 3,dup_check = T)
bnd4 <-Check_progress(path = dat,bnd = 4,dup_check = T)
gt_full <-Check_progress(path = dat,bnd = "all",dup_check = T)

# write data
#write.xlsx(gt_full,file.path(wd,"German_tales_band_1_status.xlsx"))


# check data
Check_df(gt_full,c("Region/Gebiet","Ort","ungenau","spezifisch","unklar","Insel"))

# stats
table(gt_full$Ort_Klasse)

################################################################################
### get map data

# get spatial data
src <- "+proj=longlat +datum=WGS84 +no_defs" # WGS 84 (Long / Lat)
colnames(gt_full)

gt_full$x <- as.numeric(gt_full$x )
gt_full$y <- as.numeric(gt_full$y )

gt_full <- gt_full[-which(is.na(gt_full$y)),]

df_spt <- sp::SpatialPointsDataFrame(gt_full[,16:15],gt_full)

crs(df_spt)
# set projection
sp::proj4string(df_spt) <- src

mapview(df_spt)

# write shp
writeOGR(df_spt,file.path(wd,"name.shp"),driver="ESRI Shapefile",layer=1)


