### Get HiWi Progress and Stats for German Tales

# load current data, check progress and get full data
# check for unsure and WS

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
dat <- file.path(wd,"Data")
r <- "C:/Envimaster/DWA/German_Tales/R"


# source function script
source(file.path(r,"DWA_GT_status_functions.R"))

# check stats
gt_full <-DWA_GT_stats(path = dat,bnd = 1,dup_check = T)
gt_full <-DWA_GT_stats(path = dat,bnd = 2,dup_check = T)
gt_full <-DWA_GT_stats(path = dat,bnd = 3,dup_check = T)
gt_full <-DWA_GT_stats(path = dat,bnd = 4,dup_check = T)
gt_full <-DWA_GT_stats(path = dat,bnd = "all",dup_check = T)

# write data
write.xlsx(gt_full,file.path(wd,"German_tales_band_1_status.xlsx"))

# clean all brackets
for (i in 1:nrow(gt_full)){
  print(i)
  gt_full$Ort_Klasse[i]<-gsub("\\[|\\]", "", gt_full$Ort_Klasse[i])
  gt_full$Ort_Klasse[i]<-str_trim(gsub("\\{[^}]*}", "", gt_full$Ort_Klasse[i]),"both")
}

gt_full$Ort_Klasse[which(gt_full$Ort_Klasse=="unklar")] <-"Unklar"
gt_full$Ort_Klasse[which(gt_full$Ort_Klasse=="ungenau")] <-"Ungenau"

# get spatial data
src <- "+proj=longlat +datum=WGS84 +no_defs" # WGS 84 (Long / Lat)
colnames(gt_full)

gt_full$x <- as.numeric(gt_full$x )
gt_full$y <- as.numeric(gt_full$y )

which(is.na(gt_full$x))
which(is.na(gt_full$y))

gt_full <- gt_full[-which(is.na(gt_full$y)),]

df_spt <- sp::SpatialPointsDataFrame(gt_full[,16:15],gt_full)
require(raster)
crs(df_spt)
# set projection
sp::proj4string(df_spt) <- src
require(mapview)
mapview(df_spt)

require(rgdal)
writeOGR(df_spt,file.path(wd,"Band_1_status.shp"),driver="ESRI Shapefile",layer=1)

# stats
table(gt_full$Ort_Klasse)
