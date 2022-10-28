### Merge GeoRef

# load data
# set environment paths
wd <- getwd() # local path to repository
wd <- "C:/Envimaster/DWA"


dat <- file.path(wd,"GeoRef/Data_GeoRef")


# load package
require(openxlsx)

# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_GeoRef_AL_final_AS.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_GeoRef_JH_final_AS.xlsx"))

# take a look
head(al)
head(jh)

# unequal colnm order detected
colnames(jh)
colnames(al)
colnames(jh)[16] <- "Nachbearbeitet"

# reorder al
al <- al[,c(1:8,12:14,9:11,15:17)]

# unequal number of columns
unique(jh$Comment)
unique(al$Comment)
unique(al$X17) # alex has addintional col
unique(jh$Comment2)

# get dummy col for joris
jh$X17 <- NA

### get all raws with places found
which(!is.na(al$Ort))

# subset and rbind
test <- al[which(!is.na(al$LONG)),]
test2 <- jh[which(!is.na(jh$LONG)),]
test3 <- rbind(test,test2)

head(test)
head(test2)
head(test3)
str(test3)

#test3$LONG <- as.numeric(test3$LONG)
df <- test3
# get spatial object
df_spt <- sp::SpatialPointsDataFrame(df[,13:14],df)
# set projection
sp::proj4string(df_spt) <- "+proj=longlat +datum=WGS84 +no_defs"
require(mapview)
mapview(df_spt)
plot(df_spt)

test4 <-LinguGeo::mergeMultiPlaces(df = test3,pos_x = 13,pos_y = 14,col = ncol(test3)-1)

# get loacl variation
length(which(test4$n_places>1)) # 236
sum(test4$n_places)-nrow(test4) # 449

480/449
nrow(test4) # 2011 places

#

mau <- openxlsx::read.xlsx(xlsxFile ="C:/Envimaster/Alleman_study_map/DATEN GESAMT_new2.xlsx")

colnames(mau[,1:20]) # 6,7
mau_plc <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 6,7,col = 5)

# get local variation
length(which(mau_plc$n_places>1)) # 124
sum(mau_plc$n_places)-nrow(mau_plc) # 153

nrow(mau_plc) # 2347 places

# math final

# hit
2011/2347 # 0.8568
# new
2090/2347 # 0.8990

2090/2011
(2011/2347 - 2090/2347)*100
(2090/2347 -2011/2347 )*100
# miss
2347-2011 # 336
# new
2347-2090 # 257

257/336

257*1.307

### get spatial dfs
require(rgdal)

colnames(test4)
DWA_plc <- sp::SpatialPointsDataFrame(test4[,13:14],test4)
# set projection
sp::proj4string(df_spt) <- "+proj=longlat +datum=WGS84 +no_defs"


writeOGR(DWA_plc,"C:/Envimaster/Alleman_study_map/DWA_places.shp", driver="ESRI Shapefile",layer= "DWA_places")

colnames(mau_plc[1:20])
str(mau_plc[1:20])
mau_plc$LONG <- as.numeric(mau_plc$LONG)
mau_plc$LAT <- as.numeric(mau_plc$LAT)

MAU_p <- sp::SpatialPointsDataFrame(mau_plc[,6:7],mau_plc)
# set projection
sp::proj4string(df_spt) <- "+proj=longlat +datum=WGS84 +no_defs"

writeOGR(MAU_p,"C:/Envimaster/Alleman_study_map/MAURER_places.shp", driver="ESRI Shapefile",layer= "MAURER_places")
