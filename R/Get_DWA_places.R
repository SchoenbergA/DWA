### Merge GeoRef AL and JH to get All DWA/Maurer matching places
# write SpatialPoints for DWA and Maurer
# write new transliteration base table with all places needed

# load data
# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"GeoRef")


# load package
require(openxlsx)
require(LinguGeo)
require(raster)
require(mapview)
require(rgdal)

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

### get all raws with places found
sub_al <- al[which(!is.na(al$LONG)),]
sub_jh <- jh[which(!is.na(jh$LONG)),]

# rbind
georef_full <- rbind(sub_al,sub_jh)
head(georef_full) # all DWA Bögen which need to be transliterated

# write out
write.xlsx(georef_full,file.path(dat,"DWA_GeoRef_full.xlsx"))

                  # get full data WITH unneeded Bögen clean
                  
                  ### get all raws with places found
                  sub_cl_al <- al[which(!is.na(al$Bearbeiter_places)),]
                  sub_cl_jh <- jh[which(!is.na(jh$Bearbeiter_places)),]
                  
                  # rbind
                  georef_cl_full <- rbind(sub_cl_al,sub_cl_jh)
                  head(georef_cl_full) # all DWA Bögen which need to be transliterated
                  
                  # compare
                  head(georef_full)
                  georef_full[which((georef_full$Ort %in% georef_cl_full$Ort)==F),c(7,8)]
                  georef_cl_full[which((georef_cl_full$Ort %in% georef_full$Ort)==F),c(7,8)]
                  nrow(georef_cl_full[which((georef_cl_full$Ort %in% georef_full$Ort)==F),c(7,8)])
                  
                  # write out
                  write.xlsx(georef_cl_full,file.path(dat,"DWA_GeoRef_full_wEmptyRaw.xlsx"))


      ### get spatial object for DWA
      df_spt <- sp::SpatialPointsDataFrame(georef_full[,13:14],georef_full)
      # set projection
      sp::proj4string(df_spt) <- "+proj=longlat +datum=WGS84 +no_defs"
      
      mapview(df_spt)
      
      # merge multi places
      dwa_places <-LinguGeo::mergeMultiPlaces(df = georef_full,pos_x = 13,pos_y = 14,col = ncol(georef_full)-1)
      
      # get local variation
      length(which(dwa_places$n_places>1)) # 255 older version 236
      sum(dwa_places$n_places)-nrow(dwa_places) # 477 older version 449
      
      nrow(dwa_places) # 2088 older version 2011 places
      
      # write out
      DWA_plc <- sp::SpatialPointsDataFrame(dwa_places[,13:14],dwa_places)
      mapview(DWA_plc)
      # set projection
      sp::proj4string(DWA_plc) <- "+proj=longlat +datum=WGS84 +no_defs"
      writeOGR(DWA_plc,file.path(dat,"Data_Vector/DWA_places.shp"), driver="ESRI Shapefile",layer= "DWA_places")



            ### get stats and places for Maurer
      
            # load maurer table
            mau <- openxlsx::read.xlsx(xlsxFile =file.path(wd,"GeoRef/Data_Places/DATEN GESAMT_new2.xlsx"))
      
            colnames(mau[,1:20]) # 6,7
            mau_plc <- LinguGeo::mergeMultiPlaces(df = mau,pos_x = 6,7,col = 5)
            
            # get local variation
            length(which(mau_plc$n_places>1)) # 124
            sum(mau_plc$n_places)-nrow(mau_plc) # 153
            
            nrow(mau_plc) # 2347 places
            
            # math final
            
            # hit old version dwa
            2011/2347 # 0.8568
            # hit new
            2088/2347 # 0.88

            # miss
            2347-2011 # 336
            # new
            2347-2088 # 257

            ### get spatial dfs
            mau_plc$LONG <- as.numeric(mau_plc$LONG)
            mau_plc$LAT <- as.numeric(mau_plc$LAT)
            MAU_p <- sp::SpatialPointsDataFrame(mau_plc[,6:7],mau_plc)
            # set projection
            sp::proj4string(MAU_p) <- "+proj=longlat +datum=WGS84 +no_defs"
            
            mapview(MAU_p)
            
            writeOGR(MAU_p,file.path(dat,"Data_Vector/Mau_places.shp"), driver="ESRI Shapefile",layer= "MAURER_places")

            
# compare Maurer and DWA
dwa <- as.data.frame(DWA_plc)            
mar <- as.data.frame(MAU_p)

which(!dwa$LONG %in% mar$LONG)
dwa[1238,]
which(!dwa$LAT %in% mar$LAT)

dwa[1871,]
mar[which(mar$GID==130436),]

which(!mar$LONG %in% dwa$LONG)
which(!mar$LONG %in% dwa$LONG)
length(which(!mar$GID %in% dwa$GID))

length(which(!mar$LONG %in% dwa$LONG))

mapview(MAU_p)+DWA_plc

nrow(mar)
nrow(dwa)

dwa$cd <- paste0(dwa$LONG,dwa$LAT)
mar$cd <- paste0(mar$LONG,mar$LAT)

which(!dwa$cd%in%mar$cd)
head(dwa)
dwa[which(!dwa$cd%in%mar$cd),c(8,12,13,14)]
mar[which(mar$GID==130436),c(2:10)]
dwa[which(dwa$Ort=="Feudenheim"),]

which(!mar$cd%in%dwa$cd)
