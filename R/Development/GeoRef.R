### DWA GeoRef function development

# set environment
require(rgdal)
require(raster)
require(geojsonR)
require(openxlsx)
require(mapview)
require(rgeos)

path <- "C:/Envimaster/DWA_Sprint/GeoRef/Data_GeoRef/"

# load data
pfz <- read.xlsx(file.path(path,"pfalz_places.xlsx"))
msc <- readOGR(file.path(path,"pfalz_mask.shp"))
grd <- readOGR(file.path(path,"DWA_Grid_clean_wgs.shp"))
all <- read.csv(file.path(path,"alle-gid-orte.csv"),sep="",encoding = "UTF-8")

str(all)
str(pfz)
# convert to spatial object
all_spt <- sp::SpatialPointsDataFrame(all[,4:5],all)
sp::proj4string(all_spt) <- "+proj=longlat +datum=WGS84 +no_defs"

mapview(msc)
mapview(grd)

head(pfz)

which(pfz$Buchstabe==999)
which(pfz$Nummer==999)
pfz <- pfz[pfz$Buchstabe!=999,]
pfz <- pfz[pfz$Nummer!=999,]

which(pfz$Buchstabe=="Z'")
pfz[which(pfz$Buchstabe=="Z'"),]

pfz$Buchstabe[which(pfz$Buchstabe=="Z'")]<-"z'"

which(pfz$Buchstabe=="Z´")
pfz[which(pfz$Buchstabe=="Z´"),]

pfz$Buchstabe[which(pfz$Buchstabe=="Z´")]<-"z'"

which(pfz$Buchstabe=="f´")
pfz[which(pfz$Buchstabe=="f´"),]

pfz$Buchstabe[which(pfz$Buchstabe=="f´")]<-"f'"

which(pfz$Nummer==1)
pfz[pfz$Nummer==1,]
pfz$Nummer[pfz$Nummer==1]<-2
# get column with grid name equal to grid shape
pfz$grid <- paste0(pfz$Nummer,pfz$Buchstabe)

msc <-grd[grd$name==unique(pfz$grid)[1],]

mapview(msc)
msc <-sp::spTransform(msc, CRSobj = "+proj=moll")
msc2 <-rgeos::gBuffer(spgeom = msc, width = buffer)

mapview(msc)+msc2

# GeoRef function
DWA_GeoRef <- function(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer=2000){
  # add x,y,GID and result columns to dwa
  dwa$x <- 999
  dwa$y <- 999
  dwa$GID <- 999
  dwa$GeoRef <-999
  dwa$comment <-999
  
  # convert dat input to spatial obj
  dat_spt <- sp::SpatialPointsDataFrame(dat[,pos_x:pos_y],dat)
  sp::proj4string(dat_spt) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  if(is.null(buffer)==F){
    dat_spt <-sp::spTransform(dat_spt, CRSobj = "+proj=moll")
  }
  # get column with grid name equal to grid shape
  dwa$grid <- paste0(dwa$Nummer,dwa$Buchstabe)
  
  # loop all grid square
  for(i in 1:length(unique(dwa$grid))){
    #print(i)
    #print(unique(dwa$grid)[i])
    # get grid mask
    msc <- grd[grd$name==unique(dwa$grid)[i],]
    if(is.null(buffer)==F){
      msc <-sp::spTransform(msc, CRSobj = "+proj=moll")
      msc <-rgeos::gBuffer(spgeom = msc, width = buffer)
    }
    # crop dat to mask
    dat_grd <- crop(dat_spt,msc)
    # subset dwa
    dwa_grd <- dwa[dwa$grid==unique(dwa$grid)[i],]
    
    #cat(paste0(nrow(dwa_grd), " DWA places in grid ",msc$name ),sep="\n")
    
    # loop p for all dwa places in grid square i
    for(p in 1:nrow(dwa_grd)){
      # check if dwa name p in dat
      if(dwa_grd$Ort[p]%in%dat_grd$name){
        
        # n places in data == DWA place
        n_dat <- length(which(dat_grd$name==dwa_grd$Ort[p]))
        # n places in DWA == DWA place
        n_dwa <- length(which(dwa_grd$Ort==dwa_grd$Ort[p]))
        
        lv <-length(unique(dwa_grd$Bogen_Nummer[which(dwa_grd$Ort==dwa_grd$Ort[p])]))
        
        # multiple matches in data
        if(n_dat>1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "multiple entries in dat = dwa ortsname"
          #cat("multiple entries in dat = dwa ortsname",sep = "\n")
        }
        
        if(n_dwa>1&lv!=1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"
          #cat("multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'",sep = "\n")
        }
        
        if(n_dwa>1&lv==1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "detected"
          
          dwa$x[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$x[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$y[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$y[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$GID[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$geometry_id[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$comment[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "local variation in DWA"
          #cat("dec",sep = "\n")
        }
        
        if(n_dwa==1&n_dat==1){
          dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "detected"
          dwa$x[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$x[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$y[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$y[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$GID[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- dat_grd$geometry_id[which(dat_grd$name==dwa_grd$Ort[p])]
          dwa$comment[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "exact 1 match in grid shape"
          #cat("dec",sep = "\n")
        }
        
        
      } else {
        dwa$GeoRef[which(dwa$Digi_Index==dwa_grd$Digi_Index[p])] <- "not detected"
        #cat("notdec",sep = "\n")
      }
    } # end loop p
    
  }# end loop i for grid square
  
  # print results
  cat(" ",sep="\n")

  cat(paste0(nrow(dwa)," places"),sep="\n")
  cat(paste0(length(which(dwa$GeoRef=="detected")), " detected (",
             round(length(which(dwa$GeoRef=="detected"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"))," multiple entries in dwa = dwa ortsname and not equal 'Bogennummer' (",
             round(length(which(dwa$GeoRef=="multiple entries in dwa = dwa ortsname and not equal 'Bogennummer'"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="multiple entries in dat = dwa ortsname"))," multiple entries in dat = dwa ortsname (",
             round(length(which(dwa$GeoRef=="multiple entries in dat = dwa ortsname"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  cat(paste0(length(which(dwa$GeoRef=="not detected"))," not detected (",
             round(length(which(dwa$GeoRef=="not detected"))/nrow(dwa),digits = 4)*100," %)",sep="\n"))
  
  # convert df to WGS84
  dwa_mol <- sp::SpatialPointsDataFrame(dwa[,13:14],dwa)
  sp::proj4string(dwa_mol) <- "+proj=moll"
  
    dwa_wgs <-sp::spTransform(dwa_mol, CRSobj = "+proj=longlat +datum=WGS84 +no_defs")
  mapview(dwa_wgs)
  LinguGeo::convertCRS()
  # add geometry to df
  geo <- raster::geom(dwa_wgs)
  
  # write UTM geometry
  dwa$x <- geo[,2]
  dwa$y <- geo[,3]
  return(dwa)
}# end of function

test0 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5)
test1 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 1000)
test2 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 2000)
test3 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 3000)
test5 <- DWA_GeoRef(dwa=pfz,grd=grd,dat=all,pos_x=4,pos_y=5,buffer = 5000)

pfz_2000 <- test2[which(test2$GeoRef=="detected"),]
duplicated(pfz_2000$GID)

# write
write.xlsx(pfz_2000,file.path(path,"Pfalz_GeoRef_Buf_2000.xlsx"))

# test compare
test2 <-dwa
which(test0$GID%in%test2$GID)
which(!test2$GID%in%test0$GID)

test2[72,]
all[which(all$name=="Enchenberg"),]
mapview(grd[grd$name=="11h'",])

msc <- grd[grd$name=="11h'",]
if(is.null(buffer)==F){
  msc <-sp::spTransform(msc, CRSobj = "+proj=moll")
  msc2 <-rgeos::gBuffer(spgeom = msc, width = 2000)
}
# crop dat to mask
dat_spt <-sp::spTransform(dat_spt, CRSobj = "+proj=moll")
dat_grd <- crop(dat_spt,msc)
dat_grd2 <- crop(dat_spt,msc2)

mapview(dat_grd2)+grd[grd$name=="11h'",]+msc2

# weitere ideen
require(stringdist)
stringsim("Marburg","Mariburg","osa")
stringsim("Marburg","Mariburg","lv")