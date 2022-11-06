### Check Data
# merge all transcription files


# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")


# load package
require(openxlsx) # excel handling
require(LinguGeo)
require(raster)
require(mapview)
require(rgdal)
require(stringr)
require(stringi)

# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_AL.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_JH.xlsx"))
fs <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_fs.xlsx"))

org <-openxlsx::read.xlsx(xlsxFile =file.path(wd,"GeoRef/Data_GeoRef/DWA_GeoRef_full.xlsx"))

# take a look
colnames(al)
colnames(jh)
# shared columns: [,c(4:10,15)]

### check equal shared columns with others
all.equal(al[,c(4:10,15)],jh[,c(4:10,15)])
all.equal(al[,c(4:10,15)],fs[,c(4:10,15)])
all.equal(jh[,c(4:10,15)],fs[,c(4:10,15)])

      al[which(al$Bogennummer_alph!=jh$Bogennummer_alph),c(4:10,15)]
      jh[which(al$Bogennummer_alph!=jh$Bogennummer_alph),c(4:10,15)]
      fs[which(al$Bogennummer_alph!=fs$Bogennummer_alph),c(4:10,15)]
      al[which(al$Bogennummer_alph!=fs$Bogennummer_alph),c(4:10,15)]
      fs[which(jh$Bogennummer_alph!=fs$Bogennummer_alph),c(4:10,15)]
      jh[which(jh$Bogennummer_alph!=fs$Bogennummer_alph),c(4:10,15)]

### check equal shared columns with org
all.equal(al[,c(4:10,15)],org[,c(4:10,15)])
all.equal(fs[,c(4:10,15)],org[,c(4:10,15)])
all.equal(jh[,c(4:10,15)],org[,c(4:10,15)])

      # Bogennummer_alpha
      org[which(fs$Bogennummer_alph!=org$Bogennummer_alph),c(4:10,15)]
      org[which(jh$Bogennummer_alph!=org$Bogennummer_alph),c(4:10,15)]
      
      fs[which(fs$Bogennummer_alph!=org$Bogennummer_alph),c(4:10,15)]
      jh[which(jh$Bogennummer_alph!=org$Bogennummer_alph),c(4:10,15)]
      
      # Ort
      org[which(al$Ort!=org$Ort),15]
      al[which(al$Ort!=org$Ort),15]
      org[which(fs$Ort!=jh$Ort),15]
      fs[which(fs$Ort!=jh$Ort),15]


### merge
# get all raws with places found
sub_al <- al[which(!is.na(al$`Bearbeiten/in`)),]
sub_jh <- jh[which(!is.na(jh$`Bearbeiten/in`)),]
sub_fs <- fs[which(!is.na(fs$`Bearbeiten/in`)),]

# rbind
#d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
data_full <- do.call("rbind", list(sub_al,sub_jh,sub_fs))
# get progress rate
nrow(data_full)/nrow(al)

# check data
x <-DWA_Sanctum(al,skip_mode = F)

x <-DWA_Sanctum(al,skip_mode = T)
x <-DWA_Sanctum(fs,skip_mode = T)
x <-DWA_Sanctum(jh,skip_mode = T)

x <-DWA_Sanctum(sub_al,skip_mode = T)
x <-DWA_Sanctum(sub_fs,skip_mode = T)
x <-DWA_Sanctum(sub_jh,skip_mode = T)

x <-DWA_Sanctum(org,skip_mode = T)
y <-DWA_Sanctum(sub_al)
z <-DWA_Sanctum(org)
x <-DWA_Sanctum(data_full,skip_mode = T) 
x <-DWA_Sanctum(d1ata_full,skip_mode = T)

