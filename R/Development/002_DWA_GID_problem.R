### Clean multiple GID
# merge all transcription files


# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")

require(openxlsx)
db <- read.xlsx(file.path(wd,"DWA_places_14_12_22.xlsx"))

head(db)
unique(db$GID)

# clean all GID entries with multiple GIDs
table(nchar(db$GID))
db2 <- db[-which(nchar(db$GID)>7),]
table(nchar(db2$GID))

write.xlsx(db2,file.path(wd,"DWA_places_19_12_22.xlsx"))

require(LinguGeo)
LinguGeo::mergeMultiPlaces()