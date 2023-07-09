### German Tales
# Modify org table to Lvl1 Tables for all Hiwis

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
dat <- file.path(wd,"Data/org")
out <- file.path(wd,"Data")
#source(file.path(dat,"R/IndRef_fun.R"))

# load package
require(openxlsx) # excel handling

# load data
org <- read.xlsx(file.path(dat,"Firmenich_Inhalt.xlsx"))
head(org)
colnames(org)[12] <- "Kommentar_Lvl1"

### add columns

# add PDF pages
org$PDF_Anfang <- org$Seite_Anfang+12
org$PDF_Ende <- org$Seite_Ende+12

# add new columns
org$Bearbeiter_Lvl2 <- NA
org$Übersetzung_Titel <-NA
org$Übersetzung_durch <- NA
org$Ort_Klasse <- NA
org$Kommentar_Lvl2 <- NA

# add ID 
org$ID <- 1:nrow(org)

head(org)
# rearrange columns
colnames(org)
org <- org[,c(20,1,2,4,5,13,14,7,8,9,12,15,3,18,10,11,6,16,17,19)]
colnames(org)
head(org)

# write table
write.xlsx(org,file.path(out,"German_tales_AL.xlsx"))
write.xlsx(org,file.path(out,"German_tales_FS.xlsx"))
write.xlsx(org,file.path(out,"German_tales_JH.xlsx"))
write.xlsx(org,file.path(out,"German_tales_JR.xlsx"))
