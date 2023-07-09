### German Tales
# Add new transliterations to hiwi tables

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
lvl1 <- file.path(wd,"Data/Lvl_1")
lvl2 <-file.path(wd,"Data")
org <- file.path(wd,"Data/org")

# load package
require(openxlsx) # excel handling

### check diffenrence load data
new <- read.xlsx(file.path(org,"Firmenich_Inhalt_2.xlsx"))
old <- read.xlsx(file.path(lvl1,"German_tales_AL_Lvl1.xlsx"))

# check if all hiwis have equal data
al <- read.xlsx(file.path(lvl1,"German_tales_AL_Lvl1.xlsx"))
fs <- read.xlsx(file.path(lvl1,"German_tales_FS_Lvl1.xlsx"))
jr <- read.xlsx(file.path(lvl1,"German_tales_JR_Lvl1.xlsx"))

all(al$ID==fs$ID)
all(jr$ID==fs$ID)

(al$Kapitelüberschrift==fs$Kapitelüberschrift)
(jr$Kapitelüberschrift==fs$Kapitelüberschrift)

all.equal(al,jr)# "Kapitelübersicht 5 missmatches
al[254,3]
jr[254,3]

fs[373,3]
jr[373,3]
################################################################################

# new data begins at row 1397
all.equal(lvl1$Kapitelüberschrift,new$Kapitelüberschrift[1:1396])

# check
nrow(new[1397:nrow(new),])+nrow(al)
nrow(new)

# colanmes unequal
colnames(new)
colnames(lvl1)

# rename X12 to "Kommentar_Lvl1"
colnames(new)[12] <- "Kommentar_Lvl1"

# get dummy cols to match lvl1 data
missing_colnames <-which(colnames(lvl1)%in%colnames(new)==F)
colnames(lvl1)[missing_colnames[1]]

for(i in 1:length(missing_colnames)){
  new[colnames(lvl1)[missing_colnames[i]]] <-NA
}

# rearrange colums to match lvl1 format
colnames(lvl1)
colnames(new)

new <- new[,c(13,1,2,4,5,14,15,7,8,9,12,16,3,17,10,11,6,18,19,20)]

# check
colnames(lvl1)==colnames(new)

# add ID and pdf pages
new$ID[1397:nrow(new)] <- 1397:nrow(new)
new$PDF_Anfang[1397:nrow(new)] <- new$Seite_Anfang[1397:nrow(new)]+12
new$PDF_Ende[1397:nrow(new)] <- new$Seite_Ende[1397:nrow(new)]+12

# rbind new rows
al2 <- rbind(al,new[1397:nrow(new),])
fs2 <- rbind(fs,new[1397:nrow(new),])
jr2 <- rbind(jr,new[1397:nrow(new),])

# write new data
write.xlsx(al2,file.path(lvl2,"German_tales_AL.xlsx"),overwrite = T)
write.xlsx(fs2,file.path(lvl2,"German_tales_FS.xlsx"),overwrite = T)
write.xlsx(jr2,file.path(lvl2,"German_tales_JR.xlsx"),overwrite = T)

# check new data
al2 <- read.xlsx(file.path(lvl2,"German_tales_AL.xlsx"))
fs2 <- read.xlsx(file.path(lvl2,"German_tales_FS.xlsx"))
jr2 <- read.xlsx(file.path(lvl2,"German_tales_JR.xlsx"))

# check

all.equal(al,al2[1:1396,])
all.equal(fs,fs2[1:1396,])
all.equal(jr,jr2[1:1396,])
