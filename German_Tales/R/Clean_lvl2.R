### Firmenich
# clean Lvl2 duplicates

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
dat <- file.path(wd,"Data/Lvl2_uncleaned")
lvl2 <- file.path(wd,"Data")

# load package
require(openxlsx) # excel handling

# check if all hiwis have equal data
al <- read.xlsx(file.path(dat,"German_tales_AL.xlsx"))
fs <- read.xlsx(file.path(dat,"German_tales_FS.xlsx"))
jr <- read.xlsx(file.path(dat,"German_tales_JR.xlsx"))

al$ID[c(1657:1770)]
fs$ID[c(1657:1770)]
jr$ID[c(1657:1770)]

al2 <-al[-c(1657:1770),]
fs2 <-fs[-c(1657:1770),]
jr2 <-jr[-c(1657:1770),]

write.xlsx(al2,file.path(lvl2,"German_tales_AL.xlsx"),overwrite = T)
write.xlsx(fs2,file.path(lvl2,"German_tales_FS.xlsx"),overwrite = T)
write.xlsx(jr2,file.path(lvl2,"German_tales_JR.xlsx"),overwrite = T)

# check
al3 <- read.xlsx(file.path(lvl2,"German_tales_AL.xlsx"))
fs3 <- read.xlsx(file.path(lvl2,"German_tales_FS.xlsx"))
jr3 <- read.xlsx(file.path(lvl2,"German_tales_JR.xlsx"))

all.equal(al2,al3)

# all.equl rownames issue (solved: rownames change due to clip and reload from disk)
rownames(al2[c(1657:1770),])
rownames(al3[c(1657:1770),])
al$ID[c(1657:1770)]
al2$ID[c(1657:1770)]
al3$ID[c(1657:1770)]
################################################################################
