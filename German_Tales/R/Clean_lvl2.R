### Firmenich
# clean Lvl2 duplicates

# set environment paths
wd <- "C:/Envimaster/DWA/German_Tales" # local path to repository
dat <- file.path(wd,"Data")

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

write.xlsx(al2,file.path(dat,"German_tales_AL.xlsx"),overwrite = T)
write.xlsx(fs2,file.path(dat,"German_tales_FS.xlsx"),overwrite = T)
write.xlsx(jr2,file.path(dat,"German_tales_JR.xlsx"),overwrite = T)

# check
al <- read.xlsx(file.path(dat,"German_tales_AL.xlsx"))
fs <- read.xlsx(file.path(dat,"German_tales_FS.xlsx"))
jr <- read.xlsx(file.path(dat,"German_tales_JR.xlsx"))

all.equal(al,al2)
al==al2
fs==fs2
fs
fs2

any(rownames(al)%in%rownames(al2)==F)
which(rownames(al)%in%rownames(al2)==F)
