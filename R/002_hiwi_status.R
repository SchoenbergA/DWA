### Get HiWi Progress and Stats

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

# source function script
source(file.path(r,"DWA_functions.R"))

# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_AL.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_JH.xlsx"))
fs <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_fs.xlsx"))
jr <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_jr.xlsx"))

# check stats
dwa_full <-DWA_stats(write_data = F,dup_check = T)
WS_stats()
DWA_unsure()

# which rows missing?
mis<- al[-which(al$Digi_Index%in%dwa_full$Digi_Index),]
mis$Digi_Index

# check duplicated
which(duplicated(dwa_full$Digi_Index))
dub1 <-dwa_full[which(duplicated(dwa_full$Digi_Index)),1:ncol(al)]
dub2 <-dwa_full[which(duplicated(dwa_full$Digi_Index,fromLast = T)),1:ncol(al)]

dub <- rbind(dub1,dub2)
view <-dub[3:4,]

all.equal(dub[which(dub$Digi_Index=="III_14_0007"),][1,25:ncol(dub)],dub[which(dub$Digi_Index=="III_14_0007"),][2,25:ncol(dub)])
