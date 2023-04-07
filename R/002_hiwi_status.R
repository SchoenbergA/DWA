### Get HiWi Progress and Stats

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

# source function script
source(file.path(r,"DWA_functions.R"))

# check stats
dwa_full <-DWA_stats(path=dat,write_data = F,dup_check = T)
WS_stats(dat)
DWA_unsure(dat)


# which rows missing?
mis<- al[-which(al$Digi_Index%in%dwa_full$Digi_Index),]
mis$Digi_Index

# check duplicated
dwa_full <-DWA_stats(write_data = F,dup_check = F)
which(duplicated(dwa_full$Digi_Index))
dup1 <-dwa_full[which(duplicated(dwa_full$Digi_Index)),1:ncol(al)]
dup2 <-dwa_full[which(duplicated(dwa_full$Digi_Index,fromLast = T)),1:ncol(al)]

dup <- rbind(dup1,dup2)
