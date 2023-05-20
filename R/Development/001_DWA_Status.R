### Get HiWi Progress and Stats

# load current data, check progress and get full data
# check for unsure and WS

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Data")
r <- file.path(wd,"R")

# source function script
source(file.path(r,"DWA_status_functions.R"))

# check stats
dwa_full <-DWA_stats(path=dat,dup_check = T)
WS_stats(dat)
DWA_unsure(dat)

