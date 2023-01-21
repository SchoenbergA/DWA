### Get full DWA Data
# merge all transcription files


# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
r <- file.path(wd,"R")

source(file.path(r,"DWA_functions.R"))

# load package
require(openxlsx) # excel handling


# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_AL.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_JH.xlsx"))
fs <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_fs.xlsx"))
jr <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DWA_full_jr.xlsx"))

# get full DWA stats
DWA_stats()
WS_stats()

DWA_unsure <- function(){
# list hiwi data
ls <- list(sub_al,sub_jh,sub_fs,jr)
hiwi <- c("AL","JH","FS","JR")

# count unsure enrties
all_unsure <- lapply(1:4, function(i)  
  # n entries with []
  unsure <- lapply(1:ncol(ls[[i]]), function(x){
    length(which((grepl("\\[", ls[[i]][,x])==T)))
  })
  
  
)
unsure <-c(sum(unlist(all_unsure[[1]])),
       sum(unlist(all_unsure[[2]])),
       sum(unlist(all_unsure[[3]])),
       sum(unlist(all_unsure[[4]])))

# get total entries
n_entries <-c(length(which(!is.na(sub_al[,c(19:56)]))),
length(which(!is.na(sub_jh[,c(19:56)]))),
length(which(!is.na(sub_fs[,c(19:56)]))),
length(which(!is.na(sub_jr[,c(19:56)]))) )

# get df
df <- as.data.frame(cbind(unsure,n_entries))

# calc unsure rate
df$unsure_rate <- round(df$unsure/df$n_entries,digits = 4)*100
rownames(df) <- hiwi
df
}

DWA_unsure()
