### IndexRef Assign DWA TrayNr to unsorted Bögen
# merge all IndexRef data and assign TrayNr

# set environment paths
wd <- "C:/Envimaster/DWA" # local path to repository
dat <- file.path(wd,"Index_Ref")

source(file.path(dat,"R/IndRef_fun.R"))

# load package
require(openxlsx) # excel handling
require(LinguGeo)
require(raster)
require(mapview)
require(rgdal)
require(stringr)
require(stringi)

### merge all IndexRef data ###################################################
# load data
al <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"AL_Index_Ref.xlsx"))
jh <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"JH_Index_Ref.xlsx"))
fs <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"FS_Index_Ref.xlsx"))
jr <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"JR_Index_Ref.xlsx"))

#
IndRef_stats()

# r bind and delete duplicates
IndRef_full <- rbind(jh,fs,jr)
IndRef_full <-IndRef_full[duplicated(IndRef_full)==F,]

# IndexRef_Status 
cat(paste0("IndexRef @ ",round(nrow(IndRef_full)/783,digits = 4)*100," %"))

# load csv
csv <-read.csv("C:/Envimaster/Digitalisierung/Data/csv/001_TrayCodes.csv")
# check for tailing whitespaces
head(csv)
tail <- lapply(1:ncol(csv), function(x){
  length(which((grepl(" $", csv[,x])==T)))
})
tail
lead <- lapply(1:ncol(csv), function(x){
  length(which((grepl("^ ", csv[,x])==T)))
})
lead

# trim WS
csv[,2] <-str_trim(csv[,2])
csv[,4] <-str_trim(csv[,4])

# function to assign TrayNr
assign_trayNr <- function(df,csv){
  cat("Assigning TrayNr",sep="\n")
  # set dummy values
  df$tray <-999
  for(t in 1:nrow(df)){
    # is num and char in csv @ i
    for(i in 1:nrow(csv)){
      # df alpha and num in csv ?
      if(df$Bogennummer_alph[t]%in%unlist(strsplit(csv$letters[i],",")) & df$Bogennummer_num[t]%in%csv$nr.from[i]:csv$nr.to[i]==T){
        # check for overlapping
        if(df$tray[t]!=999){
          warning("confict")
        }
        # assign trayNr
        df$tray[t] <- paste0(csv$römisch[i],"_",csv$Schbfch.NR[i])
        #print(i)
      }
    }
    
  }
  cat("TrayNr assigned",sep="\n")
  return(df)
} # end of function

# run
df2 <- assign_trayNr(IndRef_full,csv)
df3 <- df2[,c(5,ncol(df2))]

head(df2)

write.xlsx(df2,file.path(dat,"assigned_TrayNr.xlsx"))
