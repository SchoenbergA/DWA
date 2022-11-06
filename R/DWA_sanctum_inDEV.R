### Functions to check data

hndl_UCnew2 <- function(data,cmin=1,cmax=ncol(data),skip_message=F){
  
  # set container
  count_ncol <-0
  count_non  <-0
  count_ncolu<-0
  count_nonu <-0
  #loop
  
  if(skip_message==FALSE){
    for(i in cmin:cmax){
      # print for each row the name, Non NFC strings and by this caused more uniques
      cat(paste0("Column_",i," - ",length(which(stri_trans_isnfc(data[,i])==F))," Non NFC strings detected, leading to ",
                 length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i]))),
                 " more uniques"),sep = "\n")
      
      
      # sum up
      if(length(which(stri_trans_isnfc(data[,i])==F))>0){
        count_ncol <- count_ncol+1}
      count_non <- count_non+length(which(stri_trans_isnfc(data[,i])==F))
      if(length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))>0){
        count_ncolu <- count_ncolu+1}
      count_nonu <- count_nonu+length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))
      
      # transform all columns
      data[,i] <-stri_trans_nfc(data[,i])
      
    }# end loop
    cat(paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings, leading to a total of ",
               count_nonu, " lesser uniques in ",count_ncolu, " columns."))
    res_uc <-paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings. Transformation to NFC leads to a total of ",
                    count_nonu, " lesser uniques in ",count_ncolu, " columns.")
    
  }
  # skipping mode
  if(skip_message==TRUE){
    for(i in cmin:cmax){
      # print for each row the name, Non NFC strings and by this caused more uniques
      #cat(paste0("Column_",i," - ",length(which(stri_trans_isnfc(data[,i])==F))," Non NFC strings detected, leading to ",
      #           length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i]))),
      #           " more uniques"),sep = "\n")
      
      
      # sum up
      if(length(which(stri_trans_isnfc(data[,i])==F))>0){
        count_ncol <- count_ncol+1}
      count_non <- count_non+length(which(stri_trans_isnfc(data[,i])==F))
      if(length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))>0){
        count_ncolu <- count_ncolu+1}
      count_nonu <- count_nonu+length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))
      
      # transform all columns
      data[,i] <-stri_trans_nfc(data[,i])
      
    }# end loop
    #cat(paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings, leading to a total of ",
    #           count_nonu, " lesser uniques in ",count_ncolu, " columns."))
    res_uc <-paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings. Transformation to NFC leads to a total of ",
                    count_nonu, " lesser uniques in ",count_ncolu, " columns.")
    
  }
  
  # return
  return(list(data=data,res_uc=res_uc))
}# end function

anyWS <- function(df,skip_message=F){
  
  if(skip_message==FALSE){
    # check for tailling
    tail <- lapply(1:ncol(df), function(x){
      length(which((grepl(" $", df[,x])==T)))
    })
    # print column
    for(i in 1:ncol(df)){
      cat(paste0("Column_",i, " - ", tail[i]," tailing whitespaces detected"),sep="\n")
    }
    
    # sum up
    cat(paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces"),sep = "\n")
    res_tail <-paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces")
    
    # check for leading
    lead <- lapply(1:ncol(df), function(x){
      length(which((grepl("^ ", df[,x])==T)))
    })
    # print column
    for(i in 1:ncol(df)){
      cat(paste0("Column_",i, " - ", lead[i]," leading whitespaces detected"),sep="\n")
    }
    
    # sum up
    cat(paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces"),sep = "\n")
    res_lead <-paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces")
    
    # trim both leading amnd tailing whitespaces
    for (i in 1:ncol(df)) {
      df[,i] <-str_trim(df[,i], "both") 
    }
  }
  
  # skipping mode
  if(skip_message==TRUE){
    # check for tailling
    tail <- lapply(1:ncol(df), function(x){
      length(which((grepl(" $", df[,x])==T)))
    })
    # print column
    for(i in 1:ncol(df)){
      #cat(paste0("Column_",i, " - ", tail[i]," tailing whitespaces detected"),sep="\n")
    }
    
    # sum up
    #cat(paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces"),sep = "\n")
    res_tail <-paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces")
    
    # check for leading
    lead <- lapply(1:ncol(df), function(x){
      length(which((grepl("^ ", df[,x])==T)))
    })
    # print column
    for(i in 1:ncol(df)){
      #cat(paste0("Column_",i, " - ", lead[i]," leading whitespaces detected"),sep="\n")
    }
    
    # sum up
    #cat(paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces"),sep = "\n")
    res_lead <-paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces")
    
    # trim both leading amnd tailing whitespaces
    for (i in 1:ncol(df)) {
      df[,i] <-str_trim(df[,i], "both") 
    }
  }
  
  # return
  return(list(df=df,res_lead=res_lead,res_tail=res_tail))
} # end of function

testCol <- function(df,n,skip_message=F){
  # get containers to count results
  count <-0
  count_na<-0
  count_e <-0
  count_ka <-0
  
  if(skip_message==FALSE){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]=="" ))
      nkempty <-length(which(df[,i]=="k.A." ))
      naempty <-length(which(is.na(df[,i])))
      if(nempty/nrow(df)>n){
        per=round(nempty/nrow(df),digits=5)
        cat(paste0("Column_",i, " - ", per*100,"% of entries with ' ' detected"),sep="\n")
        count<- count+1
        count_e <-count_e +1
      }
      if(naempty/nrow(df)>n){
        per=round(naempty/nrow(df),digits=5)
        cat(paste0("Column_",i, " - ", per*100,"% of entries with 'NA' detected"),sep="\n")
        count<- count+1
        count_na <-count_na +1
      }
      if(nkempty/nrow(df)>n){
        per=round(nkempty/nrow(df),digits=5)
        cat(paste0("Column_",i, " - ", per*100,"% of entries with 'k.A.' detected"),sep="\n") 
        count<- count+1
        count_ka <-count_ka +1
      }
    }
    res_col <- paste0(count," Columns have more than ",n*100,"% missing values")
    res_col_na <- paste0(count_na," Columns have more than ",n*100,"% 'NA' values")
    res_col_e <- paste0(count_e," Columns have more than ",n*100,"% ' ' (empty) values")
    res_col_ka <- paste0(count_ka," Columns have more than ",n*100,"% 'k.A.' values")
    
    return (list(res_col=res_col,res_col_na=res_col_na,res_col_e=res_col_e,res_col_ka=res_col_ka))
  }
  
  # skipping mode
  if(skip_message==TRUE){
    for(i in 1:ncol(df)){
      nempty <-length(which(df[,i]=="" ))
      nkempty <-length(which(df[,i]=="k.A." ))
      naempty <-length(which(is.na(df[,i])))
      if(nempty/nrow(df)>n){
        per=round(nempty/nrow(df),digits=5)
        #cat(paste0("Column_",i, " - ", per*100,"% of entries with ' ' detected"),sep="\n")
        count<- count+1
        count_e <-count_e +1
      }
      if(naempty/nrow(df)>n){
        per=round(naempty/nrow(df),digits=5)
        #cat(paste0("Column_",i, " - ", per*100,"% of entries with 'NA' detected"),sep="\n")
        count<- count+1
        count_na <-count_na +1
      }
      if(nkempty/nrow(df)>n){
        per=round(nkempty/nrow(df),digits=5)
        #cat(paste0("Column_",i, " - ", per*100,"% of entries with 'k.A.' detected"),sep="\n") 
        count<- count+1
        count_ka <-count_ka +1
      }
    }
    res_col <- paste0(count," Columns have more than ",n*100,"% missing values")
    res_col_na <- paste0(count_na," Columns have more than ",n*100,"% 'NA' values")
    res_col_e <- paste0(count_e," Columns have more than ",n*100,"% ' ' (empty) values")
    res_col_ka <- paste0(count_ka," Columns have more than ",n*100,"% 'k.A.' values")
    
    return (list(res_col=res_col,res_col_na=res_col_na,res_col_e=res_col_e,res_col_ka=res_col_ka))
  }
  
} # end of function

DWA_Sanctum <- function(df,skip_mode=F){
  if(skip_mode==F){
    # clean unicode -> convert all to UFC
    cat(" ",sep="\n")
    cat("Checking for Non-UFC Unicode ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_uc <-hndl_UCnew2(df,1,ncol(df))
    df <-df_uc$data
    df_uc$res_uc
    
    # clean whitespaces
    cat(" ",sep="\n")
    cat("Checking for Whitespaces ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_ws <- anyWS(df)
    df <-df_ws$df
    df_ws$res_lead
    df_ws$res_tail
    
    # check columns
    cat(" ",sep="\n")
    cat("Checking columns ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_col <-testCol(df,0)
    df_col$res_col
    
    # summary
    cat(" ",sep="\n")
    Sys.sleep(1)
    cat("########################",sep="\n")
    cat("### Summary",sep="\n")
    cat("# Solved issues",sep="\n")
    cat(df_uc$res_uc,sep="\n")
    cat(df_ws$res_lead,sep="\n")
    cat(df_ws$res_tail,sep="\n")
    cat("# Note:",sep="\n")
    cat(df_col$res_col,sep="\n")
  }
  
  # skipping mode
  if(skip_mode==T){
    # clean unicode -> convert all to UFC
    cat(" ",sep="\n")
    cat("Checking for Non-UFC Unicode ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_uc <-hndl_UCnew2(df,1,ncol(df),skip_message = T)
    df <-df_uc$data
    df_uc$res_uc
    
    # clean whitespaces
    cat(" ",sep="\n")
    cat("Checking for Whitespaces ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_ws <- anyWS(df,skip_message = T)
    df <-df_ws$df
    df_ws$res_lead
    df_ws$res_tail
    
    # check columns
    cat(" ",sep="\n")
    cat("Checking columns ",sep="\n")
    Sys.sleep(1)
    cat(" ",sep="\n")
    df_col <-testCol(df,0,skip_message = T)
    df_col$res_col
    
    # summary
    cat(" ",sep="\n")
    Sys.sleep(1)
    cat("########################",sep="\n")
    cat("### Summary",sep="\n")
    cat("# Solved issues",sep="\n")
    cat(df_uc$res_uc,sep="\n")
    cat(df_ws$res_lead,sep="\n")
    cat(df_ws$res_tail,sep="\n")
    cat("# Note:",sep="\n")
    cat(df_col$res_col,sep="\n")
  }
  
  
  
  
  
  # return
  return(df)
} # end of function
