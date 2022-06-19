#' Maurers_Sanctum
#' @description FUnction to solve severall common issues with the maurer dataframe.
#' @param df  data.frame (using the 'openxlsx::read.xlsx' is recommended).
#' @param missing_th  numeric - treshold for missing values in columns (0.1 -> 10%)
#' @param ka character - variable used for 'missing values.
#' @param ka_mm character - missmatches to check for (e.g. c("k.a.","K.A.")). Will be converted to 'ka'
#' @param long_col numeric - column position with the longitudal information
#' @param lat_col numeric - column position with the latidual information
#' @param return_spatial boolean - If TRUE returns a spatial objekt.
#' @return the cleaned dataframe (UFC Unicode, no tailling or leading whitespaces, projected to utm32, all ka_mm matches converted to ka)
#' @details Loading the original dataframe with 'openxlsx::read.xlsx' is recommended.
#' @author Andreas Schönberg


Maurers_Sanctum <- function(df,missing_th,ka,ka_mm,long_col,lat_col,return_spatial=F){
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
  df_col <-testCol(df,missing_th)
  df_col$res_col
  
  # check for places with multiple rows
  cat(" ",sep="\n")
  cat("Checking for Places with multiple rows ",sep="\n")
  Sys.sleep(1)
  cat(" ",sep="\n")
  df_mp<-multiPlaces(df)

  # check NA handling
  cat(" ",sep="\n")
  cat("Checking 'missing value' ",sep="\n")
  Sys.sleep(1)
  cat(" ",sep="\n")
  df_mm<-checkMissing(df,ka,ka_mm,T)
  df <-df_mm
    
  # convert to utm32 coordinates
  cat(" ",sep="\n")
  cat("Converting to UTM32 ",sep="\n")
  Sys.sleep(1)
  cat(" ",sep="\n")
  df_utm <-convert2utm(df,long_col,lat_col)
  df <- df_utm$df

  # summary
  cat(" ",sep="\n")
  Sys.sleep(1)
  cat("########################",sep="\n")
  cat("### Summary",sep="\n")
  cat("# Solved issues",sep="\n")
  cat(df_uc$res_uc,sep="\n")
  cat(df_ws$res_lead,sep="\n")
  cat(df_ws$res_tail,sep="\n")
  cat("Projected to UTM32 ETRS98",sep="\n")
  cat("All 'ka_mm' replaced by 'ka'",sep="\n")
  cat("# Note:",sep="\n")
  cat(df_col$res_col,sep="\n")
  cat(df_mp$res_mp,sep="\n")
  cat("Amount of unique places",sep="\n")
  cat(df_mp$n_places,sep="\n")
  
  # return
  if(return_spatial==T){
    return(list(dataframe=df,spatial=df_utm$mapview_utm))
  } else {
    return(df)
  }
  
}# end of function

