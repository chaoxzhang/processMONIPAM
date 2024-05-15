#' convert MONIPAM data variables to correct data format
#'
#' This function will convert the MONI-PAM variables to proper data format for easily used for data filtering and processing
#'
#' @param moni.data a combined organized MONI-PAM data which is generated from [read.MONIPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#'
#' @return [format.monidata] will generate a MONI-PAM data with all the variables in proper data format for later data filtering and processing
#' @export
format.monidata<-function(moni.data){
  moni.data<-data.table(moni.data)
  if ('datetime'%in%names(moni.data)){
    ##<<- convert 'date'|'dateBack12h' from factor to Date format
    moni.data$date<-as.Date(moni.data$date)
    moni.data$dateBack12h<-ymd(moni.data$dateBack12h)

    ##<<- convert 'datetime'|'datetimeBack12h'|'sunrise'|'sunsetStart'
    ##<<- from factor to timeseries using ymd_hms() from library(lubridate)
    for (i in c("datetime","datetimeBack12h")){

      moni.data[[i]]<-ymd_hms(moni.data[[i]],truncated = 3)
    }

    for (i in c("sunrise","sunriseEnd","solarNoon",
                "sunsetStart","sunset","dusk","dawn")){

      moni.data[[i]]<-ymd_hms(moni.data[[i]],truncated = 3)
    }


  } else {
    moni.data$date<-as.Date(moni.data$date)
  }

  moni.data$tree_num<-as.factor(moni.data$tree_num)
  moni.data$head<-as.factor(moni.data$head)
  if ('head_tree'%in%names(moni.data)){
    moni.data$head_tree<-as.factor(moni.data$head_tree)
  } else {
    moni.data$head_tree<-
      as.factor(paste0('H',moni.data$head,'_',moni.data$tree_num))
  }
  return(moni.data)
}
