#' Filter MONI-PAM data step 1,remove unrecorded or abnormal low F' and Fm' value
#'
# Details see Zhang et al.,202X. paper link url.
#'
#' @param moni.data a combined organized MONI-PAM data which is generated from [read.MONIPAM] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#'
#' @return This function will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'MONI_Year1_Year2_filter1NA.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season. This output file will contain two new column named as 'flag1.NA.Yield' and 'flag.all'. These two columns only contain two values: 0 and 1, where 0 means F', Fm' and Yield in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset. 'flag1.NA.Yield' is used to mark which data is/are abnormal data from the data filtering function. 'flag.all' is used to mark all the abnormal data from the all data filtering function.
#' @export
filter1.NA<-function(moni.data,
                     save.path, save.file){

  moni.data<-format.monidata(moni.data = moni.data)

  ##<<- add a column 'flag1.NA.Yield' to flag empty data of Yield
  ##<<- firstly all the data of column 'flag1.NA.Yield' will be 1,
  ##<<- since the good/kept data will be flagged as 1
  moni.data$flag1.NA.Yield<-1


  ##<<- 1. replace F and Fm as NA, when
  moni.data[which(is.na(moni.data$F_)|
                    is.na(moni.data$Fm_)|
                    moni.data$F_<=10|#if MONI-PAM ChlF is corrected by temperature, F' can be less than 10
                    moni.data$Fm_<=50),#if MONI-PAM ChlF is corrected by temperature, Fm' can be less than 50
            c('F_','Fm_')]<-NA

  ##<<- 2. when F or Fm is NA, flag 0 in column 'flag1.NA.Yield', and replace
  ##       Yield as NA
  moni.data$flag1.NA.Yield[is.na(moni.data$F_)|is.na(moni.data$Fm_)]<-0
  moni.data[which(moni.data$flag1.NA.Yield==0),c('F_','Fm_','Yield')]<-NA

  ##<<- 3. when Fm <=F and fm>50, we only removed Yield but not
  ##       flag the data as 0, since F and Fm data will be kept
  moni.data$Yield[which(moni.data$Fm_<=moni.data$F_&
                          moni.data$Fm_>50)]<-NA

  moni.data$flag.all<-moni.data$flag1.NA.Yield
  if (save.file==T){
    write.table(moni.data,file =
                  paste0(save.path,'/MONI_',year(range(moni.data$date)[1]),'_',
                         year(range(moni.data$date)[2]),
                         '_filter1NA.dat'),
                row.names = F,sep = ';')
  }
  return(moni.data)
}
