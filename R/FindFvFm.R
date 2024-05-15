#' Retrieve Fv/Fm data from MONI-PAM data
#'
#' This function will get maximum values of quantum yield of photosystem II (maximum Yield) for each observing night (highest Yield between sunset of Day i and sunrise of Day i+1) for each MONI-head. We use these maximum Yield to represent FV/FM because during the night time, the PAR is close to 0 and almost all photosynthetic reaction center should be closed for hours.  In principle, the maximum Yield should be observed during the dark period when the PAR is close to 0 and almost all photosynthetic reaction centers should be closed for hours. This is the reason that we can use the maximum Yield from nighttime to represent FV/FM in continuous MONI-PAM measurement
#'
#' @param moni.data a combined organized MONI-PAM data which is generated from [read.MONIPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as filter1.NA, filter2.night and so on.
#' @param save.path local folder for saving your output data
#' @param save.title any text that will be used as the saved file name
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if False, the file will not be saved into local folder
#'
#' @return [FindFvFm] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a '.dat' file.
#' @export
FindFvFm<-function(moni.data,save.path,save.title, save.file){

  ##--> the output data includes FvFm, and the corresponding F',Fm',
  ##--> temperature,and PAR when Yield is maximum (i.e., when FvFm is selected)

  moni.data<-format.monidata(moni.data = moni.data)
  noonmean.PAR<-moni.data[moni.data$datetime>=as.POSIXct(paste(moni.data$date,'10:00:00'))&
                            moni.data$datetime<=as.POSIXct(paste(moni.data$date,'15:00:00')),
                          mean(par_moni,na.rm=T),
                          by=c('head','tree_num','head_tree','date')]
  names(noonmean.PAR)[5]<-'noonmean_PAR'

  moni.night<-
    moni.data[-which(moni.data$datetime<=moni.data$sunsetStart&
                       moni.data$datetime>=moni.data$sunrise),
              c("dateBack12h","head_tree","head","tree_num","par_moni",
                "temp_moni","Fm_","Yield")]
  names(moni.night)[1]<-'date'
  moni.night$date<-moni.night$date+1
  moni.night$Yield[moni.night$Yield<0|moni.night$Yield>=1]<-NA
  fvfm<-
    setDT(moni.night[,-c("par_moni","temp_moni","Fm_")])[
      ,.SD[which.max(Yield)], by=c('date','head_tree','head','tree_num')]
  fm<-
    setDT(moni.night[,-c("par_moni","temp_moni","Yield")])[
      ,.SD[which.max(Fm_)], by=c('date','head_tree','head','tree_num')]
  night.temp<-
    moni.night%>%select(-c("par_moni","Fm_","Yield"))%>%
    group_by(date , head_tree , head , tree_num )%>%
    summarise(nightmean_temp=mean(temp_moni,na.rm = T),.groups='drop')

  range.date<-
    setDT(fvfm[,c('head_tree','date')])[,lapply(.SD,range,na.rm=T),
                                        by=c('head_tree')]
  range.date$group<-rep(c('start.date','end.date'),nrow(range.date)/2)
  range.date$date<-as.character(range.date$date)
  range.date<-
    dcast(range.date,head_tree~group,value.var = 'date')
  range.date$start.date<-ymd(range.date$start.date)
  range.date$end.date<-ymd(range.date$end.date)

  fvfm.filldate<-
    ldply(1:nrow(range.date),function(i){
      date.seq<-seq(date(range.date$start.date)[i],
                    date(range.date$end.date)[i],by='day')
      fvfm<-
        fvfm[fvfm$head_tree==range.date$head_tree[i],]
      fill.dategap<-
        complete(fvfm,date=date.seq,
                 nesting(head_tree,head,tree_num))
      return(fill.dategap)
    })

  fvfm.all<-Reduce(function(x,y)
    merge(x,y,all=T,by=c('date','head_tree','head','tree_num')),
    list(fvfm.filldate,fm,noonmean.PAR,night.temp))
  names(fvfm.all)[c(5,6)]<-c('FvFm','Fm')
  fvfm.all$F0<-(1-fvfm.all$FvFm)*fvfm.all$Fm

  fvfm.all<-
    fvfm.all[,c("date","head_tree","head","tree_num",'noonmean_PAR',
                "nightmean_temp","F0","Fm",'FvFm')]
  if (save.file==T) {
    write.table(fvfm.all,file =
                  paste0(save.path,'/MONI_FvFm_',save.title,'_',
                         year(range(fvfm.all$date)[1]),'_',
                         year(range(fvfm.all$date)[2]),
                         '.dat'),
                row.names = F,sep = ';')
  }

  return(data.table(fvfm.all))

}
