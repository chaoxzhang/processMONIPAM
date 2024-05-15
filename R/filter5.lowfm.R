filter.lowfm.fc<-function(data,fm1,fmPhiPratio){
  data=data[
    data$flag5.lowfm==1,c('date','datetime','F_','Fm_','Yield','flag5.lowfm')]
  data<-data[order(data$datetime),]
  data$ID<-c(1:nrow(data))
  data.filter<-na.omit(data)
  data.filter$dif.ID<- c(NA,diff(data.filter$ID))
  #calculate percentage changes in Yield, Fm', and F'
  data.filter$vary.Yield<-c(NA,diff(data.filter$Yield))/data.filter$Yield
  data.filter$vary.Fm<-c(NA,diff(data.filter$Fm_))/data.filter$Fm_
  data.filter$vary.F<-c(NA,diff(data.filter$F_))/data.filter$F_
  ##>> select data only when at least 2 points have been filtered by
  ##   filter functions 1 to 4
  data.filter<-data.filter[data.filter$dif.ID>=3,]

  if (fm1>=0&
      fmPhiPratio>0){
    data.filter$flag5.lowfm[
      (data.filter$vary.Yield>=0&data.filter$vary.Fm<=(-1*fm1))|
        (data.filter$vary.Fm<=(-1*fm1)&
           data.filter$vary.Fm/data.filter$vary.Yield>=fmPhiPratio)]<-0
  }


  data$flag5.lowfm[
    data$datetime%in%(data.filter$datetime[data.filter$flag5.lowfm==0])]<-0
  return(data)
}

#' Filter MONI-PAM data step 5,remove additionally abnormal low Fm’ data that is or are adjacent to previously filtered data
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @param moni.data a data.table or data.frame MONI-PAM data generated from [filter4.maxYield] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param fm1 the threshold of percentage change of Fm' between time2 and time1. Default value is 0.2, we recommend this argument can be adjusted from 0.05 to 0.5 by an interval of 0.05.
#' @param fmPhiPratio the threshold of ratio between percentage change of Fm' between time2 and time1 and of Yield between time2 and time1. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1.
#'
#' @return [filter5.lowfm] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'MONI_Year1_Year2_filter5lowfm.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter4.maxYield] function named as 'flag5.lowfm'. This column only contain two values: 0 and 1, where 0 means F', Fm' and Yield in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
filter5.lowfm<-function(moni.data,
                        save.path,
                        save.file,
                        fm1=0.2,
                        fmPhiPratio=3){
  start.time<-Sys.time()
  print('This function will run mins')
  moni.data<-format.monidata(moni.data = moni.data)
  lowfm.filter<-
    ldply(levels(moni.data$head_tree),function(i){
      ##<<- the data should be filtered following the
      ##<<- correct date and time order
      ##<<- so order() function is used here to make sure correct order
      moni.onetree<- moni.data[moni.data$head_tree==i,]


      if (nrow(moni.onetree)>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        moni.onetree<-moni.onetree[order(moni.onetree$datetime),]
        moni.onetree$flag5.lowfm<-1

        filter.onetree<-
          filter.lowfm.fc(data=moni.onetree,fm1=fm1,fmPhiPratio=fmPhiPratio)
        trials <- 0
        print(paste0(i, ' is filtering...'))
        while(nrow(filter.onetree[filter.onetree$flag5.lowfm==0,])>0){
          filter.onetree<-
            filter.lowfm.fc(data=filter.onetree,fm1=fm1,fmPhiPratio=fmPhiPratio)
          trials <- trials +1
        }

        moni.onetree$flag5.lowfm[
          !moni.onetree$datetime%in%filter.onetree$datetime]<-0

      }

      return(moni.onetree)
    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  lowfm.filter[lowfm.filter$flag5.lowfm==0,c('F_','Fm_','Yield')]<-NA

  lowfm.filter$flag.all<-
    lowfm.filter$flag1.NA.Yield*lowfm.filter$flag2.night*
    lowfm.filter$flag3.day*lowfm.filter$flag4.maxYield*lowfm.filter$flag5.lowfm


  lowfm.filter<-
    droplevels(lowfm.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "Yield", "par_moni","temp_moni","ETR",
                               'head_tree',"flag1.NA.Yield",
                               'flag2.night','flag3.day',
                               'flag4.maxYield',
                               'flag5.lowfm','flag.all')])
  if (save.file==T){
    write.table(lowfm.filter,file =
                  paste0(save.path,'/MONI_',year(range(lowfm.filter$date)[1]),
                         '_', year(range(lowfm.filter$date)[2]),
                         '_filter5lowfm.dat'),
                row.names = F,sep = ';')

  }
  print(Sys.time()-start.time)
  return(lowfm.filter)
}
