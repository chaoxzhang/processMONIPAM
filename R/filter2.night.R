filter.night.fc<-function(data,period,fm1,fmPhiPratio){

  data<-data[data$flag2.night==1,]

  moni.night<-na.omit(data[data$datetime%in%period,
                           c('dateBack12h','datetime','F_','Fm_',
                             'Yield','flag2.night')])

  moni.night<-ldply(unique(moni.night$dateBack12h),function(i){
    #i=1
    moni.night.oneday<-moni.night[moni.night$dateBack12h==i,]
    moni.night.oneday<-
      moni.night.oneday[order(moni.night.oneday$datetime),]

    moni.night.oneday$vary.Yield<-
      c(NA,diff(moni.night.oneday$Yield))/moni.night.oneday$Yield
    moni.night.oneday$vary.Fm<-
      c(NA,diff(moni.night.oneday$Fm_))/moni.night.oneday$Fm_
    moni.night.oneday$vary.F<-
      c(NA,diff(moni.night.oneday$F_))/moni.night.oneday$F_
    if (fm1>=0&
        fmPhiPratio>=0){


      ##> impossible cases
      ##> 1. Yield increases, but Fm' decreases a lot and F' decreases
      ##> 2. Fm' decreases a lot, F' decreases, and decreases in Fm'
      ##     is fmPhiPratio times of decreases in Yield
      moni.night.oneday$flag2.night[
        (moni.night.oneday$vary.Yield>=0&
           moni.night.oneday$vary.Fm<=(-1*fm1)&
           moni.night.oneday$vary.F<=0)|
          (moni.night.oneday$vary.Fm<=(-1*fm1)&
             moni.night.oneday$vary.F<=0&
             moni.night.oneday$vary.Fm/moni.night.oneday$vary.Yield>=
             fmPhiPratio)]<-0
    }
    return(moni.night.oneday)
  })
  data$flag2.night[
    data$datetime%in%
      (moni.night$datetime[moni.night$flag2.night==0])]<-0
  return(data)
}

#' Filter MONI-PAM data step 2,remove abnormal F', Fm' and Yield value from the night
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @usage filter2.night(moni.data,fm1=0.03,fmPhiPratio=3,save.path,save.file)
#' @param moni.data a data.table or data.frame MONI-PAM data generated from [filter1.NA] function.
#' @param fm1 the threshold of percentage change of Fm' between time2 and time1. Default value is 0.03, we recommend this argument can be adjusted from 0.01 to 0.05 by an interval of 0.01.
#' @param fmPhiPratio the threshold of ratio between percentage change of Fm' between time2 and time1 and of Yield between time2 and time1. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#'
#' @return [filter2.night] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'MONI_Year1_Year2_filter2night.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter1.NA] function named as 'flag2.night'. This column only contain two values: 0 and 1, where 0 means F', Fm' and Yield in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter2.night<-function(moni.data,
                        fm1=0.03,
                        fmPhiPratio=3,
                        save.path,
                        save.file)
{
  print('This function will run mins...')
  start.time<-Sys.time()
  moni.data<-formatMONIdata(moni.data = moni.data)

  night.filter<-
    ldply(levels(moni.data$head_tree),function(i){
      #i=levels(moni.data$head_tree)[2]
      moni.onetree <- moni.data[moni.data$head_tree==i,]

      if (nrow(moni.onetree)>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        moni.onetree<-moni.onetree[order(moni.onetree$datetime),]
        moni.onetree$flag2.night<-1

        # one hour buffer added to sunset time, but not dawn time because
        # it will filter the good data between dawn and sunrise
        flag.period<-
          moni.onetree$datetime[
            -which(moni.onetree$datetime<=moni.onetree$sunsetStart-3600&
                     moni.onetree$datetime>=moni.onetree$dawn)]

        filter.onetree<-
          filter.night.fc(data=moni.onetree, period=flag.period,
                          fm1=fm1,fmPhiPratio=fmPhiPratio)
        trials <- 0
        print(paste0(i, ' is filtering...'))
        while(nrow(filter.onetree[filter.onetree$flag2.night==0,])>0){
          filter.onetree<-
            filter.night.fc(data=filter.onetree,period=flag.period,
                            fm1=fm1,fmPhiPratio=fmPhiPratio)
          trials <- trials +1
        }
        moni.onetree$flag2.night[
          !moni.onetree$datetime%in%filter.onetree$datetime]<-0

        return(moni.onetree)
      }
    }
    )


  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}

  night.filter[night.filter$flag2.night==0,c('F_','Fm_','Yield')]<-NA

  night.filter$flag.all<-
    night.filter$flag1.NA.Yield*night.filter$flag2.night

  night.filter<-
    droplevels(night.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "Yield", "par_moni","temp_moni","ETR",
                               'head_tree',"flag1.NA.Yield",
                               'flag2.night','flag.all')])
  if (save.file==T){
    write.table(night.filter,row.names = F,sep = ';', file =
                  paste0(save.path,'/MONI_',year(range(night.filter$date)[1]),
                         '_',year(range(night.filter$date)[2]),
                         '_filter2night.dat'))

  }
  print(Sys.time()-start.time)
  return(night.filter)
}
