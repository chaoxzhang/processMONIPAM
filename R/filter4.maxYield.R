# fisrt filter logical function
filter.maxYield.fc41<-function(data,fm1,fmPhiPratio){
  data<-data[data$flag4.maxYield==1,]
  moni.night<-
    na.omit(data[-which(data$datetime<=data$sunsetStart&
                          data$datetime>=data$sunrise),])
  fvfm<-
    moni.night%>%
    subset(select=c('dateBack12h','datetime','Yield'))%>%
    group_by(dateBack12h)%>%
    filter(Yield==max(Yield,na.rm = T))%>%
    rename(FvFm=Yield)
  fm<-
    moni.night%>%
    subset(select=c('dateBack12h','datetime','Fm_'))%>%
    group_by(dateBack12h)%>%
    filter(Fm_==max(Fm_,na.rm = T))%>%
    rename(Fm=Fm_)

  #by merging fvfm and Fm, there will be many repeated dateback12h due to one to
  #several max FvFm or Fm.
  #for the aim of filter, if one is bad point, other same bad value point
  #that have different datetime will also be filtered.
  fvfm<-
    fvfm%>%
    merge(fm,by=c('dateBack12h'),all = T)%>%
    mutate(F0=Fm*(1-FvFm))%>%
    rename(datetime.FvFm=datetime.x,datetime.Fm=datetime.y)

  Dfvfm<-
    fvfm%>%
    subset(select=c('dateBack12h','FvFm','Fm','F0'))%>%
    unique%>%
    mutate(DFvFm=c(diff(FvFm),NA)/FvFm,
           DFm=c(diff(Fm),NA)/Fm,
           DF0=c(diff(F0),NA)/F0)%>%
    mutate(DFvFm=c(NA,DFvFm[-length(DFvFm)]),
           DFm=c(NA,DFm[-length(DFm)]),
           DF0=c(NA,DF0[-length(DF0)]),
           flag4.maxYield=1)%>%
    mutate(flag4.maxYield=case_when(
      (DFm<=(-1*fm1)&DF0<=0&DFm/DFvFm>=fmPhiPratio)|# fm decreases, F0 decreases,
        #and decreases in Fm were much higher then Fv/FM (maxYield) or
        (DFm<=(-0.2)&DFvFm>=0)| #Fm decreases and fv/fm (maxYield) increases or didnot change or
        (DF0<=(-0.3)&DFvFm>=0)| # F0 decrease and fv/fm (maxYield) increases or didnot change or
        (Fm<=100&DFm/DFvFm>1.5&DFm<0)| #Fm<100 and Fm decreases and decreases in Fm were much higher then Fv/FM (maxYield)
        (DFvFm>=0.4&DF0<0&DFm<DFvFm)| # Fv/Fm (maxYield) increases, and F0 decreases, and percentage increase in Fm
        # were lower than percentage increase in FvFm
        (DFvFm>=0.5&DFvFm/DF0>=2&  # Fv/Fm increases and increase in Fv/Fm were much higher than increase in F0 and Fm<200
           Fm<=200)
      ~0
    ))

  flag.time<-
    c(fvfm$datetime.FvFm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.maxYield==0]],
      fvfm$datetime.Fm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.maxYield==0]])

  data$flag4.maxYield[data$datetime%in%flag.time]<-0

  return(data)
}
# second filter logical function
filter.maxYield.fc42<-function(data,fm1,fmPhiPratio){
  data<-data[data$flag4.maxYield==1,]
  moni.night<-
    na.omit(data[-which(data$datetime<=data$sunsetStart&
                          data$datetime>=data$sunrise),])
  fvfm<-
    moni.night%>%
    subset(select=c('dateBack12h','datetime','Yield'))%>%
    group_by(dateBack12h)%>%
    filter(Yield==max(Yield,na.rm = T))%>%
    rename(FvFm=Yield)
  fm<-
    moni.night%>%
    subset(select=c('dateBack12h','datetime','Fm_'))%>%
    group_by(dateBack12h)%>%
    filter(Fm_==max(Fm_,na.rm = T))%>%
    rename(Fm=Fm_)

  #by merging fvfm and Fm, there will be many repeated dateback12h due to one to
  #several max FvFm or Fm.
  #for the aim of filter, if one is bad point, other same bad value point
  #that have different datetime will also be filtered.
  fvfm<-
    fvfm%>%
    merge(fm,by=c('dateBack12h'),all = T)%>%
    mutate(F0=Fm*(1-FvFm))%>%
    rename(datetime.FvFm=datetime.x,datetime.Fm=datetime.y)

  Dfvfm<-
    fvfm%>%
    subset(select=c('dateBack12h','FvFm','Fm','F0'))%>%
    unique%>%
    mutate(DFvFm=c(diff(FvFm),NA)/FvFm,
           DFm=c(diff(Fm),NA)/Fm,
           DF0=c(diff(F0),NA)/F0)%>%
    mutate(DFvFm=c(NA,DFvFm[-length(DFvFm)]),
           DFm=c(NA,DFm[-length(DFm)]),
           DF0=c(NA,DF0[-length(DF0)]),
           flag4.maxYield=1)%>%
    mutate(flag4.maxYield=case_when(
      (DFvFm<0&DFm>0.5)|
        (DFvFm<0&DF0>0.5)|
        (DFvFm>0&DFm>1&DFvFm<DFm)
      ~0
    ))

  flag.time<-
    c(fvfm$datetime.FvFm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.maxYield==0]],
      fvfm$datetime.Fm[fvfm$dateBack12h%in%Dfvfm$dateBack12h[Dfvfm$flag4.maxYield==0]])

  data$flag4.maxYield[data$datetime%in%flag.time]<-0

  return(data)
}

#' Filter MONI-PAM data step 4,remove abnormal maximum Yield (or Fv/Fm) value
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @param moni.data a data.table or data.frame MONI-PAM data generated from [filter3.day] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param fm1 the threshold of percentage change of Fm' between time2 and time1. Default value is 0.15, we recommend this argument can be adjusted from 0.05 to 0.2 by an interval of 0.05.
#' @param fmPhiPratio the threshold of ratio between percentage change of Fm' between time2 and time1 and of Yield between time2 and time1. Default value is 3, we recommend this argument can be adjust between 2 and 5 by an interval of 1.
#'
#' @return [filter4.maxYield] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'MONI_Year1_Year2_filter4maxYield.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter3.day] function named as 'flag4.maxYield'. This column only contain two values: 0 and 1, where 0 means F', Fm' and Yield in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter4.maxYield<-function(moni.data,
                           save.path,
                           save.file,
                           fm1=0.15,
                           fmPhiPratio=3
                           ){
  print('This function will run mins...' )

  moni.data<-format.monidata(moni.data = moni.data)
  start.time<-Sys.time()
  maxYield.filter<-
    ldply(levels(moni.data$head_tree),function(i){
      print(paste0(i, ' is filtering...'))
      moni.onetree<-moni.data[moni.data$head_tree==i,]

      if (nrow(moni.onetree[which((!is.na(moni.onetree$Yield))&
                                  !is.na(moni.onetree$Fm_)&
                                  !is.na(moni.onetree$F_)),])>0) {#if data exsit

        ##<<- the data should be filtered following the
        ##<<- correct date and time order
        ##<<- so order() function is used here to make sure correct time order
        moni.onetree<-moni.onetree[order(moni.onetree$datetime),]
        moni.onetree$flag4.maxYield<-1
        filter.onetreeYield1<-
          filter.maxYield.fc41(data=moni.onetree,fm1=fm1,
                               fmPhiPratio=fmPhiPratio)
        trials <- 0
        while(nrow(filter.onetreeYield1[
          filter.onetreeYield1$flag4.maxYield==0,])>0){
          filter.onetreeYield1<-
            filter.maxYield.fc41(filter.onetreeYield1,fm1=fm1,
                                 fmPhiPratio=fmPhiPratio)
          trials <- trials +1
        }

        filter.onetreeYield2<-
          filter.maxYield.fc42(data=filter.onetreeYield1,fm1=fm1,
                               fmPhiPratio=fmPhiPratio)
        trials <- 0
        while(nrow(filter.onetreeYield2[
          filter.onetreeYield2$flag4.maxYield==0,])>0){
          filter.onetreeYield2<-
            filter.maxYield.fc42(filter.onetreeYield2,fm1=fm1,
                                 fmPhiPratio=fmPhiPratio)
          trials <- trials +1
        }


        moni.onetree$flag4.maxYield[
          !moni.onetree$datetime%in%filter.onetreeYield2$datetime]<-0
        #if all data during night is filtered, remove whole day
        to.fvfm<-moni.onetree
        to.fvfm[to.fvfm$flag4.maxYield==0,c('F_','Fm_','Yield')]<-NA
        sub.fvfm<-FindFvFm(to.fvfm,save.file = F,
                           save.path = save.path,save.title = '')
        sub.fvfm<-sub.fvfm[is.na(sub.fvfm$Fm),]
        moni.onetree$flag4.maxYield[
          (moni.onetree$dateBack12h%in%(sub.fvfm$date-1))&
            (moni.onetree$flag.all==1)]<-0
      }

      return(moni.onetree)

    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  maxYield.filter<-data.table(maxYield.filter)
  maxYield.filter[maxYield.filter$flag4.maxYield==0,c('F_','Fm_','Yield')]<-NA

  maxYield.filter$flag.all<-
    maxYield.filter$flag.all*maxYield.filter$flag4.maxYield
  maxYield.filter<-
    droplevels(maxYield.filter[,c("date", "plot.group","dateBack12h","datetime",
                                  "datetimeBack12h","head", "tree_num",
                                  "sunrise","sunriseEnd",
                                  "solarNoon","sunsetStart",
                                  "sunset","dusk",
                                  "dawn","F_","Fm_","Yield",
                                  "par_moni","temp_moni","ETR",'head_tree',
                                  "flag1.NA.Yield",'flag2.night',
                                  'flag3.day',
                                  "flag4.maxYield",'flag.all')])
  if (save.file==T){
    write.table(maxYield.filter,file =
                  paste0(save.path,'/MONI_',year(range(maxYield.filter$date)[1]),
                         '_', year(range(maxYield.filter$date)[2]),
                         '_filter4maxYield.dat'),
                row.names = F,sep = ';')
  }
  print(Sys.time()-start.time)
  return(maxYield.filter)
}
