#' obtain extreme values of MONI-PAM data
#'
#' These extreme values of MONI-PAM data will be used for MONI-PAM data visualization
#'
#'
#' @param moni.data a combined organized MONI-PAM data which is generated from [read.MONIPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#'
#' @return [scale.monidata] will return a data.table which contains the maximum and minimum values of F',FM',Yield, PAR, and temperature values for each plotting groups (every 10 days as a group) and each MONI-head
#' @export
scale.monidata<-function(moni.data){

  moni.data<-format.monidata(moni.data=moni.data)

  data<-
    moni.data[,c('plot.group','head','tree_num','head_tree',
                 'F_','Fm_','Yield','par_moni','temp_moni')]
  max.res.F<-data[,lapply(.SD,max,na.rm=T),
                  by=c('plot.group','head','tree_num','head_tree')]
  names(max.res.F)[5:9]<-
    c('max.F_','max.Fm_','max.Yield','max.par_moni','max.temp_moni')
  min.res.F<-data[,lapply(.SD,min,na.rm = T),
                  by=c('plot.group','head','tree_num','head_tree')]
  names(min.res.F)[5:9]<-
    c('min.F_','min.Fm_','min.Yield','min.par_moni','min.temp_moni')
  maxmin.res<-
    merge(max.res.F,min.res.F,by=c('plot.group','head','tree_num','head_tree'),all = T)
  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA

  maxmin.res$scale.Fm.a<-
    (maxmin.res$max.Yield-maxmin.res$min.Yield)/
    (maxmin.res$max.Fm_-maxmin.res$min.Fm_)
  maxmin.res$scale.Fm.b<-
    maxmin.res$max.Yield-maxmin.res$scale.Fm.a*maxmin.res$max.Fm_

  maxmin.res$scale.temp.a<-
    (maxmin.res$max.Yield-maxmin.res$min.Yield)/
    (maxmin.res$max.temp-maxmin.res$min.temp)
  maxmin.res$scale.temp.b<-
    maxmin.res$max.Yield-maxmin.res$scale.temp.a*maxmin.res$max.temp
  maxmin.res$scale.par.a<-
    (maxmin.res$max.Yield-maxmin.res$min.Yield)/
    (maxmin.res$max.par-maxmin.res$min.par)
  maxmin.res$scale.par.b<-
    maxmin.res$max.Yield-maxmin.res$scale.par.a*maxmin.res$max.par
  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res[sapply(maxmin.res, is.nan)]<-NA
  maxmin.res[,c(15,17,19)][sapply(maxmin.res[,c(15,17,19)],is.na)]<-1
  maxmin.res[,c(16,18,20)][sapply(maxmin.res[,c(16,18,20)],is.na)]<-0
  return(maxmin.res)
}
