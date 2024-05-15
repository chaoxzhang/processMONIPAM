#' Filter MONI-PAM data step 6,remove additionally a few more data points adjacent to the previously filtered data
#'
#' Details see Zhang et al.,202X. paper link url.
#'
#' @param moni.data a data.table or data.frame MONI-PAM data generated from [filter5.lowfm] function.
#' @param save.path local folder for saving your output file
#' @param save.file If this argument is set as TRUE, the returned file will be saved to local folder, if FALSE, the file will not be saved into local folder
#' @param expand.time an integer value.  This value means a time window in minutes and allows users using different sampling intervals to decide how many points adjacent to filtered data they want to remove. For example, for a 20-min MONI-PAM measurement interval dataset, if expand.time=60 (it means 60 mins), and two points of both left and right side of current removed point will be removed from this function.
#'
#' @return [filter6.neighbor] will return a data table. Meanwhile, if save.file = TRUE, the output data.table will also be saved into local folder as a 'MONI_Year1_Year2_filter6passTOneighbor.dat' file, where Year1 and Year2 are the minimum and maximum year during this observation season respectively. This output file will contain one new column compared with output file from [filter5.lowfm] function named as 'flag6.neighbor'. This column only contain two values: 0 and 1, where 0 means F', Fm' and Yield in corresponding row(s) are abnormal data and should be removed from the dataset and 1 means good dataset.
#' @export
filter6.neighbor<-function(moni.data,
                        save.path,
                        save.file,
                        expand.time){
  start.time<-Sys.time()
  print('This function will around 1 min.')
  moni.data<-format.monidata(moni.data = moni.data)
  neighbor.filter<-
    ldply(levels(moni.data$head_tree),function(i){
      print(paste0(i, ' is filtering...'))
      moni.onetree<- moni.data[moni.data$head_tree==i,]

       if (nrow(moni.onetree)>0) {#if data exsit

        ##<<- order() to make sure correct time order
        moni.onetree<-moni.onetree[order(moni.onetree$datetime),]
        moni.onetree$flag6.neighbor<-1
        #expand to neighbor: 1 hour before and after the filtered data
        expand.back<-
          moni.onetree$datetime[is.na(moni.onetree$Fm_)]-60*expand.time
        expand.forward<-
          moni.onetree$datetime[is.na(moni.onetree$Fm_)]+60*expand.time
        flag.time<-ldply(1:length(expand.back),function(i){

         flag.time<-
           data.frame(datetime=moni.onetree$datetime[
            moni.onetree$datetime>=expand.back[i]&
              moni.onetree$datetime<=expand.forward[i]])
        })

        moni.onetree$flag6.neighbor[(moni.onetree$datetime%in%
                                      flag.time$datetime)]<-0
       }

      return(moni.onetree)
    }
    )
  if (save.file==T){
    print('The filter is done, now save the filtered data into file.')
  } else {print('The filter is done')}
  neighbor.filter[neighbor.filter$flag6.neighbor==0,c('F_','Fm_','Yield')]<-NA

  neighbor.filter$flag.all<-
    neighbor.filter$flag1.NA.Yield*neighbor.filter$flag2.night*
    neighbor.filter$flag3.day*neighbor.filter$flag4.maxYield*
    neighbor.filter$flag5.lowfm*neighbor.filter$flag6.neighbor


  neighbor.filter<-
    droplevels(neighbor.filter[,c("date", "plot.group","dateBack12h","datetime",
                               "datetimeBack12h","head", "tree_num",
                               "sunrise","sunriseEnd",
                               "solarNoon","sunsetStart",
                               "sunset","dusk",
                               "dawn","F_","Fm_",
                               "Yield", "par_moni","temp_moni","ETR",
                               'head_tree',"flag1.NA.Yield",
                               'flag2.night','flag3.day',
                               'flag4.maxYield',
                               'flag5.lowfm','flag6.neighbor','flag.all')])
  if (save.file==T){
    write.table(neighbor.filter,file =
                  paste0(save.path,'/MONI_',year(range(neighbor.filter$date)[1]),
                         '_', year(range(neighbor.filter$date)[2]),
                         '_filter6passTOneighbor.dat'),
                row.names = F,sep = ';')

  }
  print(Sys.time()-start.time)
  return(neighbor.filter)
}
