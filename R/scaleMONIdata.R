#' obtain extreme values of MONI-PAM data
#'
#' These extreme values of MONI-PAM data will be used for MONI-PAM data visualization
#'
#' @usage scaleMONIdata(moni.data)
#' @param moni.data a combined organized MONI-PAM data which is generated from [readMONIPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#'
#' @return [scaleMONIdata] will return a data.table which contains the maximum and minimum values of F',FM',Yield, PAR, and temperature values for each plotting groups (every 10 days as a group) and each MONI-head
#' @export
scaleMONIdata<-function(moni.data){

  moni.data<-formatMONIdata(moni.data=moni.data)
  data<-moni.data %>%
    dplyr::select(plot.group,head,tree_num,head_tree,
                  F_,Fm_,Yield,par_moni,temp_moni)

  # Define custom functions to handle Inf values
  safe_max <- function(x) {
    result <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }

  safe_min <- function(x) {
    result <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.infinite(result)) NA else result
  }

  maxmin.res<-
    data %>%
    group_by(plot.group,head,tree_num,head_tree) %>%
    dplyr::summarise(
      across(
        .col=c('F_','Fm_','Yield','par_moni','temp_moni'),
        .fns = list(max = safe_max, min = safe_min),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    )
  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res<-
    maxmin.res %>%
    mutate(scale.Fm.a=(max.Yield-min.Yield)/(max.Fm_-min.Fm_)) %>%
    mutate(scale.Fm.b=min.Yield-scale.Fm.a*min.Fm_) %>%
    mutate(scale.temp.a=(max.Yield-min.Yield)/(max.temp_moni-min.temp_moni)) %>%
    mutate(scale.temp.b=min.Yield-scale.temp.a*min.temp_moni) %>%
    mutate(scale.par.a=(max.Yield-min.Yield)/(max.par_moni-min.par_moni)) %>%
    mutate(scale.par.b=min.Yield-scale.par.a*min.par_moni)


  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res[sapply(maxmin.res, is.nan)]<-NA
  maxmin.res[,c(15,17,19)][sapply(maxmin.res[,c(15,17,19)],is.na)]<-1
  maxmin.res[,c(16,18,20)][sapply(maxmin.res[,c(16,18,20)],is.na)]<-0
  return(maxmin.res)
}
