
scaleNPQYield<-function(diurnal.para){

  diurnal.para<-formatMONIdata(moni.data = diurnal.para)
  data<-diurnal.para %>%
    dplyr::select(plot.group,head,tree_num,head_tree,
                  Phi_NPQ,Phi_NPQr,Phi_NPQs,
                  par_moni,temp_moni)

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
        .col=c('Phi_NPQ','Phi_NPQr','Phi_NPQs',
               'par_moni','temp_moni'),
        .fns = list(max = safe_max, min = safe_min),
        .names = "{.fn}.{.col}"
      ),
      .groups = 'drop'
    )

  maxmin.res<-
    maxmin.res %>%
    mutate(scale.NPQr.a=(max.Phi_NPQ-min.Phi_NPQ)/(max.Phi_NPQr-min.Phi_NPQr)) %>%
    mutate(scale.NPQr.b=min.Phi_NPQ-scale.Fm.a*min.Phi_NPQr) %>%
    mutate(scale.NPQs.a=(max.Phi_NPQ-min.Phi_NPQ)/(max.Phi_NPQs-min.Phi_NPQs)) %>%
    mutate(scale.NPQs.b=min.Phi_NPQ-scale.Fm.a*min.Phi_NPQs) %>%

    mutate(scale.temp.a=(max.Phi_NPQ-min.Phi_NPQr)/(max.temp_moni-min.temp_moni)) %>%
    mutate(scale.temp.b=min.Phi_NPQr-scale.temp.a*min.temp_moni) %>%
    mutate(scale.par.a=(max.Phi_NPQ-min.Phi_NPQr)/(max.par_moni-min.par_moni)) %>%
    mutate(scale.par.b=min.Phi_NPQr-scale.par.a*min.par_moni)

  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res[sapply(maxmin.res, is.nan)]<-NA
  maxmin.res[,c(15,17,19,21)][sapply(maxmin.res[,c(15,17,19,21)],is.na)]<-1
  maxmin.res[,c(16,18,20,22)][sapply(maxmin.res[,c(16,18,20,22)],is.na)]<-0
  return(maxmin.res)
}


#' Diurnal NPQ yield parameters data visualization with dairy note and sunlight time marked in the figure
#'
#' This function will visualize MONI-PAM diurnal NPQ yield parameters data (PhiNPQ,PhiNPQr,PhiNPQs) and PAR and temperature in one figure with 10 days as a group plotting for each head and the dairy note will be marked in the figure if the note file is available. At the same time, this function will also show  the sunlight time (colorful vertical lines) in the plot, sunrise and sunsetStart are 'indianred', solarNoon is 'goldenrod', and dawn and dusk are 'purple'
#'
#' @usage plotNPQYield(diurnal.para,diary,diary.data,save.path)
#' @param diurnal.para estimated diurnal parameters generated from [diurnalParams] function in this R package
#' @param diary TRUE or FALSE. If diary = TRUE, dairy data should be assigned.
#' @param diary.data a diary file which can be such as .csv or .dat file but with a specific data format. You can check the example data from this package using data("diary_2014_2015"). In this dataset, head and tree_num should be same with your MONI-PAM data, text in 'remark.plot' column will be shown in the plotted figure to show what has happened for that 'head'/ 'tree_num'/'head_tree' on which 'datetime' so it should be very short and clean. 'remark' column can can contain full description for remark.plot. 'manager' column shows who found that issue or conducted certain activity for that sensor (e.g., calibration)
#' @param save.path local folder for saving your plotted figures
#'
#' @return [plotNPQYield] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotNPQYield<-function(diurnal.para,
                       diary,
                       diary.data,
                       save.path
                       ){

  start.time<-Sys.time()
  plot.title='Diurnal NPQ yield parameters'
  diurnal.para<-formatMONIdata(moni.data = diurnal.para)
  diurnal.para<-diurnal.para %>% mutate(plot.group=as.factor(plot.group))
  extreme.data<-scaleNPQYield(diurnal.para)

  if (isTRUE(diary==TRUE)){
    diary.data=diary.data
    diary.data$datetime<-ymd_hms(diary.data$datetime,truncated = 3)
  }

  lapply(levels(diurnal.para$plot.group),function(i){

    diurnal.para.onegroup<-
      droplevels(diurnal.para[diurnal.para$plot.group==i,])

    if (isTRUE(sum(na.omit(diurnal.para.onegroup$Yield))>0)){#when there is data exsit

      scaledata.onegroup<-extreme.data %>% subset(plot.group==i) %>% droplevels()

      plot.onegroup<-
        lapply(levels(diurnal.para.onegroup$head_tree),function(j){

          scale.data<-scaledata.onegroup %>% subset(head_tree==j) %>% droplevels()


          NPQrtoNPQ<-function(x){scale.data$scale.NPQr.a*x+scale.data$scale.NPQr.b}
          NPQstoNPQ<-function(x){scale.data$scale.NPQs.a*x+scale.data$scale.NPQs.b}
          TtoNPQ<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
          PARtoNPQ<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}

          plot.data<-diurnal.para.onegroup %>% subset(head_tree==j) %>% droplevels()

          if (isTRUE(diary==TRUE)){

            diary.group<-
              diary.data %>%
              subset(head_tree==j&plot.group==i) %>%
              dplyr::select(datetime,head_tree,plot.group,remark.plot) %>%
              droplevels()

            if (isTRUE(length(plot.data$Yield[!is.na(plot.data$Yield)])<=2&#if Yield does not exist
                       length(plot.data$par_moni[!is.na(plot.data$par_moni)])<=2&#if PAR and T also do not exist
                       length(diary.group$remark.plot[!is.na(diary.group$remark.plot)])>0#but there is a diary,
                       #then only plot text of remark.plot column in diary data
            )) {
              print(paste0('plot.group[',i,']:',j))
              plot.moni.figure<-
                ggplot(diary.group,aes(x=datetime))+
                geom_point(aes(y=0.55), color='red',size=4,na.rm = T)+
                geom_text(aes(y=0.6,label=remark.plot),color='red',size=5,na.rm = T)+
                scale_x_datetime(breaks = date_breaks('1 day'),
                                 labels = date_format("%b-%d\n%H:%M"),
                                 limits = c(range(na.omit(plot.data$datetime))[1],
                                            range(na.omit(plot.data$datetime))[2]))+
                ylim(0.4,0.7)
            } else if (isTRUE(length(plot.data$Yield[!is.na(plot.data$Yield)])<=2&#if Yield does not exist
                              length(plot.data$par_moni[!is.na(plot.data$par_moni)])>2&#if PAR and T exist
                              length(diary.group$remark.plot[!is.na(diary.group$remark.plot)])>0#but there is a diary,
                              #then plot PAR and Temperature with remark.plot
            )) {
              print(paste0('plot.group[',i,']:',j))
              point.posi<-mean(plot.data$temp_moni,na.rm = T)
              scale.parA<-
                (max(plot.data$temp_moni,na.rm=T)-min(plot.data$temp_moni,na.rm=T))/
                (max(plot.data$par_moni,na.rm=T)-min(plot.data$par_moni,na.rm=T))
              scale.parB<-
                max(plot.data$temp_moni,na.rm=T)-
                scale.parA*max(plot.data$par_moni,na.rm=T)

              p1<-
                ggplot(plot.data,aes(x=datetime))+
                geom_point(data=diary.group,aes(y=point.posi), color='red',size=2,na.rm = T)+
                geom_text(data=diary.group,aes(y=point.posi+2,label=remark.plot),
                          color='red',size=5,na.rm = T)+
                geom_line(aes(y=temp_moni),color='darkorchid',na.rm = T)+
                geom_line(aes(y=scale.parA*par_moni+scale.parB),
                          color='yellow3',na.rm = T)+
                scale_x_datetime(breaks = date_breaks('1 day'),
                                 labels = date_format("%b-%d\n%H:%M"),
                                 limits = c(range(na.omit(plot.data$datetime))[1],
                                            range(na.omit(plot.data$datetime))[2]))+
                scale_y_continuous(sec.axis = sec_axis(~(.-scale.parB)/scale.parA,
                                                       name='F or Fm'),
                                   name = bquote(atop(paste(Phi,'PSII'),
                                                      .(j))))+
                theme_bw()+
                theme(legend.position = 'none',
                      title = element_text(size=12,color='black',face='bold'),
                      axis.text.y.left = element_text(size = 15,color = 'white'),
                      axis.text.y.right = element_text(size = 15,color = 'white'),

                      axis.text.x =  element_text(size = 15,color = 'black'),
                      axis.title.y.left = element_text(size = 16,color = 'forestgreen'),
                      axis.title.y.right = element_text(size = 16,color = 'black'),

                      axis.title.x =element_blank(),
                      axis.ticks.y =element_line(color = 'white'),
                      axis.line.y.left = element_line(color = 'forestgreen'),
                      plot.margin = unit(c(0,0,0,0),'cm'))
              p23.fc<-function(p23.color){
                ggplot(plot.data,aes(x=datetime))+
                  geom_line(aes(y=temp_moni),
                            size=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=scale.parA*par_moni+scale.parB),
                            size=0.1,color='white',na.rm = T)+
                  theme_minimal()+
                  theme(legend.position = 'none',
                        axis.text.y.right = element_text(size = 15,color = p23.color),
                        axis.text.y.left = element_blank(),
                        axis.line.y.right = element_line(color = p23.color),
                        axis.text.x =   element_blank(),
                        axis.title.y.right  = element_text(size = 15,color = p23.color),
                        axis.title.y.left = element_blank(),
                        axis.title.x =element_blank(),
                        axis.ticks.y.left = element_blank(),
                        axis.ticks.y.right =element_line(color = p23.color),
                        panel.grid=element_blank(),
                        plot.margin = unit(c(0,0,0,0),'cm'))
              }
              #plot second left-y-axis: Temperature
              p2<-p23.fc('darkorchid')+
                scale_y_continuous(
                  sec.axis = sec_axis(~.,name=expression(paste('Temp ('^'o','C)'))))
              #plot third left-y-axis: PAR
              p3<-p23.fc('yellow3')+
                scale_y_continuous(
                  sec.axis = sec_axis(~(.-scale.parB)/scale.parA,
                                      name='PAR'))
              plot.moni.figure<-p1 + p2 + p3 +
                plot_layout(nrow =1, widths = c(15, 0.1, 0.1))
            }
          }

          if (isTRUE(sum(na.omit(plot.data$Yield))>0&
                     length(na.omit(plot.data$Yield))>2)) {#if data exist
            night<-unique(plot.data[,c('date','sunsetStart','sunrise')])
            night<-night[order(night$sunsetStart),]

            if (isTRUE(nrow(night)>1)){
              night$sunriseDay2<-ymd_hms(night$sunrise[2:nrow(night)],NA)
            } else if(isTRUE(nrow(night)==1)){
              night$sunriseDay2<-night$sunrise+3600*24
            }

            night<-merge(plot.data[,c('datetime','date')],night,by='date',all=T)

            p1<-ggplot(plot.data,aes(x=datetime))+
              geom_rect(data=night,fill='lightskyblue1',na.rm = T,alpha=0.008,
                        aes(xmin=sunsetStart,xmax=sunriseDay2,
                            ymin=-Inf,ymax=Inf))+
              geom_vline(aes(xintercept=sunrise),na.rm = T,
                         color='indianred1',linetype=2)+
              geom_vline(aes(xintercept=solarNoon),na.rm = T,
                         color='goldenrod1',linetype=2)+
              geom_vline(aes(xintercept=sunsetStart),na.rm = T,
                         color='indianred1',linetype=2)+
              geom_vline(aes(xintercept=dusk),na.rm = T,
                         color='purple',linetype=2)+
              geom_vline(aes(xintercept=dawn),na.rm = T,
                         color='purple',linetype=2)+
              geom_line(aes(y=TtoNPQ(temp_moni)),color='darkorchid',na.rm = T,alpha=0.5)+
              geom_line(aes(y=PARtoNPQ(par_moni)),color='yellow3',na.rm = T)+
              geom_line(aes(y=Phi_NPQ),color='darkred',na.rm = T)+
              geom_point(aes(y=Phi_NPQ),size=0.5,color='darkred',na.rm = T)+
              geom_line(aes(y=Phi_NPQr),color='forestgreen',na.rm = T)+
              geom_point(aes(y=Phi_NPQr),color='forestgreen',size=0.5,na.rm = T)+
              geom_line(aes(y=Phi_NPQs),color='black',na.rm = T)+
              geom_point(aes(y=Phi_NPQs),size=0.5,color='black',na.rm = T)+
              scale_x_datetime(breaks = date_breaks("1 day"),
                               labels = date_format("%b-%d\n%H:%M"),
                               limits = c(range(na.omit(diurnal.para.onegroup$datetime))[1],
                                          range(na.omit(diurnal.para.onegroup$datetime))[2]))+
              scale_y_continuous(
                sec.axis = sec_axis(~.,name="<span style='color:forestgreen;'>&#934;NPQr or <span style='color: black;'>&#934;NPQs"),
                name = bquote(atop(paste(Phi,'NPQ'),
                                   .(j))))+
              theme_bw()+
              theme(legend.position = 'none',
                    title = element_text(size=12,color='black',face='bold'),
                    axis.text.y.left = element_text(size = 15,color = 'darkred'),
                    axis.text.y.right = element_text(size = 15,color = 'black'),
                    axis.text.x =  element_text(size = 15,color = 'black'),
                    axis.title.y.right  = element_markdown(size = 16),
                    axis.title.y.left = element_text(size = 16,color = 'darkred'),
                    axis.title.x =element_blank(),
                    axis.ticks.y.left =element_line(color = 'darkred'),
                    axis.line.y.left = element_line(color = 'darkred'),
                    plot.margin = unit(c(0,0,0,0),'cm'))

            if (isTRUE(diary==TRUE)){

              if (isTRUE(length(na.omit(diary.group$remark.plot))>0)) {

                p1<-p1+
                  geom_point(data=na.omit(diary.group),
                             aes(x=datetime,y=max(plot.data$Yield,na.rm = T)-0.05),
                             color='red',size=4,na.rm=T)+
                  geom_text(data=na.omit(diary.group),
                            aes(x=datetime,y=max(plot.data$Yield,na.rm = T),
                                label=remark.plot),color='red',size=3,na.rm = T)
              } else {
                p1<-p1
              }
            } else {
              p1<-p1
            }

            p23.fc<-function(p23.color){
              ggplot(plot.data,aes(x=datetime))+
                geom_line(aes(y=TtoNPQ(temp_moni)),linewidth=0.1,color='white',na.rm = T)+
                geom_line(aes(y=PARtoNPQ(par_moni)),linewidth=0.1,color='white',na.rm = T)+
                geom_line(aes(y=Phi_NPQr),color='white',na.rm = T)+
                geom_point(aes(y=Phi_NPQr),size=1,color='white',na.rm = T)+

                geom_line(aes(y=Phi_NPQs),color='white',na.rm = T)+
                geom_point(aes(y=Phi_NPQs),size=1,color='white',na.rm = T)+

                geom_line(aes(y=Phi_NPQ),color='white',na.rm = T)+
                geom_point(aes(y=Phi_NPQ),size=1,color='white',na.rm = T)+
                theme_minimal()+
                theme(legend.position = 'none',
                      axis.text.y.right = element_text(size = 15,color = p23.color),
                      axis.text.y.left = element_blank(),
                      axis.line.y.right = element_line(color = p23.color),
                      axis.text.x =   element_blank(),
                      axis.title.y.right  = element_text(size = 15,color = p23.color),
                      axis.title.y.left = element_blank(),
                      axis.title.x =element_blank(),
                      axis.ticks.y.left = element_blank(),
                      axis.ticks.y.right =element_line(color = p23.color),
                      panel.grid=element_blank(),
                      plot.margin = unit(c(0,0,0,0),'cm'))
            }
            #plot second left-y-axis: Temperature
            p2<-p23.fc('darkorchid')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.temp.b)/scale.data$scale.temp.a,
                                    name=expression(paste('Temp ('^'o','C)'))))
            #plot third left-y-axis: PAR
            p3<-p23.fc('yellow3')+
              scale_y_continuous(
                sec.axis = sec_axis(~(.-scale.data$scale.par.b)/scale.data$scale.par.a,
                                    name='PAR'))
            print(paste0('plot.group[',i,']:',j,' is plotted'))
            plot.moni.figure <- p1 + p2 + p3 +
              plot_layout(nrow =1, widths = c(15, 0.1, 0.1))
          }
        }
        )

      if (isTRUE(length(plot.onegroup[!sapply(plot.onegroup,is.null)])>0)){

        plot.all<-
          suppressWarnings(plot_grid(plotlist = plot.onegroup[!sapply(plot.onegroup,is.null)],
                                     align = 'hv',ncol = 1))

        title <- ggdraw() +
          draw_label(paste0(plot.title,
                            ' from ',range(na.omit(diurnal.para.onegroup$date))[1],
                            ' to ', range(na.omit(diurnal.para.onegroup$date))[2]),
                     x = 0.05,hjust = 0,fontface='bold',size=20)+
          theme(plot.margin = margin(0,0,0,0))

        plot.all.title<-
          plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))

        daylength.x<-as.numeric(diff(range(na.omit(diurnal.para.onegroup$date))))
        headlength.y<-length(plot.onegroup[!sapply(plot.onegroup,is.null)])
        ggsave(plot.all.title,
               filename = paste0(save.path,'/',plot.title,
                                 ' from ',range(na.omit(diurnal.para.onegroup$date))[1],
                                 ' to ',range(na.omit(diurnal.para.onegroup$date))[2],'.tiff'),
               compression='lzw',dpi=100,width = 1.5*daylength.x+6,
               height = 2*headlength.y+0.3)

      }
    }
  })
  print(Sys.time()-start.time)

}
