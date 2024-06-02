#' MONI-PAM data visualization with dairy note marked in the figure
#'
#' This function will visualize MONI-PAM data (F',FM',Yield,PAR, and temperature in one figure) with 10 days as a group plotting for each head and the dairy note will be marked in the figure if the note file is available.
#'
#' @usage plotMONIdiary(moni.data,diary.data,plot.title,save.path)
#' @param moni.data a combined organized MONI-PAM data which is generated from [readMONIPAM] function, or a MONI-PAM data after data filtering which is generated from filter function in this package such as [filter1.NA], [filter2.night] and so on.
#' @param diary TRUE or FALSE. If diary = TRUE, dairy data should be assigned.
#' @param diary.data a diary file which can be such as .csv or .dat file but with a specific data format. You can check the example data from this package using data("dairy_for_plot"). In this dataset, head and tree_num should be same with your MONI-PAM data, text in 'remark.plot' column will be shown in the plotted figure to show what has happened for that 'head'/ 'tree_num'/'head_tree' on which 'datetime' so it should be very short and clean. 'remark' column can can contain full description for remark.plot. 'manager' column shows who found that issue or conducted certain activity for that sensor (e.g., calibration)
#' @param plot.title any text to describe clearly about your figure content
#' @param save.path local folder for saving your plotted figures
#'
#' @return [plotMONIdiary] will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plotMONIdiary<-function(moni.data,
                        diary,
                        diary.data,
                        plot.title,
                        save.path
                        ){

  start.time<-Sys.time()
  #If a warning like the following one is printed, please just omit it.
  #This warning is not mean error or mistake.
  #[warning:No non-missing values found in at least one group.
  #Returning '-Inf' for such groups to be consistent
  #with baseNo non-missing values found in at least one group]
  moni.data<-formatMONIdata(moni.data=moni.data)
  extreme.data<-scaleMONIdata(moni.data)
  if (isTRUE(diary==TRUE)){
    diary.data=diary.data
    diary.data$datetime<-ymd_hms(diary.data$datetime,truncated = 3)
    }

  lapply(levels(moni.data$plot.group),function(i){
    moni.data.onegroup<-
      droplevels(moni.data[moni.data$plot.group==i,])

    if (isTRUE(sum(na.omit(moni.data.onegroup$Yield))>0)){#when there is data exit

      scaledata.onegroup<-
        droplevels(extreme.data[extreme.data$plot.group==i,])

      plot.onegroup<-
        lapply(levels(moni.data.onegroup$head_tree),function(j){
          scale.data<-
            droplevels(scaledata.onegroup[scaledata.onegroup$head_tree==j,])

          FtoYield<-function(x){scale.data$scale.Fm.a*x+scale.data$scale.Fm.b}
          TtoYield<-function(x){scale.data$scale.temp.a*x+scale.data$scale.temp.b}
          PARtoYield<-function(x){scale.data$scale.par.a*x+scale.data$scale.par.b}

          plot.data<-
            droplevels(moni.data.onegroup[moni.data.onegroup$head_tree==j,])

          if (isTRUE(diary==TRUE)){

            diary.group<-
              droplevels(setDT(diary.data)[diary.data$head_tree==j&
                                             diary.data$plot.group==i,
                                         c('datetime','head_tree',
                                           'plot.group','remark.plot')])
          }
          if (isTRUE(length(na.omit(plot.data$Yield))<=2& # when there is no Yield data,
                       length(na.omit(diary.group$remark.plot))>0)) {# but there is a diary note, plot this note
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
              } else if (isTRUE(sum(na.omit(plot.data$Yield))>0&
                     length(na.omit(plot.data$Yield))>2)) {#if data exist
                p1<-ggplot(plot.data,aes(x=datetime))+
                  geom_line(aes(y=TtoYield(temp_moni)),size=0.71,color='darkorchid',na.rm = T)+
                  geom_line(aes(y=PARtoYield(par_moni)),size=0.71,color='yellow3',na.rm = T)+
                  geom_line(aes(y=Yield),color='forestgreen',na.rm = T)+
                  geom_point(aes(y=Yield),size=0.5,color='forestgreen',na.rm = T)+
                  geom_line(aes(y=FtoYield(F_)),color='grey30',na.rm = T)+
                  geom_point(aes(y=FtoYield(F_)),size=0.5,color='grey30',na.rm = T)+
                  geom_line(aes(y=FtoYield(Fm_)),color='black',na.rm = T)+
                  geom_point(aes(y=FtoYield(Fm_)),color='black',size=0.5,na.rm = T)+
                  scale_x_datetime(breaks = date_breaks("1 day"),
                                   labels = date_format("%b-%d\n%H:%M"),
                                   limits = c(range(na.omit(moni.data.onegroup$datetime))[1],
                                              range(na.omit(moni.data.onegroup$datetime))[2]))+
                  scale_y_continuous(
                    sec.axis = sec_axis(~(.-scale.data$scale.Fm.b)/scale.data$scale.Fm.a, name='F or Fm'),
                    name = bquote(atop(paste(Phi,'PSII'),
                                       .(j))))+
                  theme_bw()+
                  theme(legend.position = 'none',
                        title = element_text(size=12,color='black',face='bold'),
                        axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                        axis.text.y.right = element_text(size = 15,color = 'black'),
                        axis.text.x =  element_text(size = 15,color = 'black'),
                        axis.title.y.right  = element_text(size = 16),
                        axis.title.y.left = element_text(size = 16,color = 'forestgreen'),
                        axis.title.x =element_blank(),
                        axis.ticks.y.left =element_line(color = 'forestgreen'),
                        axis.line.y.left = element_line(color = 'forestgreen'),
                        plot.margin = unit(c(0,0,0,0),'cm'))
                if (isTRUE(diary==TRUE)){

                  if (isTRUE(length(na.omit(diary.group$remark.plot))>0)) {
                    p1<-p1+
                      geom_point(data=na.omit(diary.group),
                                 aes(x=datetime,y=max(plot.data$Yield,na.rm = T)-0.05),
                                 color='red',size=4,na.rm = T)+
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
                  geom_line(aes(y=TtoYield(temp_moni)),size=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=PARtoYield(par_moni)),size=0.1,color='white',na.rm = T)+
                  geom_line(aes(y=Yield),color='white',na.rm = T)+
                  geom_point(aes(y=Yield),size=1,color='white',na.rm = T)+
                  geom_line(aes(y=FtoYield(F_)),color='white',na.rm = T)+
                  geom_point(aes(y=FtoYield(F_)),size=1,color='white',na.rm = T)+
                  geom_line(aes(y=FtoYield(Fm_)),color='white',na.rm = T)+
                  geom_point(aes(y=FtoYield(Fm_)),color='white',size=1,na.rm = T)+
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
          plot_grid(plotlist = plot.onegroup[!sapply(plot.onegroup,is.null)],
                    align = 'hv',ncol = 1)

        title <- ggdraw() +
          draw_label(paste0(plot.title,
                            ' from ',range(na.omit(moni.data.onegroup$date))[1],
                            ' to ', range(na.omit(moni.data.onegroup$date))[2]),
                     x = 0.05,hjust = 0,fontface='bold',size=20)+
          theme(plot.margin = margin(0,0,0,0))

        plot.all.title<-
          plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))

        daylength.x<-as.numeric(diff(range(na.omit(moni.data.onegroup$date))))
        headlength.y<-length(plot.onegroup[!sapply(plot.onegroup,is.null)])
        ggsave(plot.all.title,
               filename = paste0(save.path,'/',plot.title,
                                 ' from ',range(na.omit(moni.data.onegroup$date))[1],
                                 ' to ',range(na.omit(moni.data.onegroup$date))[2],'.tiff'),
               compression='lzw',dpi=100,width = 1.5*daylength.x+6,
               height = 2*headlength.y+0.3)

      }
      #return(plot.all.title)
    }
  })
  print(Sys.time()-start.time)

}
