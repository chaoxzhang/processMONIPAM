#' obtain extreme values of MONI-PAM Fv/Fm data
#'
#' These extreme values of MONI-PAM data will be used for MONI-PAM data visualization
#'
#'
#' @param moni.fvfm MONI-PAM fvfm data retrieved from [FindFvFm] function in this R package
#'
#' @return [scale.fvfm] will return a data.table which contains the maximum and minimum values of F0,FM, Fv/Fm, PAR, and temperature values for each MONI-head respectively.
#' @export
scale.fvfm<-function(moni.fvfm){

  data<-moni.fvfm[,-c('date','head','tree_num')]
  max.res.F<-data[,lapply(.SD,max,na.rm = T),by=c('head_tree')]
  names(max.res.F)[2:6]<-
    c('max.par','max.temp','max.F0','max.Fm','max.FvFm')
  min.res.F<-data[,lapply(.SD,min,na.rm = T),
                  by=c('head_tree')]
  names(min.res.F)[2:6]<-
    c('min.par','min.temp','min.F0','min.Fm','min.FvFm')
  maxmin.res<-
    merge(max.res.F,min.res.F,by=c('head_tree'),all = T)
  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  maxmin.res$scale.Fm.a<-
    (maxmin.res$max.FvFm-maxmin.res$min.FvFm)/
    (maxmin.res$max.Fm-maxmin.res$min.Fm)
  maxmin.res$scale.Fm.b<-
    maxmin.res$max.FvFm-maxmin.res$scale.Fm.a*maxmin.res$max.Fm

  maxmin.res$scale.F0.a<-
    (maxmin.res$max.FvFm-maxmin.res$min.FvFm)/
    (maxmin.res$max.F0-maxmin.res$min.Fm)
  maxmin.res$scale.F0.b<-
    maxmin.res$max.FvFm-maxmin.res$scale.F0.a*maxmin.res$max.F0

  maxmin.res$scale.temp.a<-
    (maxmin.res$max.FvFm-maxmin.res$min.FvFm)/
    (maxmin.res$max.temp-maxmin.res$min.temp)
  maxmin.res$scale.temp.b<-
    maxmin.res$max.FvFm-maxmin.res$scale.temp.a*maxmin.res$max.temp
  maxmin.res$scale.par.a<-
    (maxmin.res$max.FvFm-maxmin.res$min.FvFm)/
    (maxmin.res$max.par-maxmin.res$min.par)
  maxmin.res$scale.par.b<-
    maxmin.res$max.FvFm-maxmin.res$scale.par.a*maxmin.res$max.par

  maxmin.res<-data.table(maxmin.res)
  maxmin.res[,c(12,14,16,18)][sapply(maxmin.res[,c(12,14,16,18)],is.na)]<-1
  maxmin.res[,c(13,15,17,19)][sapply(maxmin.res[,c(13,15,17,19)],is.na)]<-0

  maxmin.res[sapply(maxmin.res, is.infinite)] <- NA
  return(maxmin.res)
}

#' MONI-PAM FV/FM data visualization
#'
#' This function will visualize MONI-PAM FV/FM data including F0,FM,FV/FM,PAR, and temperature in one figure each head.
#'
#' @param moni.fvfm MONI-PAM fvfm data retrieved from FindFvFm function in this R package
#' @param plot.title any text to describe clearly about your figure content
#' @param save.path local folder for saving the plotted figures generated from this function
#'
#' @return This function will not return/show the plotted figures in the end, instead, it will save the plotted figures to your folder directly.
#' @export
plot.FvFm<-function(moni.fvfm,plot.title,save.path){
  start.time<-Sys.time()
  moni.fvfm<-format.monidata(moni.data=moni.fvfm)
  extreme.data<-scale.fvfm(moni.fvfm)

  plot.onetree<-

    lapply(levels(moni.fvfm$head_tree),function(j){
      #j=levels(moni.fvfm$head_tree)[1]
      scale.data<-
        droplevels(extreme.data[extreme.data$head_tree==j,])
      attach(scale.data,warn.conflicts = F)

      plot.data<-
        droplevels(moni.fvfm[moni.fvfm$head_tree==j,])

      if (sum(na.omit(plot.data$FvFm))>0&
          length(na.omit(plot.data$FvFm))>2) {#if data exist

        p1<-
          ggplot(plot.data,aes(x=date))+
          geom_line(aes(y=scale.temp.a*nightmean_temp+scale.temp.b),
                    size=0.3,color='darkorchid',na.rm = T)+
          geom_line(aes(y=scale.par.a*noonmean_PAR+scale.par.b),
                    size=0.3,color='yellow3',na.rm = T)+
          geom_line(aes(y=scale.F0.a*F0+scale.F0.b),color='orange',na.rm = T)+
          geom_point(aes(y=scale.F0.a*F0+scale.F0.b),size=1,color='orange',na.rm = T)+
          geom_line(aes(y=scale.Fm.a*Fm+scale.Fm.b),color='black',na.rm = T)+
          geom_point(aes(y=scale.Fm.a*Fm+scale.Fm.b),color='black',size=1,na.rm = T)+
          geom_line(aes(y=FvFm),color='forestgreen',na.rm = T)+
          geom_point(aes(y=FvFm),size=1,color='forestgreen',na.rm = T)+
          scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%d"),
                       limits = c(range(na.omit(moni.fvfm$date))[1],
                                  range(na.omit(moni.fvfm$date))[2]))+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.Fm.b)/scale.Fm.a,
                                name="Fm"),
            name = bquote(atop(paste('F'[V],'/','F'[M]),
                               paste(.(j)))))+
          theme_bw()+
          theme(legend.position = 'none',
                title = element_text(size=12,color='black',face='bold'),
                axis.text.y.left = element_text(size = 15,color = 'forestgreen'),
                axis.text.y.right = element_text(size = 15,color = 'black'),
                axis.text.x =  element_text(size = 12,color = 'black'),
                axis.title.y.right  = element_markdown(size = 16),
                axis.title.y.left = element_text(size = 14.5,color = 'forestgreen'),
                axis.title.x =element_blank(),
                axis.ticks.y.left =element_line(color = 'forestgreen'),
                axis.line.y.left = element_line(color = 'forestgreen'),
                plot.margin = unit(c(0,0,0,0),'cm'))

        p234.fc<-function(p234.color){
          ggplot(plot.data,aes(x=date))+
            geom_line(aes(y=scale.temp.a*nightmean_temp+scale.temp.b),
                      size=0.1,color='white',na.rm = T)+
            geom_line(aes(y=scale.par.a*noonmean_PAR+scale.par.b),
                      size=0.1,color='white',na.rm = T)+
            #geom_hline(aes(yintercept=scale.temp.b),color='white')+
            geom_line(aes(y=FvFm),color='white',na.rm = T)+
            geom_point(aes(y=FvFm),size=1,color='white',na.rm = T)+
            geom_line(aes(y=scale.F0.a*F0+scale.F0.b),color='white',na.rm = T)+
            geom_point(aes(y=scale.F0.a*F0+scale.F0.b),size=1,color='white',na.rm = T)+
            geom_line(aes(y=scale.Fm.a*Fm+scale.Fm.b),color='white',na.rm = T)+
            geom_point(aes(y=scale.Fm.a*Fm+scale.Fm.b),color='white',size=1,na.rm = T)+
            theme_minimal()+
            theme(legend.position = 'none',
                  axis.text.y.right = element_text(size = 15,color = p234.color),
                  axis.text.y.left = element_blank(),
                  axis.line.y.right = element_line(color = p234.color),
                  axis.text.x =   element_blank(),
                  axis.title.y.right  = element_text(size = 16,color = p234.color),
                  axis.title.y.left = element_blank(),
                  axis.title.x =element_blank(),
                  axis.ticks.y.left = element_blank(),
                  axis.ticks.y.right =element_line(color = p234.color),
                  panel.grid=element_blank(),
                  plot.margin = unit(c(0,0,0,0),'cm'))
        }
        p2<-p234.fc('orange')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.F0.b)/scale.F0.a,
                                name=expression(paste('F0'))))
        p3<-p234.fc('darkorchid')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.temp.b)/scale.temp.a,
                                name=expression(paste('Temp ('^'o','C)'))))
        p4<-p234.fc('yellow3')+
          scale_y_continuous(
            sec.axis = sec_axis(~(.-scale.par.b)/scale.par.a,
                                name='PAR'))
        plot.moni.figure<-
          ggarrange(p1,p2,p3,p4,widths = c(15,0.1,0.1,0.1),nrow = 1,draw = F)
      }
    }
    )

  if (length(plot.onetree[!sapply(plot.onetree,is.null)])>0){
    plot.all<-
      plot_grid(plotlist = plot.onetree[!sapply(plot.onetree,is.null)],
                align = 'hv',ncol = 1)
    title <- ggdraw() +
      draw_label(paste0(plot.title,
                        ' from ', range(na.omit(moni.fvfm$date))[1],
                        ' to ',range(na.omit(moni.fvfm$date))[2]),
                 x = 0.05,hjust = 0,fontface='bold')
    plot.all.title<-
      plot_grid(title, plot.all, ncol=1, rel_heights=c(0.1, 1))
    daylength.x<-
      as.numeric(diff(range(na.omit(moni.fvfm$date))))
    headlength.y<-length(plot.onetree[!sapply(plot.onetree,is.null)])
    ggsave(plot.all.title,
           filename = paste0(save.path,'/',plot.title,
                             ' from ',range(na.omit(moni.fvfm$date))[1],
                             ' to ', range(na.omit(moni.fvfm$date))[2],'.tiff'),
           compression='lzw',dpi=400,width = 0.05*daylength.x+4,
           height = 1.5*headlength.y+0.3)
  }

  print(Sys.time()-start.time)

}
