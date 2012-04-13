ggkm <- function(sfit, table=T, returns=F, xlabs='Time', ylabs='survival probability',
                 ystratalabs = NULL,ystrataname=NULL,timeby=100,main='Kaplan-Meier Plot',...){
  
  require(ggplot2)
  require(survival)
  require(gridExtra)
  #     Create a blank plot for place-holding
  blank_pic<- ggplot(df, aes(time,surv))+geom_blank()+theme_bw()+
    opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(),
         axis.title.x=theme_blank(), axis.title.y=theme_blank(),
         axis.ticks=theme_blank(), panel.grid.major=theme_blank(),
         panel.border=theme_blank())
  
  #     Create Kaplan Meier plot
  if(is.null(ystratalabs)) ystratalabs <- as.character(levels(summary(sfit)$strata))
  (m<-max(nchar(ystratalabs)))
  if(is.null(ystrataname)) ystrataname <- 'Strata'
  times  <- seq(0, max(sfit$time), by=timeby)
  df  <- data.frame(
    time    =   sfit$time,
    n.risk  =   sfit$n.risk,
    n.event =   sfit$n.event,
    surv    =   sfit$surv,
    strata  =   summary(sfit, censored=T)$strata,
    upper   =   sfit$upper,
    lower   =   sfit$lower
    )
  levels(df$strata) <- ystratalabs
  zeros <- data.frame(time=0, surv=1, strata = ystratalabs,
                      upper=1, lower=1)
  df  <- rbind.fill(zeros, df)
  d <- length(levels(df$strata))
  p  <- ggplot(df, aes(time, surv, groups=strata))+
    geom_step(aes(linetype=strata), size=0.6)+
    theme_bw()+opts(axis.title.x=theme_text(vjust=0.5))+
    scale_x_continuous(xlabs, breaks=times, limits = c(0, max(sfit$time)))+
    scale_y_continuous(ylabs, limits = c(0,1))+
    opts(panel.grid.minor = theme_blank())+
    opts(legend.position=c(ifelse(m<10,.28,.35),ifelse(d < 4,.25,.35)))+
    opts(legend.key=theme_rect(colour=NA))+
    labs(linetype=ystrataname)+
    opts(plot.margin = unit(c(0, 1, .5, ifelse(m<10,1.5,2.5)),'lines'))+
    opts(title=main)
  sdiff <- survdiff(eval(sfit$call$formula), data=eval(sfit$call$data))
  pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail=F)
  pvaltxt <- ifelse(pval < 0.0001, 'P < 0.0001',
                    paste('P =', signif(pval,2)))
  
  if(table){
    #     Create table graphic to include at-risk numbers
    risk.data  <- data.frame(strata = summary(sfit, times=times,
                                              extend=T)$strata,
                             time = summary(sfit, times=times, extend=T)$time,
                             n.risk = summary(sfit, times=times, extend=T)$n.risk)
    data_table  <-  ggplot(risk.data, aes(x=time, y=strata,
                                          label=format(n.risk, nsmall=0)))+#, color=strata))+
                                            geom_text(size=3.5)+
                                            theme_bw()+
                                            scale_y_discrete(breaks=as.character(levels(risk.data$strata)),
                                                             labels=ystratalabs)+
                                                               #                 scale_y_discrete(#format1ter = abbreviate,
                                                               #                   breaks=1:3,
                                                               #                   labels=ystratalabs)+
                                                               scale_x_continuous('Numbers at risk', limits=c(0, max(sfit$time)))+
                                                               opts(axis.title.x=theme_text(size=10, vjust=1))+
                                                               opts(panel.grid.major   =   theme_blank())+
                                                               opts(panel.grid.minor   =   theme_blank())+
                                                               opts(panel.border       =   theme_blank())+
                                                               opts(axis.text.x        =   theme_blank())+
                                                               opts(axis.ticks         =   theme_blank())+
                                                               opts(axis.text.y       =   theme_text(face='bold', hjust=1))
    
    data_table  <- data_table + opts(legend.position='none')+
      xlab(NULL)+ylab(NULL)
    
    data_table <- data_table+opts(plot.margin=unit(c(-1.5, 1, 0.1, ifelse(m<10,2.5,3.5)-0.28*m),'lines'))
    
    #     Plotting the graphs
    p <- ggplotGrob(p)
    p <- addGrob(p, textGrob(x=unit(.8, 'npc'), y=unit(.25,'npc'), label=pvaltxt,
                             gp=gpar(fontsize=12)))
    grid.arrange(p, blank_pic, data_table, clip=F, nrow=3, ncol=1,
                 heights = unit(c(2,.1,.25), c('null','null','null')))
    if(returns) {
      a <- arrangeGrob(p, blank_pic, data_table, clip=F, nrow=3, ncol=1,
                       heights = unit(c(2, .1, .25), c('null','null','null')))
      return(a)
    }
  } else {
    p <- ggplotGrob(p)
    p <- addGrob(p, textGrob(x=unit(0.5,'npc'), y=unit(0.23,'npc'),
                             label=pvaltxt, gp=gpar(fontsize=12)))
    grid.arrange(p)
    if(returns) return(p)
  }
}