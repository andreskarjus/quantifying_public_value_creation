# This script contains plotting functions for generating the graphs in
# "Quantifying public value creation by public service media using big programming data"
# by Indrek Ibrus, Andres Karjus, Vejune Zemaityte, Ulrike Rohn, Maxilian Schich.
#
# These functions are meant to be called from the R script "quantifying_public_value_code.R.
#
# The code and data are provided "as is", without warranty or fitness for a particular purpose. 
# The authors cannot be held liable for any claim of damages or other liability in 
# connection with the use of this code and data.
# Code below authored by Andres Karjus; https://andreskarjus.github.io


plot_imports = function(x, titl="",yt,leg=F,cl=c("navy","#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70", "gray30"),legt="Country groups;", minper=10, mod=2, cl2=NULL ){
  if(is.null(cl2)){
    cl2=cl
  }
  g=ggplot(x  , aes(x=year, y=s, fill=country, color=country,group=country))+
    #geom_point(size=1.2)+
    #geom_line(size=1, alpha=1)+
    geom_bar(stat="identity", color=NA, width=1)+
    geom_text(aes(label=round(s),size=s2),
              data=x %>% 
                mutate(s2=case_when(s<minper | year%%mod==0 ~ 0, T~s)),
              position = position_stack(vjust = 0.5))+
    scale_size(guide = "none", range = c(0,3.5))+
    theme_bw()+
    scale_y_continuous(expand=c(0,0))+
    scale_fill_manual(values=cl, na.value = "black")+
    scale_color_manual(values=cl2 %>% lighten(0.6), na.value="black", guide="none")+
    theme_bw()+
    labs(fill=legt, title=titl,y=yt)+
    scale_x_continuous(expand=c(0,0), breaks=(2004:2020))+#, limits = c(2006.5, 2020.5))+
    facet_grid(cols = vars(channel),drop = T,  scales = "free_x", space = "free_x")+
    theme(
      axis.text = element_text(size=7),
      axis.text.x=element_text(angle=90, hjust=1,vjust=0.5,size=7), 
      legend.justification = "left",
      axis.title = element_text(size=9),
      plot.title = element_text(size=10),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
      strip.background = element_rect(fill=NA),
      strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_line(size=0.5),
      panel.grid.major.y = element_line(size=0.5),
      plot.margin = margin(0,0,5,0)
    )
  if(leg){
    g=g+
      ggplot(x, aes(x=year, y=s, fill=country, color=country,group=country))+
      geom_bar(stat="identity", color=NA, width=0)+
      scale_fill_manual(values=cl, na.value = "black")+
      labs(fill=legt)+
      theme_void()+
      theme(    
        legend.position=c(0.5, 0.5),
        legend.justification = c(0.5,0.5),
        legend.margin = margin(1,1,1,1),
        legend.title = element_text(size = 9),# element_blank(),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.text = element_text(size = 9)) +
      plot_layout(nrow=1, widths = c(2.7,1))
  }
  return(g)
}




# used only for uk plot for now
plot_content = function(dat, y, leg=T){
  cl=c(RColorBrewer::brewer.pal(8, "Set1"),"slategray", "navy") 
  g=ggplot(dat ,# %>% mutate(alamvaldkond=factor(alamvaldkond, levels(eetvald0$alamvaldkond))) , 
           aes(x=year, y=s, 
               fill=contenttype # reorder(alamvaldkond, s, function(x){ sum(x) })   
           ))+
    geom_bar(stat="identity",width = 1, color=NA, position="stack")+
    #geom_text(aes(label=round(s)),size=3, color="black", position=position_stack(vjust=0.9))+
    geom_text(aes(label=round(s),size=s2, color=contenttype),
              data=dat %>% 
                mutate(s2=case_when(s<10 | year%%2==0 ~ 0, T~s)),
              position = position_stack(vjust = 0.5))+
    scale_size(guide = "none", range = c(0,3.5))+
    #geom_vline(xintercept = 2008-0.5, color="gray3")+
    #geom_vline(xintercept = 2015-0.5, color="gray3")+
    #scale_fill_manual(values=cols3, na.value="lightgray")+
    theme_bw()+
    scale_fill_manual(values=cl)+
    scale_color_manual(values=cl %>% lighten(0.6), na.value="black", guide="none")+
    scale_y_continuous(limits=c(0,100.00001), expand=expansion(add=c(0,0)))+  #sec.axis = sec_axis(~.)
    scale_x_continuous(breaks = 2004:2020, expand=expansion(add=c(0,0)))+
    theme(
      # legend.position=c(0.87, 0.001),
      axis.title = element_text(size=10),
      plot.title = element_text(size=10),
      axis.title.x = element_blank(),
      axis.text = element_text(size=7),
      axis.text.x=element_text(angle=90, hjust=1,vjust=0.5),
      legend.justification = c(1,0),
      legend.margin = margin(0,0,0,0),
      legend.title = element_blank(),
      legend.key.height = unit(0.4,"cm"),
      legend.key.width = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
      strip.background = element_rect(fill=NA),
      strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
    )+
    ylab(y)+xlab("")+
    facet_grid(cols=vars(channel), scales="free_x", space="free_x")
  if(!leg){
    g=g+theme(legend.position="none")
  }
  g
}



reachplot = function(eetview_country,cl, ncl=1, eetb){
  th=theme(
    strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
    strip.background = element_rect(fill=NA),
    strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
    legend.position = "none", 
    #axis.title = element_text(size=10),
    axis.title = element_blank(),
    plot.title = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=7),
    plot.margin = margin(0,0,5,0)
  )
  my_ceil <- function(x) {
    ceil <- ceiling(max(x))
    ifelse(ceil > 1 & ceil %% 2 == 1, ceil + 1, ceil)
  }
  
  my_breaks <- function(x) { 
    ceil <- my_ceil(max(x))
    unique(ceiling(pretty(seq(0, ceil))))
  } 
  
  g = ggplot(
    eetview_country, 
    aes(y=rankvar,x=AvRch, color=xvar)
  )+
    #geom_beeswarm( groupOnX = F, priority = "random", size=0.01)+
    geom_violin(aes(fill=xvar), color=NA, scale="width")+
    geom_point(size=0.01)+
    geom_point(aes(x=med), shape=124, data= eetview_country %>% group_by(xvar, channel) %>% slice(1), size=6,color="black")+
    labs(y="Average airtime hours per year", x="Average audience reach %", title="(A) ERR programming consumption (audience reach, % of viewers who watched ≥ 1 min)")+
    scale_color_manual(values=cl, na.value = "black")+
    scale_fill_manual(values=cl, na.value = "black")+
    scale_x_continuous(expand=c(0,0.1), breaks=my_breaks, labels = scales::percent_format(scale = 1, accuracy = 1))+
    theme_bw()+
    th+
    #facet_wrap(~channel, scale="free_x")+
    facet_grid(cols=vars(channel), scales="free_x", space="free")+
    
    ggplot(
      eetb,
      aes(y=rankvar,x=airtime, fill=xvar)
    )+
    geom_bar(stat="identity")+
    labs(y="Average airtime hours per year", x="Average audience reach %", title="(B) ERR programming exposure for comparison (% of airtime, average of 2018-2020)")+
    scale_fill_manual(values=cl, na.value = "black")+
    scale_x_continuous(labels = scales::percent_format(scale = 1, trim=1), expand=c(0,0.1))+
    theme_bw()+
    th+
    #facet_wrap(~channel, scale="free_x") +
    facet_grid(cols=vars(channel), scales="free_x", space="free")+
    plot_layout(ncol=ncl, heights = c(0.7,0.35))+
    plot_annotation(theme=theme(plot.margin = margin(0,0,0,0)))
  g
}







plot_primetime = function(x, titl="",cl){
  yt="% of airtime"
  ggplot(x, aes(x=prime, y=s, fill=ytype, color=ytype,group=ytype))+
    #geom_point(size=1.2)+
    #geom_line(size=1, alpha=1)+
    geom_bar(stat="identity", color=NA, width=0.9)+
    geom_text(aes(label=round(s),size=s2),
              data=x %>% 
                mutate(s2=case_when(s<8 ~0, T~s)),
              position = position_stack(vjust = 0.5))+
    scale_size(guide = "none", range = c(0,3.5))+
    theme_bw()+
    scale_fill_manual(values=cl, na.value = "black")+
    scale_color_manual(values=cl %>% lighten(0.6), na.value="black", guide="none")+
    theme_bw()+
    labs( title=titl,y=yt)+
    scale_y_continuous(expand=c(0,0))+
    scale_x_discrete(expand=c(0.1,0.1))+#, limits = c(2006.5, 2020.5))+
    facet_grid(cols = vars(channel),drop = T,  scales = "free_x", space = "free_x")+
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size=7),
      axis.text.x=element_text(size=7), 
      axis.ticks.length.x = unit(0, "cm"),
      legend.justification = "left",
      legend.title = element_blank(),
      axis.title = element_text(size=10),
      plot.title = element_text(size=10),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
      strip.background = element_rect(fill=NA),
      strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
      legend.position = "right",
      legend.key.height = unit(0.4,"cm"),
      legend.key.width = unit(0.4, "cm"),
      plot.margin = margin(0,0,5,0)
    )
}




plot_prod = function(x, titl="",y,leg=F,cl=c("navy","#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70", "black"), labfreq=0, minfreq=10, minsize=0,maxsize=3, facettype="channel"){
  
  #cl=c(RColorBrewer::brewer.pal(8, "Set1"),"slategray", "navy") 
  g=ggplot(x ,# %>% mutate(alamvaldkond=factor(alamvaldkond, levels(eetvald0$alamvaldkond))) , 
           aes(x=year, y=s, 
               fill=productiontype0 # reorder(alamvaldkond, s, function(x){ sum(x) })   
           ))+
    geom_bar(stat="identity",width = 1, color=NA, position="stack")+
    #geom_text(aes(label=round(s)),size=3, color="black", position=position_stack(vjust=0.9))+
    geom_text(aes(label=round(s),size=s2, color=productiontype0),
              data=x %>% 
                mutate(s2=case_when(s<minfreq | (year%%2 %in% labfreq) ~ 0, T~s)) %>% filter(s2!=0),
              position = position_stack(vjust = 0.5), fontface="bold")+
    scale_size(guide = "none", range = c(minsize,maxsize))+
    #geom_vline(xintercept = 2008-0.5, color="gray3")+
    #geom_vline(xintercept = 2015-0.5, color="gray3")+
    #scale_fill_manual(values=cols3, na.value="lightgray")+
    theme_bw()+
    scale_fill_manual(values=cl, na.value="black")+
    scale_color_manual(values=cl %>% lighten(0.8), na.value="black", guide="none")+
    scale_y_continuous(limits=c(0,100.00001), expand=expansion(add=c(0,0)))+  #sec.axis = sec_axis(~.)
    scale_x_continuous(breaks = 2004:2020, expand=expansion(add=c(0,0)))+
    theme(
          axis.text.x=element_text(angle=90, hjust=1,vjust=0.5),
          axis.title=element_text(size=9),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
          strip.background = element_rect(fill=NA),
          strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
          # legend.position=c(0.87, 0.001),
          legend.justification = c(1,1),
          legend.position = "bottom",
          legend.margin = margin(0,0,0,0),
          legend.title = element_blank(),
          legend.key.height = unit(0.4,"cm"),
          legend.key.width = unit(0.4, "cm"),
          legend.text = element_text(size = 9),
          plot.margin = margin(0,0,5,0)
    )+
    ylab(y)+xlab("")
  if(facettype=="channel"){
    g=g+facet_grid(cols=vars(channel), scales="free_x", space="free_x")
  } else {
    g=g+facet_wrap(reformulate(facettype), nrow = 2)
  }
  g
}



plot_rep = function(x, titl="", lims=c(0,1.8), right=F){
  g=ggplot(x,aes(x=channel, fill=repval, y=ytype)) +
    geom_tile(fill="white", color="gray50",size=0.1)+
    geom_text(aes(label=round(repval,2),color=repval),size=4)+
    #scale_color_gradientn(colors=viridis_pal()(4) %>% lighten(0.8), limits=lims,guide="none")+
    #scale_fill_gradientn(colors=viridis_pal()(4), limits=lims)+
    #scale_fill_gradientn(colors=c(darken("#FFFF33",0.1), "#4DAF4A", "#377EB8") %>% rev(), limits=lims)+
    #scale_color_gradientn(colors=c(darken("#FFFF33",0.1), "#4DAF4A", "#377EB8") %>% rev() %>% lighten(0.9) , limits=lims, guide="none")+
    scale_color_gradientn(colors = c("#377EB8", "slategray", "#E41A1C"))+
    labs(title=titl)+
    coord_cartesian(expand=0)+
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size=10),
      axis.text=element_text(size=10),
      #axis.text.x=element_text(angle=45, hjust=1,vjust=1),
      axis.title = element_blank(),
      strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
      strip.background = element_rect(fill=NA),
      strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
      # legend.position=c(0.87, 0.001),
      legend.justification = c(1,1),
      legend.position = "none",
      legend.margin = margin(0,0,0,0),
      legend.title = element_blank(),
      legend.key.height = unit(0.4,"cm"),
      legend.key.width = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      plot.margin = margin(0,3,0,0),
      axis.ticks.length = unit(0,"cm")
    )
  if(right){
    g=g+theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.margin = margin(l=0))
  }
  g
}



plot_countrycounts = function(eetmaa_counts1){
  ggplot(eetmaa_counts1, aes(x=year, y=s2, color=channel, label=channel))+
    geom_line(size=0.8,lineend="round")+
    geom_point(size=1.2)+
    geom_shadowtext(data=eetmaa_counts1 %>% group_by(channel, type) %>% slice(1),bg.colour="white" , hjust=1.1,show.legend=F, size=4, fontface="bold")+
    scale_color_manual(values=c("slategray","#4DAF4A", "#984EA3", "#E41A1C"))+
    scale_x_continuous(breaks=seq(2004,2020,2), limits=c(2002,2020), expand = c(0,0.2))+
    scale_y_continuous(limits=c(0,48),breaks=c(1,seq(10,40,10)), expand=c(0,0), sec.axis = sec_axis(~.))+
    labs(y="# unique countries / # items * 1000")+
    theme_bw()+
    theme(
      axis.text.x=element_text(angle=0, hjust=1,vjust=0.5),
      axis.title=element_text(size=9),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 10, hjust=0, margin = margin(1,1,1,2)),
      strip.background = element_rect(fill=NA),
      strip.switch.pad.wrap = unit(c(0,0,0,0), "pt"),
      # legend.position=c(0.87, 0.001),
      legend.justification = c(1,1),
      legend.position = "none",
      legend.margin = margin(0,0,0,0),
      legend.title = element_blank(),
      legend.key.height = unit(0.4,"cm"),
      legend.key.width = unit(0.4, "cm"),
      legend.text = element_text(size = 9),
      plot.margin = margin(0,0,5,0)
      
    )+
    facet_wrap(~type)
}
