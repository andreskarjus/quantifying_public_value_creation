# This R script generates (most of) the plots presented in the paper
# "Quantifying public value creation by public service media using big programming data"
# by Indrek Ibrus, Andres Karjus, Vejune Zemaityte, Ulrike Rohn, Maxilian Schich.
# The code and data are provided "as is", without warranty or fitness for a particular purpose. 
# The authors cannot be held liable for any claim of damages or other liability in 
# connection with the use of this code and data.
# Code below authored by Andres Karjus; https://andreskarjus.github.io
#
# To reproduce the graphs and analyses in the paper, run the code below, line by line,
# following the provided instructions (importantly, package installation and data paths).
# Not all plots and analyses can be reproduced, as we are only able to provide
# data for the ERR channels (which is the main focus of the paper. This excludes 
# the commercial channel Kanal2, and the TV ratings survey data. 
# Code blocks relying on these subsets of the data have been commented out below.


#### Load packages ####

# Code written in R version 4.2.1.
# If not present, please install those first:
library(tidyverse) # 1.3.2
library(lubridate) 
library(scales)
library(patchwork)
library(colorspace)
library(shadowtext)

# (Uncomment and run this line to install all)
# install.packages(c("tidyverse", "lubridate", "scales", "patchwork", "colorspace", "shadowtext"))


#### Set path ####

# Please either use setwd() to set the path to the directory where the 
# scripts and data files are located, or provide full paths below.
# Otherwise, will attempt to load from this directory:
getwd()



#### Load data and plotting scripts ####

## Load scrips
source("quantifying_public_value_scripts.R")

## Load data

# Unpack the quantifying_public_value_data.csv.zip into the working directory,
# or a directory for which a full path is specified below.
eetvald = read_csv("quantifying_public_value_data.csv") 

# A summary overview of the dataset:
glimpse(eetvald)



#### Generate data objects and graphs ####

# Run these to generate some necessary objects for plotting:
levs = c('Estonia','Nordics','EU','UK','Russia (incl USSR)','USA+Canada','Rest of the world')
levs_content = c('science, education','kids, animation','music','news, politics','informative, documentary','culture, religion','sport','infotainment','entertainm., games, lifestyle','series, films, theatre')


## Run the following to generate plots.

# First domestic-foreign plots
eetdomfor = 
  rbind(
    eetvald %>% 
      filter(year>=2004, year <=2020, channel!="Kanal2") %>% 
      mutate(productiontype0=   # quick hack to use plot_prod
               case_when(prodcountry==""|is.na(prodcountry) ~NA_character_,
                         prodcountry %in% c("Eesti", "Estonia") ~ "Domestic", # if just EE
                         grepl("Eesti|Estonia",prodcountry,ignore.case = T) ~ # else if EE+
                           "Domestic", #"Domestic-foreign coproduction",
                         T~"Foreign") # if none of the above then foreign
      ) %>% 
      mutate(productiontype0 = fct_relevel(productiontype0, "Foreign")) %>% #,"Domestic-foreign coproduction")) %>% 
      group_by(year, productiontype0) %>% 
      summarise(s=sum(duration, na.rm=T))  %>% 
      group_by(year) %>%
      mutate(s=s/sum(s, na.rm=T)*100) %>% mutate(channel="All of ERR")
    ,
    eetvald %>% 
      filter(year>=2004, year <=2020) %>% 
      mutate(productiontype0=   # quick hack to use plot_prod
               case_when(prodcountry==""|is.na(prodcountry) ~NA_character_,
                         prodcountry %in% c("Eesti", "Estonia") ~ "Domestic", # if just EE
                         grepl("Eesti|Estonia",prodcountry,ignore.case = T) ~ # else if EE+
                           "Domestic", #"Domestic-foreign coproduction",
                         T~"Foreign") # if none of the above then foreign
      ) %>% 
      mutate(productiontype0 = fct_relevel(productiontype0, "Foreign")) %>% #,"Domestic-foreign coproduction")) %>% 
      group_by(year, productiontype0, channel) %>% 
      summarise(s=sum(duration, na.rm=T))  %>% 
      group_by(year, channel) %>%
      mutate(s=s/sum(s, na.rm=T)*100)
  ) 
eetdomfor$channel = factor(eetdomfor$channel, c("All of ERR", "ETV1", "ETV2", "ETV+", "Kanal2"))


plot_prod(eetdomfor %>% filter(channel=="All of ERR") %>% mutate(channel="Content across ERR by production origin"), titl="Content by production origin across channels",y="airtime %  per year",leg=T, cl=c("gray70","#377EB8", "navy"), labfreq=NA)+theme(legend.position = "right", legend.text = element_text(size=10), axis.text.x = element_text(angle=0,hjust=0.5))

plot_prod(eetdomfor %>% filter(channel!="All of ERR") %>% mutate(channel=fct_recode(channel,"ETV"="ETV1")), titl="Content by production origin across channels",y="airtime %  per year",leg=T, cl=c("gray70","#377EB8", "navy"))






# Normalized country counts

eetmaa_counts1 = 
  rbind(
    eetvald %>% filter(prodcountry!="") %>% 
      #filter(!(country %in% "Estonia") ) %>% 
      filter(channel!="Kanal2") %>% 
      filter(year>=2004, year <=2020) %>% 
      group_by(year, channel) %>% 
      summarise(
        ncountries = strsplit(prodcountry, ",[[:space:]]*") %>% unlist(T,T) %>% length,
        ucountries = strsplit(prodcountry, ",[[:space:]]*") %>% unlist(T,T) %>% unique() %>%  length,
        n=n() ) %>% 
      mutate(s=ncountries/n, s2=ucountries/n*1000, 
             type="(A) Diversity of production countries (per 1000 prog. items)")
    ,
    eetvald %>%  filter(prodcountry!="") %>%
      filter(prodcountry!="Eesti" ) %>%
      filter(channel!="Kanal2") %>%
      filter(year>=2004, year <=2020) %>%
      group_by(year, channel) %>%
      summarise(
        ncountries = strsplit(prodcountry, ",[[:space:]]*") %>% unlist(T,T) %>% length,
        ucountries = strsplit(prodcountry, ",[[:space:]]*") %>% unlist(T,T) %>% unique() %>%  length,
        n=n()
      ) %>%
      mutate(s=ncountries/n,s2=ucountries/n*1000, type="(B) Diversity, excluding domestic-only productions")
  ) %>% mutate(channel=fct_recode(channel, "ETV"="ETV1"))

plot_countrycounts(eetmaa_counts1)







# Country of production, imports

## Imports overview


#A
eetmaadur_all = eetvald %>% 
  #filter(!(country %in% "Estonia") ) %>% 
  filter(year>=2004, year <=2020, channel!="Kanal2") %>% 
  group_by(year, country) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  mutate(country=fct_relevel(country, c(levs[-1],"Estonia"))) %>% 
  mutate(channel="All 3 ERR channels")

#B
eetmaabin = eetvald %>% 
  filter(year>=2004, year <=2020) %>% 
  mutate(country=case_when(country!="Estonia"~"Foreign",T~"x")) %>% 
  group_by(year, country, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  filter(country=="Foreign") %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))


eetmaadur0 = eetvald %>% 
  filter(!(country %in% "Estonia") ) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by(year, country, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))
eetmaadur0$country = factor(eetmaadur0$country,setdiff(levs, "Estonia"))
eetmaa_count = eetvald %>% 
  filter(!(country %in% "Estonia") ) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by(year, country, channel) %>% 
  count(name="s") %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  mutate(country=factor(country,setdiff(levs, "Estonia"))) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))

# if channels separate, then this needs to be done by date not year, otherwise random biases that mess up result.
eetmaa_count_unique = eetvald %>% 
  filter(!(country %in% "Estonia") ) %>% 
  filter(!(channel %in% "Kanal2") ) %>%     # remove from here so ERR only!
  arrange(airdate) %>% 
  filter(!duplicated(id)) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by(year, country, channel) %>% 
  count(name="s") %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  mutate(country=factor(country,setdiff(levs, "Estonia"))) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))




{
  plot_imports(eetmaadur_all, titl="(A) ERR content origins",yt="%  of airtime per year", F)+annotate("text",2005,10,label="(Estonian-produced content)", color="white", hjust=0,vjust=0, size=4) +
    ggplot(eetmaabin, aes(x=year, y=s, color=channel, label=channel))+
    geom_line(size=0.4,lineend="round")+
    geom_point(size=0.8)+
    geom_shadowtext(data=eetmaabin %>% group_by(channel) %>% slice(1),bg.colour="white" , hjust=1.1,show.legend=F, size=3)+
    scale_color_manual(values=c("slategray","#4DAF4A", "#984EA3", "#E41A1C") %>% lighten(0.1))+
    scale_x_continuous(breaks=2004:2020, limits=c(2002,2020), expand = c(0,0.1))+
    scale_y_continuous(limits = c(16,80), sec.axis = sec_axis(~.))+
    labs(y="%  of airtime per year", title="(B) Foreign content per channel")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(), 
          legend.position = "none",
          axis.title = element_text(size=9),
          axis.text = element_text(size=7),
          plot.title = element_text(size=10),
          axis.text.x=element_text(angle=90, hjust=1,vjust=0.5,size=7), 
          axis.title.x = element_blank())+
    ggplot()+theme_void()+theme(plot.margin = margin(0,0,0,0))+
    plot_layout(nrow=1,widths = c(0.8,1,0.000000000000000001))
  } -
  #
  plot_imports(eetmaadur0, titl="(C) Origins of foreign imported content, as percentage of imported airtime",yt="%  of airtime per year") +
  plot_imports(eetmaa_count, titl="(D) Origins of foreign imported content, as percentage of imported programme items", yt="%  of items per year")+
  plot_imports(eetmaa_count_unique, titl="(E) Origins of foreign content, as percentage of imported new (not shown before in ERR) items",yt="%  of new items per year", leg=T)+
  plot_layout(ncol=1)+
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) 




#### UK separate panel
eetvald_content_uk = eetvald %>% 
  #filter(channel!="Kanal2") %>% 
  filter(year %in% 2004:2020, country=="UK") %>% 
  group_by(year, contenttype, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(contenttype=factor(contenttype, levs_content)) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))

levels(eetvald_content_uk$channel)  = 
  eetvald %>% 
  filter(year %in% 2004:2020) %>% 
  mutate(uk=case_when(country=="UK"~"UK", T~"x")) %>% 
  group_by(channel, country) %>% 
  summarise(s=sum(duration, na.rm=T))  %>%
  group_by(channel) %>% 
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  filter(country=="UK") %>% 
  ungroup() %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1")) %>% 
  mutate(x=paste0(channel, "\n(~", round(s,0), "% UK content)")) %>% pull(x)


plot_content(eetvald_content_uk, "airtime % per year", T)+labs(title="Percentage of content types within UK content") +theme(legend.key.height = unit(0.3,"cm"))






## Imports prime time vs non prime time



eetvald_prime = eetvald %>% filter( year>=2004 ) %>%
  mutate(start = airtime %>% gsub(":..$", "",.) %>% hms,
         length = airlength %>% gsub(":..$", "",.) %>% hms
  ) %>% 
  mutate(end = start + length) %>% 
  mutate(prime = case_when(
    is.na(end) ~ NA_character_,
    start >= hms("19:00:00") &   # if starts between 7-11pm and ends before 11.30pm
      start < hms("23:00:00") & 
      end <= hms("23:30:00") ~ "prime" ,
    T~"non-prime"
  )) %>%  # will throw warning as some time strings are nonstandard
  filter(!is.na(prime))


eetvald_prime_country = eetvald_prime %>%
  filter(year %in% 2016:2020) %>% 
  rename(ytype=country) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(prime, ytype, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(prime, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(ytype=as.factor(ytype)) %>% 
  mutate(ytype=fct_relevel(ytype, c(levs[-1],"Estonia"))) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))


plot_primetime(eetvald_prime_country, "Prime-time vs non-prime-time composition by content type (2016-2020)",  c("navy","#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70", "gray30")) 


# plot_primetime(eetvald_prime_country, "Prime-time vs non-prime-time composition by origin (2016-2020)", c("navy","#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70", "gray30")) 




## Import repetitiveness (generates eet_rep object also used below)



eet_rep = eetvald %>% 
  #bind_rows(k2)  %>%                             # merge with k2
  arrange(airdate) %>%   # here ok
  mutate(reps = case_when(duplicated(id) ~ T, T~F)) %>% # across ERR here for B (likelihood)
  #filter(year %in% (2016:2020)) %>%  # year needed below now!
  mutate(channel=fct_relevel(channel, "ETV1", "ETV2", "ETV+")) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))



# ratio per (new) content type over time, just within each given year.
eetrep_content_av= 
  eet_rep %>%
  filter(channel!="Kanal2") %>% 
  group_by(id) %>% filter(year==min(year, na.rm=T)) %>%  # ! filter to only include first year of show
  filter(year %in% (2016:2020)) %>% # filter done, can zoom to year range
  rename(ytype=contenttype) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(ytype, year, channel) %>% 
  summarize( repval=(n()/n_distinct(id))-1 ) %>%  # total to distinct
  group_by(ytype, channel) %>% 
  summarize(repval=mean(repval))
eetrep_content_lik = 
  eet_rep %>% 
  filter(channel!="Kanal2") %>% 
  filter(year %in% (2016:2020)) %>% 
  rename(ytype=contenttype) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(ytype, channel) %>% 
  summarize( repval=sum(reps)/n() ) # reps per items (over content&channel)

eetrep_country_av= 
  eet_rep %>%
  group_by(id) %>% filter(year==min(year, na.rm=T)) %>%  # ! filter to only include first year of show
  filter(year %in% (2016:2020)) %>% 
  rename(ytype=country) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(ytype, year, channel) %>% 
  summarize( repval=(n()/n_distinct(id))-1 ) %>%  # total to distinct
  group_by(ytype, channel) %>% 
  summarize(repval=mean(repval))
eetrep_country_lik = 
  eet_rep %>% 
  filter(year %in% (2016:2020)) %>% 
  rename(ytype=country) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(ytype, channel) %>% 
  summarize( repval=sum(reps)/n() ) # reps per items (over content&channel)




a="(A) Average repetitiveness of new content\n(within same channel and year)"
b="(B) Likelihood of being a repeat, i.e. not a\nnew programme (across ERR, across years)"

plot_rep(eetrep_country_av, a)+ plot_rep(eetrep_country_lik, b,c(0,1),T)+ plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) 








## Imports reach

# Reach (TV viewer ratings data) is not included in the published dataset; the code to generate the plots is commented out below.

# eetb_country = eetvald %>% filter(year %in% 2018:2020, channel!="Kanal2") %>% # ! range, channel
#   mutate(xvar=country, rankvar=country) %>%  
#   filter(!is.na(xvar)) %>%
#   group_by(xvar) %>% 
#   summarise(airtime=sum(duration, na.rm=T), rankvar=rankvar[1]) %>% 
#   mutate(airtime=airtime/sum(airtime)*100) %>% 
#   mutate(xvar=as.factor(xvar), xvar=fct_relevel(xvar, levs)) %>% 
#   mutate(rankvar=as.factor(rankvar), rankvar=fct_reorder(rankvar,airtime, mean,.desc = T)) %>% 
#   mutate(channel="")
# 
# eetview_country = eetview %>% filter(!is.na(country)) %>%  # excludes Kanal2
#   mutate(xvar=country, rankvar=country) %>%  filter(!is.na(xvar)) %>%  
#   group_by(xvar, channel) %>% mutate(tmp=sum(duration)) %>%  
#   group_by(xvar, channel) %>% 
#   mutate(med=median(AvRch)) %>% 
#   ungroup() %>% 
#   mutate(xvar=as.factor(xvar), xvar=fct_relevel(xvar, levs)) %>% 
#   #mutate(rankvar=as.factor(rankvar), rankvar=fct_reorder(rankvar,tmp, mean,.desc = T)) %>% 
#   mutate(rankvar=as.factor(rankvar), rankvar=fct_relevel(rankvar, levels(eetb_country$rankvar))) %>%
#   mutate(channel=as.factor(channel),channel=fct_relevel(channel, "ETV1","ETV2", "ETV+"))
# 
# eetview_country_all = eetview %>% 
#   mutate(xvar=country, rankvar=country) %>%  filter(!is.na(xvar)) %>%  
#   group_by(xvar) %>% mutate(tmp=sum(duration)) %>%  
#   group_by(xvar) %>% 
#   mutate(med=median(AvRch)) %>% 
#   ungroup() %>% 
#   mutate(xvar=as.factor(xvar), xvar=fct_relevel(xvar, levs)) %>% 
#   #mutate(rankvar=as.factor(rankvar), rankvar=fct_reorder(rankvar,tmp, mean,.desc = T)) %>% 
#   mutate(rankvar=as.factor(rankvar), rankvar=fct_relevel(rankvar, levels(eetb_country$rankvar))) %>%
#   mutate(channel="All of ERR")
#
# reachplot(eetview_country_all, cl=c("gray30", lighten("navy", 0.1),"#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70"), 1, eetb_country) +theme(strip.background = element_blank(), strip.text.x = element_blank())
# 






# Content types 

## Content types overview


eetvald_content_dur = eetvald %>% 
  filter(!is.na(contenttype)) %>%  # few NAs in k2, creates legend entry
  filter(year>=2004, year <=2020) %>% 
  group_by(year, contenttype, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(contenttype=factor(contenttype, levs_content)) %>% 
  rename(country=contenttype) %>%   # hack to use same function
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))

eetvald_content_dur_all = eetvald %>% 
  filter(!is.na(contenttype)) %>%  # few NAs in k2, creates legend entry
  filter(year>=2004, year <=2020, channel!="Kanal2") %>% 
  group_by(year, contenttype) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(year) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(contenttype=factor(contenttype, levs_content)) %>% 
  rename(country=contenttype) %>%  # hack to use same function
  mutate(channel="All of ERR")


eetvald_content_count = eetvald  %>% 
  filter(!is.na(contenttype)) %>%  # few NAs in k2, creates legend entry
  filter(year>=2004, year <=2020) %>% 
  group_by(year, contenttype, channel) %>% 
  count(name="s") %>% 
  group_by(year, channel) %>% 
  mutate(s=s/sum(s,na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(contenttype=factor(contenttype, levs_content)) %>% 
  rename(country=contenttype)   %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))

eetvald_content_count_unique = eetvald  %>% 
  filter(!is.na(contenttype)) %>%  # few NAs in k2, creates legend entry
  arrange(airdate) %>%  filter(!duplicated(id)) %>% 
  group_by(year, contenttype, channel) %>% 
  count(name="s") %>% 
  group_by(year, channel) %>% 
  mutate(s=s/sum(s,na.rm=T)*100) %>% 
  filter(year %in% (2004:2020)) %>% 
  ungroup() %>% 
  mutate(contenttype=factor(contenttype, levs_content)) %>% 
  rename(country=contenttype) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))



cl=c(RColorBrewer::brewer.pal(8, "Set1"),"slategray", "navy")

plot_imports(eetvald_content_dur_all, titl="Content types, as percentage of airtime across ERR",yt="%  of airtime per year", cl=cl, mod=Inf, minper = 7,
             cl2 = cl %>% {.[6]=darken(.[6],0.9);.}
)+ theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.x=element_text(angle=0,hjust=0.5), legend.position = "right", legend.key.height = unit(0.4,"cm"))+labs(fill="")  # all of err


# big plot
plot_imports(eetvald_content_dur, titl="(A) Content types, as percentage of airtime",yt="%  of airtime per year", cl=cl,  cl2 = cl %>% {.[6]=darken(.[6],0.9);.}, minper=8) +
  plot_imports(eetvald_content_count, titl="(B) Content types, as percentage of programme items", yt="%  of items per year",cl=cl,  cl2 = cl %>% {.[6]=darken(.[6],0.9);.},minper=8)+
  plot_imports(eetvald_content_count_unique, titl="(C) Content types, as percentage of new (not shown before) items",yt="%  of new items per year", leg=T,cl=cl, legt="Content types:",  cl2 = cl %>% {.[6]=darken(.[6],0.9);.}, minper=8)+
  plot_layout(ncol=1)+
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) 









## Content prime time vs non prime time

eetvald_prime = eetvald %>% filter( year>=2004 ) %>%
  mutate(start = airtime %>% gsub(":..$", "",.) %>% hms,
         length = airlength %>% gsub(":..$", "",.) %>% hms
  ) %>% 
  mutate(end = start + length) %>% 
  mutate(prime = case_when(
    is.na(end) ~ NA_character_,
    start >= hms("19:00:00") &   # if starts between 7-11pm and ends before 11.30pm
      start < hms("23:00:00") & 
      end <= hms("23:30:00") ~ "prime" ,
    T~"non-prime"
  )) %>%  # will throw warning as some time strings are nonstandard
  filter(!is.na(prime))

eetvald_prime_content = eetvald_prime %>%
  filter(year %in% 2016:2020) %>% 
  rename(ytype=contenttype) %>% 
  filter(!is.na(ytype)) %>% 
  group_by(prime, ytype, channel) %>% 
  summarise(s=sum(duration, na.rm=T))  %>% 
  group_by(prime, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  ungroup() %>% 
  mutate(ytype=as.factor(ytype)) %>% 
  mutate(ytype=fct_relevel(ytype, levs_content)) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1"))


plot_primetime(eetvald_prime_content, "Prime-time vs non-prime-time composition by content type (2016-2020)", c(RColorBrewer::brewer.pal(8, "Set1"),"slategray", "navy")) #%>% plotly::ggplotly() 


# plot_primetime(eetvald_prime_country, "Prime-time vs non-prime-time composition by origin (2016-2020)", c("navy","#377EB8","#FF7F33","#E41A1C", "#4DAF4A", "gray70", "gray30")) 




## Content repetitiveness

a="(A) Average repetitiveness of \nnew content (within same\nchannel and year)"
b="(B) Likelihood of being a repeat,\ni.e. not a new programme\n(across ERR, all years)"
cc = "(C) Production types\nrepetitiveness\nin ERR"

prodrep = eet_rep %>%  
  mutate(ytype=contenttype) %>% 
  group_by(id) %>% filter(year==min(year, na.rm=T)) %>%  # ! filter to only include first year of show
  filter(!is.na(ytype)) %>%  # none actually
  filter(channel!="Kanal2") %>% 
  filter(year %in% (2016:2020)) %>%  
  group_by(ytype, year, productiontype0) %>% 
  summarize( repval=(n()/n_distinct(id))-1 ) %>%  # total to distinct
  group_by(ytype, productiontype0) %>% 
  summarize(repval=mean(repval)) %>% 
  rename(channel=productiontype0) %>% mutate(repval=round(repval,1)) %>% 
  mutate(channel=as.factor(channel)) %>% 
  mutate(ytype=as.factor(ytype)) %>% 
  mutate(ytype = fct_relevel(ytype,replevs))
levels(prodrep$channel) = c("Bought", "Com.", "Own")


plot_rep(eetrep_content_av, a)+
  plot_rep(eetrep_content_lik, b,c(0,1), T)+
  plot_rep(prodrep, cc,c(0,1), T)+ theme(plot.margin = margin(l=2.5))+
  plot_layout(nrow=1, widths = c(1,1,0.6))+
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) 





## Content reach

# Reach (TV viewer ratings data) is not included in the published dataset; the code to generate the plots is commented out below.

# eetb_content = eetvald %>% filter(year %in% 2018:2020, channel!="Kanal2") %>% # ! range, channel
#   mutate(xvar=contenttype, rankvar=contenttype) %>%  
#   filter(!is.na(xvar)) %>%
#   group_by(xvar, channel) %>% 
#   summarise(airtime=sum(duration, na.rm=T), rankvar=rankvar[1]) %>% 
#   group_by(channel) %>% 
#   mutate(airtime=airtime/sum(airtime)*100) %>% 
#   mutate(xvar=as.factor(xvar), xvar=fct_relevel(xvar, levs_content)) %>% 
#   ungroup() %>% 
#   mutate(rankvar=as.factor(rankvar), rankvar=fct_reorder(rankvar,airtime, mean,.desc = T)) %>%
#   mutate(channel=as.factor(channel),channel=fct_relevel(channel, "ETV1","ETV2", "ETV+")) %>% 
#   mutate(channel=fct_recode(channel, "ETV"="ETV1"))
# 
# eetview_content = eetview %>% filter(!is.na(contenttype)) %>%  
#   mutate(xvar=contenttype, rankvar=contenttype) %>%  filter(!is.na(xvar)) %>%  
#   group_by(xvar, channel) %>% mutate(tmp=sum(duration)) %>%  
#   group_by(xvar, channel) %>% 
#   mutate(med=median(AvRch)) %>% 
#   ungroup() %>% 
#   mutate(xvar=as.factor(xvar), xvar=fct_relevel(xvar, levs_content)) %>% 
#   mutate(rankvar=as.factor(rankvar), rankvar=fct_relevel(rankvar, levels(eetb_content$rankvar))) %>% 
#   mutate(channel=as.factor(channel),channel=fct_relevel(channel, "ETV1","ETV2", "ETV+")) %>% 
#   mutate(channel=fct_recode(channel, "ETV"="ETV1"))
# 
# 
# reachplot(eetview_content,cl=c(RColorBrewer::brewer.pal(8, "Set1"),"slategray", lighten("navy", 0.1)) %>% {.[6]=darken(.[6],0.3);.}, 1, eetb_content) 








# Production types

# This section uses counts of unique (new, non-repeat) items.

eetprod_new_all = eetvald %>% filter(channel!="Kanal2") %>% 
  arrange(airdate) %>% filter(!duplicated(id)) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by(year, productiontype0) %>% 
  summarise(s=n())  %>%  # ! items
  group_by(year) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% mutate(channel="All of ERR")

eetprod_new = 
  eetvald %>% filter(channel!="Kanal2") %>%  
  arrange(airdate) %>% filter(!duplicated(id)) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by( year, productiontype0, channel) %>% 
  summarise(s=n())  %>% 
  group_by(year, channel) %>%
  mutate(s=s/sum(s, na.rm=T)*100) %>% 
  mutate(channel=as.factor(channel)) %>% 
  mutate(channel=fct_recode(channel, "ETV"="ETV1")) %>% 
  mutate(channel=fct_relevel(channel,  "ETV", "ETV2", "ETV+"))

plot_prod(eetprod_new_all, "Production types", "", labfreq=100,minfreq=1, minsize=2.2)+theme(legend.position = "none",axis.text.x=element_text(angle=0,hjust=0.5))+
  plot_prod(eetprod_new, "Production types", "%  of new items per year",minfreq=5,labfreq = 100, minsize=1.2)+theme(axis.title.y = element_text(hjust=-1.5)) + plot_layout(ncol=1)+plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) 



eetprod_new_content = eetvald %>% filter(channel!="Kanal2") %>% 
  arrange(airdate) %>% filter(!duplicated(id)) %>% 
  filter(year>=2004, year <=2020) %>% 
  group_by(year, productiontype0, contenttype) %>% 
  summarise(s=n())  %>% 
  group_by(year, contenttype) %>% mutate(s=s/sum(s, na.rm=T)*100) 


plot_prod(eetprod_new_content, "Production types", "%  of new items per year", facettype="contenttype", minsize=1.5,maxsize=2.3)


























