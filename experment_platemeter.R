#experment_platemeter
#clean data set with all platemeter readings from experiment area.
# vars: plot, cover, date

library(lubridate)
library(readxl)


# old 23 read in data for reference and joining to 24/25 ----
platemeter23<-read_csv('experiment_platemeter.csv')%>%
  mutate(date=mdy(date1))%>%
  mutate(paddock_id_new=fct_reorder(as.factor(paddock_id_new), desc(cover)))
platemeter23$impact2<-factor(platemeter23$impact2, levels= c("no-early", "pre-arrival", "pre-nesting", "early-nesting"))
platemeter23$treatment...6<-factor(platemeter23$treatment...6, levels= c("no-early", "pre-arrival", "pre-nesting", "early-nesting"))
#platemeter$hotspot<-factor(platemeter$hotspot, levels= c("no", "low", "moderate", "high"))


# put all platemeter plot + dm together: platemeter_all (missing data from moves in 25) ---- 

platemeter23_simp<-platemeter23%>%
  mutate(plot=paste("Plot", paddock_id_new))%>%
  select(plot, cover, date)

apr2525<-read_excel("./plate25/2025-4-25 old zone 1.xlsx")%>%
  mutate(date=mdy("4/25/25"))
may1425<-read_excel("./plate25/2025-5-14.xlsx")%>%
  mutate(date=mdy("5/14/25"))
may1925<-read_excel("./plate25/2025-5-19.xlsx")%>%
  mutate(date=mdy("5/19/25"))
may3025<-read_excel("./plate25/2025-5-30.xlsx")%>%
  mutate(date=mdy("5/30/25"))
jun625<-read_excel("./plate25/2025-6-05.xlsx")%>%
  mutate(date=mdy("6/5/25"))
jun2325<-read_excel("./plate25/2025-6-23 newfields.xlsx")%>%
  mutate(date=mdy("6/23/25"))
platemeter25<-rbind(apr2525, may1425, may1925, may3025, jun625, jun2325)
rm(apr2525, may1425, may1925, may3025, jun625, jun2325)

apr424<-read_csv("./plate24/2024-4-24-experiment.csv")%>%
  mutate(date=mdy("4/24/24"))
apr3024<-read_csv("./plate24/2024-4-30-experiment.csv")%>%
  mutate(date=mdy("4/30/24"))
may524<-read_csv("./plate24/2024-5-09-experiment.csv")%>%
  mutate(date=mdy("5/9/24"))
may1624<-read_csv("./plate24/2024-5-16-experiment.csv")%>%
  mutate(date=mdy("5/16/24"))
may2224<-read_csv("./plate24/2024-5-22-experiment.csv")%>%
  mutate(date=mdy("5/22/24"))
may2924<-read_csv("./plate24/2024-5-29-experiment.csv")%>%
  mutate(date=mdy("5/29/24"))
jun524<-read_csv("./plate24/2024-6-05-experiment.csv")%>%
  mutate(date=mdy("6/5/24"))
jun1224<-read_excel("./plate24/2024-6-12.xlsx")%>%
  mutate(date=mdy("6/12/24"))
jun1924<-read_csv("./plate24/2024-6-19.csv")%>%
  mutate(date=mdy("6/19/24"))
jul1024<-read_excel("./plate24/2024-7-10-00-00-00.xlsx")%>%
  mutate(date=mdy("7/10/24"))
platemeter24<-rbind(apr424, apr3024, may524, may1624, may2224, may2924, jun524, jun1224, jun1924,  jul1024)
rm(apr424, apr3024, may524, may1624, may2224, may2924, jun524, jun1224, jun1924,  jul1024)

platemeter_all<-rbind(platemeter23_simp, platemeter24, platemeter25)
rm(platemeter23_simp, platemeter24, platemeter25)

## old 23 platemeter analysis for reference----

ggplot(platemeter23, aes(date, cover, group=paddock_id_new, shape=impact)) + geom_point() + 
  geom_line() + facet_wrap(~paddock_id_new) + xlab("") + ylab("estimated dm kg/ha")+theme_classic()

ggplot(platemeter23, aes(paddock_id_new, cover)) + geom_point() + 
  geom_line() + facet_wrap(~date1) + xlab("") + ylab("estimated dm kg/ha")+theme_classic()

ggplot(subset(platemeter23, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover, color=treatment...6, shape=as.factor(date))) + geom_point() + 
  xlab("") + ylab("estimated dm kg/ha")+theme_classic()

#each plot showing cover change over time. organized by average cover.
#highlights which plots have early impact
a<-ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover))  + 
  geom_line() +  xlab("plotID") + ylab("estimated dm kg/ha")+theme_classic()+ geom_point(size=2, aes(shape=as.factor(date), color=treatment...6))

#number of nests
c<-ggplot(subset(platemeter, date1<"2023-05-22"), aes(x=paddock_id_new, y=num_nests, color=impact2))+geom_point()+
  theme_classic()+ylab("Total Nests Found")+ xlab("plotID")

#hotspot
b<-ggplot(subset(platemeter, date1<"2023-05-22"), aes(x=paddock_id_new, y=hotspot, color=impact2))+geom_point()+theme_classic()+ylab("Early Season Activity (May 26-June 7)")+ xlab("plotID")

library(ggpubr)
ggarrange(a, b, c, ncol=1, nrow=3, common.legend = TRUE, legend="right")


ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover))  +   geom_bar(aes(y=num_nests*2000), stat="identity")+
  geom_line() +  xlab("plotID") + ylab("Estimated dm kg/Ha")+theme_classic()+ geom_point(size=2, aes(shape=as.factor(date), color=treatment...6))+
  scale_y_continuous(sec.axis = sec_axis(~ . /2000, name="Nests Found"))


ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover))  +   geom_bar(aes(y=hotspot*2000), stat="identity")+
  geom_line() +  xlab("plotID") + ylab("Estimated dm kg/Ha")+theme_classic()+ geom_point(size=2, aes(shape=as.factor(date), color=treatment...6))+
  scale_y_continuous(sec.axis = sec_axis(~ . /2000, name="Hotspot Level"))

ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(cover, hotspot))  + 
  theme_classic()+ geom_point(size=2, aes(shape=as.factor(date), color=treatment...6)) +geom_smooth(method = "lm", se=F)

ggplot(subset(platemeter, date1<"2023-05-22"&date!="2023-05-30"), aes(cover, hotspot))  + 
  theme_classic()+ geom_point(size=2, aes(color=treatment...6)) +geom_smooth(method = "lm", se=F)+ylab("Number of Hotspots")+xlab("Forage kg/Ha")

ggplot(subset(platemeter, date1<"2023-05-22"&date!="2023-05-30"), aes(cover, num_nests))  + 
  theme_classic()+ geom_point(size=2, aes(color=treatment...6))+ylab("Number of Nests")+xlab("Forage kg/Ha")



ggplot(filter(platemeter25, plot%in%c("Plot5", "Plot6", "Plot19", "Plot20", "Plot21")), aes(date, cover)) + geom_point() + facet_wrap(~plot)
       
       
       
       
       