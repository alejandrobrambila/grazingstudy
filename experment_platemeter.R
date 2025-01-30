#experment_platemeter

# read in data desribing vegetative cover in plots at different times in the year.
platemeter<-read_csv('/Users/alejandro/Downloads/experiment_platemeter.csv')%>%
  mutate(date=ymd(date1))%>%
  mutate(paddock_id_new=fct_reorder(paddock_id_new, desc(cover)))
platemeter$impact2<-factor(platemeter$impact2, levels= c("no-early", "pre-arrival", "pre-nesting", "early-nesting"))
platemeter$treatment...6<-factor(platemeter$treatment...6, levels= c("no-early", "pre-arrival", "pre-nesting", "early-nesting"))
#platemeter$hotspot<-factor(platemeter$hotspot, levels= c("no", "low", "moderate", "high"))


ggplot(platemeter, aes(date, cover, group=paddock_id_new, shape=impact)) + geom_point() + 
  geom_line() + facet_wrap(~paddock_id_new) + xlab("") + ylab("estimated dm kg/ha")+theme_classic()

ggplot(platemeter, aes(paddock_id_new, cover)) + geom_point() + 
  geom_line() + facet_wrap(~date1) + xlab("") + ylab("estimated dm kg/ha")+theme_classic()

ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover, color=treatment...6, shape=as.factor(date))) + geom_point() + 
  xlab("") + ylab("estimated dm kg/ha")+theme_classic()

#each plot showing cover change over time. organized by average cover.
#highlights which plots have early impact
ggplot(subset(platemeter, date1<"2023-06-10"&date!="2023-05-30"), aes(paddock_id_new, cover))  + 
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

