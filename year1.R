### year 1 (2023): grazing experiment great pasture 

### questions: 
# impact of light grazing (residuals on the 16th, 23rd) encourage into specific plots? 
# activity in different plots over time (how long after grazing does activity pick back up)

library(tidyverse)
library(lubridate)
library(ggpubr)

setwd("/Users/alejandro/Library/CloudStorage/OneDrive-TTOR/Farm Team/Agroecology Initiative")

nests<-read_csv("./Projects/lookout foundation study/data/spreadsheets/2023/nest_sheets.csv")
grazing<-read_csv("./Projects/lookout foundation study/data/spreadsheets/2023/plot_grazing.csv")
platemeter_long<-read_csv("./Projects/lookout foundation study/data/spreadsheets/2023/platemeter_long23.csv")


### Part 1: How did timing of grazing affect where we find nests? 
nestplotcounts<-grazing%>%
  group_by(category)%>%
  summarize(foundnests=sum(foundnests), plots=n())%>%
  mutate(category=ifelse(category=="after", "Grazed after May 16th", ifelse(category=="before", "Grazed up to May 16th", "Fallow")))
nestplotcounts$category<-factor(nestplotcounts$category, levels=c("Fallow", "Grazed up to May 16th", "Grazed after May 16th"))

plot1<-ggplot(nestplotcounts, aes(x=category, y=plots))+geom_bar( width=.5, stat="identity")+ylab("Number of two acre plots in each category")+xlab("")
plot2<-ggplot(nestplotcounts, aes(x=category, y=foundnests))+geom_bar(width=.5, stat="identity")+ylab("Number of nests found in each category")+xlab("")
#ggplot(nestplotcounts, aes(x=category, y=foundnests/plots))+geom_bar( stat="identity")+ylab("Average number of nests per plot")+xlab("")

ggarrange(plot1, plot2) #could adjust this to suitable acres in each category, and nests/acre

#rough attempt
plot3<-ggplot(nestplotcounts, aes(x=category, y=plots*2))+geom_bar( width=.5, stat="identity")+ylab("Suitable nesting acres in each category")+xlab("")
plot4<-ggplot(nestplotcounts, aes(x=category, y=foundnests/(plots*2)))+geom_bar( width=.5, stat="identity")+ylab("Average nests per acre")+xlab("")
ggarrange(plot3, plot4)

#notes: about 1 nest per 1.5 acres in the fallow plots! this seems congested, perhaps because some moved out of others.


### Part 2: Do residuals predict where nests are? 

a<-ggplot(grazing, aes(y=dm_apr27, x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_apr27))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("Dry Matter (Lbs/Acre) on April 27th") +xlab("Nests (Future)")
b<-ggplot(grazing, aes(y=dm_midmay, x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category, shape=(as.factor(gr_midmay))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on May 17th")+xlab("Nests (Future)")
c<-ggplot(grazing, aes(y=dm_may23, x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_may23))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on May 23rd")+xlab("Nests (Starting)")
d<-ggplot(grazing, aes(y=dm_may30, x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_may30))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on May 30th")+xlab("Nests")
e<-ggplot(grazing, aes(y=as.numeric(dm_jun9), x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_jun9))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on June 9th")+xlab("Nests")
f<-ggplot(grazing, aes(y=as.numeric(dm_jun27), x=foundnests))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_jun27))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on June 27th")+xlab("Nests (Fledged)")

ggarrange(a, b, c, d,e,f ,ncol=6, nrow=1, common.legend = T)
# notes, what is happening on april 27th doesnt matter.  On may 27th 


c<-ggplot(grazing, aes(y=dm_may23, x=ne_may23))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_may23))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("Dry Matter (Lbs/Acre) on May 23rd")+xlab("Active Nests")
d<-ggplot(grazing, aes(y=dm_may30, x=ne_may30))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_may30))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on May 30th")+xlab("Active Nests")
e<-ggplot(grazing, aes(y=as.numeric(dm_jun9), x=ne_jun9))+scale_shape_manual(values=c(16,3))+
  geom_point(aes(color=category,shape=(as.factor(gr_jun9))))+ scale_y_continuous(limits = c(1500, 6000)) +ylab("... on June 9th")+xlab("Active Nests")

ggarrange( c, d,e ,ncol=3, nrow=1, common.legend = T)


#platemeter over time

ggplot(filter(platemeter_long, !is.na(dm)==T), aes(x=julian, y=dm))+geom_line()+geom_point(aes(color=(notes)))+facet_grid(~plot)

#nests over time (shows spread)
ggplot(nests, aes(x=expected_fledge, y=reorder(nestid, -expected_fledge)))+geom_point(aes())
    #add grouping by when category/when grazed (or add grazing points)       
       