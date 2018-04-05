library(readr)
library(reshape2)
library(tidyverse)
library(directlabels)

fish_data<-read_csv("fish.csv")
names(fish_data)[3]<-make.names("SampleID")


fish_name<-read_csv("fish_names.csv")

#wide format to long format
fish_long<-melt(fish_data, id.vars = c("SampleID", "Site","MetCode","SamplerType"),
     variable.name = "treatment_variable", 
     value.name = "treatment_value")



#merge name data with  long format data.
fish_name$treatment_variable<- fish_name$abbr
full<-merge(fish_long, fish_name)
(full%>%select(-abbr))->full


(full%>%group_by(Site,drugcat)
  %>%summarise(n=n(),avg=mean(treatment_value))
)->a

p11<-(ggplot(a,aes(x=Site,y=avg,colour=treatment_variable))
     +geom_point(alpha=0.3)
     +facet_wrap(~drugcat,labeller = label_both,scales = "free"))

pp11<-(p1+geom_line(aes(group=treatment_variable))
      +geom_dl(aes(label=treatment_variable),,method="last.points")
      +theme(axis.text.x = element_text(angle = 60))
      +theme(legend.position="none")
)
print(pp11)

p1<-(ggplot(full,aes(x=Site,y=treatment_value,colour=treatment_variable))
     +geom_point(alpha=0.3)
     +facet_wrap(~drugcat,labeller = label_both,scales = "free"))

pp1<-(p1+geom_line(aes(group=treatment_variable))
   +geom_dl(aes(label=treatment_variable),,method="last.points")
   +theme(axis.text.x = element_text(angle = 60))
   +theme(legend.position="none")
)



ggplotly(pp1)

p2<-ggplot(full,aes(x=treatment_variable,y=treatment_value,colour=Site))
print(p2+geom_line(aes(group=Site))+facet_wrap(~Site,labeller = label_both,scales = "free")
      +geom_point()
      +theme(axis.text.x = element_text(angle = 60))
      +theme(legend.position="none"))



