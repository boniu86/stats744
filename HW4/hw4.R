library(readr)
library(reshape2)
library(tidyverse)
library(directlabels)

fish_data<-read_csv("fish.csv",col_names = TRUE)
colnames(fish_data)[3]<-"SampleID"
fish_name<-read_csv("fish_names.csv",col_names = TRUE)

#wide format to long format
fish_long<-melt(fish_data, id.vars = c("SampleID", "Site","MetCode","SamplerType"),
     variable.name = "treatment_variable", 
     value.name = "treatment_value")

#merge name data with  long format data.
fish_name$treatment_variable<- fish_name$abbr
full<-merge(fish_long, fish_name)
(full%>%select(-abbr))->full




p1<-(ggplot(full,aes(x=Site,y=treatment_value,colour=treatment_variable))
     +geom_point(alpha=0.3)
     +facet_wrap(~drugcat,labeller = label_both,scales = "free"))

print(p1+geom_line(aes(group=treatment_variable))
   +geom_dl(aes(label=treatment_variable),,method="last.points")
   +theme(axis.text.x = element_text(angle = 60))
   +theme(legend.position="none")
)

p2<-ggplot(full,aes(x=treatment_variable,y=treatment_value,colour=Site))
print(p2+geom_line(aes(group=Site))+facet_wrap(~Site,labeller = label_both,scales = "free")
      +geom_point()
      +theme(axis.text.x = element_text(angle = 60))
      +theme(legend.position="none"))



