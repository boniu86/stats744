


(contraception
  %>%group_by(age,urban)
  %>%summarise(
    prop=mean(as.numeric(use)-1),
    n=n(),
    se=sqrt(prop*(1-prop)/n))
)->contr.sum

ggplot(contr_sum,aes(age,prop,colour=urban))+
  geom+point(aes(size=1))+
  geom_linerange(aes(ymin=prop-se,ymax=prop+se))+
  scale_colour_brewer(palette="Dark2")