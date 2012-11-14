require(ggplot2)
require(plyr)

source('theme_map.R')

plot.df<-structure(list(years = c(2008, 2004, 2000, 1996, 1992, 1988, 
1984, 1980, 1976, 1972, 2008, 2004, 2000, 1996, 1992, 1988, 1984, 
1980, 1976, 1972), margins = c(4.58, 2.11, 3.51, 6.36, 1.83, 
10.85, 18.76, 10.6, 0.27, 21.56, 7.26, 2.46, 0.52, 8.52, 5.56, 
7.73, 18.72, 9.74, 2.06, 23.15), party = c("D", "R", "R", "D", 
"D", "R", "R", "R", "D", "R", "D", "R", "D", "D", "D", "R", "R", 
"R", "D", "R"), level = c("ohio", "ohio", "ohio", "ohio", "ohio", 
"ohio", "ohio", "ohio", "ohio", "ohio", "national", "national", 
"national", "national", "national", "national", "national", "national", 
"national", "national")), .Names = c("years", "margins", "party", 
"level"), row.names = c(NA, 20L), class = "data.frame")

ohio.df <-
  data.frame(
             years = c(2008, 2004, 2000, 1996, 1992, 1988, 1984, 1980, 1976, 1972),
             margins =c(4.58, 2.11, 3.51, 6.36, 1.83, 10.85, 18.76, 10.6, 0.27, 21.56),
             party =  c("D",  "R",  "R",  "D",  "D",  "R",    "R",   "R",  "D",  "R"))

local.df <-subset(mutate(ohio.df,local_sign=c(-1,1)[(party=='R')+1]),select=c(years,local_sign))

offset.df<-ddply(mutate(join(plot.df,local.df), party_sign=c(-1,1)[(party=='R')+1], sign_margin=local_sign*party_sign*margins), .(years),summarize,offset=(max(sign_margin)+1))

margin.text.df<-mutate(join(join(ohio.df,local.df),offset.df),signed_offset=offset*local_sign)

margin.text.df$label<-paste0(ifelse(margin.text.df$margin>1,round(margin.text.df$margin),margin.text.df$margin),'%')
ohio.gg<-ggplot(plot.df,aes(x=years,width=.4)) +
  geom_bar(aes(y= -margins,width=.8),fill="grey",stat="identity",
           data=subset(plot.df,party=="D" & level=="national")) +
  geom_bar(aes(y= margins,width=.8),fill="grey",stat="identity",
           data=subset(plot.df,party=="R" & level=="national")) +
  geom_bar(width=.5, aes(y= -margins),stat="identity",fill="blue",
           data=subset(plot.df,party=="D" & level=="ohio"))+
  geom_bar(aes(y= margins),stat="identity",fill="red",
           data=subset(plot.df,party=="R" & level=="ohio"))  +
 geom_segment(aes(y=0,x=(1972 -.4),yend=0,xend=(2010+.4)))+
  geom_text(data=subset(ohio.df,party=="R"),aes(x=years,y=-2,label=as.character(years)),size=3) +
  geom_text(data=subset(ohio.df,party=="D"),aes(x=years,y=1,label=as.character(years)),size=3) +
  geom_text(data=margin.text.df,aes(x=years,y=signed_offset,label=label),size=3)+
  theme_map() +
  coord_flip()

