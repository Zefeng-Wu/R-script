new<-data.frame(chro_name=c("M1","M1","M2","M2","M3","M3"),y_axis=c(2,2,4,4,2,2),x_axis=c(1,10,1,20,12,18))
d<- data.frame(link_x=c(5,7), y=c(2,2), vx=c(6,3), vy=c(4,4))

ggplot()+geom_line(data=new,aes(x_axis, y_axis, group = chro_name,size=1)) + geom_text(data = new,aes(x=x_axis+1,y=y_axis, label=c(NA,"ChrA",NA,"ChrB",NA,"ChrC")))+geom_segment(data=d, aes(x=link_x, y=y, xend=vx, yend=vy), size=5, color="blue")+theme_void()+theme(legend.position="none")
