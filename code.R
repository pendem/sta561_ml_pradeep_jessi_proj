
Sys.setenv(TZ = "UTC")

# load packages
source('ipak.R')

# load data
load("C:/Users/Pradeep/Desktop/research/taxi/yellow/analysis/r/jun_week1.RData")


# excluding observations with abnormal distance (miles)
jun_week1 = jun_week1 %>% filter(dist_mil>0 & dist_mil <=35) 


# excluding observations with abnormal duration (secs)
jun_week1 = jun_week1 %>% filter(dur_secs_cal>0 & dur_secs_cal<=6300) 


# excluding observations with abnormal speed (miles per hour)
jun_week1 = jun_week1 %>% filter(vel_mph_cal>0 & vel_mph_cal<=70) 


# excluding observations with abnormal total fare ($)
jun_week1 = jun_week1 %>% filter(tot_fare>0 & tot_fare<=150) 


# creating wait time variable
jun_week1 = jun_week1 %>% arrange(driver_id,pickup_datetime)
jun_week1 = jun_week1 %>% group_by(driver_id,pickup_date) %>% 
  mutate(lag_dropoff_datetime = lag(dropoff_datetime),wait_time = as.numeric(difftime(pickup_datetime,lag_dropoff_datetime, units = c("secs"))))
jun_week1 = ungroup(jun_week1)
jun_week1 = jun_week1 %>% filter(is.na(wait_time) | wait_time>0)


# data prep for cluster and principal component analysis
temp1 = jun_week1 %>% group_by(driver_id,day,pickup_slot) %>% summarise(trips = n())
temp1 = ungroup(temp1)
        
temp2 = ungroup(temp1) %>% group_by(driver_id,day) %>% summarise(tot_trips = sum(trips))
temp2 = ungroup(temp2)
        
temp3 = left_join(temp1,temp2,by=c("driver_id","day")) %>% mutate(prop_trips = round(trips/tot_trips,4)) %>% select(-trips)
temp3 = spread(temp3,pickup_slot,prop_trips)
names(temp3)[4:27] = paste("pickup_slot-",names(temp3)[4:27],sep="")
temp3[is.na(temp3)]=0
temp3 = ungroup(temp3)
        
temp4 = jun_week1 %>% group_by(driver_id,day) %>% summarise(tot_fare=sum(tot_fare),tot_dist_mil = sum(dist_mil))

temp5 = left_join(temp3,temp4,by=c("driver_id","day")) 
        
temp6 = temp5 %>% select(-driver_id,-day,-tot_trips,-tot_fare,-tot_dist_mil)
pr.out = prcomp(temp6 , scale =TRUE)
pr.var = pr.out$sdev^2
pve =pr.var/sum (pr.var)
cumsum(round(pve*100,3))
        

# generation of principal components 
t1 = data_frame(x = 1:24,y=cumsum(round(pve*100,3)))
p1 = ggplot(t1, aes(x = x,y=y)) + geom_line(stat="identity",size=1.2,color = "blue")+
     geom_segment(aes(x = 12, y = 0, xend = 12, yend = 80.285), color="black",linetype = 2,size=1.2)+
     geom_segment(aes(x = 0, y = 80.285, xend = 0, yend = 80.285), color="black")+
     theme_grey(base_size = 20)+
     theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
     scale_x_continuous("Principal Components",labels = comma)+
     scale_y_continuous("Cumulative Variance",labels = comma)
print(p1)
     
   
# within sum of squares for different clusters 
wss=0
for (i in 1:15) wss[i] = sum(kmeans(temp6,centers=i,nstart = 25)$withinss)
t2 = data_frame(x = 1:15,y=wss,z = "wss")
  
p2 = ggplot(t2, aes(x = x,y=y)) + geom_line(stat="identity",size=1.2,color = "red")+
     geom_segment(aes(x = 4, y = 0, xend = 4, yend = 12520.297), color="black",linetype = 2,size=1.2)+
     theme_grey(base_size = 20)+
     theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
      scale_x_continuous("Clusters",labels = comma)+
      scale_y_continuous("With-in Sum of Squares",labels = comma)
print(p2)
        

# k-means output for 4 clusters  
km.out = kmeans(scale(temp6),centers = 4,nstart = 25)
temp7 = cbind(km.out$cluster,temp5)
names(temp7)[1] = "cluster"

  
temp = gather(temp7,"slot","prop",5:28)
temp = temp %>% group_by(cluster,slot) %>% summarise(mean_prop= mean(prop),med_prop= quantile(prop,0.5))
temp = temp %>% group_by(cluster) %>% mutate(pickup_slot=1:24)

        
# med_prop_trips_by_cluster_slot  
p = ggplot(temp, aes(pickup_slot, mean_prop,group=factor(cluster),color = factor(cluster)))+
    geom_line(stat="identity",size=1.2)+
    scale_color_manual(values=c("red", "blue","darkgreen","black"))+
    theme_grey(base_size = 20)+
    theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
    scale_x_continuous("Hour",breaks=seq(1,24,2),minor_breaks = seq(1,24,1),
                         labels = c("12AM",paste0(seq(2,10,2),"AM"),"12PM",paste0(seq(2,10,2),"PM")))+ 
    scale_y_continuous("Median Proportion of trips",labels = percent)
print(p)
        

y = pr.out$x %>% as.data.frame()
temp8 = y[,1:2]
temp8 = cbind(km.out$cluster,temp8)
names(temp8)[1] = "cluster"

temp9 = jun_week1 %>% group_by(driver_id,day) %>% summarise(tot_fare = sum(tot_fare),tot_dist_mil=sum(dist_mil))
temp9 = cbind(km.out$cluster,temp9)
names(temp9)[1] = "cluster"

        
# tot_fare_dist_boxplot  
p1 = ggplot(temp9, aes(x = factor(cluster), y = tot_fare)) + 
     geom_boxplot() + stat_summary(fun.y=median, geom="smooth", aes(group=1),size=1,colour = "red")+
     theme_grey(base_size = 20)+
     theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
     scale_x_discrete("Cluster")+ 
     scale_y_continuous("Total fare per trip")
print(p1)
        

p2 = ggplot(temp9, aes(x = factor(cluster), y = tot_dist_mil)) + 
     geom_boxplot() + stat_summary(fun.y=median, geom="smooth", aes(group=1),size=1,colour = "red")+
     theme_grey(base_size = 20)+
     theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
     scale_x_discrete("Cluster")+ 
     scale_y_continuous("Total distance (mile)")
print(p2)
        

# tot_fare_dist_by_cluster 
p = ggplot(temp8, aes(PC1, PC2)) + aes(shape = factor(cluster)) + 
    geom_point(aes(colour = factor(cluster)), size = 2)+
    scale_color_manual(values=c("red", "blue","darkgreen","black"))+
    theme_grey(base_size = 20)+
    theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
    guides(colour = guide_legend(override.aes = list(size = 5)))+
    scale_x_continuous("PC1")+ 
    scale_y_continuous("PC2")
print(p)
        

# med_trips_by_slot_day_type
temp = jun_week1 %>%
       group_by(pickup_slot,day,day_type) %>% summarise(trips=n())
temp = temp %>% group_by(pickup_slot,day_type) %>% summarise(med= quantile(trips,0.5))
        
p = ggplot(temp, aes(pickup_slot, med,colour=day_type)) +  geom_line(stat="identity",size=1.2)+
    scale_color_manual(values=c("red", "blue"))+
    theme_grey(base_size = 20)+
    theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
    scale_x_continuous("Hour",breaks=seq(1,24,2),minor_breaks = seq(1,24,1),
                           labels = c("12AM",paste0(seq(2,10,2),"AM"),"12PM",paste0(seq(2,10,2),"PM")))+ 
    scale_y_continuous("Median trips",labels = comma)
print(p)

        
# tot_fare_dist_mil_by_slot
temp = jun_week1 %>%
       group_by(pickup_slot) %>% summarise(q1_tot_fare= quantile(tot_fare,0.25),
                                              med_tot_fare= quantile(tot_fare,0.5),
                                              q3_tot_fare= quantile(tot_fare,0.75),
                                              q1_dist_mil= quantile(dist_mil,0.25),
                                              med_dist_mil= quantile(dist_mil,0.5),
                                              q3_dist_mil= quantile(dist_mil,0.75))
p = ggplot(temp, aes(x = pickup_slot)) +
    geom_line(aes(y =med_tot_fare,color = "total fare ($)"),size=1.2)+
    geom_ribbon(aes(ymin=q1_tot_fare, ymax=q3_tot_fare,x=pickup_slot), alpha = 0.3)+
    geom_line(aes(y =med_dist_mil,color = "distance (mil)"),size=1.2)+
    geom_ribbon(aes(ymin=q1_dist_mil, ymax=q3_dist_mil,x=pickup_slot), alpha = 0.3)+
    scale_color_manual(values=c("total fare ($)"="red", "distance (mil)"="blue"))+
    theme_grey(base_size = 20)+
    theme(axis.text = element_text(colour = "black"),
                legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size = 20, face = "bold"),
                legend.key.width = unit(1.5, "line"),
                panel.grid.major = element_line(colour = "grey80", size = 0.25),
                panel.grid.minor = element_line(colour = "grey80", size = 0.25))+
    scale_x_continuous("",breaks=seq(1,24,2),minor_breaks = seq(1,24,1),
                             labels = c("12AM",paste0(seq(2,10,2),"AM"),"12PM",paste0(seq(2,10,2),"PM")))+ 
    scale_y_continuous("",labels = comma)
print(p)
        