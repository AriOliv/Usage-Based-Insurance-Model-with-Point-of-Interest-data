#package and load####
library("ggplot2")
library("ggmap")
library('plotROC') #ROC 
library('cowplot') # combine pircures
library("lubridate")
library("dplyr")
library('plyr')
library('stringr')

load('data.rda') 
#mydat_raw is the raw data and mydat is the data with standardized IOV predictors
load('trip_sample.rda')# a sample set of trip
load('map.rda')# map for ploting trace
load('poimap.rda')# map for ploting poi
load('poi_sample.rda') 

#model#####
glm0 <- glm(y~., data = mydat[,c(1:11)], family = binomial(link = 'logit'))#model without POI
glm0_aic <- step(glm0,trace=0)
glm0_aic$formula
summary(glm0_aic)

glm_p <- glm(y~., data = mydat, family = binomial(link = 'logit'))#model with POI
glm_p_aic <- step(glm_p,trace=0)
glm_p_aic$formula
summary(glm_p_aic)


#figure plot####
#break~y
hist_mileage <- ggplot(mydat_raw, aes(cumkilo.x/1e4))+
  geom_histogram(aes(y = (..count..)), bins = 15,
                 fill = I('gray65'), col = I('black'))+
  scale_x_continuous(limits = c(0,3), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  labs(x='Mileage (in 10,000 km)', y='Frequency')+
  theme(axis.title = element_text(size=13),
        axis.text=element_text(size = 13)
  )

df <- data.frame(numNeg=mydat_raw$numNeg, y=mydat_raw$y)
df$numNeg <- cut(df$numNeg, breaks = c(0,10,20,30,Inf))
y_brake <- df %>% group_by(numNeg = str_extract(numNeg, "\\d+")) %>% 
  dplyr::summarise(x = mean(y)) %>% 
  ggplot(., aes(x = numNeg, y = x)) + 
  geom_col(fill = I('gray60'),position = position_dodge(width = 1)) + 
  ylim(0, 1)+
  labs(x='Frequency of harsh braking \n (per 100 km)', y='Proportion of accident')+
  scale_x_discrete(labels=c('0'='0~10','10'='10~20','20'='20~30','30'='>30'))+
  theme(axis.text=element_text(size = 13
  ),
  axis.title = element_text(size=13))

plot_grid(hist_mileage,y_brake, scale = 0.9,align = 'v',
          label_fontface = 'plain', label_size = 13,
          labels=c('(a)','(b)'), label_x = 0.5, label_y = 0.085)

ggsave('hist_mileage&y_brake.jpg', width = 7.5, height = 3.8,units = 'in',
       ggplot2::last_plot(),dpi = 500)


#trace
trace <- ggmap(map)+
  geom_point(data = trip_sample, aes(Longitude, Latitude, alpha = 0.5), size=0.5)+
  theme(legend.position="none",
        axis.text = element_text(size = 8),axis.line=element_blank(),
        axis.ticks = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),plot.margin = unit(c(1,1,1,1), "lines"))+
  labs(x='', y='')

#time~mileage
mileage <- ggplot(trip_sample,aes(t,KilometreMileage))+theme_gray()+
  geom_smooth(color=I('black'),se=F, size=0.8)+labs(y='Mileage (km)',x='Time')+
  theme(axis.title = element_text(size=9),
        axis.text = element_text(size = 9))
#time~velocity
velocity <- ggplot(trip_sample,aes(t,VehicleSpeed))+theme_gray()+
  geom_smooth(span=0.05, method = 'loess', color=I('black'),se=F, size=0.8)+
  labs(y='Velocity (kph)',x='Time')+
  theme(axis.title = element_text(size=9),axis.text = element_text(size = 9))

#time~Acceleration
acceleration <- ggplot(trip_sample,aes(t,LongAcceleration))+theme_gray()+
  geom_line(alpha=0.5, size=0.2)+ylim(-3.5,3.5)+
  labs(y=expression(Acceleration~(m/s^2)),x='Time')+
  geom_hline(yintercept = c(3,-3), colour = 'red')+
  theme(axis.title = element_text(size=9),axis.text = element_text(size = 9))

plot_grid(trace, mileage, velocity, acceleration,
          labels=c('(a)','(b)','(c)','(d)'),label_size = 9.5,
          label_fontface = 'plain',
          scale = c(1.1,rep(0.9,3)),
          label_x = 0.5, label_y = 0.08)
ggsave('trace.jpg',ggplot2::last_plot(),
       width=5.4, height=5, units = 'in',  dpi = 500)

#ROC
ROCdata <- data.frame(Y=mydat$y, 
                      Without.POI=predict(glm0_aic,newdata = mydat),
                      With.POI=predict(glm_p_aic, newdata = mydat)) %>%
  melt_roc(., 'Y', c('Without.POI','With.POI'))
names(ROCdata) <- c('D', 'M', 'Model')
ROCdata$Model <- revalue(ROCdata$Model, 
                         replace = c('Without.POI'='Without POI',
                                     'With.POI'='With POI'))
ggroc <- ggplot(ROCdata, 
                aes(d=D, m=M, color=Model))+geom_roc(labels = F)
AUC <- data.frame(round(calc_auc(ggroc)['AUC'],3))
ggroc <- ggroc+style_roc(major.breaks = c(0, 0.25, 0.5, 0.75, 1),
                         minor.breaks = c(seq(0, 0.25, by = 0.05), 
                                          seq(0.75, 1, by = 0.05)))+
  annotate("text", x = c(.45,.55), y = c(.85, .35), size = 5,
           label = paste0('AUC: ', AUC$AUC), 
           color = c('#F8766D', '#00BFC4'))+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size=16),
        legend.text = element_text(size=13),
        legend.title = element_text(size=18))
ggsave('ROC.jpg', ggroc, width = 6, height = 4.5,units = 'in', dpi = 500)

#coefficient
coef <- glm_p_aic %>% summary() %>% coef() 
coef <- coef[-(1:5),] #remove driving behavior
coef <- data.frame(name=rownames(coef), 
                   lower=coef[,1]+qnorm(0.025)*coef[,2],
                   value=coef[,1],
                   upper= coef[,1]+qnorm(0.975)*coef[,2])
coef$name <- c('Chinese food.Bus station', 'Fast food.Hospital', 'Bank', 'Gate',
               'Supermarket', 'Temple', 'Police office', 'Sea food', 'Korean cuisine',
               'Museum.Training', 'City square', 'Night club', 'Government',
               ' Real estate company', 'Mosque.Tourism consultation', 'E-business company',
               'Trade company.Gift shop. \n Press.Casino', 'Youth hotel', 'Used-car trade.Metro station.Beef. \n Antique shop.Hainan food.Firehouse', 'III A hospital', 'Franch food', 'Special hospital') #translation from Chinese to English

coef$name <- factor(coef$name, levels = coef$name[order(abs(coef$value))])

coef_plot <- ggplot(data=coef,aes(y=value,x=name)) + geom_col(fill = I('gray60'))+
  labs(y='Coefficient estimation',x='POI label')+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size =  12)) +coord_flip()
ggsave('coef.jpg',coef_plot, width=5, height=5, units = 'in',  dpi = 500)


#result group
predY <- data.frame(
  pred = glm_p_aic$fitted.values %>% 
    cut(breaks=seq(0,1,,5)) %>%
    mapvalues(from = levels(.),to = c('0~0.25','0.25~0.5','0.5~0.75','0.75~1')),
  actual = mydat$y)

pred_group <- predY %>% group_by(pred) %>% 
  dplyr::summarise(x = mean(actual)) %>% 
  ggplot(., aes(x = pred, y = x)) + geom_col(fill = I('gray60')) + ylim(0, 1)+
  labs(x='Score of risk', y='Proportion of accident')+
  theme(axis.title = element_text(size=12.5))
pred_group
ggsave('pred_group.jpg',pred_group, width=5, height=3.5, units = 'in',  dpi = 500)


predY_q <- data.frame(
  pred = glm_p_aic$fitted.values %>% 
    cut(breaks=quantile(., seq(0,1,,5))) %>%
    mapvalues(from = levels(.),to = seq_along(levels(.))),
  actual = mydat$y) %>% na.omit()

pred_group_q <- predY_q %>% group_by(pred) %>% 
  dplyr::summarise(x = mean(actual)) %>% 
  ggplot(., aes(x = pred, y = x)) + geom_col(fill = I('gray60')) + ylim(0, 1)+
  labs(x='Level of risk', y='Proportion of accident')+
  theme(axis.title = element_text(size=12.5))
ggsave('pred_group_q.jpg',pred_group_q, width=5, height=3.5, units = 'in',  dpi = 500)

#poimap
ggmap(poimap)+
  theme(
    axis.text = element_text(size = 6),legend.text=element_text(size=10.5),
    axis.line=element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank(),
    axis.text.y = element_blank(),plot.margin = unit(c(0.2,-2,-1,-2), "lines"))+
  geom_point(data=poi_sample,aes(lon,lat, shape=Label, colour=Label),size=3, alpha= 0.8)+
  labs(x="",y="")+scale_color_brewer(palette="Dark2")+
  scale_shape_manual(values = c(15,8,16:18,12))
ggsave('poimap.jpg', ggplot2::last_plot(), width = 5, height = 3.5, units = 'in', dpi = 300)

