## Try yourself



# Due to data restrictions we can not share the data
# dream_predictions is the matrix of predictions size patients X methods
# dream_labels are the actual labels and a vector of patients it takes values 0 or 1
# radiologist_dream is prediction vector for radiologist and it is a a vector of patients it takes values 0 or 1
# Define values Ca cost of algorithm
# Cl liability cost
# Ce cost of using human expert

df=create_dream_cost(dream_predictions,dream_labels,radiologist_dream,
                                Ca,Cl,Ce,Nsamples=10)



sp<-ggplot(data.frame(df), aes(x=AUC, y=Optimal_Cost, 
                               shape=Optimal_Strategy)) +
  xlab(TeX("Performance of Algorithm (AUC)"))+
  ylab(TeX("Optimal Cost"))+
  #scale_y_continuous(labels=scaleFUN,ylim=c(375,600))+
  scale_x_continuous(labels=scaleFUN,limits=c(0.48,0.9))+
  theme_classic()+
  ylim(340, 560)+
  scale_shape_manual(labels = c(TeX("Delegation"), 
                                TeX("Human Expert")),
                     values=c(1, 2))+geom_point()+
  theme(legend.position = c(0.2, 0.25))+
  labs(shape = NULL)
#ggtitle(paste0("The correlation between A1 and A2, rho=",rho))+

sp