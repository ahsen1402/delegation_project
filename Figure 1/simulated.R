
### Load packages as needed
library(plotly)
library(ggplot2)
library(latex2exp)
library(pracma) 





### The  False positive rates of human expert. 

PFP=0.095491


####  define cancer prevalance

P1=126.9/10^5
P0=1-P1


### Different costs associated 

# algorithm cost per use
## Change Ca and Cl for figures A B and C

Ca=5.96 #5.96 low cost, 42.59 high cost

## liability cost
Cl=756156 # low Cl 756156, high cl 1157866

## Cost of False positives 
Cf=1918.18
## Expert Cost

Ce=167.04


## Define the search space I is a representative of algorithm performance
## alpha defines human TPR
n=50

alphas_all=seq(0, 1, length.out=n)
Is_all=logspace(-0.5,0.5,n)


df=data.frame(human_perf=numeric(),algorithm_perf=numeric(),
              Strategy=character(),
              CD=numeric(),
              CM=numeric(),
              CE=numeric(),
              stringsAsFactors = FALSE)

kk=1
for( i in 1:length(alphas_all))
{
  for( j in 1:length(Is_all))
  {
    alpha=alphas_all[i]
    I=Is_all[j]
    
    df[kk,1]=alpha
    df[kk,2]=pnorm(I/sqrt(2))
    
    df[kk,3]=get_optimal_decision(P0,P1,Ca,Ce,Cf,Cl,alpha,PFP,I)
    
    
    CD=calculate_CD(P0,P1,Ca,Ce,Cf,Cl,alpha,PFP,I)  
    CE=calculate_CE(P0,P1,Ca,Ce,Cf,Cl,alpha,PFP,I)  
    CM=calculate_CM(P0,P1,Ca,Ce,Cf,Cl,alpha,PFP,I) 
    
    df[kk,'CD']=CD
    df[kk,'CE']=CE
    df[kk,'CM']=CM
    
    #dummy=which.min(c(CE,CD,CM))-2 #which.min(c(CE,CD,CM))-2
    
    # if(dummy==(-1))
    # {
    # df[kk,3]="Human Expert"
    # }
    # 
    # if(dummy==0)
    # {
    #   if(CM==CD)
    #   {
    #     df[kk,3]="Delegation"
    #   }
    #   else
    #   {
    #   df[kk,3]="Delegation"
    #   }
    # }
    # if(dummy==1)
    # {
    #   df[kk,3]="Automation"
    # }
    kk=kk+1
  }
}



sp<-ggplot(df, aes(x=human_perf, y=algorithm_perf, shape=Strategy)) +
  xlab("Human Performance (True Positive Rate)")+ylab("Algorithmic Performance (AUC)")+
  scale_y_continuous(labels=scaleFUN)+
  geom_point()+
  theme_classic()+
  scale_shape_manual(values=c(16, 3, 17))+
theme(legend.position = c(0.2, 0.8))+
  labs(shape = NULL)
 

sp
