create_dream_cost <-function(dream_predictions,dream_labels,radiologist_dream,
                             Ca,Cl,Ce,Nsamples=100)
{
  
  

  dream_cost_list=list()
  
  methods=colnames(dream_predictions)
  nmethods=length(methods)
  
  
  
  ### First define list elements
  for(i in 1:nmethods)
  {
    dream_opt=as.data.frame(matrix(0,1,14))
    
    colnames(dream_opt)=c('Optimal_Cost','Optimal_Strategy',
                          'AUC','D_pct',
                          'TPR_D','FPR_D','CD',
                          'A_pct',
                          'TPR_A','FPR_A','CA',
                          'TPR_H','FPR_H','CH')
    dream_cost_list[[paste0('A',i)]]=dream_opt
  }
 
  
  for(j in 1:Nsamples)
  {
    idx_pos_sample=which(dream_labels==1)
    idx_neg_sample=which(dream_labels==0)
    idx_pos_sample=sample(idx_pos_sample,floor(P1*length(idx_neg)/P0)+1)
    dream_labels_sample=dream_labels[c(idx_neg_sample,idx_pos_sample)]
    dream_predictions_sample=dream_predictions[c(idx_neg_sample,idx_pos_sample),]
    radiologist_dream_sample=radiologist_dream[c(idx_neg_sample,idx_pos_sample)]
    idx=sample(1:length(c(idx_neg_sample,idx_pos_sample)))
    dream_labels_sample=dream_labels_sample[idx]
    radiologist_dream_sample=radiologist_dream_sample[idx]
    dream_predictions_sample=dream_predictions_sample[idx,]
    for(i in 1:nmethods)
    {
      
      dummy=calculate_dream_cost_all(dream_predictions_sample[,i],
                                     dream_labels_sample,
                                     radiologist_dream_sample,Ca,Ce,Cf,Cl)
      
      dream_cost_list[[paste0('A',i)]][j,]=dummy
      
    }
    print(j)
    
  }
  
## Summarize the above data using some statistics
  
  nmethods=dim(dream_predictions)[2]
  pval_th=0.05
  
  dream_cost_matrix=as.data.frame(matrix(0,nmethods,14))
  
  colnames(dream_cost_matrix)=c('Optimal_Cost','Optimal_Strategy',
                                'AUC','D_pct',
                                'TPR_D','FPR_D','CD',
                                'A_pct',
                                'TPR_A','FPR_A','CA',
                                'TPR_H','FPR_H','CH')
  
  for(i in 1:nmethods)
  {
    dream_cost_matrix[i,-2]=colMeans(dream_cost_list[[paste0('A',i)]][,-2])
    ### First compare delegation to Human only
    
    DH_t=t.test(dream_cost_list[[paste0('A',i)]]$CD,dream_cost_list[[paste0('A',i)]]$CH)
    AD_t=t.test(dream_cost_list[[paste0('A',i)]]$CD,dream_cost_list[[paste0('A',i)]]$CA)
    AH_t=t.test(dream_cost_list[[paste0('A',i)]]$CH,dream_cost_list[[paste0('A',i)]]$CA)
    
    m_CA=mean(dream_cost_list[[paste0('A',i)]]$CA)
    m_CD=mean(dream_cost_list[[paste0('A',i)]]$CD)
    m_CH=mean(dream_cost_list[[paste0('A',i)]]$CH)
    
    
    
    dream_cost_matrix[i,'Optimal_Strategy']='H'
    
    dream_cost_matrix[i,"Optimal_Cost"]=m_CH
    if((m_CD<m_CH)&&(DH_t$p.value <pval_th))
    {
      dream_cost_matrix[i,'Optimal_Strategy']='D'
      dream_cost_matrix[i,"Optimal_Cost"]=m_CD
      if((m_CA<m_CD)&&(AH_t$p.value <pval_th))
      {
        dream_cost_matrix[i,'Optimal_Strategy']='A'
        dream_cost_matrix[i,"Optimal_Cost"]=m_CA
      }
    }else if((m_CA<m_CH)&&(AH_t$p.value <pval_th)){
      dream_cost_matrix[i,'Optimal_Strategy']='A'
      dream_cost_matrix[i,"Optimal_Cost"]=m_CA
    }
  }
  

  
return(df)  
} 


###########################################
###########################################

calculate_dream_cost_all <- function(alg_pred,gold_labels,
                                     human_pred,c_a,c_e,c_f,c_l)
{
  
  predicted_neg_human=which(human_pred == 0)
  predicted_pos_human=which(human_pred == 1)
  P=which(gold_labels==1)
  N=which(gold_labels==0)
  
  
  
  
  
  dream_opt=as.data.frame(matrix(0,1,14))
  
  colnames(dream_opt)=c('Optimal_Cost','Optimal_Strategy',
                        'AUC','D_pct',
                        'TPR_D','FPR_D','CD',
                        'A_pct',
                        'TPR_A','FPR_A','CA',
                        'TPR_H','FPR_H','CH')
  
  h_cost=calculate_dream_CH(c_a,c_e,c_f,c_l, 
                            predicted_neg_human,
                            predicted_pos_human,
                            0,
                            1,P,N)
  
  
  
  dream_opt[1,'CH']=h_cost[1]
  dream_opt[1,'TPR_H']=h_cost[2]
  dream_opt[1,'FPR_H']=h_cost[3]
  
  
  ## Get  now CD and CM costs
  
  kk=1
  df=data.frame(threshold=numeric())
  
  thresholds_all=unique(alg_pred)
  thresholds_all=c(0,sample(thresholds_all,
                            min(500,length(thresholds_all))))
  
  
  for(j in thresholds_all)
  {
    predicted_neg_ML=which(alg_pred < j)
    predicted_pos_ML=which(alg_pred >= j)
    
    ### First delegation
    
    d_cost=calculate_dream_CD(c_a,c_e,c_f,c_l, 
                              predicted_neg_human,
                              predicted_pos_human,
                              predicted_neg_ML,
                              predicted_pos_ML,
                              P,N)
    
    a_cost=calculate_dream_CM(c_a,c_e,c_f,c_l, 
                              predicted_neg_human,
                              predicted_pos_human,
                              predicted_neg_ML,
                              predicted_pos_ML,P,N)
    
    df[kk,'threshold']=j
    df[kk,'CD']=d_cost[1]
    df[kk,'CH']=h_cost[1]
    df[kk,'CA']=a_cost[1]
    df[kk,'TPR_D']=d_cost[2]
    df[kk,'FPR_D']=d_cost[3]
    df[kk,'TPR_A']=a_cost[2]
    df[kk,'FPR_A']=a_cost[3]
    df[kk,'D_pct']=length(predicted_pos_ML)/length(alg_pred)
    
    kk=kk+1
  }
  
  
  
  idx_d=which.min(df$CD)
  idx_a=which.min(df$CA)
  
  
  #View(df)
  dream_opt[1,'D_pct']=df[idx_d,'D_pct']
  dream_opt[1,'CD']=df$CD[idx_d]
  dream_opt[1,'TPR_D']=df$TPR_D[idx_d]
  dream_opt[1,'FPR_D']=df$FPR_D[idx_d]
  
  dream_opt[1,'A_pct']=df[idx_a,'D_pct']
  dream_opt[1,'CA']=df$CA[idx_a]
  dream_opt[1,'TPR_A']=df$TPR_A[idx_a]
  dream_opt[1,'FPR_A']=df$FPR_A[idx_a]
  
  
  ## Now get
  
  dream_opt[1,'Optimal_Cost']=min(dream_opt$CH,dream_opt$CD,
                                  dream_opt$CA)
  idx_min=which.min(c(dream_opt$CH,
                      dream_opt$CD,
                      dream_opt$CA))
  
  dream_opt[1,'Optimal_Strategy']=c('H','D','A')[idx_min]
  
  dream_opt[1,'AUC']=pROC::auc(gold_labels,alg_pred,quiet=TRUE)
  
  
  return(dream_opt)
  
}

########### Calculate individual cost
##############

calculate_dream_CH <- function(C_a,C_e,C_f,C_l,
                               predicted_neg_human,
                               predicted_pos_human,
                               predicted_neg_ML,
                               predicted_pos_ML,P,N){
   
  
  Npatient=length(P)+length(N)
  
  ## Human cost 
  
  PE_TP=length(intersect(P,predicted_pos_human))/Npatient
  PE_FN=length(intersect(P,predicted_neg_human))/Npatient
  PE_FP=length(intersect(N,predicted_pos_human))/Npatient
  
  #####
  CE=C_e+PE_TP*C_f+PE_FN*C_l+PE_FP*C_f
  
  ### Get TPR FPR
  
  TPR=length(intersect(P,predicted_pos_human))/length(P)
  FPR=length(intersect(N,predicted_pos_human))/length(N)
 
  ## ML cost 
  return(c(CE,TPR,FPR))
}

calculate_dream_CD <- function(C_a,C_e,C_f,C_l,
                               predicted_neg_human,
                               predicted_pos_human,
                               predicted_neg_ML,
                               predicted_pos_ML,
                               P,N){
  
  
  
  Npatient=length(P)+length(N)
  

  
  
  
  ## Delegation cost 
  PE_FN_M=length(intersect(predicted_neg_ML,P))
  PE_FN_ME=length(intersect(intersect(predicted_pos_ML,predicted_neg_human),P))
  PE_TP_D=length(intersect(intersect(predicted_pos_ML,predicted_pos_human),P))
  PE_FP_D=length(intersect(intersect(predicted_pos_ML,predicted_pos_human),N))
  PE_TN_D=length(intersect(intersect(predicted_pos_ML,predicted_neg_human),N))
  
  
  
  
  
  CD=C_a+PE_FN_M*C_l/Npatient+PE_FN_ME*(C_l+C_e)/Npatient+
    (C_f+C_e)*PE_TP_D/Npatient+(C_f+C_e)*PE_FP_D/Npatient+
    PE_TN_D*C_e/Npatient
  
  TPR=length(intersect(intersect(predicted_pos_ML,predicted_pos_human),P))/length(P)
  FPR=length(intersect(intersect(predicted_pos_ML,predicted_pos_human),N))/length(N)
  
  return(c(CD,TPR,FPR))
}

calculate_dream_CM <- function(Ca,Ce,Cf,Cl,
                               predicted_neg_human,
                               predicted_pos_human,
                               predicted_neg_ML,
                               predicted_pos_ML,P,N){
  
  
  
  Npatient=length(P)+length(N)
  
  
  
 
  ## Automation cost 
  
  PE_TP=length(intersect(P,predicted_pos_ML))/Npatient
  PE_FN=length(intersect(P,predicted_neg_ML))/Npatient
  PE_FP=length(intersect(N,predicted_pos_ML))/Npatient
  
  TPR=length(intersect(P,predicted_pos_ML))/length(P)
  FPR=length(intersect(N,predicted_pos_ML))/length(N)
  
  
  #####
  CM=Ca+PE_TP*Cf+PE_FN*Cl+PE_FP*Cf
  
  return(c(CM,TPR,FPR))
}