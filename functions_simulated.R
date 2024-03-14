calculate_CD <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  #Pe1=PTP*P1+PFP*P0  
  options(digits=22)
  t=abs((P1*(Ce+PTP*(Cf-Cl)))/(P0*(Ce+PFP*Cf)))
  #print(-0.5*I-log(t)/I)
  Cost=Ca+P1*Cl+P1*(Ce+PTP*(Cf-Cl))*(1-pnorm(-0.5*I-log(t)/I))+P0*(Ce+PFP*Cf)*(1-pnorm(0.5*I-log(t)/I))
  return(Cost)
}

calculate_CE <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  #Pe1=PTP*P1+PFP*P0  
  Cost=Ce+P1*Cl+P1*(Cf-Cl)*PTP+P0*Cf*PFP#Ce+Pe1*Cf+(1-PTP)*P1*Cl  # 
  return(Cost)
}

calculate_CM <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  t=abs((P1*((Cf-Cl)))/(P0*(Cf)))
  #print(t)
  Cost=Ca+P1*Cl+P1*(Cf-Cl)*(1-pnorm(-0.5*I-log(t)/I))+(P0*Cf)*(1-pnorm(0.5*I-log(t)/I))
  #print(log(t)/I)
  return(Cost)
}

calculate_CD_assym <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  #Pe1=PTP*P1+PFP*P0  
  options(digits=22)
  Cl_alg=1.5*Cl
  t=abs((P1*(Ce+PTP*(Cf-Cl)+Cl-Cl_alg))/(P0*(Ce+PFP*Cf)))
  #print(-0.5*I-log(t)/I)
  Cost=Ca+P1*Cl_alg+P1*(Ce+PTP*(Cf-Cl)+Cl-Cl_alg)*(1-pnorm(-0.5*I-log(t)/I))+P0*(Ce+PFP*Cf)*(1-pnorm(0.5*I-log(t)/I))
  return(Cost)
}


calculate_CM_assym <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  
  #print(t)
  Cl_alg=1.5*Cl
  t=abs((P1*((Cf-Cl_alg)))/(P0*(Cf)))
  Cost=Ca+P1*Cl_alg+P1*(Cf-Cl_alg)*(1-pnorm(-0.5*I-log(t)/I))+(P0*Cf)*(1-pnorm(0.5*I-log(t)/I))
  #print(log(t)/I)
  return(Cost)
}

scaleFUN <- function(x) sprintf("%.2f", x)


get_optimal_decision <- function(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I){
  
  ####  
  CD=calculate_CD(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I)  
  CE=calculate_CE(P0,P1,Ca,Ce,Cf,Cl,PTP,PFP,I)  
  CM=calculate_CM(P0,P1,Ca,Ce,Cf,Cl,PTP,PTP,I) 
  
  
  
  #liability is low
  if((Cf/Cl) > P1)
  {
    dummy=which.min(c(CE,CD,CM))-2 #which.min(c(CE,CD,CM))-2
    
    if(dummy==(-1))
    {
      opt_dec="Human Expert"
    }
    else if(CM==CD)
    {
      if(PTP < (PFP+Ce*Cl/(Cf*(Cl-Cf))))
      {
        opt_dec="Automation"
      }
      else if(PTP <= (PFP*P0*Cf+Ce-Ca)/(P1*(Cl-Cf)))
      {
        opt_dec="Delegation"
      }
      else
      {
        opt_dec="Automation"
      }
      
    }
    
    
    else if(dummy==0)
    {
      opt_dec="Delegation"
    }
    else
    {
      opt_dec="Automation"
    }
    
    
  }  
  
  else
  {
    dummy=which.min(c(CE,CD,CM))-2 #which.min(c(CE,CD,CM))-2
    
    if(dummy==(-1))
    {
      opt_dec="Human Expert"
    }
    # else if(CM==CD)
    # {
    #   if(PTP < (PFP+Ce*Cl/(Cf*(Cl-Cf))))
    #   {
    #     opt_dec="Automation"
    #   }
    #   else if(PTP <= (PFP*P0*Cf+Ce-Ca)/(P1*(Cl-Cf)))
    #   {
    #     opt_dec="Delegation"
    #   }
    #   else
    #   {
    #     opt_dec="Automation"
    #   }
    #   
    # }
    
    
    else if(dummy==0)
    {
      opt_dec="Delegation"
    }
    else
    {
      opt_dec="Automation"
    }
    
    
  }  
  
  return(opt_dec)
}