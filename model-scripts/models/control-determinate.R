control <- function(t,y,parms,f1,f2,f3) {
  
  # Ve, Le, In, Fl are the four entries in y (ODE)
  Ve = y[1]
  Le = y[2]
  In = y[3]
  Fl = y[4]
  
  ## upper limit of meristem division rate
  # m1: maximum rate of primary meristem division
  # m2: maximum rate of inflorescence meristem division
  m1=parms[1];m2=parms[2];alpha=parms[3];gamma=parms[4];
  
  ## control functions calculated at different time points
  # probability of meristem division producing primary meristems
  u <- f1(t)
  # rate of primary meristem division
  beta1 <- f2(t)
  # rate of inflorescence meristem division
  beta2 <- f3(t)
  
  # apply positivity constraints, penalize if violated
  #SPE: change power from 2 to 1.25, so small errors are reduced less but penalty is still differentiable. 
  ut = max(u,0) ; bad = abs(u-ut)^1.25; 
  beta1t = max(beta1,0); bad = bad + abs(beta1-beta1t)^1.25;
  beta2t = max(beta2,0); bad = bad + abs(beta2-beta2t)^1.25;
  
  # apply upper bound to u constraint, penalize if violated
  #SPE: change power from 2 to 1.25, so small errors are reduced less but penalty is still differentiable. 
  ut2 = min(ut,1) ; bad = bad + abs(ut-ut2)^1.25; 
  
  if (m1*Ve + m2*In <= alpha*Le) {
 # if (beta1t*Ve + beta2t*In <= alpha*Le) {
    beta1t2 = min(beta1t, m1); bad = bad + abs(beta1t-beta1t2)^1.25; beta1t = beta1t2;
    beta2t2 = min(beta2t, m2); bad = bad + abs(beta2t-beta2t2)^1.25; beta2t = beta2t2;
  } else if (m1*Ve + m2*In > alpha*Le) {
  #} else if (beta1t*Ve + beta2t*In > alpha*Le) {
    # apply constraint beta1*Ve+beta2*In=Le, penalize if violated  
    Letot = (1/alpha)*(beta1t*Ve + beta2t*In); 
    if (Letot >= alpha*Le) {
      # bad = bad + (Letot-Le)^2; 
      beta1t2 = beta1t*(Le/Letot); 
      beta2t2 = beta2t*(Le/Letot); 
      
      if(beta1t2>m1) {
        # added lines checking if In==0
        if (In == 0 ) {
          beta1t2 = m1
        } else if ( In !=0 ) {
          beta1t2 = m1
          beta2t2 = (alpha*Le - m1*Ve)/In
        }
      }
      
      if (beta2t2>m2){
        beta2t2 = m2
        beta1t2 = (alpha*Le - m2*In)/Ve
      }
      
    } else {
      beta1t2 = beta1t;
      beta2t2 = beta2t;
    }
    
    # penalize for being above maximum
    bad = bad + abs(beta1t-beta1t2)^1.25; beta1t = beta1t2;
    bad = bad + abs(beta2t-beta2t2)^1.25; beta2t = beta2t2;
    
  }  
  
  ## cumulative penalty increases in proportion to squared constraint violation 
  derivs[1] = gamma*(beta1t * ut2 * Ve) - (beta1t) * ((1 - ut2) * Ve)
  derivs[2] = (beta1t) * ( Ve )
  derivs[3] = (beta1t) * ((1 - ut2) * Ve) - (beta2t) * ( In )
  derivs[4] = (beta2t) * In
  derivs[5] = bad
  # derivs[6] = log(Fl); # SPE: season's end is Uniform(2,5). 
  # Uniform probability of season end over second half
   derivs[6] = ifelse(t>=seasonEnd-seasonEnd/2,log(Fl),0); # SPE: season's end is Uniform(2,5). 
  # derivs[6] = dnorm(t,mean=mu,sd=sigma)*log(Fl)
  
  return(list(derivs));
}
