data{
  int N; //number of row of datasheet
  int I; //number of studies included
  int study[N]; //study ID
  int status[N]; //dummy variable for patient status (1 for pts w/ dis,0 for pts w/o dis)
  int np[N]; //number of patients with positive test result in each status
  int nn[N]; //number of patients with negative test result in each status
  real<lower=0,upper=1> one_snsp; //fix one of sn/sp
}

parameters{
  vector[I] theta; //theta for each study
  vector[I] alpha; //alpha for each study
  real beta; //common scale parameter
  real theta_g; //global mean of theta
  real alpha_g; //global mean of alpha
  real<lower=0> s_theta; //square of between study variance for theta
  real<lower=0> s_alpha; //square of between study variance for alpha
}

transformed parameters{
  vector<lower=0,upper=1>[I] pi_1; //prob of having positive result among pts w/ dis
  vector<lower=0,upper=1>[I] pi_0; //prob of having positive result among pts w/o dis
  vector<lower=0,upper=1>[I] sn; //sensitivity of each study(=pi_1)
  vector<lower=0,upper=1>[I] sp; //specificitiy of each study(=-pi_0)
  vector<lower=0,upper=1>[I+1] other_snsp;
  pi_1 = inv_logit((theta + 0.5*alpha)*exp(-0.5*beta));
  pi_0 = inv_logit((theta - 0.5*alpha)*exp(0.5*beta));
  sn = pi_1;
  sp = 1-pi_0;
  for(i in 1:I){
    other_snsp[i] = inv_logit((0.25*(alpha[i]^2) - theta[i]^2)/log(one_snsp/(1-one_snsp)));
  }
  other_snsp[I+1] = inv_logit((0.25*(alpha_g^2) - theta_g^2)/log(one_snsp/(1-one_snsp)));
}

model{
  for(n in 1:N){
    if(status[n]==1){
      np[n] ~ binomial(np[n]+nn[n],pi_1[study[n]]); //Level I model for true positive
    }else{
      np[n] ~ binomial(np[n]+nn[n],pi_0[study[n]]); // Level I modek for false positive
    }
  }
  theta ~ normal(theta_g,s_theta); //Level II
  alpha ~ normal(alpha_g,s_alpha); //Level II
}
