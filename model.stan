data {
  int N;
  int K_2;
  int K_3;

  int N_2;
  int<lower=0> iss_2[N_2];
  int<lower=0> cand_2[N_2];
  int<lower=0> Y_2[N_2];

  int N_3;
  int<lower=0> iss_3[N_3];
  int<lower=0> cand_3[N_3];
  int<lower=0> Y_3[N_3];
  
  
  int<lower=0> J; 
  
  int<lower=0> N_resp_2;
  int resp_2[N_resp_2];
  int ch_2[N_resp_2];
  int iss_cat_2_1[N_resp_2];
  int iss_cat_2_2[N_resp_2];
  int iss_ch_2_1[N_resp_2];
  int iss_ch_2_2[N_resp_2];
  
  int<lower=0> N_resp_3;
  int resp_3[N_resp_3];
  int ch_3[N_resp_3];
  int iss_cat_3_1[N_resp_3];
  int iss_cat_3_2[N_resp_3];
  int iss_ch_3_1[N_resp_3];
  int iss_ch_3_2[N_resp_3];
  
  int<lower=0> N_resp_mix;
  int resp_mix[N_resp_mix];
  int ch_mix[N_resp_mix];
  int iss_cat_mix_1[N_resp_mix];
  int iss_cat_mix_2[N_resp_mix];
  int iss_ch_mix_1[N_resp_mix];
  int iss_ch_mix_2[N_resp_mix];



}

parameters {
  vector[N] theta_cand;
  vector[2] theta_iss_2[K_2];
  vector[3] theta_iss_3[K_3];

  vector<upper=0>[K_2] beta_iss_2;
  vector<upper=0>[K_3] beta_iss_3;
  
  vector<lower=0>[J] beta_resp;


}

model{


  {
    vector[2] prob;
    for(ii in 1:N_2){
        
        prob[1] = beta_iss_2[iss_2[ii]] .* square(theta_iss_2[iss_2[ii],1] - theta_cand[cand_2[ii]]);
        prob[2] = beta_iss_2[iss_2[ii]] .* square(theta_iss_2[iss_2[ii],2] - theta_cand[cand_2[ii]]);

        Y_2[ii] ~ categorical_logit(prob);
    }
  }
  {
    vector[3] prob;
    for(ii in 1:N_3){
        
        prob[1] = beta_iss_3[iss_3[ii]] .* square(theta_iss_3[iss_3[ii],1] - theta_cand[cand_3[ii]]);
        prob[2] = beta_iss_3[iss_3[ii]] .* square(theta_iss_3[iss_3[ii],2] - theta_cand[cand_3[ii]]);
        prob[3] = beta_iss_3[iss_3[ii]] .* square(theta_iss_3[iss_3[ii],3] - theta_cand[cand_3[ii]]);

        Y_3[ii] ~ categorical_logit(prob);
    }
  }



  beta_iss_2 ~ normal(0, 1);
  beta_iss_3 ~ normal(0, 1);
  // theta_cand[1] ~ normal(1, .1);
  // theta_cand[2] ~ normal(-1, .1);
  theta_cand ~ normal(0, 1);
  // theta_iss_2[1,1] ~ normal(-1, .1);
  // theta_iss_2[1,2] ~ normal(1, .1);

  theta_iss_2[,1] ~ normal(-0.5, 0.5);
  theta_iss_2[,2] ~ normal(0.5, 0.5);

  theta_iss_3[,1] ~ normal(-0.5, 0.5);
  theta_iss_3[,2] ~ normal(0, 0.5);
  theta_iss_3[,3] ~ normal(0.5, 0.5);
  // for(ii in 1:3){
  //     theta_iss_3[,ii] ~ normal(0, 1);
  // }
  
  
  beta_resp ~ gamma(1, 1);
  
  for(ii in 1:N_resp_2){
    ch_2[ii] ~ bernoulli_logit(beta_resp[resp_2[ii]] .* (theta_iss_2[iss_cat_2_1[ii], iss_ch_2_1[ii]] - 
                                                theta_iss_2[iss_cat_2_2[ii], iss_ch_2_2[ii]]));
  }
  

  for(ii in 1:N_resp_3){
    ch_3[ii] ~ bernoulli_logit(beta_resp[resp_3[ii]] .* (theta_iss_3[ iss_cat_3_1[ii], iss_ch_3_1[ii]] - 
                                                  theta_iss_3[iss_cat_3_2[ii], iss_ch_3_2[ii]])); 
  }
  
  
  for(ii in 1:N_resp_mix){
    ch_mix[ii] ~ bernoulli_logit(beta_resp[resp_mix[ii]] .* (theta_iss_3[iss_cat_mix_1[ii], iss_ch_mix_1[ii]] -
                                                  theta_iss_2[iss_cat_mix_2[ii], iss_ch_mix_2[ii]])); 
  }
}
