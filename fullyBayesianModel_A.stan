data {
  int<lower=0,upper=1> useClimateVars; // 1 = include climate vars, 0 = ignore
  
  int<lower=1> n; // number of datapoints
  int<lower=1> num_countries;
  int<lower=1> num_provinces;
  int<lower=1> num_years;
  
  array[n] int<lower=1, upper=num_provinces> province_idx;
  array[n] int<lower=1, upper=num_years>     year_idx;
  array[num_provinces] int<lower=1, upper=num_countries> country_of_province;

  vector[n] growth;
  vector[n] t;
  vector[n] t_sq;
  vector[n] tx5d;
  vector[n] tx5d_t;
  int<lower=0> k;
  matrix[n, k] remVarsMat;
  
  // variables for testing
  int<lower=0> n_test; // 0 means no test data
  array[n_test] int<lower=1, upper=num_provinces> province_idx_test;
  int<lower=0, upper=num_years> year_idx_test; // 0 means no test data
  vector[n_test] growth_test;
  vector[n_test] t_test;
  vector[n_test] t_sq_test;
  vector[n_test] tx5d_test;
  vector[n_test] tx5d_t_test;
  matrix[n_test, k] remVarsMat_test;
}
parameters {
  // fixed effects
  real      beta_0;
  real      beta_t;
  real      beta_t_sq;
  real      beta_tx5d;
  real      beta_tx5d_t;
  vector[k] beta_remVars;

  // Spatial  
  vector[num_countries] z_country; 
  vector[num_provinces] z_province;
  real<lower=0> sigma_country;
  real<lower=0> sigma_province;

  // Temporal: AR(1) year 
  vector[num_years] z_time; 
  real<lower=0> sigma_time;
  real<lower=-1, upper=1> phi;

  // Observation sd
  real<lower=0> sigma; 
}
transformed parameters {
  vector[n] climateLinpred;
  vector[num_countries] alpha_country; 
  vector[num_provinces] alpha_province;
  vector[num_years] alpha_time;
  vector[n] linpred;

  climateLinpred = beta_t * t
                 + beta_t_sq * t_sq
                 + beta_tx5d * tx5d
                 + beta_tx5d_t * tx5d_t
                 + remVarsMat * beta_remVars;
           
  // Spatial      
  alpha_country = sigma_country * z_country;
  for (p in 1:num_provinces) {
    alpha_province[p] = alpha_country[country_of_province[p]] + sigma_province * z_province[p];
  }
      
  // Temporal    
  alpha_time[1] = sigma_time * z_time[1];
  for (yr in 2:num_years) {
    alpha_time[yr] = phi * alpha_time[yr-1] + sigma_time * z_time[yr];
  }

  linpred = beta_0
          + alpha_province[province_idx]
          + alpha_time[year_idx]
          + useClimateVars * climateLinpred;
      
}
model {
  // priors
  phi ~ normal(0, 0.5);
  sigma_country ~ normal(0, 2);
  sigma_province ~ normal(0, 2);
  sigma_time ~ normal(0, 2);
  sigma ~ normal(0, 2);
  
  // fixed effects priors
  beta_0 ~ normal(0, 10);
  beta_t ~ normal(0, 10);
  beta_t_sq ~ normal(0, 10);
  beta_tx5d ~ normal(0, 10);
  beta_tx5d_t ~ normal(0, 10);
  beta_remVars ~ normal(0, 10);

  // spatial & temporal priors
  z_country ~ normal(0,1);
  z_province ~ normal(0,1);
  z_time ~ normal(0,1);
  
  // likelihood
  growth ~ normal(linpred, sigma);  
}
generated quantities {
  real ME_tx5d_t5;
  real ME_tx5d_t25;
  vector[n_test] climateLinpred_test;
  vector[n_test] pred_test;
  vector[n_test] err_test;
  real rmse_test;
  
  // marginal effect of tx5d
  ME_tx5d_t5 = beta_tx5d + beta_tx5d_t*5;
  ME_tx5d_t25 = beta_tx5d + beta_tx5d_t*25;
  
  // test predictions and loss
  if (n_test > 0) {
    climateLinpred_test = beta_t * t_test
                        + beta_t_sq * t_sq_test
                        + beta_tx5d * tx5d_test
                        + beta_tx5d_t * tx5d_t_test
                        + remVarsMat_test * beta_remVars;
    pred_test = beta_0
            + alpha_province[province_idx_test]
            + phi * alpha_time[year_idx_test-1] // predict from previous year's value
            + useClimateVars * climateLinpred_test;
    err_test = growth_test - pred_test;
    rmse_test = sqrt( mean( err_test .* err_test ) );
  } else {
    // Must assign something; NaN clearly signals "no test set"
    climateLinpred_test = rep_vector(not_a_number(), n_test);
    pred_test = rep_vector(not_a_number(), n_test);
    err_test = rep_vector(not_a_number(), n_test);
    rmse_test = not_a_number();
  }
}
