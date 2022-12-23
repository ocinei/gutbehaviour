// model1 for VAN_SPF data; memory factor introduced; see documentation for the mathematical form of the model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> t_max; // Time series length
  vector[t_max] y; // Observations, usually normalised velocity
  vector[t_max] I; // Vector representing stimulus; of the same size as the observations

  real theta1_m; // mean and variance of prior distributions for the states
  real theta2_m;
  cov_matrix[1] theta1_initial_noise;
  cov_matrix[1] theta2_initial_noise;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  /* parameters for the linear transformation of state*/
  real alpha;
  real beta;
  real gamma;
  real delta;
  real frequency;
  real Amplitude;

  vector[t_max] theta1; // theta_1 state, representing contribution by the freezing circuit
  vector[t_max] theta2; // theta_2 state, representing contribution by the flight circuit
  vector[t_max] theta3; // theta_3 state, representing contribution by the memory circuit
  real theta1_0; // theta1 initial state 0
  real theta2_0; // theta2 initial state 0
  real theta3_0; // theta3 initial state 0

  cov_matrix[1] sigma_noise; // variance of the observation noise
  cov_matrix[1] theta1_noise; // variance of the state noise for theta1
  cov_matrix[1] theta2_noise; // variance of the state noise for theta2
  cov_matrix[1] theta3_noise; // variance of the state noise for theta3
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  /* observation equation */
  for (t in 1:t_max) {
    y[t] ~ normal(theta1[t]+theta2[t], sqrt(sigma_noise[1,1]));
  }

  /* prior distribution for the state */
  theta1_0 ~ normal(theta1_m, sqrt(theta1_initial_noise[1,1]));
  theta2_0 ~ normal(theta2_m, sqrt(theta2_initial_noise[1,1]));

  /* State evolution equation */
  theta1[1] ~ normal(gamma*theta2_0+alpha*theta3_0, sqrt(theta1_noise[1,1]));
  theta2[1] ~ normal(delta*theta1_0+beta*theta3_0, sqrt(theta2_noise[1,1]));
  theta3[1] ~ normal(Amplitude*cos(frequency*1), sqrt(theta3_noise[1,1]));

  for (t in 2:t_max) {
    theta1[t] ~ normal(gamma*theta2[t-1]+alpha*theta3[t-1]*I[t-1], sqrt(theta1_noise[1,1]));
    theta2[t] ~ normal(delta*theta1[t-1]+beta*theta3[t-1]*I[t-1], sqrt(theta2_noise[1,1]));
    theta3[t] ~ normal(Amplitude*cos(frequency*t), sqrt(theta3_noise[1,1]));
  }
}
// generate quantities
generated quantities{
  vector[t_max] y_rep;

  for(t in 1:t_max){
    y_rep[t] = normal_rng(theta1[t] + theta2[t], sqrt(sigma_noise[1,1]));
  }
}
