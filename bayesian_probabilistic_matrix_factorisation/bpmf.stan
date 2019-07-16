data {
    int<lower=0> N;           // number of observations
    int<lower=0> D;           // latent dimension
    int<lower=0> N_u;         // number of users
    int<lower=0> N_i;         // number of items
    int<lower=0> uid[N];      // training user id
    int<lower=0> iid[N];      // training item id
    real R[N];                // rating
    int<lower=0> M;           // number of test observations
    int<lower=0> uidTest[M];  // test user id
    int<lower=0> iidTest[M];  // test item id
    vector[M] T;                // test ratings
    real<lower=0> meanRating;
    real<lower=0> tau;
    real<lower=0> mu0;
    real<lower=0> a0;
    real<lower=0> b0;
}

transformed data {
    real<lower=0> sigma;
    sigma = 1 / tau;
}

parameters {
    vector[D] U[N_u];
    vector[D] V[N_i];
    vector[D] mu_U;
    vector[D] mu_V;
    vector<lower=0>[D] Lambda_U;
    vector<lower=0>[D] Lambda_V;
}

transformed parameters {
    real UTV[N];
    vector<lower=0>[D] sig_U;
    vector<lower=0>[D] sig_V;
    for (i in 1:N)
        UTV[i] = dot_product(U[uid[i]], V[iid[i]]);
    sig_U = 1 ./ Lambda_U;
    sig_V = 1 ./ Lambda_V;
}

model {
    R ~ normal(UTV, sigma);
    for (i in 1:N_u)
        U[i] ~ normal(mu_U, sig_U);
    for (i in 1:N_i)
        V[i] ~ normal(mu_V, sig_V);
    mu_U ~ normal(mu0, Lambda_U);
    mu_V ~ normal(mu0, Lambda_V);
    Lambda_U ~ gamma(a0, b0);
    Lambda_V ~ gamma(a0, b0);
}

generated quantities{
real rmse;
vector[M] pred;
    for (i in 1:M)
        pred[i] = dot_product(U[uidTest[i]], V[iidTest[i]]) + meanRating;
    rmse = sqrt(dot_self(T-pred)/M);
}