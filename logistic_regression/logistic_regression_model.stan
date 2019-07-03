data {
    int<lower=0> N;
    int<lower=0> d;
    matrix[N,d] X;
    int y[N];
    matrix[d,d] Sigma0;
}

transformed data {
    vector[d] mu0;
    for (j in 1:d)
        mu0[j] = 0;
}

parameters {
    vector[d] beta;
}

transformed parameters {
    real Xbeta[N];
    for (i in 1:N)
        Xbeta[i] = dot_product(row(X, i), beta);
}

model {
    beta ~ multi_normal(mu0, Sigma0);
    y ~ bernoulli_logit(Xbeta);
}

