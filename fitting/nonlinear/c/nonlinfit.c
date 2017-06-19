//Normal distribution fitting code using Levenberg-Marquard method
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define PI  3.14159265359

//Fitting function Normal distribution
//func(x, mu, sig) = Exp[-(x-mu)^2/(2*sig^2)]//(sig*Sqrt[2 * Pi])
float func(float x, float *a, int n) {
  float func;

  int i;
  float mu, sig;
  
  //a[0] = mu
  //a[1] = sig
  mu = a[0];
  sig = a[1];

  func = exp(-(x-mu)*(x-mu)/(2*sig*sig))/(sig*sqrt(2*PI));
  return func;
}

//Function to calculate Chi2
float chi2(float *x, float *y, float *sig, int n, float *a, int nprm) {
  float chi2;
  float dy;
  int i;

  chi2 = 0.0;
  for (i = 0; i < n; i++) {
    dy = y[i] - func(x[i], a, nprm);
    chi2 += dy * dy / (sig[i]*sig[i]);
  }

  return chi2;

}

void dyda(float x, float *a, int nsize, float *res) {
  int i, j;
  float tmp, mu, sig;

  for (i = 0; i < nsize; i++)
    res[i] = 0.0;

  mu  = a[0];
  sig = a[1];
  
  res[0] = (mu - x) * func(x, a, nsize);
  res[1] = -( 1 + (mu-x)*(mu-x)/(sig*sig)) * func(x, a, nsize) / sig;  
}

int main() {
  //Data array
  float *x, *y, *sig;
  //work variables
  int n, i, j, k, nprm;
  
  float da[2], a[2], anew[2], dchi2[2], beta[2], hess[2][2];
  float lambda, chi2cur, chi2new;
  int c1, c2, pivot[2], ok;
  int cnt, maxit;

  fscanf(stdin,"%d",&n);
  
  if (n < 2) {
    fprintf(stderr,"Bad number of records %d\n",n);
    return 1;
  } 

  a[0] = 195;
  a[1] = 45.0;
  nprm = 2;

  x = (float *) malloc(sizeof(float) * n);
  y = (float *) malloc(sizeof(float) * n);
  sig = (float *) malloc(sizeof(float) * n);

  
  for (i = 0; i < n; i++) {
    fscanf(stdin,"%f",&x[i]);
    fscanf(stdin,"%f",&y[i]);
    sig[i] = 1.0;
  }

  //Begin parametrization search
  //
  //calculate da, solve Hess x da = Beta and store result into Beta
  c1 = nprm;
  c2 = 1;
  sgesv_(&c1, &c2, hess, &c1, pivot, beta, &c1, &ok);
  //
  //
  //End parametrization search

  free(x);
  free(y);
  free(sig);

  return 0;

}
