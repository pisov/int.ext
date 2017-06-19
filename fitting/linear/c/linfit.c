#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
  float *x, *y, *sig;
  int n, i, j;

  fscanf(stdin,"%d",&n);
  
  if (n < 2) {
    fprintf(stderr,"Bad number of records %d\n",n);
    return 1;
  } 

  x = (float *) malloc(sizeof(int) * n);
  y = (float *) malloc(sizeof(int) * n);
  sig = (float *) malloc(sizeof(int) * n);

  for (i = 0; i < n; i++) {
    fscanf(stdin,"%f",&x[i]);
    fscanf(stdin,"%f",&y[i]);
  }

  free(x);
  free(y);
  free(sig);

  return 0;

}
