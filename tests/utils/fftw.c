/* Generate some test cases using fftw3  */
#include <stdlib.h>
#include <stdio.h>
#include <fftw3.h>

void dump_vector(int n, double* vec) {
    for(int i = 0; i < n; i++)
        printf("%20.15f ", vec[i]);
    printf("\n");
}

void dct(int flag, int n) {
    double* in  = malloc( n * sizeof(double));
    double* out = malloc( n * sizeof(double));
    //
    fftw_plan plan = fftw_plan_r2r_1d(n, in, out, flag, FFTW_ESTIMATE);
    for( int k = 0; k < n; k++) {
        // Init input vector
        for( int i = 0; i < n; i++)
            in[i] = 0;
        in[k] = 1;
        // Perform DFT
        fftw_execute(plan);
        // Print results
        dump_vector(n, in );
        dump_vector(n, out);
        printf("\n");
    }
    //
    free(in);
    free(out);
    fftw_destroy_plan(plan);
}

int main(void)
{
    printf("DCT II (the DCT)\n");
    dct( FFTW_REDFT10, 2);
    dct( FFTW_REDFT10, 4);
    
    printf("DCT III (Inverse DCT)\n");
    dct( FFTW_REDFT01, 2);
    dct( FFTW_REDFT01, 4);
    
    return 0;    
}
