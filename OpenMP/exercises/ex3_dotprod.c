#include <stdio.h>

#define NX 102400

int main(void)
{
    double vecA[NX], vecB[NX];
    double sum, psum;
    int i;

    /* Initialization of the vectors */
    for (i = 0; i < NX; i++) {
        vecA[i] = 1.0/((double) (NX-i));
        vecB[i] = vecA[i] * vecA[i];
    }

    sum = 0.0;
    for (i = 0; i < NX; i++) {
        sum += vecA[i] * vecB[i];
    }
    printf("Sum: %18.16f\n", sum);

    return 0;
}
