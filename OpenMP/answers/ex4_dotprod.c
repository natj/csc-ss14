#include <stdio.h>

#define NX 102400

int main(void)
{
    double vecA[NX], vecB[NX];
    double sum, psum;
    int i;

    /* Initialization of the vectors */
    for (i = 0; i < NX; i++) {
        vecA[i] = 1.0 / ((double) (NX - i));
        vecB[i] = vecA[i] * vecA[i];
    }

    sum = 0.0;
    /* Version with data race */
#pragma omp parallel for default(shared) private(i)
    for (i = 0; i < NX; i++) {
        sum += vecA[i] * vecB[i];
    }
    printf("Sum with data race:                              %18.16f\n",
           sum);

    sum = 0.0;
    /* Dot product using critical section = SERIAL CODE! */
#pragma omp parallel for default(shared) private(i)
    for (i = 0; i < NX; i++) {
#pragma omp critical(dummy)
        sum += vecA[i] * vecB[i];
    }
    printf("Sum using critical section:                      %18.16f\n",
           sum);

    sum = 0.0;
    /* Dot product using private partial sums and critical section */
#pragma omp parallel default(shared) private(i, psum)
    {
        psum = 0.0;
#pragma omp for
        for (i = 0; i < NX; i++) {
            psum += vecA[i] * vecB[i];
        }
#pragma omp critical(par_sum)
        sum += psum;
    }
    printf("Sum using private variable and critical section: %18.16f\n",
           sum);

    sum = 0.0;
    /* Dot product using reduction */
#pragma omp parallel for default(shared) private(i) reduction(+:sum)
    for (i = 0; i < NX; i++) {
        sum += vecA[i] * vecB[i];
    }
    printf("Reduction sum:                                   %18.16f\n",
           sum);

    return 0;
}
