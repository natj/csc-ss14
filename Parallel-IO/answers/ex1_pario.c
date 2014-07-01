#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <mpi.h>

#define DATASIZE   100
#define WRITER_ID    0

void single_writer(int, int, int*);
void subset_of_writers(int, int, int*);
void single_writer_mpi(int, int, int*);
void subset_mpi(int, int, int*);


int main(int argc, char *argv[]) 
{
    int my_id, ntasks, i;
    int localvector[DATASIZE];

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_id);
    
    if (ntasks != 8) {
        if (my_id == 0) {
            printf("This program assumes 8 tasks, you have %i!\n"
                   "Let\'s see what happens...\n", ntasks);
        }
    }

    for (i = 0; i < DATASIZE; i++)
        localvector[i] = my_id;
    
    /* ex 1a */
    single_writer(my_id, ntasks, localvector);
    /* ex 1b */
    subset_of_writers(my_id, ntasks, localvector);
    /* ex 1c */
    single_writer_mpi(my_id, ntasks, localvector);
    /* ex 1d */
    subset_mpi(my_id, ntasks, localvector);
    
    MPI_Finalize();
    return 0;
}

/* Exercise 1a, single writer, POSIX IO */
void single_writer(int my_id, int ntasks, int* localvector) 
{
    FILE *fp;
    int *fullvector;
    
    fullvector = (int *) malloc(ntasks * DATASIZE * sizeof(int));

    MPI_Gather(localvector, DATASIZE, MPI_INT, fullvector, DATASIZE,
               MPI_INT, WRITER_ID, MPI_COMM_WORLD);

    if (my_id == WRITER_ID) {
        if ((fp = fopen("ex1a.dat", "wb")) == NULL) {
            fprintf(stderr, "Error: %d (%s)\n", errno, strerror(errno));
            MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
        } else {
            fwrite(fullvector, sizeof(int), ntasks * DATASIZE, fp);
            fclose(fp);
            printf("Wrote %i elements to file ex1a.dat\n", DATASIZE);
        }
    }

    free(fullvector);
}

/* Exercise 1b, subset of writers, POSIX IO */
void subset_of_writers(int my_id, int ntasks, int* localvector)
{
    int color, iocomm_size, iocomm_id;
    int *fullvector;
    char fname[9];
    FILE *fp;
    MPI_Comm iocomm;

    if (my_id <= 3)
        color = 1;
    else
        color = 2;

    MPI_Comm_split(MPI_COMM_WORLD, color, my_id, &iocomm);
    MPI_Comm_size(iocomm, &iocomm_size);
    MPI_Comm_rank(iocomm, &iocomm_id);

    fullvector = (int *) malloc(iocomm_size * DATASIZE * sizeof(int));
    
    MPI_Gather(localvector, DATASIZE, MPI_INT, fullvector, DATASIZE,
               MPI_INT, WRITER_ID, iocomm);
    
    if (iocomm_id == WRITER_ID) {
        sprintf(fname, "ex1b%1i.dat", color);
        if ((fp = fopen(fname, "wb")) == NULL) {
            fprintf(stderr, "Error: %d (%s)\n", errno, strerror(errno));
            MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
        } else {
            fwrite(fullvector, sizeof(int), iocomm_size * DATASIZE, fp);
            fclose(fp);
            printf("Task %i wrote %i elements to file %s\n", my_id,
                   iocomm_size * DATASIZE, fname);
        }
    }
    
    free(fullvector);
}

/* Exercise 1c, single file, MPIIO */
void single_writer_mpi(int my_id, int ntasks, int* localvector)
{
    MPI_File fh;
    MPI_Offset displ;
    int intsize;

    MPI_File_open(MPI_COMM_WORLD, "ex1c.dat", MPI_MODE_CREATE|MPI_MODE_WRONLY,
                  MPI_INFO_NULL, &fh);
    MPI_Type_size(MPI_INT, &intsize);
    displ = my_id * DATASIZE * intsize;
    MPI_File_write_at(fh, displ, localvector, DATASIZE, MPI_INT, 
                      MPI_STATUS_IGNORE);
    MPI_File_close(&fh);

    if (my_id == WRITER_ID) {
        printf("Wrote %i elements to file ex1c.dat\n", ntasks * DATASIZE);
    }
}

/* Exercise 1d, two files, MPIIO */
void subset_mpi(int my_id, int ntasks, int* localvector)
{
    int color, iocomm_size, iocomm_id, intsize;
    char fname[32];
    MPI_File fh;
    MPI_Offset displ;
    MPI_Comm iocomm;

    if (my_id <= 3)
        color = 1;
    else
        color = 2;

    MPI_Comm_split(MPI_COMM_WORLD, color, my_id, &iocomm);
    MPI_Comm_size(iocomm, &iocomm_size);
    MPI_Comm_rank(iocomm, &iocomm_id);

    sprintf(fname, "ex1d%1i.dat", color);
    
    MPI_File_open(iocomm, fname, MPI_MODE_CREATE|MPI_MODE_WRONLY, 
                  MPI_INFO_NULL, &fh);
    MPI_Type_size(MPI_INT, &intsize);
    displ = iocomm_id * DATASIZE * intsize;
    MPI_File_write_at(fh, displ, localvector, DATASIZE, MPI_INT,
                      MPI_STATUS_IGNORE);
    MPI_File_close(&fh);
    
    if (iocomm_id == WRITER_ID) {
        printf("Task %i wrote %i elements to file %s\n", my_id, 
               iocomm_size * DATASIZE, fname);
    }
}
