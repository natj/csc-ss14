#include<stdio.h>
#include<stdlib.h>
#include<mpi.h>

void init_comm(int ntask, int *id, MPI_Comm *comm2d, int *coord)
{
    int dims[2];
    int period[2] = {1, 1};

    if (ntask < 16)
       dims[0] = 2;
    else if (ntask >= 16 && ntask < 64)
       dims[0] = 4;    
    else if (ntask >= 64 && ntask < 256 )
       dims[0] = 8;    
    else
       dims[0] = 16;

    dims[1] = ntask / dims[0];

    if (dims[0] * dims[1] != ntask) {
      printf("Sorry no go %i x %i != %i\n", dims[0], dims[1], ntask);
      MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, period, 1, comm2d);
    MPI_Comm_rank(*comm2d, id);
    MPI_Cart_coords(*comm2d, *id, 2, coord);
}

int main(int argc, char *argv[])
{
    int myid, ntasks;
    MPI_Comm comm2d;
    int coord[2];

    int i, n;
    int *test_send, *test_recv;


    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);
    init_comm(ntasks, &myid, &comm2d, coord);

    // case neighbor_allgather
    n = 2;
    test_send = (int *) malloc(n * sizeof(int));
    test_recv = (int *) malloc(n *4 * sizeof(int));
    for (i=0; i < n; i++)
      test_send[i] = myid * 10 + i;

    for (i=0; i < 4*n; i++)
      test_recv[i] = 0;

    MPI_Neighbor_allgather(test_send, 2, MPI_INT, test_recv, n, 
                            MPI_INT, comm2d);

    for (i=0; i < ntasks; i++) {
       if (myid == i) {
          printf("%i = (%i, %i) \n", myid, coord[0], coord[1]);
          printf("Data: %i %i \n", test_send[0], test_send[1]);
          printf("Recv: ");
          for (n=0; n < 8; n++)
             printf("%i ", test_recv[n]);
          printf("\n");
       }
       MPI_Barrier(comm2d);
    }
    free(test_send);
    free(test_recv);
 
    // case neighbor_alltoall
    n = 4;
    test_send = (int *) malloc(n * sizeof(int));
    test_recv = (int *) malloc(n * sizeof(int));
    for (i=0; i < n; i++) {
      test_send[i] = myid * 10 + i;
      test_recv[i] = 0;
    }

    MPI_Neighbor_alltoall(test_send, n/4, MPI_INT,
                          test_recv, n/4, MPI_INT, comm2d);

    for (i=0; i < ntasks; i++) {
       if (myid == i) {
          printf("%i = (%i, %i) \n", myid, coord[0], coord[1]);
          printf("Data: ");
          for (n=0; n < 4; n++)
             printf("%i ", test_send[n]);
          printf("\n");
          printf("Recv: ");
          for (n=0; n < 4; n++)
             printf("%i ", test_recv[n]);
          printf("\n");
       }
       MPI_Barrier(comm2d);
    }
    free(test_send);
    free(test_recv);

    MPI_Finalize();
    return 0;
}
