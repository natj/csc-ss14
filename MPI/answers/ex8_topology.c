#include<stdio.h>
#include<stdlib.h>
#include<mpi.h>


int main(int argc, char *argv[])
{
    int i, myid, ntasks;
    int size = 100;
    int *message;
    int *receiveBuffer;
    MPI_Status statuses[2];
    MPI_Request requests[2];

    MPI_Comm cart_comm;
    int dims[1];
    int periods[1] = { 0 };

    int source, destination;
    int count;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);

    // Create cartesian topology
    dims[0] = ntasks;
    MPI_Cart_create(MPI_COMM_WORLD, 1, dims, periods, 1, &cart_comm);
    MPI_Comm_rank(cart_comm, &myid);

    /* Allocate message */
    message = malloc(sizeof(int) * size);
    receiveBuffer = malloc(sizeof(int) * size);
    /* Initialize message */
    for (i = 0; i < size; i++)
        message[i] = myid;

    MPI_Cart_shift(cart_comm, 0, 1, &source, &destination);

    /* Send and receive messages as defined in exercise */
    MPI_Irecv(receiveBuffer, size, MPI_INT, source, MPI_ANY_TAG,
              MPI_COMM_WORLD, &requests[0]);
    MPI_Isend(message, size, MPI_INT, destination, myid + 1,
              MPI_COMM_WORLD, &requests[1]);
    printf("Sender: %d. Sent elements: %d. Tag: %d. Receiver: %d\n", myid,
           size, myid + 1, destination);

    /* Blocking wait for messages */
    MPI_Waitall(2, requests, statuses);

    MPI_Get_count(&statuses[0], MPI_INT, &count);
    printf("Receiver: %d. received elements: %d. Tag %d. Sender: %d.\n",
           myid, count, statuses[0].MPI_TAG, statuses[0].MPI_SOURCE);

    free(message);
    free(receiveBuffer);
    MPI_Finalize();
    return 0;
}
