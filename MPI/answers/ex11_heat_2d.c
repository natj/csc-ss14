/* 2D heat equation

   Authors: Jussi Enkovaara, ...
   Copyright (C) 2014  CSC - IT Center for Science Ltd.

   Licensed under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Code is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Copy of the GNU General Public License can be onbtained from
   see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <mpi.h>

#include "heat_mpi.h"
#include "pngwriter.h"

// Utility routine for allocating a two dimensional array
double **malloc_2d(int nx, int ny)
{
    double **array;
    int i;

    array = (double **) malloc(nx * sizeof(double *));
    array[0] = (double *) malloc(nx * ny * sizeof(double));

    for (i = 1; i < nx; i++) {
        array[i] = array[0] + i * ny;
    }

    return array;
}


// Utility routine for deallocating a two dimensional array
void free_2d(double **array)
{
    free(array[0]);
    free(array);
}

// Initialize the metadata. Note that the nx is the size of the first
// dimension and ny the second.
void initialize_field_metadata(field * temperature, int nx, int ny,
                               parallel_data * parallel)
{
    int nx_local, ny_local;

    nx_local = nx / ((int) sqrt(parallel->size));
    ny_local = ny / ((int) sqrt(parallel->size));

    temperature->dx = DX;
    temperature->dy = DY;
    temperature->dx2 = DX * DX;
    temperature->dy2 = DY * DY;
    temperature->nx = nx_local;
    temperature->ny = ny_local;
    temperature->nx_full = nx;
    temperature->ny_full = ny;
}

// Copy data on temperature1 into temperature2
void copy_field(field * temperature1, field * temperature2)
{
    assert(temperature1->nx = temperature2->nx);
    assert(temperature1->ny = temperature2->ny);
    memcpy(temperature2->data[0], temperature1->data[0],
           (temperature1->nx + 2) * (temperature1->ny +
                                     2) * sizeof(double));
}

// Swap the data of fields temperature1 and temperature2
void swap_fields(field * temperature1, field * temperature2)
{
    double **tmp;
    tmp = temperature1->data;
    temperature1->data = temperature2->data;
    temperature2->data = tmp;

}

void parallel_initialize(parallel_data * parallel, int nx, int ny)
{
    int nx_local;
    int ny_local;

    int world_size;
    int dims[2];
    int periods[2] = { 0, 0 };

    MPI_Comm_size(MPI_COMM_WORLD, &world_size);
    dims[0] = (int) sqrt(world_size);
    dims[1] = dims[0];
    if (dims[0] * dims[1] != world_size) {
        printf
            ("Cannot make square MPI grid, please use number of CPUs which is power of two %d x %d != %d\n",
             dims[0], dims[1], world_size);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    nx_local = nx / dims[0];
    ny_local = ny / dims[0];

    if (nx_local * dims[0] != nx) {
        printf
            ("Cannot divide grid evenly to processors in x-direction %d x %d != %d\n",
             nx_local, dims[0], nx);
        MPI_Abort(MPI_COMM_WORLD, -2);
    }

    if (ny_local * dims[1] != ny) {
        printf
            ("Cannot divide grid evenly to processors in y-direction %d x %d != %d\n",
             ny_local, dims[0], ny);
        MPI_Abort(MPI_COMM_WORLD, -2);
    }
    // Create cartesian communicator
    MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, 1, &parallel->comm);
    MPI_Cart_shift(parallel->comm, 0, 1, &parallel->nup, &parallel->ndown);
    MPI_Cart_shift(parallel->comm, 1, 1, &parallel->nleft,
                   &parallel->nright);

    MPI_Comm_size(parallel->comm, &parallel->size);
    MPI_Comm_rank(parallel->comm, &parallel->rank);

    // Create datatypes for halo exchange
    MPI_Type_vector(nx_local + 2, 1, ny_local + 2, MPI_DOUBLE,
                    &parallel->columntype);
    MPI_Type_contiguous(ny_local + 2, MPI_DOUBLE, &parallel->rowtype);
    MPI_Type_commit(&parallel->columntype);
    MPI_Type_commit(&parallel->rowtype);

    // Create datatype for subblock needed in I/O
    int sizes[2];
    int subsizes[2] = { nx_local, ny_local };
    int offsets[2] = { 0, 0 };

    // Rank 0 uses datatype for receiving data into full array
    if (parallel->rank == 0) {
        sizes[0] = nx;
        sizes[1] = ny;
    }
    // Other ranks use datatype for sending the inner part of array
    else {
        sizes[0] = nx_local + 2;
        sizes[1] = ny_local + 2;
    }

    MPI_Type_create_subarray(2, sizes, subsizes, offsets, MPI_ORDER_C,
                             MPI_DOUBLE, &parallel->subarraytype);

    MPI_Type_commit(&parallel->subarraytype);

}

void initialize(field * temperature1, field * temperature2,
                parallel_data * parallel)
{
    int i, j;

    int dims[2], coords[2], periods[2];

    // Allocate also ghost layers
    temperature1->data =
        malloc_2d(temperature1->nx + 2, temperature1->ny + 2);
    temperature2->data =
        malloc_2d(temperature2->nx + 2, temperature2->ny + 2);

    // Initialize to zero
    memset(temperature1->data[0], 0.0,
           (temperature1->nx + 2) * (temperature1->ny + 2)
           * sizeof(double));

    MPI_Cart_get(parallel->comm, 2, dims, periods, coords);

    // Left boundary
    if (coords[1] == 0)
        for (i = 0; i < temperature1->nx + 2; i++)
            temperature1->data[i][0] = 30.0;

    // Upper boundary
    if (coords[0] == 0)
        for (j = 0; j < temperature1->ny + 2; j++)
            temperature1->data[0][j] = 15.0;

    // Right boundary
    if (coords[1] == dims[1] - 1)
        for (i = 0; i < temperature1->nx + 2; i++)
            temperature1->data[i][temperature1->ny + 1] = -10.0;

    // Lower boundary
    if (coords[0] == dims[0] - 1)
        for (j = 0; j < temperature1->ny + 2; j++)
            temperature1->data[temperature1->nx + 1][j] = -25.0;

    copy_field(temperature1, temperature2);

}

void evolve(field * curr, field * prev, double a, double dt)
{
    int i, j;

    // Determine the temperature field at next time step
    // As we have fixed boundary conditions, the outermost gridpoints
    // are not updated
    // *INDENT-OFF*
    for (i = 1; i < curr->nx + 1; i++)
        for (j = 1; j < curr->ny + 1; j++) {
            curr->data[i][j] = prev->data[i][j] + a * dt *
                ((      prev->data[i + 1][j] -
                  2.0 * prev->data[  i  ][j] +
                        prev->data[i - 1][j]) / prev->dx2 +
                 (      prev->data[i][j + 1] -
                  2.0 * prev->data[i][  j  ] +
                        prev->data[i][j - 1]) / prev->dy2);
        }
    // *INDENT-ON*

}

void exchange(field * temperature, parallel_data * parallel)
{

    // Send to the up, receive from down
    MPI_Sendrecv(temperature->data[1], 1, parallel->rowtype,
                 parallel->nup, 11,
                 temperature->data[temperature->nx + 1], 1,
                 parallel->rowtype, parallel->ndown, 11, parallel->comm,
                 MPI_STATUS_IGNORE);

    // Send to the down, receive from up
    MPI_Sendrecv(temperature->data[temperature->nx], 1,
                 parallel->rowtype, parallel->ndown, 12,
                 temperature->data[0], 1, parallel->rowtype,
                 parallel->nup, 12, parallel->comm, MPI_STATUS_IGNORE);

    // Send to the left, receive from right
    MPI_Sendrecv(&temperature->data[0][1], 1, parallel->columntype,
                 parallel->nleft, 13,
                 &temperature->data[0][temperature->ny + 1], 1,
                 parallel->columntype, parallel->nright, 13,
                 parallel->comm, MPI_STATUS_IGNORE);

    // Send to the right, receive from left
    MPI_Sendrecv(&temperature->data[0][temperature->ny], 1,
                 parallel->columntype,
                 parallel->nright, 14, &temperature->data[0][0], 1,
                 parallel->columntype,
                 parallel->nleft, 14, parallel->comm, MPI_STATUS_IGNORE);

}

void finalize(field * temperature1, field * temperature2,
              parallel_data * parallel)
{
    free_2d(temperature1->data);
    free_2d(temperature2->data);

    MPI_Type_free(&parallel->rowtype);
    MPI_Type_free(&parallel->columntype);
    MPI_Type_free(&parallel->subarraytype);
}

void output(field * temperature, int iter, parallel_data * parallel)
{
    char filename[64];

    // The actual write routine takes only the actual data
    // (without ghost layers) so we need array for that
    int height, width;
    double **full_data;

    int coords[2];
    int ix, jy;

    int i, p;

    height = temperature->nx_full;
    width = temperature->ny_full;

    if (parallel->rank == 0) {
        // Copy the inner data
        full_data = malloc_2d(height, width);
        for (i = 0; i < temperature->nx; i++)
            memcpy(full_data[i], &temperature->data[i + 1][1],
                   temperature->ny * sizeof(double));

        // Receive data
        for (p = 1; p < parallel->size; p++) {
            MPI_Cart_coords(parallel->comm, p, 2, coords);
            ix = coords[0] * temperature->nx;
            jy = coords[1] * temperature->ny;
            MPI_Recv(&full_data[ix][jy], 1, parallel->subarraytype, p, 22,
                     parallel->comm, MPI_STATUS_IGNORE);
        }
    } else {
        // Send data
        MPI_Ssend(&temperature->data[1][1], 1, parallel->subarraytype, 0,
                  22, parallel->comm);
    }

    if (parallel->rank == 0) {
        sprintf(filename, "%s_%04d.png", "heat", iter);
        save_png(full_data[0], height, width, filename, 'c');

        free_2d(full_data);
    }
}

void read_input(field * temperature1, field * temperature2, char *filename,
                parallel_data * parallel)
{
    FILE *fp;
    int nx, ny, i, j;

    double **full_data;

    int coords[2];
    int ix, jy, p;

    fp = fopen(filename, "r");
    // Read the header
    fscanf(fp, "# %d %d \n", &nx, &ny);

    parallel_initialize(parallel, nx, ny);
    initialize_field_metadata(temperature1, nx, ny, parallel);
    initialize_field_metadata(temperature2, nx, ny, parallel);

    // Allocate arrays (including ghost layers)
    temperature1->data =
        malloc_2d(temperature1->nx + 2, temperature1->ny + 2);
    temperature2->data =
        malloc_2d(temperature2->nx + 2, temperature2->ny + 2);

    if (parallel->rank == 0) {
        // Full array
        full_data = malloc_2d(nx, ny);

        // Read the actual data
        for (i = 0; i < nx; i++) {
            for (j = 0; j < ny; j++) {
                fscanf(fp, "%lf", &full_data[i][j]);
            }
        }
        // Copy to own local array
        for (i = 0; i < temperature1->nx; i++)
            memcpy(&temperature1->data[i + 1][1], full_data[i],
                   temperature1->ny * sizeof(double));
        // Send to other processes
        for (p = 1; p < parallel->size; p++) {
            MPI_Cart_coords(parallel->comm, p, 2, coords);
            ix = coords[0] * temperature1->nx;
            jy = coords[1] * temperature1->ny;
            MPI_Send(&full_data[ix][jy], 1, parallel->subarraytype, p, 44,
                     parallel->comm);
        }
    } else
        // Receive data
        MPI_Recv(&temperature1->data[1][1], 1, parallel->subarraytype, 0,
                 44, parallel->comm, MPI_STATUS_IGNORE);

    // Set the boundary values
    for (i = 0; i < temperature1->nx + 1; i++) {
        temperature1->data[i][0] = temperature1->data[i][1];
        temperature1->data[i][temperature1->ny + 1] =
            temperature1->data[i][temperature1->ny];
    }
    for (j = 0; j < temperature1->ny + 2; j++) {
        temperature1->data[0][j] = temperature1->data[1][j];
        temperature1->data[temperature1->nx + 1][j] =
            temperature1->data[temperature1->nx][j];
    }

    copy_field(temperature1, temperature2);

    if (parallel->rank == 0)
        free_2d(full_data);

    fclose(fp);
}
