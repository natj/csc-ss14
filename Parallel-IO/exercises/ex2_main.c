/* 2D heat equation with MPI I/O checkpointing

   Authors: Jussi Enkovaara, Martti Louhivuori
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
#include <mpi.h>
#include <unistd.h>

#include "ex2_heat.h"

int main(int argc, char **argv)
{
    double a = 0.5;             //!< Diffusion constant
    field current, previous;    //!< Current and previous temperature fields

    double dt;                  //!< Time step
    int nsteps = 500;           //!< Number of time steps
	int start = 0;              //!< Start from this iteration

    int rows = 200;             //!< Field dimensions with default values
    int cols = 200;

    char input_file[64];        //!< Name of the optional input file

    int image_interval = 10;    //!< Image output interval
	int restart_interval = 200; //!< Checkpoint output interval

    parallel_data parallelization;

    int iter;

    /* Following combinations of command line arguments are possible:
       No arguments:    use default field dimensions and number of time steps
       One argument:    read initial field from a given file
       Two arguments:   initial field from file and number of time steps
       Three arguments: field dimensions (rows,cols) and number of time steps

       Note: If a checkpoint file for restarts exists, command line arguments
             are ignored.
     */

    MPI_Init(&argc, &argv);

	// Check if checkpoint exists
	if (!access(CHECKPOINT, F_OK)) {
		read_restart(&current, &parallelization, &start);
		if (parallelization.rank == 0)
			printf("Restarting from an earlier checkpoint saved at iteration %d.\n", start);
		initialize_field_metadata(&previous, current.nx_full, 
				current.ny_full, &parallelization);
		allocate_field(&previous);
		copy_field(&current, &previous);
		switch (argc) {
		case 1:
			// use defaults
			break;
		case 2:
			// Number of time steps
			nsteps = atoi(argv[1]);
			break;
		default:
			printf("Only the number of time steps allowed as a command-line input when using restart\n");
			return -1;
		}
	} else {
		switch (argc) {
		case 1:
			// use defaults
			parallel_initialize(&parallelization, rows, cols);
			initialize_field_metadata(&current, rows, cols, &parallelization);
			initialize_field_metadata(&previous, rows, cols, &parallelization);
			initialize(&current, &previous, &parallelization);
			break;
		case 2:
			// Initial field from a file
			strncpy(input_file, argv[1], 64);
			read_input(&current, &previous, input_file, &parallelization);
			break;
		case 3:
			// Initial field from a file
			strncpy(input_file, argv[1], 64);
			read_input(&current, &previous, input_file, &parallelization);
			// Number of time steps
			nsteps = atoi(argv[2]);
			break;
		case 4:
			// Field dimensions
			rows = atoi(argv[1]);
			cols = atoi(argv[2]);
			parallel_initialize(&parallelization, cols, rows);
			initialize_field_metadata(&current, cols, rows, &parallelization);
			initialize_field_metadata(&previous, cols, rows, &parallelization);
			initialize(&current, &previous, &parallelization);
			// Number of time steps
			nsteps = atoi(argv[3]);
			break;
		default:
			printf("Unsupported number of command line arguments\n");
			return -1;
		}
	}

    // Output the initial field
    output(&current, start, &parallelization);

    // Largest stable time step
    dt = current.dx2 * current.dy2 /
        (2.0 * a * (current.dx2 + current.dy2));

    // Time evolve
    for (iter = start; iter < start+nsteps; iter++) {
        exchange(&previous, &parallelization);
        evolve(&current, &previous, a, dt);
        // output every 10 iteration
        if ((iter+1) % image_interval == 0) 
            output(&current, iter+1, &parallelization);
        // write a checkpoint now and then for easy restarting
		if ((iter+1) % restart_interval == 0)
			write_restart(&current, &parallelization, iter+1);
        // make current field to be previous for next iteration step
        swap_fields(&current, &previous);
    }

    finalize(&current, &previous, &parallelization);
    MPI_Finalize();

    return 0;
}
