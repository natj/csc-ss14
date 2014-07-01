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

#include "heat.h"


int main(int argc, char **argv)
{
    double a = 0.5;             //!< Diffusion constant
    field current, previous;    //!< Current and previous temperature fields

    double dt;                  //!< Time step
    int nsteps = 500;           //!< Number of time steps

    int rows = 200;             //!< Field dimensions with default values
    int cols = 200;

    char input_file[64];        //!< Name of the optional input file

    int image_interval = 100;   //!< Image output interval
    int iter;

    int error;
    /* Following combinations of command line arguments are possible:
       No arguments:    use default field dimensions and number of time steps
       One argument:    read initial field from a given file
       Two arguments:   initial field from file and number of time steps
       Three arguments: field dimensions (rows,cols) and number of time steps
     */

    error = 0;

#pragma omp parallel private(iter)
    {
        switch (argc) {
        case 1:
            // use defaults
#pragma omp single
            {
                initialize_field_metadata(&current, rows, cols);
                initialize_field_metadata(&previous, rows, cols);
            }
            initialize(&current, &previous);
            break;
        case 2:
            // Initial field from a file
#pragma omp single
            {
                strncpy(input_file, argv[1], 64);
                read_input(&current, &previous, input_file);
            }
            break;
        case 3:
#pragma omp single
            {
                // Initial field from a file
                strncpy(input_file, argv[1], 64);
                read_input(&current, &previous, input_file);
                // Number of time steps
                nsteps = atoi(argv[2]);
            }
            break;
        case 4:
#pragma omp single
            {
                // Field dimensions
                rows = atoi(argv[1]);
                cols = atoi(argv[2]);
                initialize_field_metadata(&current, rows, cols);
                initialize_field_metadata(&previous, rows, cols);
            }
            initialize(&current, &previous);
            // Number of time steps
#pragma omp single
            nsteps = atoi(argv[3]);
            break;
        default:
#pragma omp single
            {
                printf("Unsupported number of command line arguments\n");
                error = 1;
            }
            break;
        }

        if (!error) {           // Enter the computation loop if initialization was ok
#pragma omp single
            {
                // Output the initial field
                output(&current, 0);

                // Largest stable time step
                dt = current.dx2 * current.dy2 /
                    (2.0 * a * (current.dx2 + current.dy2));
            }
            // Time evolve
            for (iter = 1; iter < nsteps; iter++) {
                evolve(&current, &previous, a, dt);
                if (iter % image_interval == 0) {
#pragma omp single
                    output(&current, iter);
                }
                // make current field to be previous for next iteration step
#pragma omp single
                swap_fields(&current, &previous);
            }
        }
    }                           // end of parallel region

    if (!error)
        finalize(&current, &previous);
    return error;
}
