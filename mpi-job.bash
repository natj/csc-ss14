#!/bin/bash
#SBATCH --reservation=summer_school
#SBATCH --ntasks=8
#SBATCH --output=ex1_output-%J.txt

aprun -B ./ex1
