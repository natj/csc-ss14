#!/bin/bash
#SBATCH --reservation=summer_school
#SBATCH --ntasks=2
#SBATCH --cpus-per-task=8
#SBATCH --output=ex1-%J.txt

export OMP_NUM_THREADS=8
aprun -B ./cpuidtest
