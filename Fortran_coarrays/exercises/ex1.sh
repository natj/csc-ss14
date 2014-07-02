#!/bin/bash -l
#SBATCH -J nattila_job
#SBATCH -o output/%J.out
#SBATCH -e output/%J.err
#SBATCH -N 1
#SBATCH --ntasks-per-node=16
#SBATCH -t 00:01:00
#SBATCH -p test
#SBATCH --reservation=summer_school
aprun -n 16 ./ex1