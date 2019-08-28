#!/bin/sh
#SBATCH -t 02:00:00
#SBATCH -N 1
#SBATCH --tasks-per-node=20
#SBATCH --exclusive
#SBATCH --mail-user=martingerdin@gmail.com
#SBATCH --mail-type=END

mpirun R --slave -f MainCodeRun.R

