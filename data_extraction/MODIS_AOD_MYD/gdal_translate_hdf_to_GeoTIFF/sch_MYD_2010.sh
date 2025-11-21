#!/bin/bash
#SBATCH --job-name="2010"
#SBATCH --time=12:00:00
#SBATCH --output=MYD_translate_2010.out      # Output file
#SBATCH --ntasks-per-node=16        # number of processes
#SBATCH --mem=20G      # memory; default unit is megabytes
#SBATCH --nodes=1
#SBATCH --account=def-xx


echo 'TASK ID:'
echo 'MYD_TRANSLATE'

echo '----'

echo 'SLURM JOB ID:'
echo $SLURM_JOB_ID

echo '----'


module load StdEnv/2023 gcc/12.3 python/3.11 gdal/3.7.2

./MYD_2010.sh
