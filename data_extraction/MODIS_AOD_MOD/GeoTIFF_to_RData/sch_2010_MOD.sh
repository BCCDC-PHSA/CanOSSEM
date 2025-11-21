#!/bin/bash
#SBATCH --job-name="MOD_2010"
#SBATCH --time=10:00:00
#SBATCH --output=MOD_2010.out      # Output file
#SBATCH --cpus-per-task=1        # number of processes
#SBATCH --mem-per-cpu=20G      # memory; default unit is megabytes
#SBATCH --nodes=1
#SBATCH --account=def-xx


echo 'TASK ID:'
echo 'MOD_2010'

echo '----'

echo 'SLURM JOB ID:'
echo $SLURM_JOB_ID

echo '----'


module load StdEnv/2023  gcc/12.3  udunits/2.2.28  hdf/4.2.16  gdal/3.7.2  r/4.3.1

Rscript --vanilla script_2010_MOD.R
