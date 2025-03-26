#!/bin/bash -l

#SBATCH --job-name=disa_msf
#SBATCH --time=0-30:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --mem=28G 
#SBATCH --array=1-1
#SBATCH --mail-user=ftsiboe@ksu.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < 003_tech_inefficiency_disability_TE.R
