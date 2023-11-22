#!/bin/bash
#SBATCH --job-name=male_step2
#SBATCH --account=p471_tsd
#SBATCH --time=16:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=25G
#SBATCH --cpus-per-task=18
#SBATCH --output=./%x_%j.out



source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/ess/p471/cluster/projects/neruodev_structure/Genomic/Output_files"

export SINGULARITYENV_LC_ALL=C

singularity exec -B /ess/p471/cluster /ess/p471/cluster/regenie_v3.1.1.gz.sif regenie \
--step 2 \
--bed /ess/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v5.txt \
--phenoFile /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/pheno_gwas_EC.txt \
--keep /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/maleID_list.txt \
--bsize 200 \
--pred /ess/p471/cluster/projects/neruodev_structure/Genomic/Output_files/neurodev_EC_m_pred.list \
--out ${OUTM}/Neurodev_EC_m