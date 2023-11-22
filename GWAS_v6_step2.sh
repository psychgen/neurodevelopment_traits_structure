#!/bin/bash
#SBATCH --job-name=V6_step2
#SBATCH --account=p471
#SBATCH --time=16:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=18



source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files"

export SINGULARITYENV_LC_ALL=C

singularity exec -B /cluster/p/p471/cluster /cluster/p/p471/cluster/regenie_v3.1.1.gz.sif regenie \
--step 2 \
--bed /cluster/p/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v6.txt \
--phenoFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/pheno_gwas_EC.txt \
--bsize 200 \
--pred /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files/neurodev_EC_v6_pred.list \
--out ${OUTM}/Neurodev_EC_v6
