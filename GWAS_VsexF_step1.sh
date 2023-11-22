#!/bin/bash
#SBATCH --job-name=females_step1
#SBATCH --account=p471
#SBATCH --time=15:00:00
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
--step 1 \
--bed /ess/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v5.txt \
--phenoFile /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/pheno_gwas_EC.txt \
--extract /ess/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/_lauraeh/500k_N_genotyped_1+_info_0.97785.snps \
--keep /ess/p471/cluster/projects/neruodev_structure/Genomic/Input_files/femaleID_list.txt \
--bsize 1000 \
--strict \
--lowmem \
--lowmem-prefix tmp_rg_f \
--out ${OUTM}/neurodev_EC_f
