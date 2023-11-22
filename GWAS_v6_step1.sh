#!/bin/bash
#SBATCH --job-name=V6_step1
#SBATCH --account=p471
#SBATCH --time=15:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=18
#SBATCH --output=Output_files/v6_%j.log


#script for continous (factor score) GWAS

source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files"

export SINGULARITYENV_LC_ALL=C

singularity exec -B /cluster/p/p471/cluster /cluster/p/p471/cluster/regenie_v3.1.1.gz.sif regenie \ #location of regenie
--step 1 \
--bed /cluster/p/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v6.txt \ #location of covar file - included sex, first 10pc, and batch
--phenoFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/pheno_gwas_EC.txt \
--extract /cluster/p/p471/cluster/projects/neruodev_structure/_lauraeh_oldregeniearchive/500k_N_genotyped_1+_info_0.97785.snps \ #500K snp list copy from of list in qcd genetic data dir
--bsize 1000 \ #size of the genotype blocks
--strict \ # complete cases
--lowmem \ #low memory option
--lowmem-prefix tmp_rg \
--out ${OUTM}/neurodev_EC_v6
