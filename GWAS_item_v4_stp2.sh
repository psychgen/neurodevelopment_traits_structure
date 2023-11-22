#!/bin/bash
#SBATCH --job-name=item_step2
#SBATCH --account=p471_tsd
#SBATCH --time=45:00:00
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=24



source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files/Item_level/V3"



export SINGULARITYENV_LC_ALL=C



singularity exec -B /cluster/p/p471/cluster /cluster/p/p471/cluster/regenie_v3.1.1.gz.sif regenie \
--step 2 \
--bed /cluster/p/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v6.txt \
--phenoFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/item_gwas_EC_v4.txt \
--bsize 200 \
--pred /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files/Item_level/V3/item_v4_pred.list \
--firth --approx \ #computational speed up of firth correction that helps reduce bias from unbalanced case/control ratios
--bt \  
--out ${OUTM}/item_v3
