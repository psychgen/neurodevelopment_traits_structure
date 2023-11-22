#!/bin/bash
#SBATCH --job-name=item_stage2
#SBATCH --account=p471_tsd
#SBATCH --time=5:00:00
#SBATCH --array=1-6
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=30



source /cluster/bin/jobsetup
module purge
module load singularity/3.7.3

set -o errexit

OUTM="/cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Output_files/Item_level/V3"



export SINGULARITYENV_LC_ALL=C



singularity exec -B /cluster/p/p471/cluster /cluster/p/p471/cluster/regenie_v3.1.1.gz.sif regenie \
--step 1 \
--bed /cluster/p/p471/cluster/data/genetic_data/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--covarFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/covar_gwas_EC_v6.txt \
--phenoFile /cluster/p/p471/cluster/projects/neruodev_structure/Genomic/Input_files/item_gwas_EC_v4.txt \
--extract /cluster/p/p471/cluster/data/genetic_data/qcd_genetic_data/regenie-master/_lauraeh/500k_N_genotyped_1+_info_0.97785.snps \
--bsize 1000 \
--bt --lowmem \
--lowmem-prefix tmp_item \
--out ${OUTM}/fit_v4_bin_l0_$SLURM_ARRAY_TASK_ID \
--run-l0 ${OUTM}/fit_bin_parallel.master,$SLURM_ARRAY_TASK_ID






