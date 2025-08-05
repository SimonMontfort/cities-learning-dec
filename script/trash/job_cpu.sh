#!/bin/bash
#SBATCH --job-name=train
#SBATCH --output=train.out
#SBATCH --error=train_err.txt
#SBATCH --time=72:00:00
#SBATCH --cpus-per-task=30
#SBATCH --partition=bigmem
#SBATCH --qos=bigmem
#SBATCH --mem-per-cpu=14000  

export BASE_DIR="/home/smontfor/cities-learning-dec"

module purge
module load gcc

source ~/miniconda3/etc/profile.d/conda.sh
conda activate tf-cpu

# Run your training in the background
python -u script/01_DEC_training_parallel.py 