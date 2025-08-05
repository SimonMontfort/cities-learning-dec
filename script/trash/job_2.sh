#!/bin/bash
#SBATCH --job-name=train
#SBATCH --output=train2.out
#SBATCH --error=train_err2.txt
#SBATCH --time=72:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=256G
#SBATCH --partition=h100
#SBATCH --gres=gpu:1

export BASE_DIR="/home/smontfor/cities-learning-dec"

module purge
module load gcc cuda # cudnn # python openmpi py-tensorflow

# Manually source Conda's shell hook
source ~/miniconda3/etc/profile.d/conda.sh
conda activate tf2.14

python -u script/01_DEC_training.py