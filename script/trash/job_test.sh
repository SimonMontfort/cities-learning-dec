#!/bin/bash
#SBATCH --job-name=train
#SBATCH --output=train.out
#SBATCH --error=train_err.txt
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=20
#SBATCH --mem=256G
#SBATCH --partition=l40s
#SBATCH --gres=gpu:1

export BASE_DIR="/home/smontfor/cities-learning-DEC"

# module purge
module load gcc cuda cudnn

conda init tf2.14
conda activate tf2.14

python -u test_gpu.py