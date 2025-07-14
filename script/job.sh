#!/bin/bash
#SBATCH --job-name=train
#SBATCH --output=train.out
#SBATCH --error=train_err.txt
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=20
#SBATCH --mem=256G
#SBATCH --partition=l40s
#SBATCH --gres=gpu:1

# Load the one that works, e.g.:
module purge
module load gcc cuda cudnn openmpi py-tensorflow

# Run your script
export TOKENIZERS_PARALLELISM=false
nvidia-smi              
# python -u 01_DEC_training.py
python -u test_gpu.py