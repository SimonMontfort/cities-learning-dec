# test_gpu.py
import os
import tensorflow as tf
print("CUDA_VISIBLE_DEVICES =", os.environ.get("CUDA_VISIBLE_DEVICES"))
print("GPUs:", tf.config.list_physical_devices('GPU'))
