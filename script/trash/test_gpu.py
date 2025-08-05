# test_gpu.py
import os
import tensorflow as tf
print("TensorFlow version:", tf.__version__)
print("CUDA_VISIBLE_DEVICES =", os.environ.get("CUDA_VISIBLE_DEVICES"))
print("GPUs:", tf.config.list_physical_devices('GPU'))
print("GPUs available:", tf.config.list_physical_devices('GPU'))
