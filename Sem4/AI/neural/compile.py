import tensorflow as tf

tf.random.set_seed(279679)

# cnn model (better for image data)
model = tf.keras.models.Sequential([

    # scans image with 32 filters of size 3x3
    tf.keras.layers.Conv2D(32, (3, 3), activation='relu', input_shape=(28, 28, 1)),

    # downsamples the image (bochen)
    tf.keras.layers.MaxPooling2D((2, 2)),
    
    # relu - rectified linear unit
    # zero negative values, shut down neuron, help with vanishing gradient
    tf.keras.layers.Conv2D(64, (3, 3), activation='relu'),

    # flattens image to 1D array
    tf.keras.layers.Flatten(),

    # final interpretation layer
    tf.keras.layers.Dense(128, activation='relu'),
    tf.keras.layers.Dense(10, activation='softmax')
])

# compilation
model.compile(optimizer='adam', loss='sparse_categorical_crossentropy', metrics=['accuracy'])
model.save("neural_model.keras")