import numpy as np
import tensorflow as tf
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score
from custom_data.process_img import get_imgs

np.random.seed(279679)
tf.random.set_seed(279679)

# mnist dataset
(x_train, y_train), (x_test, y_test) = tf.keras.datasets.mnist.load_data()

# normalize
x_train = x_train / 255.0
x_test = x_test / 255.0

# reshape (60000, 784) -> (60000, 28, 28, 1)
x_train = x_train.reshape(-1, 28, 28, 1)
x_test = x_test.reshape(-1, 28, 28, 1)

# custom data (my handwriting)
x_custom, y_custom = get_imgs()

# model
model = tf.keras.models.load_model("neural_model.keras")

# training and testing
train_accuracies = []
val_accuracies = []
custom_accuracies = []

epochs = 30
for epoch in range(epochs):

    # train for one epoch
    data = model.fit(x_train, y_train, epochs=1, validation_data=(x_test, y_test))

    # accuracy on train data
    train_acc = data.history['accuracy'][-1] # last element
    val_acc = data.history['val_accuracy'][-1]

    # accuracy on custom data
    y_custom_pred = np.argmax(model.predict(x_custom), axis=1)
    custom_acc = accuracy_score(y_custom, y_custom_pred)

    print(f"MY ACCURACY: {custom_acc}")

    # Store accuracies
    train_accuracies.append(train_acc)
    val_accuracies.append(val_acc)
    custom_accuracies.append(custom_acc)

# Plot the accuracy trend
plt.figure(figsize=(10, 8))
plt.plot(range(1, epochs + 1), train_accuracies, label='Train Accuracy')
plt.plot(range(1, epochs + 1), val_accuracies, label='Validation Data Accuracy')
plt.plot(range(1, epochs + 1), custom_accuracies, label='Custom Data Accuracy')
plt.xlabel("Epoch")
plt.ylabel("Accuracy")
plt.title("Accuracy Over Training Epochs")
plt.legend()
plt.savefig('epoch_plot.png')