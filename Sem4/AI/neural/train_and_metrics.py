import numpy as np
import tensorflow as tf
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, classification_report
from custom_data.process_img import get_imgs

BEST_EPOCHS = 4

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

# custom dataset
x_custom, y_custom = get_imgs()

# model
model = tf.keras.models.load_model("neural_model.keras")

# train
model.fit(x_train, y_train, epochs=BEST_EPOCHS, validation_data=(x_test, y_test))

# argmax - returns the index of the maximum value at the specified axis
# in this case index = predicted digit
y_pred_classes = np.argmax(model.predict(x_test), axis=1)
y_custom_pred_classes = np.argmax(model.predict(x_custom), axis=1)

# training_data
base_file = 'training_data/'

report_dict = classification_report(y_test, y_pred_classes, output_dict=True)
accuracy = report_dict["accuracy"]
report_df = pd.DataFrame(report_dict).T
report_df = report_df.drop(["accuracy", "macro avg", "weighted avg"], errors="ignore")
plt.figure(figsize=(10, 7))
sns.heatmap(report_df, annot=True, cmap="coolwarm", fmt=".2f", linewidths=0.5)
plt.title(f"Classification Report - accuracy: {accuracy:.2f}")
plt.xlabel("Metrics")
plt.ylabel("Classes")
plt.savefig(base_file + 'classification_report.png')
plt.close()

cm = confusion_matrix(y_test, y_pred_classes)
plt.figure(figsize=(10, 7))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=np.arange(10), yticklabels=np.arange(10))
plt.title('Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('True')
plt.savefig(base_file + 'confusion_matrix.png')
plt.close()

# custom_data
base_file = 'custom_data/'

report_dict = classification_report(y_custom, y_custom_pred_classes, output_dict=True)
accuracy = report_dict["accuracy"]
report_df = pd.DataFrame(report_dict).T
report_df = report_df.drop(["accuracy", "macro avg", "weighted avg"], errors="ignore")
plt.figure(figsize=(10, 7))
sns.heatmap(report_df, annot=True, cmap="coolwarm", fmt=".2f", linewidths=0.5)
plt.title(f"Classification Report - accuracy: {accuracy:.2f}")
plt.xlabel("Metrics")
plt.ylabel("Classes")
plt.savefig(base_file + 'classification_report.png')
plt.close()

cm = confusion_matrix(y_custom, y_custom_pred_classes)
plt.figure(figsize=(10, 7))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=np.arange(10), yticklabels=np.arange(10))
plt.title('Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('True')
plt.savefig(base_file + 'confusion_matrix.png')
plt.close()