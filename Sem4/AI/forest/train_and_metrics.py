import numpy as np
import tensorflow as tf
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, confusion_matrix
from custom_data.process_img import get_imgs

np.random.seed(279679)
tf.random.set_seed(279679)

# mnist dataset
(x_train, y_train), (x_test, y_test) = tf.keras.datasets.mnist.load_data()

# reshape
x_train_flat = x_train.reshape(x_train.shape[0], -1)  # Shape: (60000, 784)
x_test_flat = x_test.reshape(x_test.shape[0], -1)     # Shape: (10000, 784)

# normalize
x_train_flat = x_train_flat / 255.0
x_test_flat = x_test_flat / 255.0

# custom dataset
x_custom, y_custom = get_imgs()

# model
rf_model = RandomForestClassifier(n_estimators=100, random_state=279679, n_jobs=-1)
rf_model.fit(x_train_flat, y_train)

# predict
y_pred_classes = rf_model.predict(x_test_flat)
y_custom_pred_classes = rf_model.predict(x_custom)

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
