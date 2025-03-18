import numpy as np
import cv2
import os
import seaborn as sns
import matplotlib.pyplot as plt
from tensorflow.keras.models import load_model
from sklearn.metrics import confusion_matrix, classification_report

model = load_model("../exc1/neural_model.keras")

def process_img(img_path):
    img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
    img = cv2.resize(img, (28, 28))
    img = img / 255.0
    return img

x_test = np.array([])
y_test = np.array([])

for file in [f for f in os.listdir('img') if f.endswith('.png')]:
    digit = int(file[0])
    img = process_img(f'img/{file}')

    x_test = np.append(x_test, img)
    y_test = np.append(y_test, digit)

x_test = x_test.reshape(-1, 28, 28, 1)

y_pred = model.predict(x_test)

y_pred_classes = np.argmax(y_pred, axis=1)

cm = confusion_matrix(y_test, y_pred_classes)
plt.figure(figsize=(10, 7))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=np.arange(10), yticklabels=np.arange(10))
plt.title('Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('True')
plt.savefig('confusion_matrix.png')
plt.close()

report = classification_report(y_test, y_pred_classes)
with open('classification_report.txt', 'w') as f:
    f.write(report)