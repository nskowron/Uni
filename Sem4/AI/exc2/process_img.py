import numpy as np
import cv2
import os
from tensorflow.keras.models import load_model
from sklearn.metrics import confusion_matrix, classification_report

model = load_model("../exc1/neural_model.h5")

def process_img(img_path):
    img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
    img = cv2.resize(img, (28, 28))
    img = img / 255.0
    img = img.reshape(1, 28, 28, 1)
    return img

x_test = []
y_test = []

for file in [f for f in os.listdir('img') if f.endswith('.png')]:
    digit = int(file[0])
    img = process_img(f'img/{file}')

    x_test.append(img)
    y_test.append(digit)

y_pred = model.predict(np.array(x_test))

y_pred_classes = np.argmax(y_pred, axis=1)
