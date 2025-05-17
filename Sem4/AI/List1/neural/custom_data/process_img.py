import numpy as np
import cv2
import os

def process_img(img_path):
    img = cv2.imread(img_path, cv2.IMREAD_GRAYSCALE)
    img = cv2.resize(img, (28, 28))
    img = img / 255.0
    img = 1 - img
    return img

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
IMG_DIR = os.path.join(BASE_DIR, 'img')

def get_imgs():
    x_test = np.array([])
    y_test = np.array([])

    for file in [f for f in os.listdir(IMG_DIR) if f.endswith('.png')]:
        digit = int(file[0])
        img = process_img(f"{IMG_DIR}/{file}")

        x_test = np.append(x_test, img)
        y_test = np.append(y_test, digit)

    x_test = x_test.reshape(-1, 28, 28, 1)
    return x_test, y_test
