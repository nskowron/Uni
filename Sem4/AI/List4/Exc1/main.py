import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from tensorflow.keras.datasets import mnist
from sklearn.metrics import confusion_matrix
from skimage.transform import resize
from kmeans import KMeans


def downscale_images(X):
    X_images = X.reshape(-1, 28, 28)
    X_resized = np.array([resize(img, (8, 8), anti_aliasing=True) for img in X_images])
    return X_resized.reshape(-1, 64)

def plot_centroids(centroids, file):
    _, axes = plt.subplots(1, len(centroids), figsize=(8, 8))
    for i, ax in enumerate(axes):
        ax.imshow(centroids[i].reshape(8, 8), cmap='gray')
        ax.axis('off')
        ax.set_title(f'C{i}')
    plt.suptitle('Centroids')
    plt.tight_layout()
    plt.savefig(file)

def plot_confusion_matrix(cm, file):
    plt.figure(figsize=(8, 6))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues')
    plt.title('Confusion Matrix')
    plt.xlabel('Predicted')
    plt.ylabel('True')
    plt.savefig(file)

def run_experiment(k):
    print(f"Running KMeans with {k} clusters...")
    kmeans = KMeans(k=k, random_state=279679)
    kmeans.fit(X_train)

    predicted_labels = kmeans.predict(X_test)
    cm = confusion_matrix(y_test, predicted_labels, labels=range(10))

    cluster_to_digit = np.argmax(cm, axis=0)
    mapped_preds = np.array([cluster_to_digit[cl] for cl in predicted_labels])

    cm = confusion_matrix(y_test, mapped_preds, labels=range(10))

    plot_confusion_matrix(cm, file=f"plots/k{k}/cm.png")
    plot_centroids(kmeans.centroids, file=f"plots/k{k}/centroids.png")


(X_train, y_train), (X_test, y_test) = mnist.load_data()

X_train = downscale_images(X_train)
X_test = downscale_images(X_test)

X_train = X_train / 255.0
X_test = X_test / 255.0

for k in [10, 15, 20, 30]:
    run_experiment(k)

