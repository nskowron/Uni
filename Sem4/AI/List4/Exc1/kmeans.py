import numpy as np
import math

class KMeans:
    def __init__(self, k: int, n_init: int=10, tolerance: float=1e-4, random_state: int=42, max_iterations: int=300):
        self.k = k
        self.num_iterations = n_init
        self.centroids = None
        self.labels = []
        self.inertia = None
        self.tolerance = tolerance
        self.random_state = random_state
        self.max_iterations = max_iterations

    def initialize_centroids(self, X: np.ndarray):
        n_samples, _ = X.shape
        centroids = []
        rng = np.random.RandomState(self.random_state)
        centroids.append(X[rng.randint(n_samples)])
        dist_sq = np.full(n_samples, np.inf)                        # distances to closest centroid

        for c in range(1, self.k):
            dist_to_new = np.sum((X - centroids[c-1])**2, axis=1)   # distances to new centroid
            dist_sq = np.minimum(dist_sq, dist_to_new)              # if new centroid is closer - overwrite
            probabilities = dist_sq / dist_sq.sum()
            next_idx = rng.choice(n_samples, p=probabilities)       # pref furthest from known centroids
            centroids.append(X[next_idx])

        return np.array(centroids)


    def caluculate_inertia(self, X: np.ndarray, labels: np.ndarray, centroids: np.array):
        min_distances_from_centroids = np.sum((X - centroids[labels])**2, axis=1)   # labels - idx of assigned centroid
        return np.sum(min_distances_from_centroids)


    def fit(self, X: np.ndarray):
        best_centroids = None
        best_inertia = math.inf
        best_labels = None

        X_sq = np.sum(X * X, axis=1).reshape(-1,1)  # euclidean norm squared

        for _ in range(self.num_iterations):            # a few random iterations for best inertia
            centroids = self.initialize_centroids(X)
            for _ in range(self.max_iterations):
                cent_sq = np.sum(centroids*centroids, axis=1).reshape(1,-1) # euclidean norm squared for centroids
                cross = X.dot(centroids.T)
                distances = X_sq + cent_sq - 2*cross                        # faster way to calc all distances
                labels = np.argmin(distances, axis=1)                       # assign to centroids

                new_centroids = []                                          # new centroids - mean or old
                for j in range(self.k):
                    cluster_points = X[labels == j]
                    new_centroids.append(np.mean(cluster_points, axis=0) if len(cluster_points) > 0 else centroids[j])
                new_centroids = np.array(new_centroids)

                shift = np.linalg.norm(new_centroids - centroids, axis=1)   # euclidean norm - difference new centroids
                centroids = new_centroids
                if np.all(shift < self.tolerance):                          # centroids didnt move much
                    break

            inertia = self.caluculate_inertia(X, labels, centroids)
            if inertia < best_inertia:
                best_inertia = inertia
                best_labels = labels.copy()
                best_centroids = centroids.copy()

            self.centroids = best_centroids
            self.labels = best_labels
            self.inertia = best_inertia

        return self


    def predict(self, X: np.ndarray):
        distances = np.linalg.norm(X[:, np.newaxis] - self.centroids, axis=2)   # numpy magic
        labels = np.argmin(distances, axis=1)
        return labels
