import subprocess
import random
import statistics
import matplotlib.pyplot as plt

n_values = list(range(10_000, 100_001, 10_000))
num_tests = 20

# Zbierane dane
results = {
    'n': [],
    'avg_comparisons': [],
    'max_comparisons': [],
    'avg_pointers': [],
    'max_pointers': [],
    'avg_height': [],
    'max_height': [],
    'avg_comparisons_d': [],
    'max_comparisons_d': [],
    'avg_pointers_d': [],
    'max_pointers_d': [],
    'avg_height_d': [],
    'max_height_d': [],
    'avg_comparisons_inc': [],
    'max_comparisons_inc': [],
    'avg_pointers_inc': [],
    'max_pointers_inc': [],
    'avg_height_inc': [],
    'max_height_inc': [],
    'avg_comparisons_inc_d': [],
    'max_comparisons_inc_d': [],
    'avg_pointers_inc_d': [],
    'max_pointers_inc_d': [],
    'avg_height_inc_d': [],
    'max_height_inc_d': [],
}

def run_test(ops):
    print("dupa1")
    proc = subprocess.Popen(
        ["java", "Main"],  # lub ["./twoj_program"] jeśli nie używasz make
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )

    comparisons = []
    pointers = []
    heights = []
    comparisons_del = []
    pointers_del = []
    heights_del = []

    for op in ops:
        proc.stdin.write(f"{op}\n")
        proc.stdin.flush()
        line = proc.stdout.readline()
        print(f"Processing operation: {op}, output: {line.strip()}")
        if op.startswith('i'):
            if "Comparisons" in line:
                comparisons.append(int(line.split(":")[-1].strip()))
            elif "Pointers" in line:
                pointers.append(int(line.split(":")[-1].strip()))
            elif "Height" in line:
                heights.append(int(line.split(":")[-1].strip()))
        elif op.startswith('d'):
            if "Comparisons" in line:
                comparisons_del.append(int(line.split(":")[-1].strip()))
            elif "Pointers" in line:
                pointers_del.append(int(line.split(":")[-1].strip()))
            elif "Height" in line:
                heights_del.append(int(line.split(":")[-1].strip()))
    print("dupa3")
    proc.stdin.write("q\n")  # Zakończ program
    proc.stdin.flush()
    proc.stdin.close()
    proc.wait()

    print("dupa2")
    return comparisons, pointers, heights, comparisons_del, pointers_del, heights_del

def plot_metric(x, y_rand, y_inc, ylabel, title, filename):
    plt.figure()
    plt.plot(x, y_rand, label="Wstawianie losowe")
    plt.plot(x, y_inc, label="Wstawianie rosnące")
    plt.xlabel("n")
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend()
    plt.savefig(filename)
    plt.close()

for n in n_values:
    print(f"we're alive for n={n}")

    results['n'].append(n)
    results['avg_comparisons'].append(0)
    results['max_comparisons'].append(0)
    results['avg_pointers'].append(0)
    results['max_pointers'].append(0)
    results['avg_height'].append(0)
    results['max_height'].append(0)
    results['avg_comparisons_d'].append(0)
    results['max_comparisons_d'].append(0)
    results['avg_pointers_d'].append(0)
    results['max_pointers_d'].append(0)
    results['avg_height_d'].append(0)
    results['max_height_d'].append(0)

    for _ in range(num_tests):
        # random perm
        keys = list(range(1, n + 1))
        insert_seq = random.sample(keys, len(keys))
        delete_seq = random.sample(keys, len(keys))

        ops = [f"i {k}" for k in insert_seq] + [f"d {k}" for k in delete_seq]
        comp, ptr, h, comp_d, ptr_d, h_d = run_test(ops)

        results['avg_comparisons'][-1] += statistics.mean(comp)
        results['max_comparisons'][-1] = max(results['max_comparisons'][-1], max(comp))
        results['avg_pointers'][-1] += statistics.mean(ptr)
        results['max_pointers'][-1] = max(results['max_pointers'][-1], max(ptr))
        results['avg_height'][-1] += statistics.mean(h)
        results['max_height'][-1] = max(results['max_height'][-1], max(h))
        results['avg_comparisons_d'][-1] += statistics.mean(comp_d)
        results['max_comparisons_d'][-1] = max(results['max_comparisons_d'][-1], max(comp_d))
        results['avg_pointers_d'][-1] += statistics.mean(ptr_d)
        results['max_pointers_d'][-1] = max(results['max_pointers_d'][-1], max(ptr_d))
        results['avg_height_d'][-1] += statistics.mean(h_d)
        results['max_height_d'][-1] = max(results['max_height_d'][-1], max(h_d))
    results['avg_comparisons'][-1] /= num_tests
    results['avg_pointers'][-1] /= num_tests
    results['avg_height'][-1] /= num_tests
    results['avg_comparisons_d'][-1] /= num_tests
    results['avg_pointers_d'][-1] /= num_tests
    results['avg_height_d'][-1] /= num_tests

    # increasing perm
    results['avg_comparisons_inc'].append(0)
    results['max_comparisons_inc'].append(0)
    results['avg_pointers_inc'].append(0)
    results['max_pointers_inc'].append(0)
    results['avg_height_inc'].append(0)
    results['max_height_inc'].append(0)
    results['avg_comparisons_inc_d'].append(0)
    results['max_comparisons_inc_d'].append(0)
    results['avg_pointers_inc_d'].append(0)
    results['max_pointers_inc_d'].append(0)
    results['avg_height_inc_d'].append(0)
    results['max_height_inc_d'].append(0)

    for _ in range(num_tests):
        # random perm
        keys = list(range(1, n + 1))
        insert_seq = keys
        delete_seq = random.sample(keys, len(keys))

        ops = [f"i {k}" for k in insert_seq] + [f"d {k}" for k in delete_seq]
        comp, ptr, h, comp_d, ptr_d, h_d = run_test(ops)

        results['avg_comparisons_inc'][-1] += statistics.mean(comp)
        results['max_comparisons_inc'][-1] = max(results['max_comparisons_inc'][-1], max(comp))
        results['avg_pointers_inc'][-1] += statistics.mean(ptr)
        results['max_pointers_inc'][-1] = max(results['max_pointers_inc'][-1], max(ptr))
        results['avg_height_inc'][-1] += statistics.mean(h)
        results['max_height_inc'][-1] = max(results['max_height_inc'][-1], max(h))
        results['avg_comparisons_inc_d'][-1] += statistics.mean(comp_d)
        results['max_comparisons_inc_d'][-1] = max(results['max_comparisons_inc_d'][-1], max(comp_d))
        results['avg_pointers_inc_d'][-1] += statistics.mean(ptr_d)
        results['max_pointers_inc_d'][-1] = max(results['max_pointers_inc_d'][-1], max(ptr_d))
        results['avg_height_inc_d'][-1] += statistics.mean(h_d)
        results['max_height_inc_d'][-1] = max(results['max_height_inc_d'][-1], max(h_d))
    results['avg_comparisons_inc'][-1] /= num_tests
    results['avg_pointers_inc'][-1] /= num_tests
    results['avg_height_inc'][-1] /= num_tests
    results['avg_comparisons_inc_d'][-1] /= num_tests
    results['avg_pointers_inc_d'][-1] /= num_tests
    results['avg_height_inc_d'][-1] /= num_tests

plot_metric(
    results['n'],
    results['avg_comparisons'],
    results['avg_comparisons_inc'],
    "Średnia liczba porównań",
    "Średnia liczba porównań w BST",
    "plots/insert/comparisons/avg.png"
)
plot_metric(
    results['n'],
    results['max_comparisons'],
    results['max_comparisons_inc'],
    "Maksymalna liczba porównań",
    "Maksymalna liczba porównań w BST",
    "plots/insert/comparisons/max.png"
)
plot_metric(
    results['n'],
    results['avg_pointers'],
    results['avg_pointers_inc'],
    "Średnia liczba odwołań do wskaźników",
    "Średnia liczba odwołań do wskaźników w BST",
    "plots/insert/pointers/avg.png"
)
plot_metric(
    results['n'],
    results['max_pointers'],
    results['max_pointers_inc'],
    "Maksymalna liczba odwołań do wskaźników",
    "Maksymalna liczba odwołań do wskaźników w BST",
    "plots/insert/pointers/max.png"
)
plot_metric(
    results['n'],
    results['avg_height'],
    results['avg_height_inc'],
    "Średnia wysokość drzewa",
    "Średnia wysokość drzewa BST",
    "plots/insert/height/avg.png"
)
plot_metric(
    results['n'],
    results['max_height'],
    results['max_height_inc'],
    "Maksymalna wysokość drzewa",
    "Maksymalna wysokość drzewa BST",
    "plots/insert/height/max.png"
)
plot_metric(
    results['n'],
    results['avg_comparisons_d'],
    results['avg_comparisons_inc_d'],
    "Średnia liczba porównań (usuwanie)",
    "Średnia liczba porównań w BST (usuwanie)",
    "plots/delete/comparisons/avg.png"
)
plot_metric(
    results['n'],
    results['max_comparisons_d'],
    results['max_comparisons_inc_d'],
    "Maksymalna liczba porównań (usuwanie)",
    "Maksymalna liczba porównań w BST (usuwanie)",
    "plots/delete/comparisons/max.png"
)
plot_metric(
    results['n'],
    results['avg_pointers_d'],
    results['avg_pointers_inc_d'],
    "Średnia liczba odwołań do wskaźników (usuwanie)",
    "Średnia liczba odwołań do wskaźników w BST (usuwanie)",
    "plots/delete/pointers/avg.png"
)
plot_metric(
    results['n'],
    results['max_pointers_d'],
    results['max_pointers_inc_d'],
    "Maksymalna liczba odwołań do wskaźników (usuwanie)",
    "Maksymalna liczba odwołań do wskaźników w BST (usuwanie)",
    "plots/delete/pointers/max.png"
)
plot_metric(
    results['n'],
    results['avg_height_d'],
    results['avg_height_inc_d'],
    "Średnia wysokość drzewa (usuwanie)",
    "Średnia wysokość drzewa BST (usuwanie)",
    "plots/delete/height/avg.png"
)
plot_metric(
    results['n'],
    results['max_height_d'],
    results['max_height_inc_d'],
    "Maksymalna wysokość drzewa (usuwanie)",
    "Maksymalna wysokość drzewa BST (usuwanie)",
    "plots/delete/height/max.png"
)
