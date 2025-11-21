#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.14"
# dependencies = [
#     "matplotlib",
#     "numpy",
#     "scipy",
# ]
# ///

import numpy as np
import matplotlib.pyplot as plt
import scipy.signal
from numpy.typing import NDArray

camBlue = "#8ee8d8"


def generate_oscillatory_signal(
    num_steps: int = 500, noise_std_dev: float = 15.0, phi: float = 0.85
) -> NDArray[np.float64]:
    """
    Generates an oscillatory random signal using an Auto-Regressive AR(1) process.
    X(t) = phi * X(t-1) + epsilon(t).
    """
    rng = np.random.default_rng(seed=42)
    noise: NDArray[np.float64] = rng.normal(loc=0, scale=noise_std_dev, size=num_steps)

    signal: NDArray[np.float64] = np.zeros(num_steps)
    signal[0] = noise[0] / np.sqrt(1 - phi**2)

    for t in range(1, num_steps):
        signal[t] = phi * signal[t - 1] + noise[t]
    signal += 100.0

    return signal


def apply_lowpass_filter(
    data: NDArray[np.float64], cutoff: float = 0.1, order: int = 5
) -> NDArray[np.float64]:
    """
    Applies a low-pass Butterworth filter to smooth the signal.
    """

    normal_cutoff = cutoff / 0.5

    # Design the filter
    b, a = scipy.signal.butter(order, normal_cutoff, btype="low", analog=False)

    # Apply the filter using zero-phase filtering to avoid phase shift
    smoothed_data: NDArray[np.float64] = scipy.signal.filtfilt(b, a, data)
    return smoothed_data


def plot_signal() -> None:
    """Generates a noisy signal, filters it to be simple, and plots it with standard professional styling."""
    # Parameters
    N: int = 2000

    plt.rcParams["font.family"] = "sans-serif"
    plt.rcParams["font.sans-serif"] = [
        "Open Sans",
        "DejaVu Sans",
        "Arial",
        "sans-serif",
    ]

    # 1. Generate the raw oscillatory signal
    raw_signal: NDArray[np.float64] = generate_oscillatory_signal(
        N, noise_std_dev=25.0, phi=0.8
    )

    # 2. Apply Signal Filtering (Low-pass) to get the "simple" data
    smoothed_signal: NDArray[np.float64] = apply_lowpass_filter(
        raw_signal, cutoff=0.005, order=4
    )

    time_points: NDArray[np.int_] = np.arange(N)

    # --- Plotting Configuration (Standard Professional Style) ---
    fig, ax = plt.subplots(figsize=(20, 6))

    # Plot the smoothed signal (The primary data)
    # Using standard line width and color
    (line,) = ax.plot(time_points, smoothed_signal, linewidth=3)
    line.set_linewidth(5)
    # line.set_color(camBlue)
    ax.tick_params(
        axis="both", which="both", length=0, labelleft=False, labelbottom=False
    )
    # ax.set_xlabel("Time", fontsize=32)
    ax.set_ylabel(r"$\hat{\Phi}(t)$", rotation="horizontal", fontsize=32, ha="right")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    fig.tight_layout()

    output_filename: str = "changing_field_operator.svg"
    fig.savefig(output_filename, format="svg", transparent=True)


if __name__ == "__main__":
    plot_signal()
