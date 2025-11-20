# Caten.py

**Caten** is a Python-based Polyhedral Compiler framework designed for deep learning and high-performance computing education and experimentation. It provides Python bindings for the [Integer Set Library (ISL)](https://libisl.sourceforge.io/) and high-level abstractions for tensor scheduling and optimization.

## Vision

Caten aims to bridge the gap between high-level tensor operations (like in PyTorch/NumPy) and low-level loop optimizations (Tiling, Fusion, Vectorization). By leveraging the Polyhedral Model, Caten allows users to:

*   **Visualize and manipulate** loop structures as mathematical objects (Schedule Trees).
*   **Perform complex transformations** like *Conv+Pool Fusion* or *Symbolic Tiling* using a Pythonic API.
*   **Generate optimized code** for various backends (CPU, CUDA, Metal).

## Features

*   **`caten.isl`**: Auto-generated, type-safe Python bindings for ISL. No manual C-types management required.
*   **`caten.polyhedral`** (Planned): A high-level DSL for schedule construction and manipulation.
*   **Implicit Context**: ISL context is managed automatically; no boilerplate code.
*   **Operator Overloading**: Use standard Python operators (`|`, `&`, `-`, `+`) for set/map operations.

## Documentation

*   **[Project Overview & Architecture](docs/PROJECT.md)**
*   **[Polyhedral Loop Transformation Spec](docs/POLYHEDRAL_SPEC.md)**
*   **[IR & Class Design](docs/IR_AND_DESIGN.md)**
*   **[ISL API Coverage](docs/ISL_APIS.md)**

## Tutorial

Check out `examples/isl_tutorial.ipynb` for a hands-on introduction to using ISL bindings in Python.

## Installation

*Requirements*: `libisl` must be installed (e.g., `brew install isl`).

```bash
# Clone the repository
git clone https://github.com/hikettei/Caten.py.git
cd Caten.py

# Install dependencies
uv sync
```

## Roadmap

1.  [x] **ISL Bindings**: Complete `caten.isl`.
2.  [ ] **Polyhedral DSL**: Implement `caten.polyhedral`.
3.  [ ] **IR & Kernel**: Implement `caten.ops` and `caten.kernel`.
4.  [ ] **Runtime & Renderer**: Code generation for CPU/C.
5.  [ ] **Auto-Scheduler**: Basic search for optimal schedules.

## License

MIT