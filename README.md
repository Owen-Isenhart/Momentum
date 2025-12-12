# Momentum

Momentum is a terminal-based speedrunning timer written in Rust. It provides a lightweight, keyboard-driven interface for tracking splits, comparing against personal bests, and managing multiple run categories directly from your command line.

## Prerequisites

You need to have **Rust** and **Cargo** installed on your machine.  
If you don't have them, install them via [rustup.rs](https://rustup.rs/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Installation

### Clone the repository:

```bash
git clone <your-repo-url>
cd momentum
```


### Install Locally

Run the following command in the project root to compile and install the binary globally:

```bash
cargo install --path .
```

This ensures the `momentum` command is added to your `PATH`.

## Usage

Once installed, you can start the timer from any terminal window:

```bash
momentum
```

## Controls

Navigation is handled via tabs:

- **1**: Timer View  
- **2**: Load/Save View  
- **3**: Settings View  

### Timer Defaults:

- **Space**: Split  
- **p**: Pause  
- **r**: Reset  
- **q**: Quit  

*Note: You can customize these key bindings in the Settings tab.*

## Data Storage

Momentum stores your configuration and splits in a hidden folder in your home directory so they persist regardless of where you launch the app.

- **Linux/macOS:** `~/.momentum/`  
- **Windows:** `%USERPROFILE%\.momentum\`

The app creates two files in this directory:

- `config.json`: Stores your custom key bindings (Only gets created if custom bindings are set)
- `splits.json`: Stores all your run data and times.

## Building for Development

If you want to run the app without installing it globally (useful for development):

```bash
cargo run
```
