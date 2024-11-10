# Haskell Expense Tracker

A command-line application built in Haskell to track expenses. This project allows you to add, view, categorize, edit, and delete your expenses, storing them in a CSV file.

## Prerequisites

Before you can run the project, you need to have the following installed:

1. **Haskell** – The programming language used for this project.
2. **Stack** – A build tool for Haskell projects that simplifies dependency management and builds.

### Install Haskell

To install Haskell, follow these instructions for **Windows**:

- Download the [Haskell Platform](https://www.haskell.org/platform/) for Windows or use the [GHCup installer](https://www.haskell.org/ghcup/) to install Haskell and GHC (Glasgow Haskell Compiler).

### Install Stack

To install **Stack** on Windows:

- Download the Stack installer from [here](https://stack.org/) and follow the instructions.

Alternatively, you can install Stack using the command:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

## Running the Project

### 1. Clone the repository

First, clone the project to your local machine:

```bash
git clone https://github.com/yourusername/Haskell-Expense-Tracker.git
cd Haskell-Expense-Tracker
```

### 2. Install dependencies

Run the following command to install the necessary dependencies for the project:

```bash
stack build
```

This will download and install all the dependencies and set up the build environment.

### 3. Run the application

Navigate to the `src` directory:

```bash
cd src
```

Then run the application with:

```bash
stack run
```

This will start the application, and you'll see the main menu with options to add, view, edit, and delete expenses.
