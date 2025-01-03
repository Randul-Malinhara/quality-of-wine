# Machine Learning & Data Mining Coursework (5DATA002W)

## University of Westminster
**School of Computer Science & Engineering**

**Module Leader**: Dr. V.S. Kontogiannis  
**Academic Year**: 2021/22  

---

## Overview
This repository contains the coursework implementation for the module **5DATA002W: Machine Learning & Data Mining**. The project focuses on clustering and regression techniques using real-world datasets and is implemented in the **R programming environment**.

### Key Objectives
1. **Clustering Analysis**
   - Perform k-means clustering on a wine dataset with pre-processing steps.
   - Apply Principal Component Analysis (PCA) for dimensionality reduction.
   - Compare clustering performance with and without PCA.

2. **Energy Forecasting**
   - Use a Multi-Layer Perceptron (MLP) Neural Network to predict electricity consumption based on time-series data.
   - Evaluate models using statistical indices such as RMSE, MAE, and MAPE.

---

## Learning Outcomes
This coursework demonstrates:
- Preparation of realistic datasets for machine learning and data mining.
- Evaluation, validation, and optimization of models.
- Effective communication of models and analyses to diverse audiences.

---

## Coursework Breakdown

### Clustering Part
- **Dataset**: White wine dataset containing 4710 samples with chemical properties and quality ratings.
- **Tasks**:
  1. Pre-process the dataset (scaling, outlier removal).
  2. Define the optimal number of clusters using various methods (e.g., Elbow, Gap statistics, Silhouette).
  3. Perform k-means clustering for \(k = 2, 3, 4\) and evaluate results.
  4. Use PCA to reduce dimensions and repeat k-means clustering.
  5. Compare clustering results before and after PCA.

- **Deliverables**:
  - R scripts and outputs for k-means clustering.
  - Confusion matrix and metrics: accuracy, precision, recall.
  - PCA analysis and transformed dataset clustering.

### Energy Forecasting Part
- **Dataset**: Daily electricity consumption data for the University Building at 115 New Cavendish Street (2018-2019).
- **Tasks**:
  1. Implement MLP Neural Networks using autoregressive (AR) and NARX approaches.
  2. Normalize input/output matrices.
  3. Experiment with different network structures (hidden layers, nodes, activation functions).
  4. Evaluate models using RMSE, MAE, and MAPE indices.
  5. Visualize prediction results and compare efficiency of different models.

- **Deliverables**:
  - R scripts for MLP implementation.
  - Performance comparison tables for various models.
  - Graphical plots of predictions vs actual data.

---

## Repository Structure
```
|-- datasets/
    |-- whitewine_v2.xls
    |-- UoW_load.xlsx
|-- src/
    |-- clustering_analysis.R
    |-- energy_forecasting.R
|-- results/
    |-- clustering_outputs/
    |-- forecasting_outputs/
|-- docs/
    |-- coursework_report.pdf
    |-- appendices/
        |-- full_code.R
|-- README.md
```

---

## Getting Started

### Prerequisites
- **Software**: R version 4.0+ and RStudio.
- **R Libraries**:
  - `ggplot2`
  - `cluster`
  - `factoextra`
  - `NbClust`
  - `neuralnet`

### Installation
1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/ml-datamining-coursework.git
   ```
2. Navigate to the repository directory:
   ```bash
   cd ml-datamining-coursework
   ```
3. Install required R libraries using the provided script:
   ```R
   source("src/install_packages.R")
   ```

---

## Usage

### Clustering Analysis
1. Open `src/clustering_analysis.R` in RStudio.
2. Run the script to:
   - Pre-process the white wine dataset.
   - Perform k-means clustering.
   - Apply PCA and re-run clustering.
3. View outputs in the `results/clustering_outputs/` folder.

### Energy Forecasting
1. Open `src/energy_forecasting.R` in RStudio.
2. Run the script to:
   - Train and test MLP models using AR and NARX approaches.
   - Generate statistical performance indices.
3. View outputs in the `results/forecasting_outputs/` folder.

---

## Evaluation
The coursework will be evaluated based on:
- Clustering implementation and results.
- MLP model development and testing.
- Discussion and justification of methodological decisions.
- Presentation of findings in the coursework report.

---

## References
- Relevant literature and resources are cited within the report and code comments.
- Dataset references: Provided by University of Westminster Estates Planning & Services Department.

---

## License
This project is for academic use only and is subject to University of Westminster assessment regulations.

---

## Contact
For queries, please contact the module leader or teaching assistant via the University of Westminster Blackboard portal.
