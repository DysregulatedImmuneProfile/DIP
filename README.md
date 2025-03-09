# DIP - A Machine Learning Framework for Predicting the Degree of Immune Dysregulation 

🚀 IP (Dysregulated Immune Profile) is an open-source machine-learning framework designed to predict the degree of immune dysregulation in patients with an infection using just three biomarkers:

- ✅ **Procalcitonin (PCT)**
- ✅ **Interleukin-6 (IL-6)**
- ✅ **Soluble TREM-1 (sTREM-1)**

This framework enables precise quantification of immune dysregulation, stratifying patients into Dysregulated Immune Profiles (DIP stages) or a continuous dysregulation scale. DIP stages and cDIP scores can be directly compared across cohorts and infections, as they are based on absolute biomarker concentrations without the need for scaling or transformation. Validated in multiple independent cohorts, this tool has promising applications in precision immunomodulatory therapy.

For questions, licensing, or commercial use, contact: 📧 E.H.A. Michels – e.h.michels@amsterdamumc.nl
If you use this package, please cite: Michels, E.H.A. (2024). xxxxx [Software]. DOI/Repository Link (if applicable)

---

## 🩸 **Why Is This Informative?**
- 📈 Higher values of DIP/cDIP, both at admission and over time, are associated with increased mortality and secondary infections—independent of clinical severity.
- ⏳ Monitoring the DIP or cDIP score over time provides critical prognostic insights, helping to track disease progression.
- 🔬 It enables continuous immune system monitoring, offering a dynamic assessment of host response dysregulation rather than a single static measurement.
- 🏥 It can help evaluate the effectiveness of immunomodulatory treatments over time, identifying patients who may benefit or fail to respond to interventions.
- ⚕️ Reanalysis of a corticosteroid RCT showed that patients with similar clinical severity had different immune dysregulation profiles, and only those with a high DIP/cDIP benefited from corticosteroids.
- 🔍 This highlights the potential of DIP for precision-guided immunotherapy, ensuring that treatments are targeted to the right patients at the right time.

---

## 🔍 **DIP_stage vs. cDIP: Key Differences**

| Feature              | **DIP_stage** 🏷️ | **cDIP** 📈 |
|----------------------|------------------|-------------|
| **Output Type**     | Categorical (DIP1, DIP2, DIP3) | Continuous (0-1 scale) |
| **Ease of Interpretation** | ✅ **Clear-cut** – Well-defined immune dysregulation stages | ⚡ **More nuanced** – Detects small immune shifts |
| **Model Type**      | Extreme Gradient Boosting (XGBoost) | Random Forest Regressor |
| **Best For**        | **Stratification & Prognosis** – Groups patients into distinct immune dysregulation stages | **More Detailed Prognosis** – Tracks immune dysregulation trends more precisely |
| **Granularity**     | **Grouped categories** – Easier for clinical decision-making | **Higher precision** – Differentiates borderline cases |
| **Prognostic Power** | ✅ **Strong** – Predicts mortality & secondary infections | 🔥 **More sensitive** – Captures subtle changes in immune state over time |
| **Corticosteroid Response** | ✅ Identifies responders vs. non-responders | ✅ **Better differentiation** in borderline responders |
| **Tracking Over Time** | ✅ **Useful for monitoring**, but stage transitions may be sudden | ✅ **More gradual tracking** – Detects improvement/deterioration even within a stage |
| **Clinical Insights** | Helps categorize patients & predict outcomes | Provides **continuous assessment** of immune progression & treatment effects |
| **Technical Requirements** | Runs in **R only** | Requires **Python (via reticulate)** |



---

### To install DIP ###
- Install devtools if not installed

```r
install.packages("devtools")
devtools::install_github("ErikMichels/DIP")
```

⚠️ The cDIP function **requires** Python. Please install Python at https://www.python.org/downloads/. During the installation make sure you TICK the box of 'Add Python to PATH' prior to pressing 'install now'

---

## 🧬 How Does It Work?

In infectious patients, immune dysregulation varies independently from clinical severity.  
Traditional scoring systems fail to accurately reflect immune state, limiting their utility in guiding immunomodulatory treatments.

### 🔹 The DIP Framework Solves This By:
- Modeling immune dysregulation using 35 biomarkers reflecting key host response domains: inflammation, coagulation, and endothelial cell activation. These biomarkers were measured in pneumonia patients with and without sepsis across different care settings, including the emergency room (ER), general ward, and intensive care unit (ICU).
- Deriving a simplified three-biomarker machine-learning model that accurately predicts the full 35-biomarker profile.
- Stratifying patients into:
  - DIP1 (minor dysregulation)
  - DIP2 (moderate dysregulation)
  - DIP3 (major dysregulation)
- Providing a continuous dysregulation score (cDIP) for more granular assessment.

### 🔹 Two Independent Machine Learning Models
- The **stage-based DIP model** (DIP_stage) uses an extreme gradient boosting decision tree (XGBoost) to classify patients into DIP1, DIP2, or DIP3.
- The **continuous dysregulation model** (cDIP) uses a random forest regressor to provide a continuous immune dysregulation score between 0 and 1.
- Both models were originally built in Python but are now fully accessible in R by one line of code.
- The cDIP model requires a Python installation, as it uses reticulate to run the machine-learning model in a Python environment within R.


---

## 📌 Model 1: DIP stage

### 🔹 DIP_stage: Stage-Based Immune Dysregulation Prediction

#### 🧪 Input:
- A data frame with three biomarker concentrations:
  - Procalcitonin (PCT)
  - Interleukin-6 (IL-6)
  - Soluble TREM-1 (sTREM-1)
- Measurements should be in **pg/mL**, untransformed, and unscaled.

#### 🖥 Output:
- DIP stage classification: **DIP1 (Minor), DIP2 (Moderate), or DIP3 (Major)**
- Prediction probabilities for each stage
- Interactive plots:
  - **3D scatter plot** of DIP probabilities
  - **Pie chart** showing the distribution of predicted stages

#### How are DIP stages assigned?

- DIP stages are assigned based on the highest probability predicted by the model.
- Since DIP is an ordered factor, patients with high probabilities for two adjacent stages (e.g., DIP1 and DIP2) are likely at the upper range of DIP1, approaching DIP2 and vice versa.

#### 💻 How to Use DIP_stage
```r
# Load the package
library(DIP)

# Create test data
test_data <- data.frame(
  ID =  1:20,
  TREM_1 = c(182, 400, 1000, 560, 230, 900, 450, 710, 620, 350, 150, 800, 250, 490, 780, 340, 900, 1100, 220, 510),
  IL_6 = c(70, 5, 10000, 450, 88, 3000, 150, 680, 740, 50, 30, 600, 120, 470, 800, 60, 5000, 9000, 33, 200),
  Procalcitonin = c(877, 66, 20000, 1500, 500, 10000, 800, 2700, 1800, 460, 250, 12000, 600, 1100, 14000, 350, 15000, 18000, 310, 900))

# Run DIP_stage to predict immune dysregulation stage
DIP_stage(test_data)

```
📊 Results will be saved in the global environment (DIP_stage_results) along with the generated plots (DIP_stage_piechart, DIP_stage_3D).

---
## 📌 Model 2 cDIP

### 🔹 cDIP: Continuous Immune Dysregulation Scale

📈 Instead of discrete stages (DIP1-3), **cDIP provides a continuous immune dysregulation score ranging from 0 to 1**.

### ⚠️ Requirement
- The cDIP function requires Python. Please install Python at https://www.python.org/downloads/.
- During the installation make sure you tick the box of 'Add Python to PATH' prior to pressing 'install now'

#### 🧪 Input:
- Same **3-biomarker input** as DIP_stage.

#### 🖥 Output:
- **cDIP Score (0-1):** Higher values indicate greater immune dysregulation.
- **Interactive beeswarm plot** to visualize dysregulation distribution.

#### 💻 How to Use cDIP
```r
# Load the package
library(DIP)

# Run cDIP to get continuous immune dysregulation scores
cDIP_results <- cDIP(test_data)

```
📊 Results will be saved in the global environment (cDIP_results) along with an interactive beeswarm plot (cDIP_plot).

----

## 📖 Reference values from the main paper

![Reference values](https://github.com/user-attachments/assets/08caa30b-391f-4469-a03d-8b70dd6a468a)

----

## 📌 Key Applications

- 🔬 **Research:** Helps study immune responses in **sepsis, pneumonia, and infections**.
- 🏥 **Clinical Trials:** Stratifies patients based on **immune dysregulation** for **personalized immunotherapy**.
- 🧑‍⚕️ **Precision Medicine:** Guides **immunomodulatory treatments** (e.g., corticosteroids in pneumonia).

---

## ⚠️ Disclaimer

🚨 **For Research Use Only** 🚨  
This tool is intended **exclusively for research and academic purposes**.

- 🚫 **Not for clinical decision-making, patient diagnosis, or treatment guidance.**  
- 🚫 **Not a substitute for professional medical judgment.**  
- 🚫 **Not validated for direct clinical care** – use **only in research settings**.  
