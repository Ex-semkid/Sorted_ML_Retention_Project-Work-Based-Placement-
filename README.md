# Sorted_ML_Retention_Project-Work-Based-Placement
All codes: Data cleaning, EDA, feature Engineering, preprocessing workflows, ML Evaluation etc. 


**------------ PROJECT CODE SUMMARY: SORTED APP 7-WEEK RETENTION PREDICTION -------------**

---

**LEVEL 1: DATA CLEANING AND COHORT CONSTRUCTION**
This is the foundation of the entire pipeline and arguably the most consequential stage analytically. 
The raw Sorted App export is ingested and processed to define a clean, analysis-ready cohort. 
Key decisions made here include applying inclusion and exclusion criteria, removing test accounts, 
users with fewer than three days of recorded activity, and those with incomplete outcome data due to 
export timing. The binary outcome variable, 7-week retention, is derived here from raw activity 
timestamps. Any errors or biases introduced at this stage propagate through every downstream model, 
so the logic is kept explicit and auditable. In R, this is handled primarily with tidyverse.

---

**LEVEL 2: EDA & FEATURE ENGINEERING**
About Nineteen predictors are constructed across three conceptual domains: baseline demographics, 
symptom severity and early engagement behaviour in weeks one and two. The most analytically 
meaningful features are the derived ones, PHQ-9 and GAD-7 change scores between baseline and 
week two, session frequency trajectory (week-2 sessions divided by week-0 sessions), and cumulative
modules completed. These dynamic features capture early response signal that static intake data cannot. 

---

**LEVEL 3: PREPROCESSING RECIPE**
Built using the recipes package within tidymodels. The recipe performs bag imputation for missing 
predictor values using bagged decision trees fitted on training data only, one-hot encoding of all 
categorical variables, removal of zero-variance predictors post-encoding, and downsampling of the 
majority class. The critical decision here is that downsampling is placed inside the recipe and 
applied only within cross-validation folds, not to the full training set beforehand. This 
distinction prevents data leakage and ensures that performance estimates from cross-validation 
reflect genuinely held-out data. Many applied ML pipelines get this wrong, so it is worth making 
explicit in any academic writeup.

---

**LEVEL 4: MODEL TRAINING AND HYPERPARAMETER TUNING**
Three candidate models are trained: Random Forest using the ranger engine, XGBoost using the xgboost
engine, and Elastic Net using glmnet. All three are wrapped in tidymodels workflows, pairing each 
model specification with the shared preprocessing recipe. Stratified 5-fold cross-validation is 
used throughout, with stratification on the outcome to preserve class proportions across folds. 
Hyperparameters for Random Forest and XGBoost are tuned using Bayesian optimisation via tune_bayes(),
which uses a Gaussian process surrogate model to intelligently navigate the hyperparameter space 
rather than exhaustively searching a grid. This is more efficient and typically finds better 
solutions with fewer iterations. Elastic Net uses a standard grid over penalty and mixture. 
The tuning objective is PR-AUC, chosen because it is the most appropriate metric under high class 
imbalance.

---

**LEVEL 5: MODEL EVALUATION**
Performance is assessed using collect_metrics() across all resamples, producing PR-AUC, ROC-AUC, 
F1-score, and Matthews Correlation Coefficient (MCC) for each candidate model. MCC is included 
because it is sensitive to all four cells of the confusion matrix and is a more informative 
single-number summary than accuracy or F1 alone under imbalance. ROC and PR curves are plotted 
using roc_curve() and pr_curve() with autoplot(). The confusion matrix is produced using conf_mat() 
and interpreted in clinical terms, specifically, the asymmetric cost of false negatives (missing a 
user who will churn and who might have benefited from outreach) versus false positives 
(flagging a retained user unnecessarily). 

---

**LEVEL 6: FEATURE IMPORTANCE**
Variable importance is estimated using permutation-based methods via vip::vi(), applied to the final
fitted Random Forest. Permutation importance works by randomly shuffling each predictor in turn and 
measuring the resulting drop in model performance, predictors causing the largest drop are most 
important. This is preferred over impurity-based importance (the default in many RF implementations)
because it is less biased toward high-cardinality variables and is model-agnostic in principle. 
Results are visualised as a ranked bar chart and interpreted against the four predictor domains, 
with clinical commentary on which features are actionable versus merely associative.

---

**LEVEL 7: FINAL MODEL EXPORT**
The best-performing model (Random Forest) is finalised by calling finalize_workflow() with the 
optimal hyperparameters, then fitted on the complete training set using last_fit(). The fitted 
workflow object is saved as final_model.rds using saveRDS(). This single object contains both the 
preprocessing recipe and the fitted model, meaning it can be loaded and applied to new data with a 
single predict() call, which is precisely how the clinical prediction System will work when deployed.

