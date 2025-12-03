#!/usr/bin/env python3
"""
Train an XGBoost model to predict general conflicts based on builtin book features.
"""
import sys
import subprocess
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
import xgboost as xgb

# Extract features using Perl script
print("Extracting features from general pairs...")
result = subprocess.run(
    ['perl', 'extract_features.pl'],
    cwd='.',
    capture_output=True,
    text=True
)

if result.returncode != 0:
    print(f"Error extracting features: {result.stderr}", file=sys.stderr)
    sys.exit(1)

# Load features into DataFrame
from io import StringIO
df = pd.read_csv(StringIO(result.stdout))

print(f"\nDataset loaded: {len(df)} pairs")
print(f"  Working pairs (label=0): {(df['label'] == 0).sum()}")
print(f"  Conflicting pairs (label=1): {(df['label'] == 1).sum()}")

# Separate features and labels
X = df.drop('label', axis=1)
y = df['label']

# Split into train/test
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

print(f"\nTraining set: {len(X_train)} pairs")
print(f"Test set: {len(X_test)} pairs")

# Train XGBoost model
print("\nTraining XGBoost model...")
model = xgb.XGBClassifier(
    max_depth=6,
    learning_rate=0.1,
    n_estimators=100,
    objective='binary:logistic',
    eval_metric='error',
    random_state=42
)

model.fit(X_train, y_train)

# Evaluate on test set
print("\nEvaluating on test set...")
y_pred = model.predict(X_test)
accuracy = accuracy_score(y_test, y_pred)

print(f"\nTest Accuracy: {accuracy:.1%}")
print("\nClassification Report:")
print(classification_report(y_test, y_pred, 
                          target_names=['Compatible', 'Conflict']))

print("\nConfusion Matrix:")
cm = confusion_matrix(y_test, y_pred)
print(f"                 Predicted")
print(f"                 Compat  Conflict")
print(f"Actual Compat    {cm[0][0]:6d}  {cm[0][1]:8d}")
print(f"       Conflict  {cm[1][0]:6d}  {cm[1][1]:8d}")

# Feature importance
print("\nFeature Importance:")
importance = pd.DataFrame({
    'feature': X.columns,
    'importance': model.feature_importances_
}).sort_values('importance', ascending=False)

for _, row in importance.iterrows():
    print(f"  {row['feature']:25s}: {row['importance']:.3f}")

# Save model
print("\nSaving model to conflict_model.json...")
model.save_model('conflict_model.json')
print("Done!")

# Cross-validation for more robust estimate
print("\nPerforming 5-fold cross-validation...")
from sklearn.model_selection import cross_val_score
cv_scores = cross_val_score(model, X, y, cv=5, scoring='accuracy')
print(f"CV Accuracy: {cv_scores.mean():.1%} (+/- {cv_scores.std() * 2:.1%})")
