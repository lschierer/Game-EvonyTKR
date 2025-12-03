#!/usr/bin/env python3
"""
Train a conflict prediction model using gradient boosting.

Usage:
    python bin/train_conflict_model.py --training=training_data.csv --model=conflict_model.pkl

Requirements:
    pip install scikit-learn pandas numpy
"""

import argparse
import pickle
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.model_selection import cross_val_score, train_test_split
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score

def load_training_data(csv_path):
    """Load and prepare training data from CSV."""
    df = pd.read_csv(csv_path)

    # Separate features and labels
    # First two columns are general names, last column is label
    feature_cols = df.columns[2:-1]
    X = df[feature_cols].values
    y = df['label'].values

    # Check for NaN values
    nan_count = np.isnan(X).sum()
    if nan_count > 0:
        print(f"Warning: Found {nan_count} NaN values in features")
        print("Replacing NaN with 0...")
        X = np.nan_to_num(X, nan=0.0, posinf=0.0, neginf=0.0)

    print(f"Loaded {len(df)} training examples")
    print(f"  Conflicts: {sum(y == 1)}")
    print(f"  Compatible: {sum(y == 0)}")
    print(f"Features: {len(feature_cols)}")

    return X, y, feature_cols.tolist()

def train_model(X, y, feature_names):
    """Train gradient boosting classifier."""
    # Split data for validation
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42, stratify=y
    )

    print("\nTraining gradient boosting classifier...")

    # Train model with reasonable hyperparameters
    model = GradientBoostingClassifier(
        n_estimators=200,
        learning_rate=0.1,
        max_depth=5,
        min_samples_split=10,
        min_samples_leaf=5,
        subsample=0.8,
        random_state=42,
        verbose=1
    )

    model.fit(X_train, y_train)

    # Evaluate on test set
    y_pred = model.predict(X_test)
    accuracy = accuracy_score(y_test, y_pred)

    print(f"\nTest Set Performance:")
    print(f"Accuracy: {accuracy:.3f}")
    print("\nClassification Report:")
    print(classification_report(y_test, y_pred, target_names=['Compatible', 'Conflict']))
    print("\nConfusion Matrix:")
    print(confusion_matrix(y_test, y_pred))

    # Cross-validation on full dataset
    print("\n5-Fold Cross-Validation:")
    cv_scores = cross_val_score(model, X, y, cv=5, scoring='accuracy')
    print(f"CV Accuracy: {cv_scores.mean():.3f} (+/- {cv_scores.std() * 2:.3f})")

    # Retrain on full dataset for final model
    print("\nRetraining on full dataset...")
    model.fit(X, y)

    # Show feature importance
    print("\nTop 10 Most Important Features:")
    feature_importance = pd.DataFrame({
        'feature': feature_names,
        'importance': model.feature_importances_
    }).sort_values('importance', ascending=False)

    for idx, row in feature_importance.head(10).iterrows():
        print(f"  {row['feature']}: {row['importance']:.4f}")

    return model, feature_importance

def save_model(model, feature_names, output_path):
    """Save trained model and metadata."""
    model_data = {
        'model': model,
        'feature_names': feature_names,
        'version': '1.0'
    }

    with open(output_path, 'wb') as f:
        pickle.dump(model_data, f)

    print(f"\nModel saved to {output_path}")

def main():
    parser = argparse.ArgumentParser(description='Train conflict prediction model')
    parser.add_argument('--training', required=True, help='Path to training_data.csv')
    parser.add_argument('--model', required=True, help='Output path for trained model (.pkl)')
    parser.add_argument('--importance', help='Optional: save feature importance CSV')

    args = parser.parse_args()

    # Check if training file exists
    if not Path(args.training).exists():
        print(f"Error: Training file not found: {args.training}")
        sys.exit(1)

    # Load data
    X, y, feature_names = load_training_data(args.training)

    # Train model
    model, feature_importance = train_model(X, y, feature_names)

    # Save model
    save_model(model, feature_names, args.model)

    # Optionally save feature importance
    if args.importance:
        feature_importance.to_csv(args.importance, index=False)
        print(f"Feature importance saved to {args.importance}")

if __name__ == '__main__':
    main()
