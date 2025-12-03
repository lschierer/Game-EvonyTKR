#!/usr/bin/env python3
"""
Predict conflicts for all general pairs and export to JSON.

Usage:
    python bin/predict_conflicts.py --model=conflict_model.pkl --pairs=all_pairs.csv --output=conflicts.json

Requirements:
    pip install scikit-learn pandas numpy
"""

import argparse
import json
import pickle
import sys
from pathlib import Path

import numpy as np
import pandas as pd

def load_model(model_path):
    """Load trained model and metadata."""
    with open(model_path, 'rb') as f:
        model_data = pickle.load(f)

    print(f"Loaded model version {model_data['version']}")
    print(f"Features: {len(model_data['feature_names'])}")

    return model_data['model'], model_data['feature_names']

def load_pairs_data(csv_path, expected_features):
    """Load pairs data for prediction."""
    df = pd.read_csv(csv_path)

    # First two columns are general names
    general1 = df.iloc[:, 0].values
    general2 = df.iloc[:, 1].values

    # Rest are features
    feature_cols = df.columns[2:]

    if len(feature_cols) != len(expected_features):
        print(f"Warning: Feature count mismatch!")
        print(f"  Expected: {len(expected_features)}")
        print(f"  Got: {len(feature_cols)}")
        sys.exit(1)

    X = df[feature_cols].values

    # Handle NaN values
    nan_count = np.isnan(X).sum()
    if nan_count > 0:
        print(f"Replacing {nan_count} NaN values with 0...")
        X = np.nan_to_num(X, nan=0.0, posinf=0.0, neginf=0.0)

    print(f"Loaded {len(df)} general pairs for prediction")

    return general1, general2, X

def predict_conflicts(model, general1, general2, X):
    """Predict conflicts and return structured data."""
    print("Making predictions...")
    predictions = model.predict(X)
    probabilities = model.predict_proba(X)[:, 1]  # probability of conflict

    conflicts_count = sum(predictions == 1)
    compatible_count = sum(predictions == 0)

    print(f"Predicted conflicts: {conflicts_count}")
    print(f"Predicted compatible: {compatible_count}")

    # Build lookup structure
    conflicts = {}

    for i, (g1, g2, pred, prob) in enumerate(zip(general1, general2, predictions, probabilities)):
        # Create bidirectional lookup
        if g1 not in conflicts:
            conflicts[g1] = {}
        if g2 not in conflicts:
            conflicts[g2] = {}

        # Store conflict status and confidence
        conflict_info = {
            'conflict': bool(pred == 1),
            'confidence': float(prob) if pred == 1 else float(1 - prob)
        }

        conflicts[g1][g2] = conflict_info
        conflicts[g2][g1] = conflict_info

    return conflicts

def save_json(conflicts, output_path):
    """Save conflicts to JSON file."""
    with open(output_path, 'w') as f:
        json.dump(conflicts, f, indent=2)

    print(f"\nSaved conflicts to {output_path}")
    print(f"Total generals in lookup: {len(conflicts)}")

def main():
    parser = argparse.ArgumentParser(description='Predict conflicts for all general pairs')
    parser.add_argument('--model', required=True, help='Path to trained model (.pkl)')
    parser.add_argument('--pairs', required=True, help='Path to all_pairs.csv')
    parser.add_argument('--output', required=True, help='Output JSON file')

    args = parser.parse_args()

    # Check files exist
    if not Path(args.model).exists():
        print(f"Error: Model file not found: {args.model}")
        sys.exit(1)

    if not Path(args.pairs).exists():
        print(f"Error: Pairs file not found: {args.pairs}")
        sys.exit(1)

    # Load model
    model, feature_names = load_model(args.model)

    # Load pairs data
    general1, general2, X = load_pairs_data(args.pairs, feature_names)

    # Make predictions
    conflicts = predict_conflicts(model, general1, general2, X)

    # Save to JSON
    save_json(conflicts, args.output)

if __name__ == '__main__':
    main()
