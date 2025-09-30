#!/usr/bin/env python3
# coding=utf-8
"""
Python script to generate a directory containing all necessary files for a given universal.

Note that this script will not handle the analysis across the full posterior due to computational complexity.
"""
import shutil
from pathlib import Path

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Generates Analysis Directories.')
    parser.add_argument("analysis", help='analysis directory', type=Path)
    parser.add_argument("output", help='output directory', type=Path)
    args = parser.parse_args()
    
    if not args.analysis.exists():
        raise IOError("Unable to find analysis directory")
    
    if args.output.exists():
        raise IOError(f"Output directory {args.output} already exists - aborting!")
    args.output.mkdir()
    
    # copy analysis files:
    for f in ['brms.R', 'brms_spatfam.R', 'brms_spatphylo.R', 'Glottolog_Languages.csv', 'varcov.spatial_function.R']:
        f = args.analysis.parent / f
        if not f.exists():
            raise IOError(f"Cannot find {f} - make sure you are running this from the root directory of the git repository")
        shutil.copy2(f, args.output / f.name)
        
    # copy data files:
    for f in ['BT_data.txt', 'pruned_tree.tree']:
        f = args.analysis / f
        if not f.exists():
            raise IOError(f"Cannot find {f} in directory {args.analysis}. Make sure you are running this from the root directory of the git repository")
        shutil.copy2(f, args.output / f.name)
        
