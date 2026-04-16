# thesis-code
Code for thesis "Imputation Methods for Classification in the Presence of Missing Data"

## Repository structure and run order

The main code is contained in files **1. data_generation.ipynb** to **5. results.ipynb**.  
These numbers indicate the order in which the files should be run.

Run the files in this order:

1. data_generation.ipynb 
2. apply_missing.ipynb  
3. imputation.ipynb
4. classification.ipynb 
5. results.ipynb 

## Requirements

- Files **1. data_generation.ipynb**, **2. apply_missing.ipynb**, **4. classification.ipynb**, and **5. results.ipynb** require:
  - `scikit-learn==1.4.2`

- File **3. imputation.ipynb** requires:
  - `scikit-learn==1.1.3`
  - `missingpy==0.2.0`

You can install the required packages with pip.

## Additional folders

The folders:

- Low importance weights setting
- Medium importance weights setting
- High importance weights setting
- Low correlation setting

contain code for additional results and configurations.

To run these folders, follow the same order as the main code, starting from **1. data_generation.ipynb** and continuing to **5. results.ipynb**.  
Again, file **3. imputation.ipynb** requires `scikit-learn==1.1.3` and `missingpy==0.2.0`, while the other files require `scikit-learn==1.4.2`.

## Notes

This repository contains the full code used to generate the results in the thesis, including the complete, amputated and imputed datasets.

Plots included in the thesis are generated using the file **plots.R**.

Before running this R file, users will need to download the relevant result files and replace the existing file paths, since the code currently points CSV files stored in a local directory (C:/Users/evebe/...). 
