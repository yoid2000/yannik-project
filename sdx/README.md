
Workflow:

`preprocessing_syndiffix_cleaned.R` reads in the input csv file and generates `preprocess_syndiffix_data.csv`. Currently `preprocessing_syndiffix_cleaned.R`assumes that the input csv file is called 'test_data.csv'. Modify as needed to read in the real data.

The file `columns_list_python.json` contains all of the column combinations that need to be built by SynDiffix. This file was built by hand.

`build_syndiffix_tables.py` generates the SynDiffix synthetic datasets required by `evaluation_syndiffix_cleaned.R`. It reads in the `columns_list_python.json` and the file `preprocess_syndiffix_data.csv` that was generated with `preprocessing_syndiffix_cleaned.R`, creates the directory `sdx_tables`, and places all of the synthetic datasets there. It also creates the file `filekeys.json` in `sdx_tables`. This contains a mapping of the file keys to file names.

`evaluation_syndiffix_cleaned.R` uses the files in `sdx_tables` for its evaluation. It knows which files has which tables from `filekeys.json`.