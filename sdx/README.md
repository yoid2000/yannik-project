
There are two modes for this code, one where the original data is filtered before anonymization (pre_filtered), and one where it is filtered after (post_filtered). The two modes produce two different sets of synthetic tables in two different directories.

In the case with of post-filtered, the workflow is:

1.  `Rscript preprocessing_syndiffix_cleaned.R` --> `preprocess_syndiffix_data.csv`.
2.  `python build_syndiffix_tables.py` --> `sdx_tables/<all synthetic files>`
3.  `Rscript evaluation_syndiffix_cleaned.R` --> output

In the case of pre-filtered, the workflow is the same, except that the term "pre_filter" is added as a command line variable:

1.  `Rscript preprocessing_syndiffix_cleaned.R pre_filter` --> `preprocess_syndiffix_data_filtered.csv`.
2.  `python build_syndiffix_tables.py pre_filter` --> `sdx_tables_filtered/<all synthetic files>`
3.  `Rscript evaluation_syndiffix_cleaned.R pre_filter` --> output

Notes:

Currently `preprocessing_syndiffix_cleaned.R`assumes that the input csv file is called 'test_data.csv'. Modify the R script by hand as needed to read in the real data.

The file `columns_list_python.json` contains all of the column combinations that need to be built by SynDiffix. This file was built by hand.