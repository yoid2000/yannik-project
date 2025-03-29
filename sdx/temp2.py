import pandas as pd

# Step 1: Read in the file at sdx/sdx_tables/EF21_TS4.csv as dataframe df_syn
df_syn = pd.read_csv("sdx_tables/EF21_TS4.csv")

# Step 2: Read in the file at test_data.csv as dataframe df_orig
df_orig = pd.read_csv("test_data.csv")

# Step 3: Define a function to process and display the required information
def process_and_display_interleaved(df_orig, df_syn):
    # Get the unique groups from both dataframes
    unique_groups = set(df_orig["TAETIGKEITSSCHLUESSEL4"].unique()).union(
        df_syn["TAETIGKEITSSCHLUESSEL4"].unique()
    )
    
    for group in unique_groups:
        print(f"Group: {group}\n")
        
        # Process df_orig for the current group
        if group in df_orig["TAETIGKEITSSCHLUESSEL4"].values:
            group_data_orig = df_orig[df_orig["TAETIGKEITSSCHLUESSEL4"] == group]
            print("df_orig:")
            
            # First 10 values of EF21, sorted descending
            if "EF21" in group_data_orig.columns:
                sorted_values_orig = group_data_orig["EF21"].sort_values(ascending=False).head(10)
                print("First 10 values of EF21 (sorted descending):")
                print(sorted_values_orig)
            else:
                print("Column EF21 not found in df_orig.")
            
            # Description of EF21
            if "EF21" in group_data_orig.columns:
                print("\nDescription of EF21:")
                print(group_data_orig["EF21"].describe())
            else:
                print("Column EF21 not found in df_orig.")
        else:
            print("Group not found in df_orig.")
        
        print("\n" + "-" * 50 + "\n")
        
        # Process df_syn for the current group
        if group in df_syn["TAETIGKEITSSCHLUESSEL4"].values:
            group_data_syn = df_syn[df_syn["TAETIGKEITSSCHLUESSEL4"] == group]
            print("df_syn:")
            
            # First 10 values of EF21, sorted descending
            if "EF21" in group_data_syn.columns:
                sorted_values_syn = group_data_syn["EF21"].sort_values(ascending=False).head(10)
                print("First 10 values of EF21 (sorted descending):")
                print(sorted_values_syn)
            else:
                print("Column EF21 not found in df_syn.")
            
            # Description of EF21
            if "EF21" in group_data_syn.columns:
                print("\nDescription of EF21:")
                print(group_data_syn["EF21"].describe())
            else:
                print("Column EF21 not found in df_syn.")
        else:
            print("Group not found in df_syn.")
        
        print("\n" + "=" * 50 + "\n")

# Call the function to process and display interleaved results
process_and_display_interleaved(df_orig, df_syn)