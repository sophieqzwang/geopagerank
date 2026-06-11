import os
import sys
import pandas as pd

# -------------------------------
# CONFIGURATION
# -------------------------------
BASE_DIR = os.path.join(wd, "data/processed") 
OUTPUT_DIR = os.path.join(wd, "data/rankings") 
SCRIPT_DIR = os.path.join(wd, "code")

# Add the pagerank script
sys.path.append(SCRIPT_DIR)

try:
    from geo_pagerank import pagerank_from_acs
except ImportError:
    import networkx as nx

    def pagerank_from_acs(file_path, source_col, dest_col, weight_col, d=0.85):
        df = pd.read_csv(file_path)
        edges = df[[source_col, dest_col, weight_col]].rename(
            columns={source_col: "source", dest_col: "target", weight_col: "weight"}
        )
        
        edges = edges.loc[edges["weight"] > 0] # added 3/13/2026 
        
        G = nx.from_pandas_edgelist(edges, edge_attr="weight", create_using=nx.DiGraph())
        pr = nx.pagerank(G, alpha=d, weight="weight", max_iter=50000)
        pr_df = pd.DataFrame(pr.items(), columns=["metro", "pagerank"])
        pr_df = pr_df.sort_values("pagerank", ascending=False).reset_index(drop=True)
        return pr_df

# -------------------------------
# PROCESS EACH YEAR
# -------------------------------
files_by_group = {
    "total": "total_flows_metro_rolling5_{}.csv",
    "age": "age_flows_metro_rolling5_{}.csv",
    "educ": "education_flows_metro_rolling5_{}.csv",
    "race": "race_flows_metro_rolling5_{}.csv",
    "industry": "industry_flows_metro_rolling5_{}.csv",
    "kids": "flow_kids_rolling5_{}.csv",
    "retired": "flow_retired_rolling5_{}.csv",
    "tenure": "flow_tenure_rolling5_{}.csv"
}

def run_year(year):
    print(f"\n========== {year} ==========")
    for group, filename_template in files_by_group.items():
        file_path = os.path.join(BASE_DIR, filename_template.format(year))
        try:
            df = pd.read_csv(file_path)
        except FileNotFoundError:
            print(f"Skipping missing: {file_path}")
            continue

        for col in df.columns:
            if col not in ["MIGMET131", "MET2013"]:
                print(f"  PageRank on {group}_{col}...")
                pr_df = pagerank_from_acs(
                    file_path=file_path,
                    source_col="MIGMET131",
                    dest_col="MET2013",
                    weight_col=col
                )
                out_name = f"{filename_template.format(year).replace('.csv', '')}_pagerank_{group}_{col}.csv"
                out_path = os.path.join(OUTPUT_DIR, out_name)
                pr_df.to_csv(out_path, index=False)
                print(f"    Saved: {out_path}")

if __name__ == "__main__":
    for year in range(2005, 2020):  # 5-year windows: 2005–2009 to 2019–2023
        run_year(year)
