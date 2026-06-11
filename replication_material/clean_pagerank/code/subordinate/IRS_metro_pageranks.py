import pandas as pd
import numpy as np
import networkx as nx
import os

# Use your local crosswalk path
crosswalk_file = os.path.join(wd, "data/raw/cbsa2fipsxw.csv")

# Read and construct full county FIPS and CBSA
crosswalk = pd.read_csv(crosswalk_file, dtype={'fipsstatecode': int, 'fipscountycode': int, 'cbsacode': int})
crosswalk['fips'] = crosswalk['fipsstatecode'] * 1000 + crosswalk['fipscountycode']
crosswalk = crosswalk[['fips', 'cbsacode']].rename(columns={'cbsacode': 'cbsa'}).dropna()
crosswalk = crosswalk.astype({'fips': int, 'cbsa': int})

# ------------------------------------------------------------------------------
# Run Metro-Level PageRank
# ------------------------------------------------------------------------------
source = 'irs'
label = 'Out'
rolling = 5
output_dir = os.path.join(wd, "data/rankings")
os.makedirs(output_dir, exist_ok=True)

for year in range(1990, 2022):
    try:
        print(f"Processing {year}...")

        st = str(year)[2:]
        end = str(year + 1)[2:]
        infile = os.path.join(wd, f"data/processed/{source}_{label}_{st}{end}.csv")
        df = pd.read_csv(infile)

        # Clean FIPS
        df = df[df['FIPS_Origin'].notna() & df['FIPS_Dest'].notna()]
        df['FIPS_Origin'] = df['FIPS_Origin'].astype(int)
        df['FIPS_Dest'] = df['FIPS_Dest'].astype(int)

        # Merge to CBSA (metro)
        df = df.merge(crosswalk.rename(columns={'fips': 'FIPS_Origin', 'cbsa': 'CBSA_Origin'}), on='FIPS_Origin', how='left')
        df = df.merge(crosswalk.rename(columns={'fips': 'FIPS_Dest', 'cbsa': 'CBSA_Dest'}), on='FIPS_Dest', how='left')

        # Aggregate to metro-metro level
        metro_flow = (
            df.groupby(['CBSA_Origin', 'CBSA_Dest'], as_index=False)
              .agg({'Return': 'sum'})
        )

        flow_outfile = os.path.join(
            wd,
            f"data/processed/irs_metro_flows_{year}.csv"
        )
        
        metro_flow.to_csv(flow_outfile, index=False)


        # Build directed graph and run PageRank
        edges = metro_flow.rename(columns={'CBSA_Origin': 'source', 'CBSA_Dest': 'target', 'Return': 'weight'})
        G = nx.from_pandas_edgelist(edges, edge_attr='weight', create_using=nx.DiGraph())
        ranks = nx.pagerank(G, alpha=0.85, weight='weight', max_iter=10000)

        # Format output
        rank_df = pd.DataFrame(list(ranks.items()), columns=['cbsa', 'rank'])
        rank_df = rank_df[rank_df['cbsa'] != 0]  # Drop unknowns again just in case

        # Save result
        outfile = f"{output_dir}/pagerank_{source}_{label}_{year}_metro.csv"
        rank_df.to_csv(outfile, index=False)
        print(f"Saved PageRank for {year}: {outfile}")

    except Exception as e:
        print(f"Failed for {year}: {e}")


