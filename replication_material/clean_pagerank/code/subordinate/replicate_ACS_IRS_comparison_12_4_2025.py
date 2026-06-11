import pandas as pd
import numpy as np
import networkx as nx
import os

# ---------------------------------------------------------------------
# 1.  File paths
# ---------------------------------------------------------------------
os.chdir(r"G:\.shortcut-targets-by-id\1boJDCakyAAS94F5KjSRVfXd4ICmDvdRP\Measuring and Pricing Neighborhood Characteristics\clean_pagerank\code")

crosswalk_file = r"..\data\raw\cbsa2fipsxw.csv"
acs_file       = (r"..\data\processed\total_flows_metro_rolling5_2019.csv")  # <─ NEW

# ---------------------------------------------------------------------
# 2.  Read the list of CBSAs that appear in ACS (col = MET2013, int)
# ---------------------------------------------------------------------
keep_cbsa = (
    pd.read_csv(acs_file, usecols=['MET2013'])
      .MET2013.dropna().astype(int).unique()
)
keep_cbsa = set(keep_cbsa)          # quick membership test later

# ---------------------------------------------------------------------
# 3.  Read county-to-CBSA cross-walk
#     • Keep the CBSA code only if it is in `keep_cbsa`
# ---------------------------------------------------------------------
cw = pd.read_csv(
        crosswalk_file,
        dtype={'fipsstatecode': int,
               'fipscountycode': int,
               'cbsacode': int}
     )

cw['fips'] = cw['fipsstatecode'] * 1000 + cw['fipscountycode']
cw['cbsacode'] = cw['cbsacode'].fillna(0).astype(int)

# retain CBSA code **only** if it occurs in ACS; else set to 0
cw['cbsa'] = cw['cbsacode'].where(cw['cbsacode'].isin(keep_cbsa), 0)
cw = cw[['fips', 'cbsa']].astype(int)

# ---------------------------------------------------------------------
# 4.  Everything below (loop, PageRank, saving) can stay the same
# ---------------------------------------------------------------------
source      = 'irs'
label       = 'Out'
rolling     = 5
output_dir  = "../data/irs_acs_subset"
os.makedirs(output_dir, exist_ok=True)

for year in range(1990, 2022):
    try:
        print(f"Processing {year}…")

        st   = str(year)[2:]
        end  = str(year + 1)[2:]
        in_f = f"../data/processed/{source}_{label}_{st}{end}.csv"
        df   = pd.read_csv(in_f)

        # Clean FIPS
        df = df[df['FIPS_Origin'].notna() & df['FIPS_Dest'].notna()]
        df['FIPS_Origin'] = df['FIPS_Origin'].astype(int)
        df['FIPS_Dest']   = df['FIPS_Dest'].astype(int)

        # Merge to CBSA (metros + micros that appear in ACS)
        df = (
            df.merge(cw.rename(columns={'fips': 'FIPS_Origin', 'cbsa': 'CBSA_Origin'}),
                     on='FIPS_Origin', how='left')
              .merge(cw.rename(columns={'fips': 'FIPS_Dest', 'cbsa': 'CBSA_Dest'}),
                     on='FIPS_Dest',   how='left')
        )

        # Drop rows whose origin or destination CBSA is 0 (not in ACS)
        df = df[(df['CBSA_Origin'] != 0) & (df['CBSA_Dest'] != 0)]

        # Aggregate county flows to CBSA–CBSA flows
        metro_flow = (
            df.groupby(['CBSA_Origin', 'CBSA_Dest'], as_index=False)
              .agg({'Return': 'sum'})
        )

        # Build directed graph and run PageRank
        edges = metro_flow.rename(columns={
                    'CBSA_Origin': 'source',
                    'CBSA_Dest':   'target',
                    'Return':      'weight'}
                )
        G      = nx.from_pandas_edgelist(edges, edge_attr='weight', create_using=nx.DiGraph())
        ranks  = nx.pagerank(G, alpha=0.85, weight='weight', max_iter=10_000)

        rank_df = pd.DataFrame(ranks.items(), columns=['cbsa', 'rank'])
        out_f   = f"{output_dir}/pagerank_{source}_{label}_{year}_acs_subset.csv"
        rank_df.to_csv(out_f, index=False)
        print(f"Saved PageRank for {year}: {out_f}")

    except Exception as e:
        print(f"Failed for {year}: {e}")


