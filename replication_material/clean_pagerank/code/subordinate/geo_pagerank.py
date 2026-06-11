'''
    Filename:geo_pagerank.py
    Project: Housing Amenities Pagerank
    Purpose: Function that runs page rank
    Author: Shusheng Zhong
'''

################################################################################
# 0. Import Necessary Packages and Collect Parameters
################################################################################
import pandas as pd
import numpy as np
import networkx as nx
import requests
import matplotlib.pyplot as plt

"""
Parameters:

"""



################################################################################
# 1. Define PageRank Function
################################################################################
def pagerank(year, source, label="Out", num_iter=10000, d=0.85,
             self_loop=False):
    """
    Purpose:    Given a year-change (1819, for example) and label
                Import and Clean The Migration Flow
                Then Run PageRank With It

    Parameters: 1. year: specify the year vintage of IRS county-to-county migration
                    flow data (example: 0910 means using the year vintage 2009-2010)
                2. label: "outflow"/"inflow" if using the IRS county-to-county migration outflow/inflow data
                3. num_iter: number of iteration for manual page rank, default=10000
                4. method: "manual" if using the hard-coded pagerank function
                           "nx" if using the built-in pagerank from nx
    """

    start_year=str(year)
    end_year=str(year+1)
    st=start_year[2:]
    end=end_year[2:]
    print('-'*80)
    print(f'Start Running PageRank for {start_year}-{end_year} using {source} Out{label}')
    print(f'Specfication: dampening factor: {d}')

    # 2.1 Import IRS Migration flow file as a dataframe object: mig_flow
    filepath = os.path.join(wd, "data", "processed", f"{source}_{label}_{st}{end}.csv")
    output_file = os.path.join(wd, "data", "rankings", f"pagerank_{source}_{label}_{year}.csv")
    mig_flow = pd.read_csv(filepath, encoding='latin-1')

    
    # 2.2 Run PageRank

    edges = pd.DataFrame(
        {"source": mig_flow['FIPS_Origin'],
         "target": mig_flow['FIPS_Dest'],
         "weight": mig_flow['Return'],
        }
    )

    DG = nx.from_pandas_edgelist(edges, edge_attr=["weight"], create_using=nx.DiGraph())

    v=pd.DataFrame.from_dict(nx.pagerank(DG, alpha=d,
                                         personalization=None, max_iter=50000,
                                         nstart=None, weight='weight', dangling=None), orient="index")
    v=v.rename({0: 'rank'}, axis='columns')
    v=v.sort_values(by=['rank'], ascending=False)


    print('-'*80)
    print("Sum of PageRank Result: ", v['rank'].sum())

    print('-'*80)
    print("Top 10 Highest Ranked Counties:")
    print(v[:10])

    print('-'*80)
    print("Top 10 Lowest Ranked Counties:")
    print(v[-10:])

    print('-'*80)
    print('-'*80)

    v.reset_index(inplace=True)
    v= v.rename({'index':'fips'}, axis='columns')

    return v



################################################################################
# 2. Run PageRank Using County Migration Outflows
################################################################################

source='irs'
label="Out"

for year in range(1990, 2022):
    try:
        rank=pagerank(year, source, label)
        
        output_file = os.path.join(wd, "data", "rankings", f"pagerank_{source}_{label}_{year}.csv")
        
        rank.to_csv(output_file, index=False)
        
    except Exception as e:
        print(f"An error occurred: {e}")





################################################################################
# 6. Run Standard PageRank Using Different Dampening Factor
################################################################################
# get an "average" dampening factor from the irs data
# The average dampening factor in IRS data is ~0.78 in both 2010 and 2019
# pop_2019['damp']=pop_2019['diagonals']/pop_2019['irs_pop']
# pop_2019.describe()

# pop_2010['damp']=pop_2010['diagonals']/pop_2010['irs_pop']
# pop_2010.describe()


# d_list=np.append(np.array(range(0,10))/10+0.05, [0.78, 0.8, 0.97, 0.99, 1])

# for year in ["0910", "1819"]:
#     if year=="0910":
#         yr=2010
#     elif year=="1819":
#         yr=2019
#     result_name="countyrank_"+str(yr)
    
    
#     for d in d_list:
#         rank_name="Rank"+"{0:.2f}".format(d)

#         rank_result=pagerank(year=year, label="outflow",
#                              num_iter=10000, method="manual",
#                              d=d, output_column=rank_name)

#         locals()[result_name]=locals()[result_name].merge(rank_result, on="fips")

#     for d in d_list:
#         rank_name="Rank"+"SL"+"{0:.2f}".format(d)
#         rank_result=pagerank(year=year, label="outflow",
#                              num_iter=10000, method="manual", self_loop=True,
#                              d=d, output_column=rank_name)

#         locals()[result_name]=locals()[result_name].merge(rank_result, on="fips")

#     locals()[result_name].describe()













