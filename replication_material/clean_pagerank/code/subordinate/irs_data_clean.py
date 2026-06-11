import pandas as pd
import re
import numpy as np
import os


def get_migration_folder(start_year: int, end_year: int) -> str:

    # Try to infer based on year difference
    diff = end_year - start_year
    if diff == 1:
    	st_yr=str(start_year)[2:]
    	end_yr=str(end_year)[2:]

    	st_year=str(start_year)
    	end_year=str(end_year)

    	# For start years from 1990 to 2003
    	if 1990 <= start_year <= 2003:
    		return os.path.join(wd, f"data/raw/irs/{start_year}to{end_year}CountyMigration") #f"../data/input/irs/{start_year}to{end_year}CountyMigration"

    	elif 2004 <= start_year <= 2010:
    		return os.path.join(wd, f"data/raw/irs/county{st_yr}{end_yr}") #f"../data/input/irs/county{st_yr}{end_yr}"

    	elif 2011 <= start_year <= 2021: 
    		return os.path.join(wd, f"data/raw/irs/{st_yr}{end_yr}migrationdata") #f"../data/input/irs/{st_yr}{end_yr}migrationdata"

    raise ValueError(f"No migration data file found for {start_year} to {end_year}")


def parse_source_line(source_raw):
    numbers = re.findall(r'\d+', source_raw)
    if len(numbers) < 2:
        return None

    source_state = numbers[0]
    source_county = numbers[1]

    # Get position after second number
    second_number_pos = [m.start() for m in re.finditer(r'\d+', source_raw)][1]
    after_second = source_raw[second_number_pos + len(source_county):]

    # Identify source_state_name: first word followed by a number
    state_name_match = re.search(r'\b([A-Za-z]+)\s*\d', after_second)
    source_state_name = state_name_match.group(1) if state_name_match else ''

    # Look for first occurrence of 'T', 'Tot', or 'Total'
    total_match = re.search(r'\b(Total|Tota|Tot|T)\b', after_second, re.IGNORECASE)
    total_pos = total_match.start() if total_match else None

    # Find position of source_state_name if it exists
    if source_state_name:
        state_name_pos = after_second.find(source_state_name)
    else:
        state_name_pos = None

    # Use the smaller index of total or state_name for cutoff
    cutoffs = [pos for pos in [total_pos, state_name_pos] if pos is not None]
    cutoff = min(cutoffs) if cutoffs else len(after_second)

    source_county_name = after_second[:cutoff].strip()

    return source_state, source_county, source_county_name, source_state_name

def parse_destination_line(dest_raw):
    # Match integers and floats as single numbers
    numbers = re.findall(r'\d+(?:\.\d+)?', dest_raw)
    
    if len(numbers) < 2:
        return None

    destination_state = numbers[0]
    destination_county = numbers[1]

    # Handle trailing return/exemption numbers
    if len(numbers) >= 6:
        return_count = numbers[-4]
        return_f = numbers[-3]
        exemption = numbers[-2]
        exemption_f = numbers[-1]
    elif len(numbers) >= 4:
        return_count = numbers[-2]
        exemption = numbers[-1]
        return_f = ''
        exemption_f = ''
    else:
        return_count = exemption = return_f = exemption_f = ''

    return {
        'Dest_State': destination_state,
        'Dest_County': destination_county,
        'Return': return_count,
        'Return_F': return_f,
        'Exemption': exemption,
        'Exemption_F': exemption_f
    }

def process_file(filename):
    data = []
    with open(filename, 'r') as f:
        lines = f.readlines()

    current_source = None
    for line in lines:
        raw = line.rstrip('\n')
        if not raw.strip():
            continue

        indent = len(raw) - len(raw.lstrip())
        stripped = raw.strip()

        if indent == 0:
            parsed = parse_source_line(stripped)
            if parsed:
                source_state, source_county, source_county_name, source_state_name = parsed
                current_source = {
                    'Source_State': source_state,
                    'Source_County': source_county,
                    'Source_County_Name': source_county_name,
                    'Source_State_Name': source_state_name,
                    'Source_Raw': stripped
                }
        elif indent > 0 and re.match(r'^\d', stripped):
            if current_source is not None:
                dest_data = parse_destination_line(stripped)
                if dest_data:
                    row = current_source.copy()
                    row.update(dest_data)
                    row['Destination_Raw'] = stripped
                    data.append(row)

    return pd.DataFrame(data)


# file="../data/input/irs/1990to1991CountyMigration/1990to1991CountyMigrationOutflow/C9091ako.txt"
# parsed_data=process_file(file)


def is_number(token):
    return re.match(r'^-?\d+(\.\d+)?$', token) is not None

def parse_line(line):
    tokens = line.strip().split()
    row = []
    current = []
    for token in tokens:
        if is_number(token):
            if current:
                row.append(' '.join(current))
                current = []
            row.append(token)
        else:
            current.append(token)
    if current:
        row.append(' '.join(current))
    return row

def read_space_delimited_dat(file_path):
    data = []
    with open(file_path, 'r') as f:
        for line in f:
            if line.strip():  # skip empty lines
                data.append(parse_line(line))
    # Pad rows to same length
    max_len = max(len(row) for row in data)
    for row in data:
        row.extend([''] * (max_len - len(row)))  # pad with empty strings

    return pd.DataFrame(data)



# Example usage

# df = read_space_delimited_dat("../data/input/irs/county0405/countyout0405us1.dat")
# print(df.head())



def clean_irs_flow_data(start_year, end_year, flow="Out"): 
    
    st=str(start_year)[2:]
    end=str(end_year)[2:]
    
    flow_l=flow.lower()
    f=flow_l[:1]
    
    folder_name=get_migration_folder(start_year, end_year)
    
    # if year between 1990 and 2003: clean each of the state data set and 
    
    if 1990 <= start_year <= 1991:
        subfolder=f"{start_year}to{end_year}CountyMigration{flow}flow"
        subfolder_name=f"{folder_name}/{subfolder}"
        i=0
        
        for filename in os.listdir(subfolder_name):
            if filename.endswith(".txt"):
                full_path = os.path.join(subfolder_name, filename)
                try:
                    data=process_file(full_path)
                    if i==0:
                        df = data
                    else:
                        df= pd.concat([df, data], ignore_index=True)
                    i=i+1
                except Exception as e:
                    print(f"Error processing {filename}: {e}")
        
        return df
    
    elif 1992<= start_year <= 2003:
        subfolder=f"{start_year}to{end_year}CountyMigration{flow}flow"
        subfolder_name=f"{folder_name}/{subfolder}"
        
        i=0 
        for filename in os.listdir(subfolder_name):
            if filename.endswith((".xls", ".XLS")):
                full_path = os.path.join(subfolder_name, filename)
                try:
                    data = pd.read_excel(full_path, engine='xlrd')
                    data = data.iloc[:, :9]
                    column_names = ["Source_State", "Source_County", 
                                   "Dest_State", "Dest_County", "state",
                                   "county", "Return", "Exemption", "AAGI"]
                   
                    data.columns=column_names
                    # Convert first two columns to numeric (coerce errors to NaN)
                    data["Source_State"] = pd.to_numeric(data["Source_State"], 
                                                          errors="coerce")
                    data["Source_County"] = pd.to_numeric(data["Source_County"], 
                                                           errors="coerce")
    
                    # Drop rows where both origin_state and origin_county are NaN
                    data = data.dropna(subset=["Source_State", "Source_County"], 
                                         how='all')
                    
                    
                    if i==0:
                        df=data
                        
                    else:
                        df= pd.concat([df, data], ignore_index=True)
                    i=i+1
                except Exception as e:
                    print(f"Error processing {filename}: {e}")
        return df
    
    elif 2004 <= start_year <= 2007:
        if start_year==2007:
            filename=f"c{f}{st}{end}us.dat"
        
        else:
            filename=f"county{flow_l}{st}{end}.dat"
            
            
        filepath=os.path.join(folder_name, filename)
        df=read_space_delimited_dat(filepath)
            
            
        column_names = ["Source_State", "Source_County", 
                       "Dest_State", "Dest_County",
                       "name", "Return", "Exemption", "n3", "n4"]
        
        df.columns=column_names
        
        return df
    
    
    elif start_year>=2008:
        filename=f"county{flow_l}flow{st}{end}.csv"
        filepath=os.path.join(folder_name, filename)
        df=pd.read_csv(filepath, encoding='latin1')
        
        column_names = ["Source_State", "Source_County", 
                       "Dest_State", "Dest_County",
                       "Dest_State_Name", "Dest_County_Name",
                       "Return", "Exemption", "agi"]
        
        df.columns=column_names
        
        
        return df



summary_list=[]
diag_list=[]



# Define a helper function to apply changes to a FIPS value
def modify_fips(fips, year):
    # Rule 1
    if year <= 1995 and fips == 51083:
        return 9951083
    # Rule 2
    if year <= 1997 and fips == 30031:
        return 9930031
    # Rule 3
    if year <= 1997 and fips == 30067:
        return 9930067
    # Rule 4
    if year <= 1997 and fips == 12025:
        return 12086
    # Rule 5
    if year <= 2001 and fips == 51005:
        return 9951005
    # Rule 6
    if year > 2001 and fips == 8013:
        return 998013
    # Rule 7
    if year > 2001 and fips == 8001:
        return 998001
    # Rule 8
    if year > 2001 and fips == 8059:
        return 998059
    # Rule 9
    if year > 2001 and fips == 8123:
        return 998123
    # Rule 10
    if year <= 2013 and fips == 51019:
        return 9951019
    # Rule 11
    if year <= 2015 and fips == 46113:
        return 46102
    # Default
    return fips

flow="Out"
for start_year in range(1990, 2022):
    end_year=start_year+1
    
    st=str(start_year)[2:]
    end=str(end_year)[2:]
    
    output_name= os.path.join(wd, f"data/processed/irs_{flow}_{st}{end}.csv") #f"../data/temp/irs_{flow}_{st}{end}.csv"
    
    data=clean_irs_flow_data(start_year, end_year, flow)
    
    for col in ["Source_State", "Source_County", "Dest_State", "Dest_County", "Return", "Exemption"]:
        data[col] = pd.to_numeric(data[col], errors='coerce') 
    
    print('-'*80)
    print(f'Start Clearning Data for IRS {flow} Flow {st}-{end}')
    
    # removing aggregate rows
    # Remove State and County Code with 0s (i.e. records of state or national aggregate flows)
    data=data.loc[(data['Source_State']!=0)
                  & (data['Source_County']!=0)
                  & (data['Dest_State']!=0)
                  & (data['Dest_State']<=56)
                  & (data['Dest_County']!=0)]
      
    # generate key variables
    
    data['FIPS_Origin']=data['Source_State']*1000+data['Source_County']
    data['FIPS_Dest']=data['Dest_State']*1000+data['Dest_County']
    
    
    data['FIPS_Origin']=data['FIPS_Origin'].astype('Int64')
    data['FIPS_Dest']= data['FIPS_Dest'].astype('Int64')
    
    data.loc[data['Return']<0, 'Return']=0
    data.loc[data['Exemption']<0, 'Exemption']=0
    
    
    data['FIPS_Origin'] = data['FIPS_Origin'].apply(lambda x: modify_fips(x, year=start_year))
    data['FIPS_Dest'] = data['FIPS_Dest'].apply(lambda x: modify_fips(x, year=start_year))
    
    summary = data.describe(include='all').transpose()
    summary['year']=start_year
    summary['variable'] = summary.index
    summary_list.append(summary.reset_index(drop=True))
    
    data['irs_pop']=data['Exemption'].groupby(data['FIPS_Dest']).transform('sum')
    
    diag=data[data['FIPS_Origin']==data['FIPS_Dest']]
    diag=diag[['FIPS_Dest', 'Exemption', 'irs_pop']]
    
    """
    Get an average of diagonals and IRS Population
    This will be one of the dampening factors used
    """
    summary = diag.describe(include='all').transpose()
    summary['year']=start_year
    summary['variable'] = summary.index
    
    diag_list.append(summary.reset_index(drop=True))

    data.to_csv(output_name, index=False)
    
    print('-'*80)
    print(f'Cleaned Data for IRS {flow} Flow {st}-{end}')



final_summary = pd.concat(summary_list, ignore_index=True)
final_summary.to_csv(os.path.join(wd, f"data/processed/irs_flow_summary.csv"), index=False)


final_diag=pd.concat(diag_list, ignore_index=True)
final_diag.to_csv(os.path.join(wd, f"data/processed/irs_diag_summary.csv"), index=False)






























