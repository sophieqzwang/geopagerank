This is for the replication of the geopagerank datasets and outputs. These are for the website: 

Structure:

The Master file runs each code in subordinate. 

These codes take raw data, clean it and put it into the processed folder; 
generate rankings, placed in the rankings folder;
then combine these into a few larger csv and geojson files, placed into the output folder.

These csv and geojson files are then manually placed in the data folder for the GitHub website.

Update instructions:

Every python file ending in _pagerank or _pageranks has a loop that goes through every start year. Extend the start year as required.

The html will also need every year listing to be extended.

No other files need adjustment; they will process all years which these files generate. Similarly, the script.js file will use any years in the html automatically.