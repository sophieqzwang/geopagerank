The website is https://sophieqzwang.github.io/geopagerank/

You can edit it by pulling from GitHub/sophieqzwang/geopagerank, making changes and pushing as one would normally in GitHub

data has a csv and geojson of each of the datasets produced by clean_pagerank/code/Master.R; the website reads these. The html, javascript, and cs files produce the websites together via git. Otherwise, the results can not be viewed in their entirety. So, when making edits, first make a GitHub website using a random account (because it must be public, but we do not want the editing process to be seen). Chat GPT can walk you through making a website from a repository. Then just copy and paste the files from geopagerank into that website. If new years of data are added, these muct be added to the html also. And if new variables are added, this will also require adjusting the javascript to correctly identify the variable to plot.

Datasets need to be renamed: add "_cbsa" for metro rankings and remove "_combined" from the IRS data. 