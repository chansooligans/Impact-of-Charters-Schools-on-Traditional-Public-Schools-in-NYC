# Charter School Project

For the NYU A3SR consulting class with Professor Ying Lu.

Collaborative Project by Chansoo Song, Erik Wang, Frankie Wunschel

## Code

(1) "compile_schools_data.R" 
- merges data from NYC DOE (https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview). 
- exports a master dataset called "all_schools_master.csv", which contains DBN, scores by grade, and indicators for math/ela and charter

(2) "data_inventory.R":
- loads the master dataset then merges with location files
- exports an updated master file that includes location
- groups data by school and adds indicator columns for each year, showing whether location is available for that school and year.

(3) "export.R": 
- creates longitude and latitude columns
- merges with demographics files
- computes diversity index
- exports files that can be imported into QGIS (e.g. separate CSV files containing elementary schools for each year)

(4) "schools_and_zones_merged.R":
- imports data from QGIS (QGIS is used to merge CSV files exported from 'export.R' with school zones). there will be a separate file for each year
- merge the data files into one master dataframe
- reshape to conduct inventory check (whether we have zones or not)

## Data

#### Test Scores:
- Test scores for year ending 2014 to 2018 from NYC: https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview
- Test scores for 2006-2012: NYC Open Data

#### Demographics:
- Received from Ying (NYC Open Data)

#### Locations:
- NYC Open Data (shapefiles from year ending 2013-2018)


