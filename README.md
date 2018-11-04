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
- Grade 3-8 state assessments in English Language Arts (ELA) and Math
- ELA and Math tests changed in 2013, when New York State moved to the Common Core Learning Standards. 
- Each Excel file contains results for all students tested, as well as results by student characteristics including disability status, English Language Learner (ELL) status, race/ethnicity, and gender.

#### Demographics:
- Received from Ying (NYC Open Data)
- Contains: free lunch percent, total enrollment, # students per grade, # ell, # special ed, # students by race, # students by sex

#### Locations:
- Source: NYC Open Data 
- e.g. link for AY 2017-2018: https://data.cityofnewyork.us/Education/2017-2018-School-Locations/p6h4-mpyy
- Available for AY 2012-13 to 2017-18
- Contains: Community District, Council District, Census Tract, NTA (Neighborhood Tabulation Area), Geographical District Code, Admin_District_Location_Code, Address (with Longitude/Lattitude)

#### Shapefiles:
- Source: NYC Open Data (shapefiles from AY 2012-2013 to 2017-2018)
- e.g. link for AY 2017-2018: https://data.cityofnewyork.us/Education/2017-2018-School-Zones/ghq4-ydq4
- Each year contains separate shapefiles for Elementary School Zones, Middle School Zones, High School Zones.
- esid, msid, hsid each indicate id of the zones


