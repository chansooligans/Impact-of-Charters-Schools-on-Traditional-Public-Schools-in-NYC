# Impact of Charters Schools on Traditional Public Schools in NYC 

Supervised by Professor Ying Lu (NYU)

Start Date: 9/12/2018
Updated: 2/5/2019

## Introduction:

#### Purpose of Project
The purpose of this study is to evaluate the impact of charter schools on traditional public schools in New York City.

#### Questions to Address:
1. How to measure competition?
- \# of charter schools within a geographic region (defined by radius or city zoning)
- distance of public school from charters (e.g. euclidean, travel-time)
- % of students who have exited to charter school
2. What is the unit of analysis?
- Students? Classrooms? Schools?
3. Causal Problem:
- Charters are not randomly located!
- Students do not randomly select schools!
- Heterogeneity in charter schools themselves

## Data

#### Test Scores:
- [Test scores for AY:2013-2014 to AY:2017-2018 from NYC DOE](https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview)
- [Test scores for AY:2005-2006 to AY:2012-2013 NYC Open Data](https://data.cityofnewyork.us/Education/2006-2011-NYS-Math-Test-Results-By-Grade-School-Le/jufi-gzgp)
- Datasets include Grades 3-8. (Analysis uses Grades 3-5) 
- New York City Results on the New York State  English Language Arts (ELA) and Math Tests 
- Four performance levels: Level I, Level II, Level III, Level IV
- Scale scores are comparable only within a given subject (math/ELA), grade (not comparable across grades or across subjects), and year
- Rows are suppressed (noted with 's') if the number of tested students was 5 or fewer

#### Demographics:
- Demographics for AY:2005-2006 to AY:2011-2012
- Demographics for AY:2013-2014 to AY:2017-2018
- Missing AY:2012-2013
- "Poverty" includes "free lunch" and "reduced free lunch""

#### Locations:
- Source: [NYC Open Data](https://data.cityofnewyork.us/Education/2017-2018-School-Locations/p6h4-mpyy) 
- Available for AY 2012-13 to 2017-18

#### Shapefiles:
- [NYC Open Data (shapefiles from AY 2012-2013 to 2017-2018)](https://data.cityofnewyork.us/Education/2017-2018-School-Zones/ghq4-ydq4)
- Each year contains separate shapefiles for Elementary School Zones, Middle School Zones, High School Zones
- esid, msid, hsid each indicate id of the zones
- [NTA Shapefile](https://data.cityofnewyork.us/City-Government/Neighborhood-Tabulation-Areas/cpf4-rkhq)
- [Community Districts Shapefile](https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4)


## Literature Review

### Effects of Charter Schools on Student Outcomes:

#### Fixed Effects:
1. Cordes (2018): In pursuit of the common good: The spillover effects of charter schools on public school students in New York City
2. Ni (2008): The impact of charter schools on the efficiency of traditional public schools: Evidence from Michigan
3. Cremata and Raymond (2014)
4. Winters (2012)
5. Bohte (2004): Examining the impact of charter schools on performance in traditional public schools
6. Booker, Gilpatric, Gronberg & Jansen (2005): The effect of charter schools on traditional public schools
7. Buddin & Zimmer (2005): Is charter school competition in California improving the performance of traditional public schools?

#### Instrumental Variables:
1. Imberman (2011): The effect of charter schools on achievement and behavior of public school students
2. Bettinger (2005): The effect of charter schools on charter students and public schools
3. Terrier & Ridley (2018): Fiscal and Education Spillovers from Charter Expansion

#### Charter School and TPS Composition:
1. Bifulco and Ladd (2006): The impacts of charter schoools on student achievement: Evidence from North Carolina
2. Sass (2006): Charter schools and student achievement in Florida
3. Buckley and Sattin-Bajaj (2011): Are ELL students underrepresented in charter schools? Demographic trends in New York City, 2006-2008
4. Booker, Zimmer and Buddin (2005): The effects of charter schools on school peer composition
5. Hoxby, Murarka, and Kang (2009): How New York City’s charter schools affect achievement
6. Tuttle et al. (2010): Student characteristics and achievement in 22 KIPP middle schools
7. Lake, Gross, and Denice (2012): Center on Reinventing Public Education

#### Charter School Effect on TPS Resources:
1. Podgusky and Ballou (2001): Personnel policy in charter schools
2. Hoxby (2002): Would school choice change the teaching profession?
3. Baker and Dickerson (2006): Charter schools, teacher labor market deregulation, and teacher quality: Evidence from teh schools and staffing survey
4. Carruthers (2010): The qualifications and classroom performance of teachers moving to charter schools
5. Ladd and Singleton (2018): The fiscal externalities of Charter Schools: Evidence from North Carolina

#### Models of Charter School Location:
1. Bifulco and Buerger (2015): School choice, racial segregation, and test-score gaps: Evidence from North Carolina’s harter school program
2. Henig and macdDonald (2002): Locational decisions of charter schools: Probing the market metaphor
3. Glomm, Harris, and Lo (2005): Charter school location
4. Stoddard and Corcoran (2007): The political economy of school choice: Support for charter schools across states and school districts
5. Ferreyra and Kosenok (2014): Charter school entry and school choice: The case of Washington, DC. 


## Code:

(1) "compile_schools_data.R" 
- imports data from NYC DOE (https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview), separate files for each AY
- imports data from QGIS (QGIS is used to merge CSV files exported from 'export.R' with school zones). separate files for each AY
- import location files. separate files for each AY
- import demographics files. separate files for each AY
- compute diversity index using Shannon Entropy (with demographics data)
- merge all files together
- exports a master dataset called "all_schools_master.csv", which contains DBN, scores by grade, and indicators for math/ela and charter

(2) "data_inventory.R":
- Define "region" as NTA, Community District, Census Tract, Elementary School Zone ID
- groups data by region. separate columns for each year indicate the # of charters/public schools contained in the region for that year

(3) "export.R": 
- creates longitude and latitude columns
- exports files that can be imported into QGIS (e.g. separate CSV files containing elementary schools for each year)
