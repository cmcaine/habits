# HABITS Decision Support Tool

## Acknowledgements

The development of this software was sponsored by the ESRC (grant ES/P01139X/1) and EU (Horizon 2020, grant 636249). The software is openly available for further use, but to fulfil sponsor requirements, **if any further use is made of the software, please contact the Principal Investigator**, Prof. Susan Grant-Muller (s.m.grant-muller@its.leeds.ac.uk) with a very brief summary of what is to be used for and the potential impact.

## Objective

Help non-specialists explore and understand how people move and travel around in a study area. The tool takes as input a list of trips (routes) and metadata associated with each trip and provides a simple UI for plotting variables of interest on a map and in graphs and for subsetting these data by geographic region (arbitrary polygons drawn by the user and/or preloaded polygons).

The tool was built for the HABITS project (ESRC) using data from participants in Newcastle City Council's GoSmarter project collected as part of the EMPOWER project (EU Horizon 2020). The tool is at an early stage of development and was built to demonstrate the *potential* of this kind of individual level data to transport planners.

If you want to make any use of this software, please contact the author (Colin Caine, cm\<lastname>@gmail.com) and ask how to use it, because I haven't bothered to document exactly how to prepare data for the tool yet. I would also be interested in understanding your use case and improving the tool, possibly for free!

## Installation

Install all dependencies listed at the top of tool/app.R.

In particular, install my fork of leaflet extras and then restart R:

```
install.packages("devtools")
devtools::install_github("cmcaine/leaflet.extras")
```

Get a trips.gpkg with CRS 4326 and put it in tools/data/sensitive.

## User stories

These are example questions we were interested in that we attempted to answer with this tool:

 1. What proportion of journeys/journey km were made by each mode over time in some area?
 1. How do personal outcomes change by IMD over time?
 1. Do people living near this train station cycle to local places and drive to the city centre?
 1. Has travel on this route increased relative to other routes in the region?
 1. What is the demand model for a locality (especially active modes)?

Here are some other questions we thought were interesting but that we did not seek to answer with this tool:

 1. What kind of people respond best to intervention X?
 1. Have in-app incentives increased use of public transport?
 1. What is the decay rate of the effectiveness of an intervention?
 1. Can we assess the value for money of an existing or in-progress scheme?

## Development status

This tool is not under active development and is more likely to be re-written than significantly expanded. Do get in touch if you have a use for this kind of thing, though.

## Development notes

These are notes I took during development. A whole load of this never made it into the tool because either: it's already done by other tools; it wasn't very interesting; I didn't have time; or we studied it some other way.

### Requirements

Measure impact of interventions and understand transport patterns by observing evolution of impact metrics:

 - Journey number (economic interest)
 - Journey distance
 - Journey number by modality
 - Journey distance by modality

 - Popularity of destinations

 - Health
   - METh benefits to travellers
   - Pollution costs to travellers
     - This may be included in some source data for METh benefits (some include transport exercise, some don't)
   - Change in pollution level
     - Effect on residents

 - Cost
   - To passengers
   - Health system
   - Maintenance
 - Congestion

 - Crossreferenced with:
   - IMD level
   - Home location
   - Work location
   - Route intersects/starts in/ends in polygon
   - Age, gender
   - Journey type (work commute or not)
   - Weather conditions
   - Pollution on route

Increase efficacy of future interventions:

 - Identify how people respond to interventions by crossreference values above
 - Sample size of people who use the app before and after interventions is small, but can measure population change

Other:

 - Identify routes with bad flow (stop/start conditions)

Types of interventions/changes:

 - Price changes
 - Network changes (pedestrianizing, bike lanes, roadworks, extra services, etc)
 - Change in supply (housing)
 - Change in demand (workplaces, shops, entertainment)
 - In-app rewards
 - Advertising campaigns
 - Good weather?

### Data available

 - "Track and Trace" (T&T) data: location data + inferred: modality, route aggregates, METh, home and work post codes, journey purpose
 - Age and gender of some T&T users
 - NO2 concentrations at some sites
 - Postcode -> IMD map
 - Postcode -> LSOA
 - LSOA -> Demographics (somewhat outdated)
 - Intervention dates
 - "Road surface cost per vehicle" - susan

 - Tom's, DEFRA's and MAPPAIR's pollution models
   - location, time of day -> NO2 exposure (Tom's only)
   - location -> NO2, PM2.5, PM10, etc exposures
 - Gillian's Health model (METh, demographics -> change in risk of Diabetes, etc)
 - Gillian's accident model (Passenger km/modality -> average accident number)

 - Dubious worth:
   - Cycle infrastructure
   - Bus routes and stops
     - See if bus stops are good places for sensors?

Unsourced, but probably useful:

 - More track and trace
   - Second batch from Newcastle GoSmarter
   - Other apps
 - Roadwork dates and locations
 - Traffic conditions/density/congestion
 - Passenger numbers on public transport
 - Cyclist counts, etc.
 - Smart Steps and Telefonica

Problems:

 - Pollution affects people differently based on metabolic load
 - Drivers experience higher pollution than cyclists?
 - Air quality sensor placement is poor
 - Only one particulate sensor in Newcastle
 - Sample size is small

Assess data value:

 - How good a predictor is T&T?
 - Sources of truth:
   - Bus tickets sold
   - Induction loops
   - Prescriptions issued
   - Hospital records?
 - Models to compare with:
   - Traffic models

 - Value add of T&T:
   - Higher resolution on travel stuff
     - Multimodal journeys
     - Real origin and destination
     - Pedestrian and cycle data
     - Easier to observe change of modality
     - En route issues
   - More demographic information

### Example questions/User stories

 1. Proportion of journeys made by car in city centre over time
 1. Proportion of journeys made by car in city centre during rush hour over time
 1. Proportion of journey km made by car over time
 1. Have in-app incentives for public transport increased usage?
 1. How many people change their main transport mode in July?
 1. How does the combined cost to the city of transport change over time?
 1. How do personal outcomes change by IMD of individual over time?
 1. Is congestion falling?
 1. What kind of people respond best to intervention X?
 1. Is pollution near this school falling?
 1. How has journey modality to St James' Park changed?
 1. Do people living near this train station cycle to local places and drive to the city centre?
    - Can we improve multimodal transport to shift them?
 1. Has travel on this route increased relative to other routes in the region?
    - E.g. to see if bus ticket price reduction improves route popularity

 1. I want to filter the above questions to routes that pass through/start at/end at a region

### Mappable data

 - Heatmap/splodgemap of journey destinations
 - Pollution
 - Change in behaviour/cost/health by workplace/home postcode
 - Routes coloured by degree of change in modality/pollution/whatever over time
 - Filter by crossreference factors, modality,
   - Multi-heatmap of destinations popularity by modality (three categories works quite well, not sure about more)

### Decision

Heatmap of change in $var since $t

Animated heatmap of change in or straight value of $var since $t with timeline

Time series of [$var, ...] split by $split

All optionally filtered by $filt

Possible values:

$var
Heatmap (either as raster or as a colored vector line)
Time series if different

Journeys by mode
Multicolour heatmap
Line per mode

Proportion of journeys by mode
Multicolour
Line per mode

Simple:

Heatmap
Default to whole region/bounding box, + select regions, line per region

 - Destination/origin popularity
 - Cost
 - Impact
 - Health impact
 - Pollution impact
 - Proportion of journeys by $mode
 - Journey number by $mode

Not time series:

Pollution
Heatmap

IMD
Region map

$filt
 - mode
 - IMD
 - region intersect/start/end at/is home/is work
 - age
 - gender
 - $var threshold/interval

$split = $filt

Other questions by Scatter of $var against $var using time series as data source, filtered as before

### Rationalised questions

(After feeding the earlier questions to team and Newcastle City Council, we settled on these)

 - What proportion of journeys/journey km were made by each mode over time in some area?
 - Have in-app incentives increased use of public transport?
 - How do personal outcomes change by IMD over time?
 - What kind of people respond best to intervention X?
 - Do people living near this train station cycle to local places and drive to the city centre?
 - Has travel on this route increased relative to other routes in the region?
 - What is the demand model for a locality (especially active modes)?
 - What is the decay rate of the effectiveness of an intervention?
 - Can we assess the value for money of an existing or in-progress scheme?

Notes:

 - Rob is particularly interested in stats on active travel, presumably because they have good data for other sources?
 - It is interesting that Rob wants to know the proportion of journeys made by car in city centre over time: I would expect them to routinely collect data like these via traffic counts, etc. Might be worth finding out what journeys they have data for
 - Filtering by route start/end/intersection point is novel for some of these questions
 - While many of these questions can be answered by a random poll of travellers around a route, you need to know in advance that you want to do that
 - Rob did not choose the following questions:
   - How many people change their main transport mode in July?
   - Is congestion falling?
   - How does the combined cost to the city of transport change over time?
   - Is pollution near this school falling?
