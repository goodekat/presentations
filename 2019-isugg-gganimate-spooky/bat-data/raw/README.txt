README

This data file is published by the Movebank Data Repository (www.datarepository.movebank.org). As of the time of publication, a version of the published animal tracking data set can be viewed on Movebank (www.movebank.org) in the study “Movement coordination in trawling bats (data from Giuggioli et al. 2015)”. Individual attributes in the data files are defined below and in the Movebank Attribute Dictionary, available at www.movebank.org/node/2381.

This data package includes the following data files:
Movement coordination in trawling bats (data from Giuggioli et al. 2015).csv
Movement coordination in trawling bats (data from Giuggioli et al. 2015)-reference-data.csv

These data are described in the following written publication:
Giuggioli L, McKetterick TJ, Holderied M (2015) Delayed response and biosonar perception explain movement coordination in trawling bats. PLOS Computational Biology. doi:10.1371/journal.pcbi.1004089.t001

Data package citation:
Holderied M, Giuggioli L, McKetterick TJ (2015) Data from: Delayed response and biosonar perception explain movement coordination in trawling bats. Movebank Data Repository. doi:10.5441/001/1.62h1f7k9

-----------

Terms of Use
This data file is licensed by the Creative Commons Zero (CC0 1.0) license. The intent of this license is to facilitate the re-use of works. The Creative Commons Zero license is a "no rights reserved" license that allows copyright holders to opt out of copyright protections automatically extended by copyright and other laws, thus placing works in the public domain with as little legal restriction as possible. However, works published with this license must still be appropriately cited following professional and ethical standards for academic citation.

We highly recommend that you contact the data creator if possible if you will be re-using or re-analyzing data in this file. Researchers will likely be interested in learning about new uses of their data, might also have important insights about how to properly analyze and interpret their data, and/or might have additional data they would be willing to contribute to your project. Feel free to contact us at support@movebank.org if you need assistance contacting data owners.

See here for the full description of this license
http://creativecommons.org/publicdomain/zero/1.0

-----------

Data Attributes
These definitions come from the Movebank Attribute Dictionary, available at www.movebank.org/node/2381.

animal ID: An individual identifier for the animal, provided by the data owner. This identifier can be a ring number, a name, the same as the associated tag ID, etc. If the data owner does not provide an Animal ID, an internal Movebank animal identifier may sometimes be shown.
	example: 91876A, Gary
	same as: individual-local-identifier

deployment comments: Additional information about the tag deployment that is not described by other reference data terms.
	example: This deployment was excluded from analysis because the tag failed.

event comments: Additional information about individual records or events in a data set that is not described by other reference data terms.
	example: we observed the animal foraging (see photo BT25)
	same as: comments

event ID: An identifier for the set of information associated with each record or event in a data set. A unique event ID is assigned to every time-location or other time-measurement record in Movebank.
	example: 6340565
	units: none

latitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
	example: -121.1761111
	units: decimal degrees, WGS84 reference system
	same as: location lat

latitude (UTM): The geographic longitude of the geographic center of a location along an animal track as estimated by the processed sensor data.
	example: 3628361.84012295
	units: meters, WGS84 reference system
	same as: utm northing

local timestamp: The date and time a sensor measurement was taken in the time zone of the study reference location.
	example: 2008-08-14 15:31:00.000
	format: yyyy-MM-dd HH:mm:ss.sss
	units: specific to the study time zone
	same as: study local timestamp

longitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
	example: -121.1761111
	units: decimal degrees, WGS84 reference system
	same as: location long

longitude (UTM): The geographic longitude of the geographic center of a location along an animal track as estimated by the processed sensor data.
	example: 756243.7836
	units: meters, WGS84 reference system
	same as: utm easting

sensor type: The type of sensor with which data were collected. Values are chosen from a controlled list:
	acceleration: The sensor collects acceleration data.
	accessory measurements: The sensor collects accessory measurements, such as battery voltage.
	Argos Doppler Shift: The sensor is using Argos Doppler shift for determining position.
	bird ring: The animal is identified by a ring that has a unique ID.
	GPS: The sensor uses GPS to find location and stores these.
	natural mark: The animal is identified by a natural marking.
	radio transmitter: The sensor is a classical radio transmitter.
	solar geolocator: The sensor collects light levels, which are used to determine position (for processed locations).
	solar geolocator raw: The sensor collects light levels, which are used to determine position (for raw light-level measurements).
THIS DATASET: Sensor type is 2D videogrammetry, listed as “other”.

study: The name of the study in Movebank in which data are stored.

study time zone: The time zone at the study reference location.
	example: Mountain Standard Time
	units: none

tag comments: Additional information about the tag that is not described by other reference data terms.
	example: custom-made Doppler shift Argos tag with a special altitude sensor

tag ID: A unique identifier for the tag, provided by the data owner. If the data owner does not provide a tag ID, an internal Movebank tag identifier may sometimes be shown.
	example: 2342, ptt_4532
	same as: tag local identifier

taxon: The scientific name of the species on which the tag was deployed, as defined by the Integrated Taxonomic Information System (ITIS, www.itis.gov). If the species name can not be provided, this should be the lowest level taxonomic rank that can be determined and that is used in the ITIS taxonomy. Additional information can be provided using the term taxon detail.
	example: Buteo swainsoni
	same as: species, animal taxon, individual taxon canonical name

timestamp: The date and time a sensor measurement was taken.
	example: 2008-08-14 18:31:00.000
	format: yyyy-MM-dd HH:mm:ss.sss
	units: UTC (Coordinated Universal Time) or GPS time, which is a few leap seconds different from UTC
THIS DATASET: Timestamps are not the same as the actual times of data collection. They are normalized so each trajectory begins at 2009-01-01 12:00:00.000. Data were actually collected during 10 evenings in 2009: June 15th, 22nd, 29th; July 1st, 7th, 8th, 9th, 15th; and August 19th and 25th.

UTM zone: The UTM zone, selected based on the location of each event, used to convert locations from decimal degrees to UTM.
	example: 14N
	units: none

visible: Determines whether an event is visible on the Movebank Search map. Values are calculated automatically, with FALSE indicating that the event has been marked as an outlier by manually marked outlier or algorithm marked outlier. Allowed values are TRUE or FALSE.

-----------

More Information
For more information about this repository, see www.movebank.org/node/15294, the FAQ at www.movebank.org/node/2220, or contact us at support@movebank.org.