# Overview
Import pin delays (ps) or pin lengths

# Steps to Run Script
1. Modify from the Symbol Library or from the Schematic Symbol directly.
- Open symbol in Schematic Library (Will only modify pins w/ matching pin names in the open symbol. Will also include the other parts in a multi-part symbol).
- or... Select the component in the Schematic
2. Run the script by opening the script project and running the ImportPinPackLenForm.pas. DXP --> Run Script...
3. Click Browse Button and select the csv to import with Ball and Pin Length
4. Click Update Mapping to verify csv is formated correctly
5. Click Execute Button

### CSV File Format
2 columns with headers Designator & Length
Each row should contain a ball number & length.
Example (Should be in csv format):
| Designator	| Length      |
| -----------   | ----------- |
| BR6			| 688.559     |
| BT6			| 886.771     |
| BK4           | 862.117     |
