# PCB Library Merger for Altium Designer

## Overview
This script consolidates footprints from multiple PCB libraries (.PcbLib files) into a single library in Altium Designer. It allows you to easily combine footprints from various libraries into one centralized library for better organization and management.

## Features
- Select a directory containing multiple PCB libraries
- Use an existing open library or create a new one as the destination
- Automatically finds all PCB libraries in the selected directory
- Copies all footprints to the destination library
- Preserves all footprint properties and components

## Usage Instructions
1. Open Altium Designer
2. Open a destination PCB library (optional - if not open, the script will prompt you to create one)
3. Run the script from the Scripts panel
4. Select the directory containing your source PCB libraries when prompted
5. If no library is currently open, select a destination file for the new consolidated library
6. The script will process all libraries and copy their footprints
7. Save the consolidated library manually

## Implementation Details
The script performs the following key operations:
1. Uses a file dialog to let the user select a directory
2. Uses FindFiles to locate all .PcbLib files in the directory
3. Loads each library using PcbServer.LoadPCBLibraryByPath
4. Creates an iterator to go through all footprints in each source library
5. Registers each footprint in the destination library
6. Updates the view to show the consolidated library

## Code Sources and References
This script was developed based on example scripts provided with Altium Designer:

1. **OpenComponentPCBLib.pas** by John Michael Go-Soco
   - Used for the pattern of finding and opening PCB libraries

2. **CreateFootprintInLibrary.pas** by Altium Limited
   - Provided methods for creating and working with PCB library components
   - Used for handling PCB objects and properties

3. **LibraryIterator.pas** by Altium Limited
   - Demonstrated the use of PCB library iterators
   - Used as reference for iterating through footprints in libraries

4. **FootprintInfoReport.pas**
   - Showed techniques for retrieving footprint information
   - Used as reference for file operations

5. **PlacePartFromDbLibToPCB.pas** by Allen GONG
   - Provided patterns for component manipulation

## Notes
- This script directly transfers footprints between libraries without creating copies
- When running the script, select a directory that contains .PcbLib files
- The script will update an existing open library if one is available
- Save your work after running the script

## License
This script is provided as-is without warranty. Use at your own risk.