# My Custom Altium scripts.

# PCB
- `AddDiffPairCompensationBumps`: Adds compensation bumps to differential pairs
- `CurrentColorizer`: Color nets in layout from current capacity calculations or by importing a csv with your own power rail currents
- `DiffPair_Finder`: Finds diff pairs without needing to manually define them. Searches obscure net names as long as they are attached through a two pin component.
- `FindNoNets`: Searches tracks, polygons, vias, etc to see if any of them are `No Net`
- `FindReplaceText`: Find/Replace text on all or a specific layer.
- `PowerPinsToNetClass`: Gets all symbol pins that are electrical type `Power` and adds them to a layout net class
- `Silkscreen_Auto_Placer`: Didn't include because I already have a separate repo for this and I've pushed it to [scripts-libraries Community Scripts](https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/AutoPlaceSilkscreen)
- `SolderPasteGrid`: Given a set of restrictions (min grid width, min gap, min %) try to create a grid that fits those specs to the center pad

# Schematic
- `Diff_Polarity_Checker`: Iterates pins on schematic and compares to the net names they are connected to. If it finds a P/N mismatch or similar (PD/DN, etc) it notifies the user
- `ImportPinPackageLengths`: Import package delays from a csv on to a component
- `CopySymbolParameterPlacement`: Tries to copy parameter positions from one symbol to another
- `Testpoint_Generator`: Adds schematic tracks, nets, and user selected surface mount pad for every net on the design.
- `UniquePartsLister`: List unique BOM parts, but don't need designator annotations and can double click part in GUI to jump to part on schematic. Good for using existing parts.

# DBLib
- BatchUpdateFromLibraries: Change from one part to another, but in batch fashion.

# PCBLib
- `PCB_Library_Merger`: Merges PCB libraries

# SchLib
- `CheckSymbolPinsFromCSV`: Compares pin names to names from a csv and exports a mismatch report

# OutJob
- `OutJob_RunAll`: Run all output job containers by reading all output job parameters from the .OutJob file (This is just an ini file)

# Half-Baked
- `Assembly_Auto_Placer`: Tries to rotate assembly designators as either 0 or 90 degrees and resize them to be in their bounding box.
- `DbLib`: Started trying to figure out how to read components from a DbLib, got some information, but not components
- `ExportPCBFabNotes`: Export fab notes to a text file from a given layer.
- `LayoutDuplicator`: This is integrated in to `altium-mcp` and does a better job there because AI can match the components. To do this programmatically is not as easy as it sounds.
- `TombstoneFinder`: Notifies user of potential tombstones using tracks, but does not use polygons