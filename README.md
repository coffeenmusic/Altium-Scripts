# My Custom Altium scripts.

# PCB
- `AddDiffPairCompensationBumps`: Adds compensation bumps to differential pairs
- `CurrentColorizer`: Color nets in layout from current capacity calculations or by importing a csv with your own power rail currents
- `DiffPair_Finder`: Finds diff pairs without needing to manually define them. Searches obscure net names as long as they are attached through a two pin component.
- `FindNoNets`: Searches tracks, polygons, vias, etc to see if any of them are `No Net`
- `PowerPinsToNetClass`: Gets all symbol pins that are electrical type `Power` and adds them to a layout net class
- `Silkscreen_Auto_Placer`: Tries to auto place silkscreen. I think the version on the community scripts-libraries is newer w/ other user contributions.
- `SolderPasteGrid`: Given a set of restrictions (min grid width, min gap, min %) try to create a grid that fits those specs to the center pad

# Schematic
- `Diff_Polarity_Checker`: Iterates pins on schematic and compares to the net names they are connected to. If it finds a P/N mismatch or similar (PD/DN, etc) it notifies the user
- `ImportPinPackageLengths`: Import package delays from a csv on to a component

# PCBLib
- `PCB_Library_Merger`: Merges PCB libraries

# SchLib
- `CheckSymbolPinsFromCSV`: Compares pin names to names from a csv and exports a mismatch report

# Half-Baked
- `CopySymbolParameterPlacement`: Tries to copy parameter positions from one symbol to another
- `DbLib`: Started trying to figure out how to read components from a DbLib, got some information, but not components
- `LayoutDuplicator`: This is integrated in to `altium-mcp` and does a better job there because AI can match the components. To do this programmatically is not as easy as it sounds.
- `TombstoneFinder`: Notifies user of potential tombstones using tracks, but does not use polygons