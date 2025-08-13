# Net Labels with Test Points Script

## Overview
This script automatically places test points with net labels on your current schematic sheet, organized by their source sheets and sorted by complexity.

## How to Use

1. **Select a Test Point Component**
   - Open the schematic sheet where you want to place all the test points
   - Select any existing test point component (designator must start with "TP")
   - This will be used as a template for all new test points

2. **Run the Script**
   - Execute the script from Altium Designer
   - You'll be prompted whether to include differential pair nets (defaults to NO)

3. **Result**
   - All test points will be placed on the current sheet with proper spacing
   - Each test point includes a wire and net label

## Features

### Differential Pair Filtering
- **Default: NO** - Differential pairs are automatically filtered out
- Detects common diff pair naming conventions: `_p`/`_n`, `_dp`/`_dn`, `+`/`-`, `_h`/`_l`, etc.
- Choose YES if you need test points for differential pairs

### Smart Organization
- **Grouped by Source Sheet**: All nets from the same sheet are grouped together with a header
- **Complexity-Based Ordering**: Sheets with fewer nets are processed first
- **Prevents Duplication**: Nets that appear on multiple sheets are only placed once

### Why Complexity-Based Ordering?
The script processes sheets starting with those that have the **least number of nets** first. This ensures that:
- Simple sheets get their nets placed early
- Complex sheets don't "claim" nets that belong to simpler sheets
- More logical organization of test points

## Layout
- Test points are arranged in columns with proper spacing
- Each sheet group has a descriptive header showing net counts
- Automatic column wrapping when reaching sheet boundaries

## Requirements
- Altium Designer with schematic project loaded
- At least one existing test point component to use as template