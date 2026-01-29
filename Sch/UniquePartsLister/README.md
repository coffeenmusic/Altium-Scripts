# Unique Parts Lister for Altium Designer

A DelphiScript tool that generates a unique parts list from schematic projects in Altium Designer.

## Features

- Collects all available parameter names from schematic components
- Lets you select which parameter to use as the unique identifier
- Lets you select additional parameters to display as columns
- Displays results in a sortable, filterable table
- Exports to CSV
- Remembers your parameter selections between sessions

## Installation

1. Copy all files to a folder accessible by Altium Designer
2. Open `UniquePartsLister.PrjScr` in Altium Designer
3. The project should show three script files:
   - `UniquePartsLister.pas` (main script)
   - `FormParamSelect.pas` (parameter selection dialog)
   - `FormResults.pas` (results dialog)

## Usage

1. Open a schematic project in Altium Designer
2. Run the script: **DXP > Run Script** and select `RunUniquePartsLister`
3. **Parameter Selection Dialog:**
   - Left list: Select the parameter to use as the unique identifier (e.g., Part Number, Comment)
   - Right list: Check additional parameters to include as columns
   - Click **OK** to generate the list
4. **Results Dialog:**
   - View the unique parts list
   - **Sort:** Click any column header to sort (click again to reverse)
   - **Filter:** Select a column from the dropdown and type in the search box
   - **Export:** Click "Export to CSV" to save the data

## Settings

Your parameter selections are automatically saved to `UniquePartsLister.ini` in the schematic project folder. The next time you run the script, your previous selections will be pre-selected.

## Tips

- The unique identifier parameter should have distinct values for each unique part (e.g., manufacturer part number)
- Components with empty unique ID values are skipped
- The filter searches for "contains" matches and is case-insensitive
- Exported CSV includes only the currently filtered/visible rows

## Requirements

- Altium Designer with scripting support
- An open schematic project with components

## Files

| File | Description |
|------|-------------|
| `UniquePartsLister.pas` | Main script logic |
| `UniquePartsLister.PrjScr` | Script project file |
| `FormParamSelect.pas/dfm` | Parameter selection dialog |
| `FormResults.pas/dfm` | Results display dialog |
| `UniquePartsLister.ini` | Saved settings (auto-created) |

---
*Created with assistance from Claude AI*
