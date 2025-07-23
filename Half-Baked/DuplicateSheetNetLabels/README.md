# Create Net Labels for All Project Nets

This Altium DelphiScript creates net labels for all unique nets found across every schematic sheet in your project.

## How to Use

1. **Open your project** in Altium Designer
2. **Create a new blank schematic sheet** (or open an existing one where you want the net labels placed)
3. **Make sure the target schematic sheet is focused/active** (visible and selected)
4. **Run the script** from the Altium scripting system

## What It Does

- Scans **all schematic sheets** in the current project
- Collects **all unique net names** (duplicates automatically removed)
- Places **net labels** for each unique net on your currently focused schematic sheet
- **Automatically arranges** the labels in columns with intelligent wrapping
- **Selects all created labels** when finished for easy manipulation

## Layout Details

- **Starting position**: 200 mils from top-left corner (200, 200)
- **Vertical spacing**: 100 mils between labels
- **Column spacing**: 1000 mils between columns  
- **Padding**: 200 mils margin on all sides
- **Auto-wrapping**: Creates new columns when reaching bottom boundary

## Requirements

- Altium Designer with DelphiScript support
- An open project with at least one schematic sheet
- A focused schematic sheet where labels will be placed

## Note

The script respects sheet boundaries and will never place labels outside the sheet area. All created net labels will be selected after the script completes, making it easy to move, modify, or delete them as needed.