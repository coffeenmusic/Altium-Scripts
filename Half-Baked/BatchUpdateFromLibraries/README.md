### How to Use
Below are the steps, but it works by changing the DesignItemID for all `Old Part Number` to `New Part Number` from the list in the csv. Then you update from libraries in schematic and it will update all the library parameters using the new DesignItemID.

1. See the example csv where you can enter an old part number and new part number in csv format (Keep column header names Old Part Number & New Part Number).
2. Run the script: `File > Run Script... > BatchUpdateFromLibraries.pas > Run`
3. The script will prompt to select the csv file. You can use `example.csv`
4. When it is done it will select all the parts that were updated in the schematic.
5. Right click on any selected part (make sure not to deselect all the changed parts)
6. Run through the standard update from libraries: `Right click > Part Actions > Update Selected From Libraries > Finish > Execute Changes > Close`