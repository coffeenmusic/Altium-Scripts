# How to Run
![Script Options](assets/script_options.jpg)

1. `ColorFromImportedCurrents`: 
- Export a CSV template (`ExportCSVTemplate`)
- Add your currents for any high current nets (you can leave the others blank)
- Run `ColorFromImportedCurrents`

OR

2. `ColorFromCalculatedCurrentCapactity`:
The script will prompt the user to do the following:
- Type in temperature rise in degrees Celsius
- Color nets based on current carrying capacity. Red (low current carrying capacity), Yellow (medium), Green (High)
- Export a CSV with the minimum current capacities. Min(All multilayer connected track/arcs)

<figure>
  <p>
    <img src="assets/before.jpg" alt="Before" width="200">
    <img src="assets/after.jpg" alt="After" width="200">
  </p>
  <figcaption>Figure 1: Before and After Comparison</figcaption>
</figure>

3. `RestoreNetColors`: If an OriginalNetColors.csv exists in the script directory (it should autogenerate when running 1. or 2.), then it will import those colors.

# References
- Used Current Calculator script by John Michael Go-Soco (ETL Systems Ltd)
- Used the SelectBadConnections script by Petar Perisin as a reference to walk tracks
- Get scripts' project path from Jeff Collins and William Kitchen's stripped down version

# TODO
- Add Progress Bar
- Iterate all polygons/regions/fills/pads after getting all track/arc/via runs. If any of the polys are lower current than tracks or if there are no tracks, update the current. I don't think this is possible, there is no poly segment for the actually poured copper that I know of. We can get the segments, but that doesn't tell you about the poured copper and thermal reliefs. We could get the thermal relief min width, but I don't know how to get the actual thermal spoke count per pad. It's a tougher problem than I originally thought.