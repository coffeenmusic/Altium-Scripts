# Current Capacity To Color
The script will prompt the user to do the following:
- Color nets based on current carrying capacity. Red (low current carrying capacity), Yellow (medium), Green (High)
- Export a CSV with the minimum current capacities. Min(All multilayer connected track/arcs)

Note: All current calculations use a 10C temperature rise assumption. TODO: Allow user to choose temperature rise.

# References
- Used Current Calculator script by John Michael Go-Soco (ETL Systems Ltd)
- Used the SelectBadConnections script by Petar Perisin as a reference to walk tracks
- Get scripts' project path from Jeff Collins and William Kitchen's stripped down version

# TODO:
- Give user the option to pick temperature rise setting