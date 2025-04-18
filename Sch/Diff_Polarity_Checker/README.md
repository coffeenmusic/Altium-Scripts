# DiffPolarityChecker
Finds all differential pairs on its own (without a rule) and then compares pin names to net names. If the two names have a different polarity, it will add that pin/net to the exported report.

1) Run the script from any schematic sheet
2) It will prompt for a path to save the `report.csv`
3) It will prompt that it is complete
4) Check the `report.csv` and compare Pin Names to Net Names to see if there is a polarity mismatch

### Note
This script will struggle with pin names like IO_L4P_T0U_N6_DBC_AD7P_A24_65 because it contains P_, _N, _L, so it will chose it's polarity on the priority order (In this case _N is the highest priority)

## How it works
- Get all diff pairs
  1. It creates a dictionary of `dict[component] = [list of nets]`.
  2. From the prev dict it creates a dictionary of `dict[string_len] = [list of nets of same length & same component]`
  3. Iterates each net in each length key. If 2 nets match all characters except 1 and the one that doesn't match is in `p, n, m, +, -, h, l`, then it is a diff pair.
- Iterate all pin/net pairs
  1. If net is in diff_pairs dictionary, get pin polarity and net polarity.
  2. If polarities don't match, add to report.

## Credits
- by Stephen Thompson
- GetPinData script by John Michael. I used this as a reference to iterate pins, but also get the pins net name. With the standard method of iterating pins, I don't know how to get the net name.

## License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.\
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.\
You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>