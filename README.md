# north-yuba

## Data management approach for this repo

This repository contains only code files and supporting files. It does not contain data. The data folder for this repository is located on Box: https://ucdavis.box.com/s/5mt2gpncsju1mvg0p773rat59x5b07ka

The scripts in this repo are set up so that all references to the data directory are relative, except for one file: `data-dir.txt`, which resides in the top-level folder of the repo. This file is not tracked by Git. Each user of the repo needs to change the contents of this file so that it lists the root location of the data directory on their machine. If the `data-dir.txt` file does not exist, you need to create it.

Once this file is set up, then you can load the absolute path of the data directory with the following:
```R 
library(here)
datadir = readLines("data-dir.txt")
```

Then you can do everything else relative to the root of the data dir. For example, to load the DEM:
```R
dem_path = file.path(datadir, "spatial/intermediate/dem.tif")
dem = rast(dem_path)
```
This way, you don't need to hard-code the data directory location anywhere except in `data-dir.txt`, and other users only have to modify `data-dir.txt` on their machine in order to point to the location of the data directory on their machine.

To work with this data directory on your local machine, you can either download it from Box manually, or use Box Drive to have the folder sync to your computer. The latter approach is recommended so that you automatically get any changes that others make to the data files, and so that your changes automatically update on Box for everyone else who is working with the data.
