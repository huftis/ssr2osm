ssr2osm
=======

Scripts for converting data from Sentralt stadnamnregister to the OpenStreetMap file format(s).

Prerequisites
-------------

R (http://www.r-project.org/) with the packages rjson, plyr, and stringr.


Usage
-----

1. Download GeoJSON files from http://data.kartverket.no/stedsnavn/GeoJSON/Fylker/.

2. Edit the first lines of the script r-scripts/ssr2osm.R to point to a downloaded file, 
the kommune number (four digits) for the output and the output file name.

3. Change to the r-scripts directory

        cd r-scripts

4. Run the ssr2srm.R script

        Rscript ssr2srm.R


Getting prerequisites on Ubuntu
-------------------------------

Optionally, add RTAN archive to get a more recent R:

* Add the line

        deb http://cran.uib.no/bin/linux/ubuntu lucid/

  to /etc/apt/sources.list. 
  Replace lucid with your Ubuntu version and cran.uib.no with your favourite mirror.

* Update and upgrade

        sudo apt-get update
        sudo apt-get dist-upgrade

Install the development package so that packages can be compiled:

        sudo apt-get install r-base-dev

Install packages from R:

        sudo R
        > install.packages()

For more detailed instructions, see 
[http://cran.r-project.org/bin/linux/ubuntu/README](http://cran.r-project.org/bin/linux/ubuntu/README)


