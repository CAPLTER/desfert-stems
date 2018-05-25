# desfert-stems

This repository contains the materials for creating the ODK stems app, and tools & procedures for processing collected data

# process steps, generally, are as follows:

1.  copy the contents of the ODK folder from each tablet into a folder with the name of the tablet.
2.  use [ODK briefcase](https://docs.opendatakit.org/briefcase-intro/){:target="\_blank"} to extract tablet XML data into tabular form
    + run unextracted briefcase java program from command line with ```java -jar ODK\ Briefcase\ v1.5.0\ Production.jar```
    + extract data from each tablet separetely into unique directories, preferably with the directory names ar, br, cr for alpha, bravo, and charlie, respectively as this is what the merge script (following step) will be looking for. be sure to change the ODK briefcase storage location corresponding to each extraction - the storage location (I think) does not matter so long as it is different for each tablet
    + close and relaunch ODK briefcase between extractions
3.  add the directories with the extracted data to a new direction titled 'superfolder'
4.  add the merge.sh script to the superfolder directory and run, this will combine data extracted from multiple tablets into common files
5.  run through steps outlined in stemMigration.R 