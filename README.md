# desfert-stems

This repository contains the materials for creating the ODK stems app, and tools & procedures for processing collected data.

### transfer collected data from the tablets

There are two workflows for converting ODK XML data to tabular form, one employing ODK's Briefcase tool and the other employing custom R scripts. The ODK Briefcase workflow was used until the fall 2020 collection, when it stopped working. This is likely due to the age of the cndep stems app. However, previous collections using previous versions of the ODK Briefcase tool did not seem to work either, so there could be a combination of Java + app + Briefcase interactions causing a problem. As this could not be resolved, custom scripts for converting ODK XML data to tabular form were developed to access the fall 2020 collection data. See `workflow_briefcase.R` for step-by-step instructions for the ODK Briefcase workflow, and `workflow_xml.R` for step-by-step instructions to extract the data using the custom R scripts. Regardless of the workflow employed to extract the ODK XML data and convert to tabular form, use `populate_database.R`, with only minor deviances between workflows, for uploading the stems data to the database.

**tablet to computer**

Copy the contents of the ODK folder from each tablet into a folder with the corresponding name of the tablet (alpha, bravo, charlie, delta).
![copy_odk_data](figures/copy_odk_data.png)

**workflow: ODK Briefcase**

- Use [ODK briefcase](https://docs.opendatakit.org/briefcase-intro/) to extract tablet XML data into tabular form
  + Run *unextracted* briefcase java program from command line with ```java -jar ODK\ Briefcase\ v1.5.0\ Production.jar```. Note that this path will be different depending on the details of the Briefcase installation. A new version of Briefcase is usually downloaded with each use rather than storing it, so details such as the version number (and the corresponding file/path) will be different each time. The steps here are specific to Linux.
  + set a Briefcase storage location (any empty directory)
  + *Pull* (collect data option) data from each tablet using Briefcase into the aforementioned storage location
  + *Export* pulled data from the Briefcase storage location into a folder titled `superfolder`
  + merge data from multiple directories/tablets^*^
- Add the directories with the extracted data to a new directory titled 'superfolder'.
- Add the `merge.sh` script to the superfolder directory and run, this will combine data extracted from multiple tablets into common files.
- Use `workflow_briefcase.R` and `populate_database.R` to extract and upload data.

^*^it is not clear that the merge step is required so long as tablet data are *pulled* to a single, common Briefcase storge location, then *exported* to a single export location (e.g., 'superfolder' from above). This section left in the workflow pending confirmation.

**workflow: custom scripts**

- Use `workflow_xml.R` and `populate_database.R` to extract and upload data.

### the condition of missing plant data

The fall 2021 collection presented a (seemingly) new problem in that there were
data for three different L3 plants collected at site 21 with no way to
distinguish the actual L3 and from L2, which was not collected. Separate but
possibly related, all L2 data were missing from plot 29 - could be a connection
between the 21 and 29 inconsistenties but they were collected on different days
so very difficult to discern. Anyway, as a result, I had no choice but to
remove all L3 data from plot 21. These data had to be filtered from the new,
old, and plotsplants objects, but then readded to plotsplants in a systematic
way. I also noted this in stem_comments.

```r
old <- dplyr::bind_rows(
  old |> dplyr::filter(plot_id == 21 & plant_id != "L3"),
  old |> dplyr::filter(plot_id != 21)
)

new <- dplyr::bind_rows(
  new |> dplyr::filter(plot_id == 21 & plant_id != "L3"),
  new |> dplyr::filter(plot_id != 21) #,
)

plotsplants <- dplyr::bind_rows(
  plotsplants |> dplyr::filter(plot_id == 21 & plant_id != "L3"),
  plotsplants |> dplyr::filter(plot_id != 21),
  dplyr::inner_join(
    tibble::tibble(
      direction   = c("North", "South", "West", "East"),
      dir         = c("N", "S", "W", "E"),
      plot_id     = 21,
      survey_date = "2021-10-11",
      plant_note  = "missing field data"
      ),
    tibble::tibble(
      plant_id    = c("L2", "L3"),
      plot_id     = 21,
      ),
    by = c("plot_id")
  ),
  dplyr::inner_join(
    tibble::tibble(
      direction   = c("North", "South", "West", "East"),
      dir         = c("N", "S", "W", "E"),
      plot_id     = 29,
      survey_date = "2021-10-13",
      plant_note  = "missing field data"
      ),
    tibble::tibble(
      plant_id    = c("L2"),
      plot_id     = 29,
      ),
    by = c("plot_id")
  )
)
```
