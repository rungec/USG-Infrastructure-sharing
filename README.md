# USG-Infrastructure-sharing
Code related to USG Infrastructure sharing project &amp; manuscript "Quantifying the conservation benefits of shared access to linear infrastructure"

Top 5 most impactful mines (scenario 3 independent):  "Iluka", "Mungana", "Eyre Iron", "Kingsgate", "Minotaur"
 - note that even though Lincoln & Valence lines are longer, and pass through a high biodiversity region, Eyre Iron happens to overlap two very range limited species (Acacia wilbleana & Haloragis eyreana). As we calculate impact as proportion of habitat lost (ie greater emphasis on small ranging species), Eyre Iron has a higher total biodiversity impact.
Top 5 most impactful mines (scenario 1 shared): "Iluka", "Valence", "Minotaur", "Lincoln", "Centrex"
Big 5 most valuable mines: "Bhp Billiton", "Iluka", "Arrium", "Iron Road", "OZMinerals"

##Notes following second round of review Cons Biol
Discovered that mine-port link FID38 from scenario 1_high was allocated to Kingsgate, this should have been allocated to Centrex. Consequently FID6 shared of 1_high has 7 NumShared rather than 8. 
Changes did not affect scenario 3, 5, 6 and were noticed before scenario 7 was made.

Manually edited  
> 1_High_infrastructure_v2.shp
> 1_bymine 
> 1_high_data
> 1_high_diffuse_biodiversity_totals.csv
> 1_high_diffuse_biodiversity_total_byspecies_areas.csv
> all the Species_Areas.....csvs
> USG_Collaboration_Scenarios_Costs.xlsx

Remade using *USG_Collaboration_AggregatebyMine_diffuse_141218.r*
> 1_high_diffuse_biodiversity_bymine_split.csv  
> 1_high_diffuse_biodiversity_bymine.csv  

Remade using *USG_Collaboration_AggregatebyMine_diffuse_areas_150602.r*
> 1_high_diffuse_biodiversity_bymine_split_area.csv
> 1_high_diffuse_biodiversity_bymine_area.csv

Remand using *USG_Collaboration_AggregatebyPort_diffuse_160705.r*
> 1_high_diffuse_biodiversity_byport.csv

Remade using *USG_Collaboration_AggregatebyMine_141205.r*
> 1_high_data_bymine_split.csv
> 1_high_data_bymine.csv

Remade manually using data from tables above
> 1_high_finalcosts_161221.csv

Remade using *USG_Collaboration_Plots_160705.r*
> 1_high_diffuse_biodiversity_by5mineportlinks.csv
> Data_for_plots_161221.csv


Noted also that Scenario 3 Lincoln had been allocated to Myponie, this is actually linked to Hardy. This was changed manually in all scenario 3 files.

##Scenario 5:
Based on combination of scenario 1 & 3.
The link for BHP, Arrium & OzMinerals to Bonython follows the shared path from scenario 1_high shared. Illuka & Iron Road share a link to Hardy, the path from Illuka to Iron Road follows that from scenario 1_high shared, then the path from Iron Road follows the path from scenario 3_low, to exclude Centrex.
All other mines use the least-cost links from the 3_low independent scenario.

From .shp
> 5_partial_infrastructure_v2.csv

Manually edited by combining .csvs from scenario 1 & 3
> 5_bymine.csv

Made .csvs using *USG_Collaboration_AggregatebyMine_diffuse_150707.r*

> 5_partial_data.csv
> 5_partial_diffuse_biodiversity_totals.csv
> 5_partial_diffuse_biodiversity_bymine_split.csv
> 5_partial_diffuse_biodiversity_bymine.csv
> 5_partial_finalcosts.csv


##Scenario 6:
Based on scenario 3, but with lines associated with "Iluka", "Mungana", "Eyre Iron", "Kingsgate", "Minotaur" deleted.
Manually deleted unwanted rows from 3_low__diffuse_biodiversity_totals.csv
> 6_lowimpact_diffuse_biodiversity_totals.csv
> 6_lowimpact_independent_finalcosts.csv
The rest of the data is pulled from Scenario 3, using script *USG_Collaboration_Plots_160705.r* on Lines 320-328

##Scenario 7:
Based on scenario 1, but with lines associated with "Iluka", "Mungana", "Eyre Iron", "Kingsgate", "Minotaur" deleted, and number of partners sharing links adjusted downwards to match these deletions.
The link for Lincoln & Valence was drawn from the scenario 3 link for Lincoln, as this is the least cost path excluding Eyre Iron.

There were two options for this scenario - exclude the mines whose independent links have the highest impact (this was what we did). An alternate could have been to calcualte the mines with the highest impact under the shared scenario 1, though this would require iterative calculation of the impacts as a mine is removed. The reason iterative calculation would be required is that the impacts of each link that are allocated to each mine depend on the number of mines sharing that link. ie if 4 mines share a link, then the impact/4. If we remove one of those mines, the impact of each mine increases (impact/3). 

From .shp & manually added "Scen" and "Shared" columns
> 7_lowimpact_shared_infrastructure_v2.csv

Manually edited by combining .csvs from scenario 1 & 3
> 7_bymine.csv
> 7_lowimpact_shared_data.csv
> 7_lowimpact_shared_diffuse_biodiversity_totals.csv

Made using *USG_Collaboration_AggregatebyMine_diffuse_150707.r*
> 7_lowimpact_shared_diffuse_biodiversity_bymine_split.csv
> 7_lowimpact_shared_diffuse_biodiversity_bymine.csv
> 7_lowimpact_shared_finalcosts.csv


