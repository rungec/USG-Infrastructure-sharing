import arcpy
from arcpy import env
import os
import glob

inpDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Infrastructure shps\\Shapefiles_v2\\"+"*.shp"
outDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Land_aquisition_costs\\Number of properties\\scenarios_v2\\"
cadasterShp="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Land_aquisition_costs\\Cadaster_SA_with_costs\\CADASTER_SA_USG_Costs_Albers.shp"

filelist=glob.glob(inpDir)

for currFile in filelist[3:]:
    temp1 = str(currFile.split("\\")[7])
    temp2 = str(temp1.split(".")[0])
    #Union_analysis
    print "\tStarting union"
    in_features = [currFile,cadasterShp]
    out_union = outDir + temp2 + "_"+ "cadaster_union.shp"
    arcpy.Union_analysis(in_features, out_union,"ALL","#","GAPS")

    #CLIP
    print "\tStarting clip"
    out_clip = outDir + temp2 + "_"+ "cadaster_clip.shp"
    xy_tolerance = ""
    arcpy.Clip_analysis(out_union, currFile, out_clip, xy_tolerance)

    #Dissolve
    print "\tDissolving"
    out_diss = outDir + temp2 + "_"+ "cadaster.shp"
    #arcpy.Dissolve_management(out_clip, out_diss, "Id;name;Port;Area_km2;Length_km;Num_shared","Count SUM","MULTI_PART","DISSOLVE_LINES")
    arcpy.Dissolve_management(out_clip, out_diss, "Id;name;Port;Area_km2;Length_km","Count SUM","MULTI_PART","DISSOLVE_LINES")
