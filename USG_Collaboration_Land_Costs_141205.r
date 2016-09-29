import arcpy
from arcpy import env
import os

inpDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Infrastructure shps\\Shapefiles_v2\\"+".shp"
outDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Land_aquisition_costs\\Number of properties\\scenarios_v2\\"
cadasterShp="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Land_aquisition_costs\\Cadaster_SA_with_costs\\CADASTER_SA_USG_Costs_Albers.shp"

filelist=list.files(inpDir)

for currFile in filelist:
temp1 = str(currFile.split("\\")[7])
temp2 = str(temp1.split(".")[0])

in_features = [currFile,cadasterShp]
out_union = outDir + temp2 + "_"+ "cadaster_union.shp"
arcpy.Union_analysis(in_features, out_union,"ALL","#","GAPS")

#CLIP
out_clip = outDir + temp2 + "_"+ "cadaster_clip.shp"
xy_tolerance = ""

# Execute Clip
arcpy.Clip_analysis(out_union, currFile, out_clip, xy_tolerance)

# Name: Dissolve_Example2.py
# Description: Dissolve features based on common attributes

 
# Import system modules
import arcpy
from arcpy import env
 
# Set environment settings
env.workspace = "C:/data/Portland.gdb/Taxlots"
 
# Set local variables
inFeatures = "taxlots"
tempLayer = "taxlotsLyr"
expression = arcpy.AddFieldDelimiters(inFeatures, "LANDUSE") + " <> ''"
outFeatureClass = "C:/output/output.gdb/taxlots_dissolved"
dissolveFields = ["LANDUSE", "TAXCODE"]
 
# Execute MakeFeatureLayer and SelectLayerByAttribute.  This is only to exclude 
#  features that are not desired in the output.
arcpy.MakeFeatureLayer_management(inFeatures, tempLayer)
arcpy.SelectLayerByAttribute_management(tempLayer, "NEW_SELECTION", expression)
 
# Execute Dissolve using LANDUSE and TAXCODE as Dissolve Fields
arcpy.Dissolve_management(tempLayer, outFeatureClass, dissolveFields, "", 
                          "SINGLE_PART", "DISSOLVE_LINES")




arcpy.Dissolve_management(out_clip, fullArea, "Prob","#","MULTI_PART","DISSOLVE_LINES")