#This script takes species shps and clips them to scenario shps, and calculates the amount of each species under each scenario
#Run first loop
#Manually batch project
#Run second loop

inpDir ="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Species\\thresholdmaps\\snes_tif\\"+"*.tif"
outDir ="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Species\\thresholdmaps\\snes_shp\\"
inpDir2="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Species\\thresholdmaps\\snes_shp_Albers\\"+"*.shp"
inpDir3="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Species\\thresholdmaps\\snes_shp_Albers\\"
shpDir ="C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Species\\thresholdmaps\\snes_shp\\"
scenDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Infrastructure shps\\Shapefiles_v2\\"
scenlist = ["1_high_infrastructure_v2.shp","2_medium_infrastructure_v2.shp","3_low_infrastructure_v2.shp","4_none_infrastructure_v2.shp"]

# Import system modules
import arcpy
from arcpy import env
import glob
import osgeo
from osgeo import gdal, ogr, osr
import sys
gdal.UseExceptions()

#spatialRef = osr.SpatialReference()
#spatialRef.ImportFromEPSG(4348) #GDA 94
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput=True

print "system modules imported"

###Make a list of files
##filelist = glob.glob(inpDir)
##print "file list generated"
##
##for currFile in filelist:
##        currFile=str(currFile)
##        temp1 = str(currFile.split("\\")[8])
##        temp2 = str(temp1.split(".")[0])
##
##        print "Starting " + temp2
##
##        # Open raster
##
##        try:
##                tempRast = gdal.Open(currFile)
##        except RuntimeError, e:
##                print 'Unable to open INPUT.tif'
##                print e
##                sys.exit(1)
##
##        try:
##                band = tempRast.GetRasterBand(1)
##        except RuntimeError, e:
##                # for example, try GetRasterBand(10)
##                print 'Band ( %i ) not found' % band_num
##                print e
##                sys.exit(1)
##        
##        #set up polygonise
##        shpDriver = ogr.GetDriverByName("ESRI Shapefile")
##        shpName = shpDir + temp2 + ".shp"
##        outDataSource = shpDriver.CreateDataSource(shpName)
##        outLayer = outDataSource.CreateLayer(shpName, geom_type=ogr.wkbPolygon)
##        newField = ogr.FieldDefn('Prob', ogr.OFTReal)
##        outLayer.CreateField(newField)
##
##        #polgyonise with NoData removed (GetMaskBand())
##        print "\tStep 1: Starting raster to polygon"
##        gdal.Polygonize(band, band.GetMaskBand(), outLayer, 0, [], callback=None)
##
##        ###################
##        #Define projection GDA94
##        print "\tStep 2: Starting projection"
##        arcpy.DefineProjection_management(shpName,"GEOGCS['GCS_GDA_1994',DATUM['D_GDA_1994',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")
#NEXT SECTION DOES NOT WORK - BATCH PROCESS IN ARCGIS
####        # Project to albers
##
##        inpProj = shpName
##        outProj = shpDir + temp2 + "_Albers.shp"
##        arcpy.Project_management(inpProj, outProj,"PROJCS['GDA_1994_Australia_Albers',GEOGCS['GCS_GDA_1994',DATUM['D_GDA_1994',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',132.0],PARAMETER['Standard_Parallel_1',-18.0],PARAMETER['Standard_Parallel_2',-36.0],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]","#","GEOGCS['GCS_GDA_1994',DATUM['D_GDA_1994',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")
##        arcpy.env.outputCoordinateSystem = None
##        arcpy.env.extent = None

##        
#####
####PART 2 - RUN AFTER GIS
filelist2 = glob.glob(inpDir2)
for currFile in filelist2:
        currFile=str(currFile)
        temp1 = str(currFile.split("\\")[9])
        temp2 = str(temp1.split(".")[0])
        temp3 = str(temp2.split("_")[0]) + "_" + str(temp2.split("_")[1])+ "_" + str(temp2.split("_")[2])

	#Calculate range size of species
        print "Starting calculate species range"
        fullArea = inpDir3 + temp2 + "_Area.shp"
        arcpy.Dissolve_management(currFile, fullArea, "Prob","#","MULTI_PART","DISSOLVE_LINES")
        arcpy.AddField_management(fullArea,"Area_km2","Double")
        expression1 = "{0}".format("!SHAPE.area@SQUAREKILOMETERS!")
        arcpy.CalculateField_management(fullArea, "Area_km2", expression1, "PYTHON")
        ##        
        for currScen in scenlist:
                currScenName= str(currScen.split("_")[0]) + "_" + str(currScen.split("_")[1])
                currScenShp = scenDir + currScen
                #currScenShp=str(currScen)
                print currScenName       

                #Union
                print "\tStep 3: Starting union"
                in_features = [currFile,currScenShp]
                out_union = outDir + currScenName + "_"+ temp3 + "_union.shp"
                arcpy.Union_analysis(in_features, out_union,"ALL","#","GAPS")

                # Execute Clip
                print "\tStep 3: Starting clip"
                xy_tolerance = ""
                clip_features = currScenShp
                outClip = outDir + currScenName + "_" + temp3 + "_uniClip.shp"
                arcpy.Clip_analysis(out_union, clip_features, outClip, xy_tolerance)

                #Calculate area (in km^2)
                print "\tStep 4: Starting calculate area"

                arcpy.AddField_management(outClip,"Area2_km2","Double")
                expression1 = "{0}".format("!SHAPE.area@SQUAREKILOMETERS!")        
                arcpy.CalculateField_management(outClip, "Area2_km2", expression1, "PYTHON")
                arcpy.AddField_management(outClip,"Sp_area","Double")
                arcpy.CalculateField_management(outClip,"Sp_area","[Area2_km2]* [Prob]","VB","#")

                #Dissolve by mine-port link
                outSpArea = outDir + currScenName + "_" + temp3 + "_spArea.shp"
                arcpy.Dissolve_management(outClip,outSpArea,"Id","name FIRST;Port FIRST;Area_km2 FIRST;Sp_area SUM","SINGLE_PART","DISSOLVE_LINES")



