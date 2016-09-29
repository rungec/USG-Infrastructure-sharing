import arcpy
from arcpy import env
import os
import glob

inpDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Infrastructure shps\\Shapefiles_v2\\"+"*.shp"
outDir = "C:\\Claire\\GPEM_Postdoc\\1_USG_Collaboration\\Data\\Infrastructure shps\\Diffuse_impacts_v2\\"

filelist=glob.glob(inpDir)
cliplist=glob.glob(inpDir)
distances=["0.25 Kilometers", "0.5 Kilometers", "1.0 Kilometers"]
names=["_250m_", "_500m_", "_1km_"]

for x in [0,1,2,3]:
        currFile=filelist[x]
        currClip=cliplist[x]
        temp1 = str(currFile.split("\\")[7])
        temp2 = str(temp1.split("_")[0]+"_"+temp1.split("_")[1])

        for i in [0,1,2]:
                currDist=distances[i]
                currName=names[i]
                #Starting buffer
                outBuff = outDir + temp2 + currName + "diffuse.shp"
                arcpy.Buffer_analysis(currFile,outBuff,currDist,"OUTSIDE_ONLY","ROUND","NONE","#")
