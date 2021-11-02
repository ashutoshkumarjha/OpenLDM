#python 3.5.2
import sys, os, random, time, re,imp
import shutil,tempfile
import os.path
from os.path import basename, splitext
from array import array
from datetime import datetime
from collections import OrderedDict

#################QT GUI IMPORTS#################
from PyQt5 import QtCore, QtGui,QtNetwork,QtWidgets
from PyQt5.QtCore import Qt,QFileInfo,QFile,QUrl
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtGui import QColor,QBrush
from PyQt5.QtWidgets import QLineEdit,QApplication,QTableWidgetItem,QPushButton,QLabel,QFileDialog,QComboBox,QMessageBox,QColorDialog
#from PyQt5.QtWebEngineWidgets import QWebEngineView
from LULCgui import Ui_LULCModel
from math import sqrt

################Resource File###################
import resources_rc

#################FOR R IMPORT####################
import rpy2.robjects as R
from rpy2.robjects.packages import importr

#################FOR MAP Display and Print########



__consti = 0


class StatusBarThread(QtCore.QThread):
    trigger = QtCore.pyqtSignal(int)

    def __init__(self,statusBar, parent=None):
        QtCore.QThread.__init__(self)
        self.statusBar=statusBar
        self.startTime = datetime.now();
        self.message=""
        

    def setup(self, thread_no):
        self.thread_no = thread_no

    def run(self):
        while(1==1):
            time.sleep(1)
            timediff=datetime.now()-self.startTime
            self.message="TotalElapsed Time(sec)"+str(timediff.seconds)
            #print(self.message)

    def getMessage(self):
        return(self.message)


class MyForm(QtWidgets.QMainWindow):

    ###############################333
    #Add all the save variable here
    global __consti
    __projectDirectory = "."
    __raster = True
    __currentDirectory="."
    __T0File = ""
    __T1File = ""
    __T0Year = 0
    __T1Year = 0
    __OutputFile= "2005.tif"
    __shpfileT0 = ""
    __shpfileT1 = ""
    __checkOnScreen = 1
    __neughbourl=[]
    __DriverDictionaryT1 = {}
    __DriverDictionaryT2 = {}
    __modelNAValue=128
    __driversT1 = []
    __transitionMatrix=[]
    __noOfClasses=0
    __className=[]
    customlist = []
    res = ""
    Coeffcient = []
    __T1File = ""
    __T0File = ""
    __MASKFile=""
    __AOIFile=""
    __modelformula=""
    __modeltype=""
    #ModelType = "logistic"
    __Drivername = []
    noOfDrivers = 0
    __confidenceinterval=[]
    __demand=[]
    processingstep="Data Preparation"
    plot="onscreen"
    __installedDir="."
    __debug=1
    __suitabilityFileDirectory=""
    __ReferenceFile=R.NA_Logical
    currentLogTime="19760101000000"

    def r_converToRaster():
        rvalue = R.r['pi']
        #


    def __init__(self, parent=None):
        #R.r(''' source('Rasterise_dev_68.r') ''')
        #R.r(self.rcode)
        QtWidgets.QWidget.__init__(self, parent)
        self.ui = Ui_LULCModel()
        self.ui.setupUi(self)
        #All the new objects here as self.ui.
        self.modelSummary="NULL"
        #self.rcode=unicode("""
        #Rcode
        #"""
        #,"utf-8")
        self.currentLogTime = datetime.now().strftime('%Y%m%d%H%M%S')     
        self.prepareExecutionEnv()
        self.prepareR()
        self.initModelParam()
        self.initGui()
        self.initStatusBar() 

    def getStatus(self):
        self.statusBar().showMessage(self.statusMessage.getMessage())#+"                       "+"Doing:-"+self.processingstep)#view+str(R.r('getRStatus()')))

    def initStatusBar(self):
        self.statusMessage=StatusBarThread(self.ui.statusBar)
        self.statusMessage.start()
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.getStatus)
        # check every second
        self.timer.start(1000*1)

    def initGui(self):
        self.setGeometry(50,50,900,600)
        self.resize(900,600)
        self.setWindowTitle("Open-source Land-use Land-cover Dynamics Modeling Platfrom ver-1.0")
        self.ui.leOutputFile_DataPreparationOutputSection.setText("")
        self.ui.twSelectDrivers_DriverSelectionT0.resizeColumnsToContents()
        self.loadHelpFile()
        #QtCore.QMetaObject.connectSlotsByName(self)

    def initModelParam(self):
        self.__demand=R.NA_Logical
        self.__modelNAValue=R.NA_Logical
        self.__MASKFile=R.NA_Logical
        self.__AOIFile=R.NA_Logical
        self.__suitabilityFileDirectory=R.NA_Logical

    #############################################################################
    def loadHelpFile(self):
        fileName=self.__installedDir+"/docs/OpenLDM.html"
        fd = QFile(fileName)
        if not fd.open(QtCore.QIODevice.ReadOnly):
            QtGui.QMessageBox.information(self, "Unable to open file",fd.errorString())
            return

        output = QtCore.QTextStream(fd).readAll()
        # Display contents.
        self.setBaseUrl(QtCore.QUrl.fromLocalFile(fileName))
        self.ui.webView_Help.setHtml(output, self.baseUrl)

    def setBaseUrl(self, url):
        self.baseUrl = url

    def setNA(self):
        navalue=self.ui.leNAValue_DataPreparationInputSectionProjectSection.text()
        if(navalue=='NA' or len(navalue)==0):
            self.__modelNAValue=R.NA_Logical
        else:
            self.__modelNAValue=int(self.ui.leNAValue_DataPreparationInputSectionProjectSection.text())


    def getRasterLayer(self,fileName):
        fileInfo = QFileInfo(fileName)
        baseName = fileInfo.baseName()
        rlayer = QgsRasterLayer(fileName, baseName)
        #if not rlayer.isValid():
            #print "Layer failed to load!"
        return rlayer

    # def setRasterAsPsuedoColor(self,rlayer):
        # print rlayer.width(), rlayer.height(),rlayer.rasterType()
        # rlayer.setDrawingStyle(QgsRasterLayer.SingleBandPseudoColor)
        # print rlayer.drawingStyle()
        # rlayer.setColorShadingAlgorithm(QgsRasterLayer.ColorRampShader)
        # lst = [ QgsColorRampShader.ColorRampItem(0, QColor(0,255,0)), QgsColorRampShader.ColorRampItem(255, QColor(255,255,0)) ]
        # fcn = rlayer.rasterShader().rasterShaderFunction()
        # fcn.setColorRampType(QgsColorRampShader.INTERPOLATED)
        # fcn.setColorRampItemList(lst)
        # if hasattr(rlayer, "setCacheImage"): rlayer.setCacheImage(None)
        # rlayer.triggerRepaint()

    def update_text(self, thread_no):
        QtGui.QApplication.processEvents()
        time.sleep(random.uniform(0,0.7))
        self.ui.tbLog.setText(str(self.__check)+"% Completed")
        self.__check = self.__check + 1
        if(self.__check == 100):
            self.ui.leOutputFile.setText(str(self.__currentDirectory)+"/"+self.__T0File+".tif")
        QtGui.QApplication.processEvents()

    #Add all the requuired signles Here
    @pyqtSlot()
    def on_pbSelectDirectory_DataPreparationInputSectionProjectSection_clicked(self):
        self.__projectDirectory = QFileDialog.getExistingDirectory(self, "Open Project Directory",".", QFileDialog.ShowDirsOnly);
        self.ui.leProjectDirectory_DataPreparationInputSectionProjectSection.setText(str(self.__projectDirectory))
        #QtCore.QObject.connect(self.ui.pbSelectDirectory_DataPreparationInputSectionProjectSection, QtCore.SIGNAL("clicked()"), self.ui.leProjectDirectory_DataPreparationInputSectionProjectSection_DataPreparationInputSectionProjectSection.clear )
        #QtCore.QObject.connect(self.ui.lineEdit, QtCore.SIGNAL("returnPressed()"), self.add_entry)

    @pyqtSlot()
    def on_pbSelectFileT0File_DataPreparationInputSelectionDataInput_clicked(self):
        file,_ = QFileDialog.getOpenFileName(self, "Open File",
                                                           self.__projectDirectory,"Raster (*.img *.tif );;Esri Shape (*.shp)");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.__T0File=splitext(fileName)[0]
        fileType = splitext(fileName)[1]
        if(fileType == ".tif" or fileType == ".img"):
            self.__T0File=str(file)
            #self.ui.leT0File_DataPreparationInputSectionDataInput.setEnabled(False)
            #self.ui.leT0File_DataPreparationInputSectionDataInput.setText(str(self.__T0File))
            self.enable_NextDataPreaparation()
        elif(fileType == ".shp" ):
            self.__shpfileT0 = str(file)
            self.__T0File=self.__T0File+".tif"
        self.ui.leT0File_DataPreparationInputSectionDataInput.setText(str(self.__T0File))


    @pyqtSlot()
    def on_pbSelectFileT1File_DataPreparationInputSelectionDataInput_clicked(self):
        file,_ = QtWidgets.QFileDialog.getOpenFileName(self, "Open File",
                                                           self.__currentDirectory,"Raster (*.img *.tif );;Esri Shape (*.shp)");
        #filename,_=file.filename()
        print(file)

        (dirName, fileName) = os.path.split(str(file))
        #self.__currentDirectory=dirName
        self.__T1File=splitext(fileName)[0]
        fileType = splitext(fileName)[1]
        if(fileType == ".tif" or fileType == ".img"):
            self.__T1File=str(file)
            #self.ui.leT1File_DataPreparationInputSectionDataInput.setEnabled(False)
            self.ui.leT1File_DataPreparationInputSectionDataInput.setText(str(self.__T1File))
            self.enable_NextDataPreaparation()
        else:
            self.__shpfileT1 = str(file)
            self.__T1File=self.__T1File+".tif"
        self.ui.leT1File_DataPreparationInputSectionDataInput.setText(str(self.__T1File))


    @pyqtSlot()
    def on_pbSelectFileOutputFile_DataPreparationOutputSection_clicked(self):
        fileLocation = QFileDialog.getSaveFileName(self, "Output File",
                                                                 self.__currentDirectory,"Raster (*.tif )")[0];
        (dirName, fileName) = os.path.split(str(fileLocation))
        print(dirName, fileName)
        #self.__currentDirectory=dirName
        fileType = splitext(fileName)[1]
        if(fileType == ".tif"):# or fileType == ".img"):
            self.__OutputFile=str(fileLocation)
        else:
            self.__OutputFile = str(fileLocation)+".tif"
        #self.ui.leOutputFile_DataPreparationOutputSection.setEnabled(False)
        self.ui.leOutputFile_DataPreparationOutputSection.setText(self.__OutputFile)
        self.ui.lePredictedFile_AccuracyAssesment.setText(self.__OutputFile)
        self.enable_NextDataPreaparation()



    @pyqtSlot()
    def on_pbConvert_T0_clicked(self):
        print('TODO')

    def raster(self, tlhread1_no):
        r_rasterize = R.globalenv['rasterise']
        if(self.ui.leYear_TO.text()!=""):
            self.__T0File=str(self.ui.leYear_TO.text())
        r_rasterize(self.__shpfileT0,self.__T0File,self.ui.sbGridsize_DataPreparationInputSectionProjectSection.value())
        self.__T0File = str(self.__currentDirectory)+"/"+self.__T0File+".tif"
        self.ui.leT0File_DataPreparationInputSectionDataInput.setText(self.__T0File)

    @pyqtSlot()
    def on_pbConvert_T1_clicked(self):
        r_rasterize = R.globalenv['rasterise']
        if(self.ui.leYear_T1.text()!=""):
            self.__T1File=str(self.ui.leYear_T1.text())
        r_rasterize(self.__shpfileT1,self.__T1File,self.ui.sbGridsize_DataPreparationInputSectionProjectSection.value())
        self.__T1File = str(self.__currentDirectory)+"/"+self.__T1File+".tif"
        self.ui.leT1File_DataPreparationInputSectionDataInput.setText(self.__T1File)

    @pyqtSlot()
    def on_pbSelectFileAreaOfInterest_DataPreparationInputSectionDataInput_clicked(self):
        file,_ = QFileDialog.getOpenFileName(self, "Open File",
                                                           self.__currentDirectory,"ESRI (*.shp )");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.ui.leAreaOfInterest_DataPreparationInputSectionDataInput.setText(str(file))

    @pyqtSlot()
    def on_pbSelectFileMask_DataPreparationInputSectionDataInput_clicked(self):
        file,_ = QFileDialog.getOpenFileName(self, "Open File",
                                                       self.__currentDirectory,"ESRI (*.shp )");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.ui.leMask_DataPreparationInputSectionDataInput.setText(str(file))

    def getCurrentDirectory():
        return(self.__currentDirectory)


    @pyqtSlot()
    def on_pbConvertToRaster_T0_clicked(self):
        if (self.__raster==False):
            self.r_convertToRaster();

    @pyqtSlot()
    def on_pbNextDataPreparation_clicked(self):
        self.setNA()
        self.ui.tabWidget.setCurrentIndex(1)
        self.ui.progressBar.setProperty("value",10)
        self.preparecbInSteps_DemandAllocationSpatialContext()
        self.ui.gbSelectDrivers_DriverSelectionT0.setEnabled(True)
        self.ui.pbAddDriver_DriverSelectionT0SelectDrivers.setEnabled(True)
        self.ui.twSelectDrivers_DriverSelectionT0.setEnabled(True)


    #################Module:2####################



    @pyqtSlot() # prevents executing following function twice
    def SelectDriver_pushed(self):
        sending_button = str(self.sender().objectName())
        rowstr = sending_button[sending_button.index('SelectDriver')+12:]
#Object name is suffiexed by the driver number and row no is less than one of that
        row = int(rowstr)-1
        filename,_ = QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)");
        (dirName, OnlyFilename) = os.path.split(filename.strip())
        self.__currentDirectory=dirName
        #self.__driversT1.append(str(file))
        disp=self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(row, 1)
        disp.setText(filename)
        disp=self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(row, 0)
        disp.setText(OnlyFilename.replace(".","_"))
        #print(disp.text())
        self.ui.twSelectDrivers_DriverSelectionT0.resizeColumnsToContents()


    @QtCore.pyqtSlot() # prevents executing following function twice
    def DeleteDriver_pushed(self):
        sending_button = str(self.sender().objectName())
        rowstr = sending_button[sending_button.index('DeleteDriver')+12:] #Object name is suffiexed by the driver number and row no is less than one of that
        row = int(rowstr)
        lastRow=self.ui.twSelectDrivers_DriverSelectionT0.rowCount();
        if(row==lastRow):
            reply= QtWidgets.QMessageBox.question(self, "Delete Row","Are You Sure?",QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No, QtWidgets.QMessageBox.No);
            if(reply==QtWidgets.QMessageBox.Yes ):
                self.ui.twSelectDrivers_DriverSelectionT0.resizeColumnsToContents()
                disp=self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(row, 1)
                self.ui.twSelectDrivers_DriverSelectionT0.removeRow(row-1)
                #print(sending_button," is Deleted from RowIndex.")
        else:
            QtWidgets.QMessageBox.about(self, "Delete Last Row","Only Last Row can be deleted");
        self.ui.twSelectDrivers_DriverSelectionT0.resizeColumnsToContents()
        if (lastRow<4):
            self.ui.gbDoModelFitting_DriverSelectionT0.setEnabled(False)
        else:
            self.ui.gbDoModelFitting_DriverSelectionT0.setEnabled(True)

    @pyqtSlot()
    def on_pbAddDriver_DriverSelectionT0SelectDrivers_clicked(self):
        #Append New Row
        workingRow=self.ui.twSelectDrivers_DriverSelectionT0.rowCount()+1
        workingRowIdx=workingRow-1
        self.ui.twSelectDrivers_DriverSelectionT0.setRowCount(workingRow)

        #Put label for driver name
        nameLabel = QtWidgets.QLineEdit("Driver"+str(workingRow))
        self.ui.twSelectDrivers_DriverSelectionT0.setCellWidget(workingRowIdx, 0, nameLabel)

        #put Place for file name
        disp = QLabel("")
        self.ui.twSelectDrivers_DriverSelectionT0.setCellWidget(workingRowIdx, 1,disp)

        #Put Select Driver Button
        pbSelectDriver = QtWidgets.QPushButton()
        pbSelectDriver.setText("Select Driver")
        pbSelectDriver.setObjectName("SelectDriver"+str(workingRow))
        pbSelectDriver.clicked.connect(self.SelectDriver_pushed)
        self.ui.twSelectDrivers_DriverSelectionT0.setCellWidget(workingRowIdx, 2, pbSelectDriver)

        #Put Delete Driver Button
        pbDeleteDriver = QtWidgets.QPushButton()
        pbDeleteDriver.setText("Delete Driver")
        pbDeleteDriver.setObjectName("DeleteDriver"+str(workingRow))
        pbDeleteDriver.clicked.connect(self.DeleteDriver_pushed)
        self.ui.twSelectDrivers_DriverSelectionT0.setCellWidget(workingRowIdx, 3, pbDeleteDriver)

        if(self.ui.twSelectDrivers_DriverSelectionT0.rowCount()>2):
            self.ui.gbDoModelFitting_DriverSelectionT0.setEnabled(True)

        self.ui.twSelectDrivers_DriverSelectionT0.resizeColumnsToContents()

    def createModelStatisticDetails(self):
        filename=self.ui.leT0File_DataPreparationInputSectionDataInput.text();
        if (filename != self.__T0File):
            self.__T0File=filename
        filename=self.ui.leT1File_DataPreparationInputSectionDataInput.text();
        if (filename != self.__T1File):
            self.__T1File=filename
        self.__Drivername=[]
        self.__driversT1=[]
        self.__DriverDictionaryT1= OrderedDict()#{};
        for i in list(range(0,self.ui.twSelectDrivers_DriverSelectionT0.rowCount())):
            self.__Drivername.append(str(self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(i,0).text()))
            self.__driversT1.append(str(self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(i,1).text()))
            self.__DriverDictionaryT1[str(self.__Drivername[i])] = self.__driversT1[i]
        self.noOfDrivers = len(self.__DriverDictionaryT1)
        if(self.__debug==1):
            print(self.__Drivername)
            print(self.__driversT1)
            
        projectNA=self.ui.leNAValue_DataPreparationInputSectionProjectSection.text()
        if (projectNA=='NA'):
            self.__modelNAValue=R.NA_Logical
        else:
            self.__modelNAValue=int(projectNA)
        filename=self.ui.leAreaOfInterest_DataPreparationInputSectionDataInput.text();
        if(len(filename)!=0):
            if (filename != self.__AOIFile):
                self.__AOIFile=str(filename)
        else:
            self.__AOIFile=R.NA_Logical
        filename=self.ui.leMask_DataPreparationInputSectionDataInput.text();
        if(len(filename)!=0):
            if (filename != self.__MASKFile):
                self.__MASKFile=str(filename)
        else:
            self.__MASKFile=R.NA_Logical
    @pyqtSlot()
    def on_pbViewModelStatistics_DriverSelectionT0DoModelFitting_clicked(self):
        if(self.__debug==1):
            print ("on_pbViewModelStatistics_DriverSelectionT0DoModelFitting_clicked")
        self.createModelStatisticDetails();
        self.processingstep="Doing Model Summary";
        self.printParameter()
        drvs = R.ListVector(self.__DriverDictionaryT1)
        genSummary = R.r['getModelFitSummary']
        modelSummary = genSummary(T1File=self.__T0File,T2File=self.__T1File,T1drivers=drvs,modelType=str(self.__modeltype),withNAvalue=self.__modelNAValue,method="NotIncludeCurrentClass",maskFile=self.__MASKFile,aoiFile=self.__AOIFile)
        path=os.path.join(str(self.__projectDirectory),self.currentLogTime+'-summary.log')
        sink=R.r['sink']
        sink(path) 
        R.r['print'](modelSummary)
        sink=R.r['sink']
        sink()
        self.modelSummary= os.linesep.join([s for s in str(modelSummary).splitlines() if s.strip()]).replace("\\t",'\t').replace("\\n",'\n')
        self.ui.teModelparameterOutput_DriverSelectionT0DoModelFitting.setPlainText(str(self.modelSummary))
        self.ui.pbNext_DriverSelectionT0.setEnabled(True)
        self.ui.teModelparameterOutput_DriverSelectionT0DoModelFitting.setEnabled(True)
        #self.releaseR()

    def buildtwSelectModelTypeAndDrivers_DriverSelectionT1(self,twSelectModelTypeAndDrivers):
        twSelectModelTypeAndDrivers.setColumnCount(self.noOfDrivers+3)
        twSelectModelTypeAndDrivers.setRowCount(self.__noOfClasses)
        row=twSelectModelTypeAndDrivers.rowCount()
        col=twSelectModelTypeAndDrivers.columnCount()
        for i in list(range(0, row, 1)):
            for j in list(range(0, col, 1)):
                item = QTableWidgetItem()
                if(j!=0):
                    item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                else:
                    item.setText('Class'+str(i+1))
                    label=QLabel(self.getSelectedModel())
                    self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,label)                    
                twSelectModelTypeAndDrivers.setItem(i, j, item)

        for i in list(range(0, row, 1)):
            for j in list(range(3, col, 1)):
                item = twSelectModelTypeAndDrivers.item(i,j)
                item.setFlags(QtCore.Qt.ItemIsDragEnabled|QtCore.Qt.ItemIsEnabled)
                item.setCheckState(QtCore.Qt.Checked)

        stringlist1 = list()#QtCore.QStringList()
        stringlist1.append('Class')
        stringlist1.append('DN Value')
        stringlist1.append('Model Type')
        for i in list(range(0,self.noOfDrivers,1)):
            stringlist1.append(list(self.__DriverDictionaryT1.keys())[i])

        twSelectModelTypeAndDrivers.setHorizontalHeaderLabels(stringlist1)
        twSelectModelTypeAndDrivers.resizeColumnsToContents()

    def getSelectedModel(self):
        if(self.ui.rbLogisiticRegression_ModelAnalysisViewModelCoeeffcient.isChecked()):
            modelname="Logistic Regression"
        elif(self.ui.rbLinearRegression_ModelAnalysisViewModelCoeeffcient.isChecked() ):
            modelname="Linear Regression"
        elif(self.ui.rbNeuralRegression_ModelAnalysisViewModelCoeeffcient.isChecked()):
            modelname="Neural Regression"
        elif(self.ui.rbRandomForest_ModelAnalysisViewModelCoeeffcient.isChecked()):
            modelname="Random Forest"        
            
        #if(currentIndexItem == ):
        return(modelname)


    def buildtwViewModelCoefficint_ModelAnalysis(self,twViewModelCoefficint):
        twViewModelCoefficint.setRowCount(self.__noOfClasses)
        twViewModelCoefficint.setColumnCount(self.noOfDrivers+3)
        #Setting Individual tabelitems
        row=twViewModelCoefficint.rowCount()
        col=twViewModelCoefficint.columnCount()
        for i in list(range(0, row, 1)):
            for j in list(range(0, col, 1)):
                item = QTableWidgetItem()
                if(j!=0):
                    item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                else:
                    item.setText('Class'+str(i+1))
                twViewModelCoefficint.setItem(i, j, item)
        #Setting Table Header
        stringlist2 = list()#QtCore.QStringList()
        stringlist2.append('Class')
        stringlist2.append('DN Value')
        stringlist2.append('Intercept')
        for i in list(range(0,len(self.__DriverDictionaryT1),1)):
            stringlist2.append(list(self.__DriverDictionaryT1.keys())[i])

        twViewModelCoefficint.setHorizontalHeaderLabels(stringlist2)
        twViewModelCoefficint.resizeColumnsToContents()

        for i in list(range(0, row, 1)):
            for j in list(range(0, col, 1)):
                item = twViewModelCoefficint.item(i,j)
                if(j>2):
                    item.setFlags(QtCore.Qt.ItemIsDragEnabled|QtCore.Qt.ItemIsUserCheckable|QtCore.Qt.ItemIsEnabled)
                    item.setCheckState(QtCore.Qt.Checked)

    def populateDataIntoTableViewModelCoefficint_ModelAnalysis(self):
        #Populate the DriversFile in ModelAnalysis tab
        for j in list(range(0,self.ui.twViewModelCoefficint_ModelAnalysis.rowCount(),1)):
            item1 = self.ui.twViewModelCoefficint_ModelAnalysis.item(j,1)
            item1.setText(self.__className[j])
            item1.setFlags(QtCore.Qt.ItemIsDragEnabled|QtCore.Qt.ItemIsEnabled)
        
        for j in list(range(0,self.ui.twViewModelCoefficint_ModelAnalysis.rowCount(),1)):
            for k in list(range(2,self.ui.twViewModelCoefficint_ModelAnalysis.columnCount(),1)): #First three colums are reserved for classname,number and modetype
                #print(str(len(self.__confidenceinterval[j]))+"---539")
                item1 = self.ui.twViewModelCoefficint_ModelAnalysis.item(j,k)
                item1.setText(str(self.__confidenceinterval[j][k-2][1])) #
                toolstr = str(self.__confidenceinterval[j][k-2][2]) #k-3+1 since Intercept is at k-3
                item1.setToolTip(toolstr)

    @QtCore.pyqtSlot()
    def on_pbNext_DriverSelectionT0_clicked(self):
        getClassNum = R.r['getClassNumber']
        self.__className = getClassNum(self.__T0File)
        self.__noOfClasses=len(self.__className)
        self.noOfDrivers=len(self.__Drivername)
        #self.buildtwViewModelCoefficint(self.ui.twViewModelCoefficint_ModelAnalysis)
        self.createLULCVsDriverCoefficientMatrix();
        #print(self.createLULCVsDriverCoefficientMatrix())
        self.buildtwMigrationOrder_ModelAnalysis(self.ui.twMigrationOrder_ModelAnalysis)
        self.buildtwSelectModelTypeAndDrivers_DriverSelectionT1(self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1)
        self.buildtwPolicies_DemandAllocation(self.ui.twPolicies_DemandAllocation);
        self.buildtwColorTable_ViewMaps(self.ui.twColorTable_ViewMaps)
        self.buildtwViewModelCoefficint_ModelAnalysis(self.ui.twViewModelCoefficint_ModelAnalysis);
        self.populateDataIntoTableViewModelCoefficint_ModelAnalysis()
        self.populateDataIntoTableModelTypeAndDriversDriverSelectionT1()
        self.ui.tabWidget.setCurrentIndex(2)
        self.ui.progressBar.setProperty("value",30)
        self.ui.gbViewModelCoefficient_ModelAnalysis.setEnabled(True)
        self.ui.gbMigrationOrder_ModelAnalysis.setEnabled(True)
        self.ui.twMigrationOrder_ModelAnalysis.setEnabled(False)
        self.setModelAnalysisModelType()
        self.ui.pbNext_ModelAnalysis.setEnabled(True)


    @pyqtSlot()
    def on_pbNext_ModelAnalysis_clicked(self):
        self.ui.gbSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(True)
        self.ui.tabWidget.setCurrentIndex(3)
        self.ui.progressBar.setProperty("value",40)
        self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(True)
        self.ui.pbNext_DriverSelectionT1.setEnabled(True)
        self.ui.twColorTable_ViewMaps.setEnabled(True)
        
    @pyqtSlot()
    def on_pbNext_DriverSelectionT1_clicked(self):
        self.ui.tabWidget.setCurrentIndex(4)
        self.ui.progressBar.setProperty("value",50)
        self.ui.gbPolicies.setEnabled(True)
        self.ui.gbSpatialContext.setEnabled(True)
###pratu
        ##self.ui.gbSuitabilityMapGeneration.setEnabled(True)
        self.ui.leSuitablityFile_OutputGenerationSuitabilityMapGeneration.setEnabled(False)
        self.ui.pbSelectFileSuitablityFile_OutputGenerationSuitabilityMapGeneration.setEnabled(False)
        self.ui.lbSuitablityfile.setEnabled(False)
###
        self.ui.pbExecute_DemandAllocation.setEnabled(True)

    @pyqtSlot(bool)
    def on_cbclassallocation_DemandAllocation_clicked(self,state):
        #print("on_cbclassallocation_DemandAllocation_clicked")
        tw=self.ui.twPolicies_DemandAllocation
        if(state):
            for i in list(range(0,tw.rowCount()-1,1)):
                item=tw.item(i,1)
                label=str(i+1)
                self.ui.twPolicies_DemandAllocation.item(i,1).setText(label)
                item.setFlags(item.flags()|QtCore.Qt.ItemIsEnabled|QtCore.Qt.ItemIsSelectable|QtCore.Qt.ItemIsEditable)


        else:
            for i in list(range(0,tw.rowCount(),1)):
                item=tw.item(i,1)
                item.setFlags(item.flags()^QtCore.Qt.ItemIsEnabled)
                
    @pyqtSlot(bool)
    def on_cbUserDefinedClassInertia_DemandAllocation_clicked(self,state):
        tw=self.ui.twPolicies_DemandAllocation
        if(state):
            for i in list(range(0,tw.rowCount()-1,1)):
                item=tw.item(i,3)
                self.ui.twPolicies_DemandAllocation.item(i,3).setText('0')
                item.setFlags(item.flags()|QtCore.Qt.ItemIsEnabled|QtCore.Qt.ItemIsSelectable|QtCore.Qt.ItemIsEditable)


        else:
            for i in list(range(0,tw.rowCount(),1)):
                item=tw.item(i,3)
                item.setFlags(item.flags()^QtCore.Qt.ItemIsEnabled)


    @pyqtSlot(bool)
    def on_cbUserDefinedDemand_DemandAllocation_clicked(self,state):
        #print("on_cbUserDefinedDemand_DemandAllocation_clicked")
        tw=self.ui.twPolicies_DemandAllocation
        if(state):
            for i in list(range(0,tw.rowCount()-1,1)):
                item=tw.item(i,2)
                item.setFlags(item.flags()|QtCore.Qt.ItemIsEnabled|QtCore.Qt.ItemIsSelectable|QtCore.Qt.ItemIsEditable)
        else:
            for i in list(range(0,tw.rowCount(),1)):
                item=tw.item(i,2)
                item.setFlags(item.flags()^QtCore.Qt.ItemIsEnabled)
####pratu

    @pyqtSlot(QTableWidgetItem)
    def on_twPolicies_DemandAllocation_itemChanged(self,item):
        #print("on_twPolicies_DemandAllocation_itemChanged")
        #if(self.ui.twPolicies_DemandAllocation.isItemSelected(item)):
        if(item.isSelected()):
            col=self.ui.twPolicies_DemandAllocation.currentColumn()
            row=self.ui.twPolicies_DemandAllocation.currentRow()
            sum=0
            if(col==1):
                getvalue=str(item.text())
                if(getvalue.isdigit()):
                    newvalue=int(getvalue)
                    oldvalue=newvalue
                    for j in list(range(0,self.__noOfClasses,1)):
                        flag=1
                        for k in list(range(0,self.__noOfClasses,1)):
                            checkitem=int(str(self.ui.twPolicies_DemandAllocation.item(k,1).text()))
                            if((j+1)==checkitem):
                                flag=0
                                break
                        if(flag!=0):
                            oldvalue=str(j+1)
                            break
                    if(0<newvalue<=int(str(self.__noOfClasses))):
                        for i in list(range(0,self.__noOfClasses,1)):
                            if(i!=row):
                                checkitem=int(str(self.ui.twPolicies_DemandAllocation.item(i,1).text()))
                        if(checkitem==newvalue):
                            newrow=i
                            self.ui.twPolicies_DemandAllocation.item(newrow,1).setText(oldvalue)
                    else:
                        print ("please enter value within range:1-",self.__noOfClasses)
                        self.ui.twPolicies_DemandAllocation.item(row,1).setText(oldvalue)
                else:
                    print ("WRONG INPUT")
                    self.ui.twPolicies_DemandAllocation.item(row,1).setText(oldvalue)
                #self.ui.twPolicies_DemandAllocation.setCurrentCell(row+1,1)
            elif(col==3):
                r=str(item.text())
                lenn=len(r)
                if(lenn==1):
                    if not(r[0]=='0'or r[0]=='1'):
                        self.ui.twPolicies_DemandAllocation.item(row,3).setText('0')
                else:
                    if(r[0]=='0'):
                        if(str(r[1])=='.'):
                            for i in list(range (2,len(r),1)):
                                if(r[i].isdigit()):
                                    print ('')
                                else:
                                    self.ui.twPolicies_DemandAllocation.item(row,3).setText('0')

                    elif(r[0]=='.'):
                        for i in list(range (1,len(r),1)):
                            if(r[i].isdigit()):
                                print ('')
                            else:
                                self.ui.twPolicies_DemandAllocation.item(row,3).setText('0')
                    else:
                        self.ui.twPolicies_DemandAllocation.item(row,3).setText('0')

                #self.ui.twPolicies_DemandAllocation.setCurrentCell(row+1,3)
            elif(col==2):
                getvalue=str(item.text())
                if(getvalue.isdigit()):
                    for i in list(range(0,self.__noOfClasses,1)):
                        var1=str(self.ui.twPolicies_DemandAllocation.item(i,2).text())
                        if(var1):
                            sum=sum+int(var1)
                    finalsum=str(sum)
                    self.ui.twPolicies_DemandAllocation.item(self.__noOfClasses,2).setText(finalsum)
                else:
                    print ("Only Posotive Integer Allowed")
                    self.ui.twPolicies_DemandAllocation.item(row,2).setText('')
                    for i in list(range(0,self.__noOfClasses,1)):
                        var1=str(self.ui.twPolicies_DemandAllocation.item(i,2).text())
                        if(var1):
                            sum=sum+int(var1)
                    finalsum=str(sum)
                    self.ui.twPolicies_DemandAllocation.item(self.__noOfClasses,2).setText(finalsum)
                #self.ui.twPolicies_DemandAllocation.setCurrentCell(row+1,2)
            self.ui.twPolicies_DemandAllocation.setCurrentCell(row+1,col)
#####
    def getFormulaFrom(self,row):
        noOfDrivers=self.ui.twSelectDrivers_DriverSelectionT0.rowCount()
        formula=self.ui.twViewModelCoefficint_ModelAnalysis.item(row,0).text()
        formula="T1."+formula+"~"
        for j in list(range(0,noOfDrivers,1)):
            isIncludeDriver=self.ui.twViewModelCoefficint_ModelAnalysis.item(row,j+3).checkState()
            if(isIncludeDriver == QtCore.Qt.Checked):
                if(formula.endswith("~")):
                    formula=formula+ "TD1."+ self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(j,0).text()
                else:
                    formula=formula+ "+" + "TD1." + self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(j,0).text()
        return(formula)

    def prepareFormula(self):
        noOfClass=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount()
        #col=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.columnCount()
        formulaList=[]
        for i in list(range(0,noOfClass,1)):
            formula=self.getFormulaFrom(i)
            formulaList.append(str(formula))
        return(formulaList)

    def prepareDriversT2(self):
        noOfDrivers=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.columnCount()-3
        driverDictionary=OrderedDict()
        for j in list(range(0,noOfDrivers,1)):
            driverDictionary[str(self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(j,0).text())]=str(self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(0,j+3).text())
        return(driverDictionary)

    def prepareDriversT1(self):
        noOfDrivers=self.ui.twSelectDrivers_DriverSelectionT0.rowCount()
        driverDictionary=OrderedDict()
        for j in list(range(0,noOfDrivers,1)):
            driverDictionary[str(self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(j,0).text())]=str(self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(j,1).text())
        return(driverDictionary)

    def getModelFrom(self,row):
        currentItem=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.cellWidget(row,2)#
        if(isinstance(currentItem,QLabel)):
            currentIndexItem=currentItem.text()
        else:
            currentIndexItem=currentItem.currentText()
        if(currentIndexItem == "Logistic Regression"):
            return('logistic')
        if(currentIndexItem == "Linear Regression"):
            return('regression')
        if(currentIndexItem == "Neural Regression"):
            return('nnet')
        if(currentIndexItem == "Random Forest"):
            return('randomForest')        
        return("Wrong")

    def preparemodelType(self):
        noOfClass=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount()
        modelType=[]
        #if(self.ui.rbLogisiticRegression_DriverSelectionT1SelectModelTypeAndDrivers.isChecked()):
        #    for i in list(range(0,noOfClass,1)):
        #        modelType.append('logistic')
        #else:
        #    for i in list(range(0,noOfClass,1)):
        #        formulanew=self.getModelFrom(i)
        #        modelType.append(str(formulanew))
        for i in list(range(0,noOfClass,1)):
            formulanew=self.getModelFrom(i)
            modelType.append(str(formulanew))        
        return(modelType)

    def prepareModelDetail(self):
        print('prepareModelDetail')

        self.__modelformula=self.prepareFormula()
        #self.__modelformula=R.StrVector(self.__modelformula)

        self.__DriverDictionaryT1=self.prepareDriversT1()
        #self.__DriverDictionaryT1=R.ListVector(self.__DriverDictionaryT1)

        self.__DriverDictionaryT2=self.prepareDriversT2()
        #self.__DriverDictionaryT2=R.ListVector(self.__DriverDictionaryT2)

        self.__modeltype=self.preparemodelType()
        #self.__modeltype=R.StrVector(self.__modeltype)
    
    @pyqtSlot(bool)
    def on_cbEnable_DemandAllocationSpatialContext_clicked(self,state):
        print('cbEnable_DemandAllocationSpatialContext')
        self.ui.cbStepOutputRequired_DemandAllocationSpatilaContext.setEnabled(state)
        self.ui.cbWindowSize_DemandAllocationSpatialContext.setEnabled(state)
        self.ui.cbStepOutputRequired_DemandAllocationSpatilaContext.setEnabled(state)
        self.ui.cbInSteps_DemandAllocationSpatialContext.setEnabled(state)
        self.ui.lbWindowSize.setEnabled(state)
        self.ui.lbInSteps.setEnabled(state)
        self.ui.lbStepOutputRequired.setEnabled(state)


    def prepareSpatialData(self):
        self.__neughbourl=[];
        if(self.ui.cbEnable_DemandAllocationSpatialContext.checkState()):
            self.__neughbourl.append(int(str(self.ui.cbWindowSize_DemandAllocationSpatialContext.currentText())))
            self.__neughbourl.append(int(str(self.ui.cbInSteps_DemandAllocationSpatialContext.currentText())))
            if(str(self.ui.cbStepOutputRequired_DemandAllocationSpatilaContext.currentText())=="Yes"):
                self.__neughbourl.append(1)
            else:
                self.__neughbourl.append(0)
            self.__neughbourl=R.IntVector(self.__neughbourl)
        else:
            self.__neughbourl=R.NA_Logical

    def prepareClassName(self):
        self.__className=[];
        #noOfClass=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount()
        for j in list(range(0,self.__noOfClasses,1)):
            self.__className.append(str(self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(j,0).text()))
        self.__className=R.StrVector(self.__className)

    def prepareDemand(self):
        self.__demand=[]
        if(self.ui.cbUserDefinedDemand_DemandAllocation.isChecked()):
            #noOfClass=self.ui.twPolicies_DemandAllocation.rowCount()
            for j in list(range(0,self.__noOfClasses,1)):
                if(str(self.ui.twPolicies_DemandAllocation.item(j,2).text())):
                    self.__demand.append(int(str(self.ui.twPolicies_DemandAllocation.item(j,2).text())))
                else:
                    self.__demand.append(0)
            self.__demand=R.IntVector(self.__demand)

        else:
            self.__demand=R.NA_Logical

    def prepareInertia(self):
        self.__restricSpatial=[]
        if(self.ui.cbUserDefinedClassInertia_DemandAllocation.isChecked()):
            #noOfClass=self.ui.twPolicies_DemandAllocation.rowCount()
            for j in list(range(0,self.__noOfClasses,1)):
                if(str(self.ui.twPolicies_DemandAllocation.item(j,3).text())=='0'or str(self.ui.twPolicies_DemandAllocation.item(j,3).text())=='1'):
                    self.__restricSpatial.append(int(str(self.ui.twPolicies_DemandAllocation.item(j,3).text())))
                else:
                    self.__restricSpatial.append(float(self.ui.twPolicies_DemandAllocation.item(j,3).text()))
            self.__restricSpatial=R.FloatVector(self.__restricSpatial)
        else:
            self.__restricSpatial=R.NA_Logical

    def prepareConversionOrder(self):
        if(self.ui.rbUserDefined_ModelAnalysisMigrationOrder.isChecked()):
            self.__conversionOrder=[]
            noOfClass=self.ui.twMigrationOrder_ModelAnalysis.rowCount()
            for col in list(range(0,self.__noOfClasses,1)):
                for row in list(range(0,self.__noOfClasses,1)):
                    self.__conversionOrder.append(int(str(self.ui.twMigrationOrder_ModelAnalysis.item(row,col+1).text())))
            self.__conversionOrder=R.r.matrix(R.IntVector(self.__conversionOrder), nrow=noOfClass)
        else:
            self.__conversionOrder='TP'


    def prepareDataForExecution(self):
        self.prepareSpatialData()
        self.prepareClassName()
        self.prepareDemand()
        self.prepareInertia()
        self.prepareConversionOrder()
        self.prepareModelDetail()
        self.prepareClassAllocationOrder()
        self.prepareSuitabilityFileDirectory()
        self.prepareReferenceFile()

        print("model.type-[[ "+str(self.__modeltype)+ " ]] ")
        print("T0File-[[ "+self.__T0File+ " ]] ")
        print("T1File-[[ "+self.__T1File+ " ]] ")
        print("with.class.name-[[ "+str(self.__className)+ " ]] ")
        print("T1drivers-[[ "+str(self.__DriverDictionaryT1)+ " ]] ")
        print("T2drivers-[[ "+str(self.__DriverDictionaryT2)+ " ]] ")
        print("withNAvalue-[[ "+str(self.__modelNAValue)+ " ]] ")
        print("demand-[[ "+str(self.__demand)+ " ]] ")
        print("restrictSpatialMigration-[[ "+str(self.__restricSpatial) + " ]] ")
        print("AllowedClassMigration-[[ "+"TODO" + " ]] ")
        print("conversionOrder-[[ "+str(self.__conversionOrder)+ " ]] ")
        print("classAllocationOrder - [["+str(self.__classAllocationOrder)+ " ]] ")
        print("neighbour-[[ "+str(self.__neughbourl)+ " ]] ")
        print("modelformula-[[ "+str(self.__modelformula)+ " ]] ")
        print("outputfile-[[ "+self.__OutputFile+ " ]] ")
        print("suitabilityFileDirectory-[[ "+str(self.__suitabilityFileDirectory)+ " ]] ")

    def prepareClassAllocationOrder(self):
        #Prepare from migration order list
        #For the time being
        self.__classAllocationOrder=[]
        if(self.ui.cbclassallocation_DemandAllocation.isChecked()):
            #noOfClass=self.ui.twPolicies_DemandAllocation.rowCount()
            for j in list(range(0,self.__noOfClasses,1)):
                self.__classAllocationOrder.append(int(str(self.ui.twPolicies_DemandAllocation.item(j,1).text())))
            self.__classAllocationOrder=R.IntVector(self.__classAllocationOrder)

        else:
            self.__classAllocationOrder=R.NA_Logical
            
    @pyqtSlot(bool)
    def on_cbEnable_SuitabilityMapGeneration_clicked(self,state):
        print('cbEnable_SuitabilityMapGeneration')
        self.ui.leSuitablityFile_OutputGenerationSuitabilityMapGeneration.setEnabled(state)
        self.ui.pbSelectFileSuitablityFile_OutputGenerationSuitabilityMapGeneration.setEnabled(state)
        self.ui.lbSuitablityfile.setEnabled(state)

    @pyqtSlot()
    def on_pbSelectFileSuitablityFile_OutputGenerationSuitabilityMapGeneration_clicked(self):
        self.__suitabilityFileDirectory = QFileDialog.getExistingDirectory(self, "Open suitablity Directory",".", QFileDialog.ShowDirsOnly);
        self.ui.leSuitablityFile_OutputGenerationSuitabilityMapGeneration.setText(str(self.__suitabilityFileDirectory))


    def prepareSuitabilityFileDirectory(self):
        name=str(self.ui.leSuitablityFile_OutputGenerationSuitabilityMapGeneration.text())
        if(len(name)!=0):
            self.__suitabilityFileDirectory=name
        else:
            self.__suitabilityFileDirectory=R.NA_Logical

    def prepareReferenceFile(self):
        name=str(self.ui.leActualFile_AccuracyAssesment.text())
        if(len(name)!=0):
            self.__ReferenceFile=name
        else:
            self.__ReferenceFile=R.NA_Logical



    def runRCommand(self):
        #print ("iirssss"+self.__AOIFile)   genratePredictedMap(modelType=model.type,T1File,T2File,withClassName=clsName,T1drivers,T2drivers,na.value, demand=mydemand,restrictSpatialMigration=restrictSpatial,neighbour=neighbourl,outputfile=PredictedFile, conversionOrder=myconversion,classAllocationOrder=myallocationorder,maskFile=NA,aoiFile=NA,modelformula=model.formula,suitabilityFileDirectory=suitabilityDirectory)
        path=os.path.join(str(self.__projectDirectory),self.currentLogTime+'-execute.log')
        sink=R.r['sink']
        sink(path)
        genMap = R.r['genratePredictedMap']
        genMap(modelType=R.StrVector(self.__modeltype),
                   T1File=self.__T0File,
                   T2File=self.__T1File,
                   withClassName=self.__className,
                   T1drivers=R.ListVector(self.__DriverDictionaryT1),
                   T2drivers=R.ListVector(self.__DriverDictionaryT2),
                   withNAvalue=self.__modelNAValue,
                   demand=self.__demand,
                   restrictSpatialMigration=self.__restricSpatial,
                   AllowedClassMigration=R.NA_Logical,
                   conversionOrder=self.__conversionOrder,
                   classAllocationOrder=R.NA_Logical,
                   neighbour=self.__neughbourl,
                   method="NotIncludeCurrentClass",
                   modelformula=R.StrVector(self.__modelformula),
                   outputfile=str(self.__OutputFile),
                   maskFile=self.__MASKFile,
                   aoiFile=self.__AOIFile,
                   suitabilityFileDirectory=self.__suitabilityFileDirectory
                   )
        sink=R.r['sink']
        sink()
        #self.ui.teSummary.setPlainText(str(self.modelSummary))
    ##############################################################R interface########################################################################################################
    #       model.type='logistic',T1File,T2File,with.class.name=NA,T1drivers,T2drivers,withNAvalue,demand=NA,restrictSpatialMigration=NA,AllowedClassMigration=NA,
            #        conversionOrder='TP',classAllocationOrder=NA,neighbour=NA,method='NotIncludeCurrentClass',modelformula=NA,outputfile,maskFile=NA,aoiFile=NA
    #
    #################################################################################################################################################################################
    @pyqtSlot()
    def on_pbExecute_DemandAllocation_clicked(self):
        
        print('pbExecute_DemandAllocation_clicked')
        self.prepareR()
        self.ui.pbFindAccuracy_AccuracyAssesment.setEnabled(False)
        self.prepareDataForExecution()
        self.runRCommand()
        self.ui.tabWidget.setCurrentIndex(5)
        self.ui.progressBar.setProperty("value",60)
        self.ui.gbSummary_AccuracyAssesment.setEnabled(True)
        self.ui.pbFindAccuracy_AccuracyAssesment.setEnabled(True)
        #self.releaseR()
        
    @pyqtSlot()
    def on_actionSave_File_triggered(self):
        print ("save file clicked")
        self.prepareDataForExecution()
        fileLocation = QFileDialog.getSaveFileName(self, "Save File",
                                                        self.__currentDirectory,"Raster (*.R )")[0];
        (dirName, fileName) = os.path.split(str(fileLocation))
        fileType = splitext(fileName)[1]
        if(fileType == ".R"):
            mysavefile=str(fileLocation)
        else:
            mysavefile= str(fileLocation)+".R"
        pk=open(mysavefile,'w')
        '"+self.__installedDir+"/Rasterise_dev_68akj.r'
        pk.write("rm(list=ls())\nlibrary(raster)\n")
        pk.write("source('"+self.__installedDir+"/Rasterise_dev_68akj.r')\n")
        pk.write("pkglist=c('data.table','raster','maptools','parallel','doParallel','nnet','maptools','rgeos','rgdal','randomForest','expm','Matrix')\nCheckInstallPackage(pkglist)\n")
        pk.write("ProjectDirectory=\"%s\"\n" %(str(self.__projectDirectory)))
        if (self.__AOIFile!=R.NA_Logical):
            pk.write("AOIFile=\"%s\"\n"%(str(self.__AOIFile)))
        else:
            pk.write("AOIFile=NA\n")
        if (self.__MASKFile!=R.NA_Logical):
            pk.write("mymaskFile=\"%s\"\n"%(str(self.__MASKFile)))
        else:
            pk.write("mymaskFile=NA\n")
        pk.write("T1File=\"%s\"\n"%(str(self.__T0File)))
        pk.write("T2File=\"%s\"\n"%(str(self.__T1File)))
        if (self.__ReferenceFile!=R.NA_Logical):
            pk.write("T3File=\"%s\"\n"%(str(self.__ReferenceFile)))
        else:
            pk.write("T3File=NA\n")
        pk.write("PredictedFile=\"%s\"\n"%(str(self.__OutputFile)))
        pk.write('drvs85<-c(\n')
        for i, ( key, value) in enumerate(self.__DriverDictionaryT1.items()):
            if(i==len(self.__DriverDictionaryT1)-1):
                pk.write("%s=\"%s\"\n"%(key,value))
            else:
                pk.write("%s=\"%s\",\n"%(key,value))
        pk.write(')\n')
        pk.write('drvs95<-c(\n')
        for i, ( key, value) in enumerate(self.__DriverDictionaryT2.items()):
            if(i==len(self.__DriverDictionaryT2)-1):
                pk.write("%s=\"%s\"\n"%(key,value))
            else:
                pk.write("%s=\"%s\",\n"%(key,value))
        pk.write(')\n')
        pk.write("T1drivers=drvs85\nT2drivers=drvs95\n")

        if (self.__restricSpatial!=R.NA_Logical):
            pk.write("restrictSpatial=c(")
            for i in list(range(len(self.__restricSpatial))):
                if(i!=len(self.__restricSpatial)-1):
                    pk.write("%s,"%(self.__restricSpatial[i]))
                else:
                    pk.write("%s"%(self.__restricSpatial[i]))
            pk.write(")\n")
        # pk.write("restrictSpatial=c(%s)\n"%(R.r['paste'](self.__restricSpatial,sep=",")))
        else:
            pk.write("restrictSpatial=NA\n")
        pk.write("clsName<-c(")
        for i in list(range(len(self.__className))):
            if(i!=len(self.__className)-1):
                pk.write("\"%s\","%(self.__className[i]))
            else:
                pk.write("\"%s\""%(self.__className[i]))
        pk.write(")\n")
        if (self.__demand!=R.NA_Logical):
            pk.write("mydemand=as.numeric(c(")
            for i in list(range(len(self.__demand))):
                if(i!=len(self.__demand)-1):
                    pk.write("%s,"%(self.__demand[i]))
                else:
                    pk.write("%s"%(self.__demand[i]))
                    pk.write("))\n")

        else:
            pk.write("mydemand=NA\n")

        if (self.__modelNAValue!=R.NA_Logical):
            pk.write("na.value=%s\n"%(str(self.__modelNAValue)))
        else:
            pk.write("na.value=NA\n")

        if (self.__neughbourl!=R.NA_Logical):
            pk.write('neighbourl<-list()\n')
            for i in list(range(len(self.__neughbourl))):
                pk.write("neighbourl[[%d]]<-%s\n"%(i+1,self.__neughbourl[i]))
        else:
            pk.write("neighbourl=NA\n")

        if (self.__classAllocationOrder!=R.NA_Logical):
            pk.write("myallocationorder<-c(")
            for i in list(range(len(self.__classAllocationOrder))):
                if(i!=len(self.__classAllocationOrder)-1):
                    pk.write("%s,"%(self.__classAllocationOrder[i]))
                else:
                    pk.write("%s"%(self.__classAllocationOrder[i]))
                    pk.write(")\n")
        else:
            pk.write("myallocationorder=NA\n")

        if (len(self.__modeltype)!=0):
            pk.write("model.type=c(")
            for i in list(range(len(self.__modeltype))):
                if(i!=len(self.__modeltype)-1):
                    pk.write("\'%s\',"%(self.__modeltype[i]))
                else:
                    pk.write("\'%s\'"%(self.__modeltype[i]))
            pk.write(")\n")
        else:
            pk.write("model.type=logistic\n")

        if (len(self.__modelformula)!=0):
            pk.write("model.formula=c(\n")
            for i in list(range(len(self.__modelformula))):
                if(i!=len(self.__modelformula)-1):
                    pk.write("%s,\n"%(self.__modelformula[i]))
                else:
                    pk.write("%s"%(self.__modelformula[i]))
                    pk.write("\n)\n")
        else:
            pk.write("model.formula=NA\n")
        

        if (self.__suitabilityFileDirectory!=R.NA_Logical):
            path=os.path.join(str(self.__suitabilityFileDirectory),"")
            pk.write("suitabilityDirectory=\"%s\"\n"%(str(path)))
        else:
            pk.write("suitabilityDirectory=NA\n")

        if (self.__conversionOrder!='TP'):
            self.__conversionOrder=self.__conversionOrder.transpose()
            pk.write("myconversion<-matrix(\nc(")
            for i in list(range(0,len(self.__conversionOrder),1)):
                if((i+1)%self.__noOfClasses==0):
                    if((i+1)==len(self.__conversionOrder)):
                        pk.write("%s\n"%(self.__conversionOrder[i]))
                    else:
                        pk.write("%s,\n  "%(self.__conversionOrder[i]))
                else:
                    pk.write("%s,"%(self.__conversionOrder[i]))
            pk.write("),nrow=length(myallocationorder),byrow=TRUE)")
        else:
            pk.write("myconversion='TP'\n")


        pk.write("\ngetModelFitSummary(T1File,T2File,T1drivers,modelType=model.type,withNAvalue=na.value,method=\"NotIncludeCurrentClass\")")
        #pk.write("\nnw<-ComputeNearByWeight(T2File,withNA=na.value)")
        #pk.write("\ncreateNeighbourMap(nw,T2File,suitabilityDirectory,clsName)")
        pk.write("\nprint(format(Sys.time(), \" %a %b %d %X %Y %Z\"))")
        pk.write("\nresult<-isDataSetCorrect(T1drivers,T2drivers,T1File,T2File,T3File)")
        pk.write("\nprint(format(Sys.time(), \"%a %b %d %X %Y %Z\"))")
        pk.write("\nif(result){")
        pk.write("\n\t genratePredictedMap(modelType=model.type,T1File,T2File,withClassName=clsName,T1drivers,T2drivers,na.value,")
        pk.write("\n\t\t demand=mydemand,restrictSpatialMigration=restrictSpatial,neighbour=neighbourl,outputfile=PredictedFile,")
        pk.write("\n\t\t conversionOrder=myconversion,classAllocationOrder=myallocationorder,maskFile=mymaskFile,aoiFile=AOIFile,")
        pk.write("\n\t\t modelformula=model.formula,suitabilityFileDirectory=suitabilityDirectory)")
        pk.write("\n}")
        pk.write("\nprint(format(Sys.time(), \"%a %b %d %X %Y %Z\"))\n")

        if(len(str(self.__ReferenceFile))!=0):
            pk.write("\ngetKappaSummary(T3File,PredictedFile,na.value,clsName)\n")
        else:
            pk.write("#getKappaSummary(T3File,PredictedFile,na.value,clsName)")

        pk.close()

    @pyqtSlot()
    def on_actionExit_triggered(self):
        quit_msg = "Are you sure you want to exit the program?"
        reply = QtWidgets.QMessageBox.question(self, 'OpenLDM Message',
                                                   quit_msg, QtWidgets.QMessageBox.Yes, QtWidgets.QMessageBox.No)
        if reply == QtWidgets.QMessageBox.Yes:
            self.statusMessage.exit(0)
            self.releaseR()
            event.accept()
        else:
            pass

    @pyqtSlot()
    def on_actionOpen_File_triggered(self):
        print ("open file clicked")
        pk=open(mysavefile,'r')

    def calculateStatistics(self):
        print('kappa')
        runRFun = R.r['createTM']
        actualFile=str(self.ui.leActualFile_AccuracyAssesment.text().strip())
        predictedFile=str(self.ui.lePredictedFile_AccuracyAssesment.text().strip())
        #tm=runRFun('/home/akjha/GUITEST/cvcvxv.tif','/home/akjha/GUITEST/cvcvxv.tif',R.NA_Logical)#Run R coammnd
        tm=runRFun(actualFile,predictedFile,R.NA_Logical)
        runRFun = R.r['kappa']
        kappa=runRFun(tm)
        runRFun=R.r['PyKappasummary']
        kappasummary=runRFun(kappa)
        print(kappasummary)
        path=os.path.join(str(self.__projectDirectory),self.currentLogTime+'-accuracy.log')        
        sink=R.r['sink']
        sink(path) 
        R.r['print'](tm)
        R.r['print'](kappasummary)
        sink=R.r['sink']
        sink()            

        summarylist = re.split('~~',str(kappasummary))
        self.__noOfObservation=re.split(":",summarylist[0])[1]
        self.__overallOfAccuracy=re.split(":",summarylist[1])[1]
        self.__overallOfAccuracyCI95=re.split(":",summarylist[2])[1]
        self.__overallOfAccuracyCI99=re.split(":",summarylist[3])[1]
        self.__userAccuracy=re.split(",",re.split(":",summarylist[4])[1])
        self.__producerReliability=re.split(",",re.split(":",summarylist[5])[1])
        self.__overallKappa=re.split(":",summarylist[6])[1]
        self.__overallKappaCI95=re.split(":",summarylist[7])[1]
        self.__overallKappaCI99=re.split(":",summarylist[8])[1]
        noOfClasses=int(sqrt(len(tm)))
        self.__transitionMatrix = [[0 for i in list(range(0,noOfClasses+1,1))] for j in range(0,noOfClasses+1,1)]
        k=0;
        for i in list(range(0,noOfClasses,1)):
            for j in list(range(0,noOfClasses,1)):
                self.__transitionMatrix[j][i]=tm[k]#tm[i+j*(noOfClasses)]
                self.__transitionMatrix[noOfClasses][i]+=int(tm[k])
                self.__transitionMatrix[j][noOfClasses]+=int(tm[k])
                self.__transitionMatrix[noOfClasses][noOfClasses]+=int(tm[k])
                k+=1
    @pyqtSlot(bool)
    def on_rbConfusionMatrix_AccuracyAssesmentDetailed_toggled(self,checked):
        if(checked):
            self.buildtwDetailed_AccuracyAssesment(self.ui.twDetailed_AccuracyAssesment,True)
            for i in list(range(0,len(self.__transitionMatrix),1)):
                for j in list(range(0,len(self.__transitionMatrix),1)):
                    self.ui.twDetailed_AccuracyAssesment.item(i,j).setText(str(int(self.__transitionMatrix[i][j])))
    @pyqtSlot(bool)
    def on_rbPreditedMapAccuracy_AccuracyAssesmentDetailed_toggled(self,checked):
        if(checked):
            self.buildtwDetailed_AccuracyAssesment(self.ui.twDetailed_AccuracyAssesment,False)
            for j in list(range(0,len(self.__userAccuracy),1)):
                self.ui.twDetailed_AccuracyAssesment.item(j,0).setText(str(self.__userAccuracy[j]))
    @pyqtSlot(bool)
    def on_rbReferenceMapReliablity_AccuracyAssesmentDetailed_toggled(self,checked):
        if(checked):
            self.buildtwDetailed_AccuracyAssesment(self.ui.twDetailed_AccuracyAssesment,False)
            for j in list(range(0,len(self.__producerReliability),1)):
                self.ui.twDetailed_AccuracyAssesment.item(j,0).setText(str(self.__producerReliability[j]))

    def populateDataIntoTableDetailed_AccuracyAssesment(self):
        self.calculateStatistics()
        self.ui.leOverallKappa_AccuracyAssesment.setText(self.__overallKappa)
        self.ui.leOverallKappaCI_AccuracyAssesment95.setText(self.__overallKappaCI95)
        self.ui.leOverallKappaCI_AccuracyAssesment99.setText(self.__overallKappaCI99)
        self.ui.leOverallAccuracy_AccuracyAssesment.setText(self.__overallOfAccuracy)
        self.ui.leOverallAccuracyCI_AccuracyAssesment95.setText(self.__overallOfAccuracyCI95)
        self.ui.leOverallAccuracyCI_AccuracyAssesment99.setText(self.__overallOfAccuracyCI99)
        self.ui.leNoOfObservation_AccuracyAssesmentSummary.setText(self.__noOfObservation)
        
    @pyqtSlot(int)
    def on_cbStepOutputRequired_DemandAllocationSpatilaContext_currentIndexChanged(self,index):
        print('StepOutputRequired_DemandAllocationSpatialContext'+str(index))

    @pyqtSlot(int)
    def on_cbInSteps_DemandAllocationSpatialContext_currentIndexChanged(self,index):
        print('cbInSteps_DemandAllocationSpatialContext'+str(index))

    @pyqtSlot(int)
    def on_cbWindowSize_DemandAllocationSpatialContext_currentIndexChanged(self,index):
        print('WindowSize_DemandAllocationSpatialContext'+str(index))

    @pyqtSlot()
    def on_pbSelectFileActualFile_AccuracyAssesment_clicked(self):
        print('pbSelectFileActualFile_AccuracyAssesment')
        fileLocation,_ = QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)")
        self.ui.leActualFile_AccuracyAssesment.setText(str(fileLocation).strip())

    @pyqtSlot()
    def on_pbSelectFilePredictedFile_AccuracyAssesment_clicked(self):
        print('pbSelectFilePredictedFile_AccuracyAssesment')
        fileLocation,_ = QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)")
        self.ui.lePredictedFile_AccuracyAssesment.setText(str(fileLocation).strip())

    @pyqtSlot()
    def on_pbFindAccuracy_AccuracyAssesment_clicked(self):
        print('pbFindAccuracy_AccuracyAssesment')
        self.prepareR()
        self.populateDataIntoTableDetailed_AccuracyAssesment()
        self.ui.gbDetailed_AccuracyAssesment.setEnabled(True)
        self.ui.gbSummarStats_AccuracyAssesementSummary.setEnabled(True)
        #self.releaseR()

    @pyqtSlot(bool)
    def on_rbAuto_ModelAnalysisMigrationOrder_toggled(self,checked):
        if(checked):
            self.ui.twMigrationOrder_ModelAnalysis.setEnabled(False)
            self.__conversionOrder='TP'

    @pyqtSlot(bool)
    def on_rbLogisiticRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(False)

    @pyqtSlot(bool)
    def on_rbLinearRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(False)

    @pyqtSlot(bool)
    def on_rbNeuralRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(False)

    def on_rbRandomForest_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setEnabled(False)

    def setModelAnalysisModelType(self):
        if(self.__modeltype == "logistic" ):
            self.ui.rbLogisiticRegression_ModelAnalysisViewModelCoeeffcient.setChecked(True)
            
    @pyqtSlot(int,int)
    def on_twViewModelCoefficint_ModelAnalysis_cellClicked(self,row,col):
        item=self.ui.twViewModelCoefficint_ModelAnalysis.item(row,col)
        if(col>2):
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(row,col).setCheckState(item.checkState())


    def makeCombo(self):
        cbModel = QComboBox()
        cbModel.setMaxCount(4)
        cbModel.setObjectName("cbModel"+str(self.ui.twSelectDrivers_DriverSelectionT0.rowCount()-1))
        cbModel.addItem("iLoR")
        cbModel.addItem("iLiR")
        cbModel.addItem("iMLP")
        cbModel.addItem("iMRF")
        cbModel.setItemText(0, "Logistic Regression")
        cbModel.setItemText(1, "Linear Regression")
        cbModel.setItemText(2, "Neural Regression")
        cbModel.setItemText(3, "Random Forest")      
        return(cbModel)

    def populateDataIntoTableModelTypeAndDriversDriverSelectionT1(self):
        #Populate the DriversFile in DriverS electionT1 tab
        for j in list(range(0,self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount(),1)):
            item1 = self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(j,1)
            item1.setText(self.__className[j])
        for j in list(range(0,self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount(),1)):
            for k in list(range(3,self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.columnCount(),1)): #First three colums are reserved for classname,number and modetype
                item1 = self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(j,k)
                item1.setText(self.__driversT1[k-3]) #The Location of Drives File
                toolstr = str(self.__confidenceinterval[j][k-2][1])+" "+str(self.__confidenceinterval[j][k-2][2]) #k-3+1 since Intercept is at k-3
                #item1.setToolTip(QtGui.QApplication.translate("LULCModel", toolstr, None, QtGui.QApplication.UnicodeUTF8))
                item1.setToolTip(toolstr)
            #self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.itemDoubleClicked.connect(self.tableSelectModelTypeAndDriversItemDoubleClicked)

    @pyqtSlot()
    def on_Suitable_clicked(self):
        #self.boxCheck()
        self.ui.tabWidget.setCurrentIndex(3)
        self.ui.progressBar.setProperty("value",40)


    def toolTip(self, final, m, n):
        self.item = self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(m, n)
        self.Coeffcient.append(self.__confidenceinterval[n-2][0])
        toolstr = str(self.__confidenceinterval[n-2][0])+" "+str(self.checkStar(self.__confidenceinterval[n-2]))
        self.item.setToolTip(QtGui.QApplication.translate("LULCModel", toolstr, None, QtGui.QApplication.UnicodeUTF8))
        return

    @pyqtSlot()
    def on_pbNextDemandAlloc_clicked(self):
        self.ui.twDemandAlloc.setRowCount(self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount())
        self.ui.tabWidget.setCurrentIndex(4)
        self.ui.progressBar.setProperty("value",50)


    def fill_table_with_label(this,thistable,labelText):
        i=0
        while i<thistable.rowCount():
            label = QLabel(labelText)
            thistable.setCellWidget(i, 3,label)
            i=i+1

    @pyqtSlot(bool)
    def on_rbLogisticRegression_DriverSelectionT0DoModelFitting_toggled(self,checked):
        if(checked):
            self.__modeltype = "logistic"
            self.ui.pbViewModelStatistics_DriverSelectionT0DoModelFitting.setEnabled(True)
            self.ui.rbLogisiticRegression_ModelAnalysisViewModelCoeeffcient.setChecked(True)
            self.ui.rbLogisiticRegression_DriverSelectionT1SelectModelTypeAndDrivers.setChecked(True)
        #self.fill_table_with_label(self.ui.twSelectDrivers_DriverSelectionT0,"Logistic R")

    @pyqtSlot(bool)
    def on_rbLinearRegression_DriverSelectionT0DoModelFitting_toggled(self,checked):
        if(checked):
            self.__modeltype = "regression"
            self.ui.pbViewModelStatistics_DriverSelectionT0DoModelFitting.setEnabled(True)
            self.ui.rbLinearRegression_ModelAnalysisViewModelCoeeffcient.setChecked(True)
            self.ui.rbLinearRegression_DriverSelectionT1SelectModelTypeAndDrivers.setChecked(True)
        #fill_table_with_label(self.ui.twSelectDrivers_DriverSelectionT0,"Linear R")

    @pyqtSlot(bool)
    def on_rbNeuralregression_DriverSelectionT0DoModelFitting_toggled(self,checked):
        if(checked):
            self.__modeltype = "nnet"
            self.ui.pbViewModelStatistics_DriverSelectionT0DoModelFitting.setEnabled(True)
            self.ui.rbNeuralRegression_ModelAnalysisViewModelCoeeffcient.setChecked(True)
            self.ui.rbNeuralRegression_DriverSelectionT1SelectModelTypeAndDrivers.setChecked(True)
        #fill_table_with_label(self.ui.twSelectDrivers_DriverSelectionT0,"Neural Network")

    @pyqtSlot(bool)
    def on_rbRandomForest_DriverSelectionT0DoModelFitting_toggled(self,checked):
        if(checked):
            self.__modeltype = "randomForest"
            self.ui.pbViewModelStatistics_DriverSelectionT0DoModelFitting.setEnabled(True)
            self.ui.rbRandomForest_ModelAnalysisViewModelCoeeffcient.setChecked(True)
            self.ui.rbRandomForest_DriverSelectionT1SelectModelTypeAndDrivers.setChecked(True)
        #fill_table_with_label(self.ui.twSelectDrivers_DriverSelectionT0,"Random Forest")

    @pyqtSlot(bool)
    def on_rbIndvidualSelection_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            self.__modeltype = "individual"
            i=0
            while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                combo=self.makeCombo()
                self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,combo)
                i=i+1
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot(bool)
    def on_rbLogisiticRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            i=0
            while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                label=QLabel("Logistic Regression")
                self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,label)
                i=i+1
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot(bool)
    def on_rbLinearRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            i=0
            while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                label=QLabel("Linear Regression")
                self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,label)
                i=i+1
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot(bool)
    def on_rbNeuralRegression_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            i=0
            while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                label=QLabel("Neural Regression")
                self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,label)
                i=i+1
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot(bool)
    def on_rbRandomForest_DriverSelectionT1SelectModelTypeAndDrivers_toggled(self,checked):
        if(checked):
            i=0
            while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                label=QLabel("Random Forest")
                self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.setCellWidget(i, 2,label)
                i=i+1
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot(int,int)
    def on_twSelectModelTypeAndDrivers_DriverSelectionT1_cellDoubleClicked(self,row,col):
        if(col>2):
            file,_ = QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)");
            (dirName, fileName) = os.path.split(str(file));
            self.__currentDirectory=dirName
            if((str(file))):
                i=0;
                while i<self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.rowCount():
                    item=self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(i,col)
                    item.setText(str(file))
                    i=i+1;
        self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.resizeColumnsToContents()

    @pyqtSlot()
    def on_leT0File_DataPreparationInputSectionDataInput_editingFinished(self):
        #QtWidgets.QMessageBox.about(self, "My message box","leT0FileOK1");
        if(self.ui.leT0File_DataPreparationInputSectionDataInput.isModified()):
            self.ui.leT0File_DataPreparationInputSectionDataInput.setModified(False)
        if(not len(self.ui.leT0File_DataPreparationInputSectionDataInput.text())) == 0:
            self.__T0File=str(self.ui.leT0File_DataPreparationInputSectionDataInput.text().strip())
            self.enable_NextDataPreaparation()

    @pyqtSlot()
    def on_leT1File_DataPreparationInputSectionDataInput_editingFinished(self):
        #QtWidgets.QMessageBox.about(self, "My message box","leT1FileOK2");
        if(self.ui.leT1File_DataPreparationInputSectionDataInput.isModified()):
            self.ui.leT1File_DataPreparationInputSectionDataInput.setModified(False)
        if(not len(self.ui.leT1File_DataPreparationInputSectionDataInput.text())) == 0:
            self.__T1File=str(self.ui.leT1File_DataPreparationInputSectionDataInput.text().strip())
            self.enable_NextDataPreaparation()

    @pyqtSlot()
    def on_leOutputFile_DataPreparationOutputSection_editingFinished(self):
        #QtWidgets.QMessageBox.about(self, "My message box","leT1FileOK2");
        if(self.ui.leOutputFile_DataPreparationOutputSection.isModified()):
            self.ui.leOutputFile_DataPreparationOutputSection.setModified(False)
        if(not len(self.ui.leOutputFile_DataPreparationOutputSection.text())) == 0:
            self.__OutputFile=self.__currentDirectory+str(self.ui.leOutputFile_DataPreparationOutputSection.text().strip())
            self.ui.lePredictedFile_AccuracyAssesment.setText(self.__OutputFile);
            self.enable_NextDataPreaparation()

    @pyqtSlot()
    def on_leT0Year_DataPreparationInputSectionDataInput_editingFinished(self):
        #QtWidgets.QMessageBox.about(self, "My message box","leT1FileOK2");
        if(self.ui.leT0Year_DataPreparationInputSectionDataInput.isModified()):
            self.ui.leT0Year_DataPreparationInputSectionDataInput.setModified(False)
        if(not len(self.ui.leT0Year_DataPreparationInputSectionDataInput.text())) == 0:
            T0Year=int(str(self.ui.leT0Year_DataPreparationInputSectionDataInput.text().strip()))
            self.__T0Year=T0Year
            self.enable_NextDataPreaparation()

    @pyqtSlot()
    def on_leT1Year_DataPreparationInputSectionDataInput_editingFinished(self):
        #QtWidgets.QMessageBox.about(self, "My message box","leT1FileOK2");
        if(self.ui.leT1Year_DataPreparationInputSectionDataInput.isModified()):
            self.ui.leT1Year_DataPreparationInputSectionDataInput.setModified(False)
        if(not len(self.ui.leT1Year_DataPreparationInputSectionDataInput.text())) == 0:
            T1Year=int(str(self.ui.leT1Year_DataPreparationInputSectionDataInput.text().strip()))
            self.__T1Year=T1Year
            self.enable_NextDataPreaparation()

    def enable_NextDataPreaparation(self):
        if(not len(self.ui.leT0File_DataPreparationInputSectionDataInput.text()) == 0 and
               not len(self.ui.leT1File_DataPreparationInputSectionDataInput.text()) == 0 and
               not len(self.ui.leOutputFile_DataPreparationOutputSection.text()) == 0 and
               not len(self.ui.leT0Year_DataPreparationInputSectionDataInput.text()) == 0 and
               not len(self.ui.leT1Year_DataPreparationInputSectionDataInput.text()) == 0# and self.__T1Year != 0 and self.__T0Year != 0 and self.__T1Year>self.__T0Year
               ):
            self.ui.leT1Year_DataPreparationInputSectionDataInput.setEnabled(False)
            self.ui.leT0Year_DataPreparationInputSectionDataInput.setEnabled(False)
            self.ui.leOutputFile_DataPreparationOutputSection.setEnabled(False)
            self.ui.leT0File_DataPreparationInputSectionDataInput.setEnabled(False)
            self.ui.leT1File_DataPreparationInputSectionDataInput.setEnabled(False)
            self.ui.pbNextDataPreparation.setEnabled(True)
        else:
            self.ui.pbNextDataPreparation.setEnabled(False)

    def preparecbInSteps_DemandAllocationSpatialContext(self):
        diffyear=self.__T1Year-self.__T0Year
        for i in list(range(1,diffyear,1)):
            self.ui.cbInSteps_DemandAllocationSpatialContext.addItem(str(i+1))
            
    def breakCoeffDetails(self, str1):
        layer = []     
        if('---' in str1 and  'Coefficients'in str1):
            coeffsubset=str1[str1.index('Coefficients'):str1.index('---')-1]
        elif('AIC:' in str1 and 'Coefficients' in str1):
            coeffsubset=str1[str1.index('Coefficients'):str1.index('AIC:')-1]
        elif('IncNodePurity' in str1 and '---' in str1):
            coeffsubset=str1[str1.index('IncNodePurity')+1:str1.index('---')-1]
        
        if('(Intercept)' in str1):
            str1=coeffsubset[coeffsubset.index('(Intercept)'):]
        else:
            str1="(Intercept)\tNA\n"+coeffsubset
        str1=str1[:str1.index('\n')]
        str1=str1.replace("< ","<")
        layer.append(str1.split()) #intercepts Details are added
        dirversNameList=list(self.__DriverDictionaryT1.keys())
        for i in range(0,self.noOfDrivers,1):
            #print(self.__DriverDictionaryT1.keys()[i])
            #print(coeffsubset.index(str(self.__DriverDictionaryT1.keys()[i])))
            str1=coeffsubset[coeffsubset.index(str(dirversNameList[i])):]
            if(str1.find('\n')>0):
                str1=str1[:str1.index('\n')]
            #str1.strip()
            #print(str1)
            str1=str1.replace("< ","<")
            layer.append(str1.split())
        return layer

    def checkStar(self, list1):
        if(list1[-1]=="***" or list1[-1]=="**" or list1[-1]=="*",list1[-1]=="."):
            _11s=list1[-1]
        else:
            _11s=""
        return _11s

    def createLULCVsDriverCoefficientMatrix(self):
        #print("In:createLULCVsDriverCoefficientMatrix:Line 1318")
        customlist = re.split('\[\[\d{1,}\]\]',str(self.modelSummary))
        #print(len(customlist))
        self.__confidenceinterval = []
        #print(self.__noOfClasses)
        for i in list(range(1,self.__noOfClasses+1,1)):#List starts at one due to first is being blank
            details = self.breakCoeffDetails(customlist[i])
            #print(details)
            coeffAndIntervalForAllDrivers=[]
            for j in list(range(0,self.noOfDrivers+1,1)):#Including Intercepts there are one extra looping
                coeffAndInterval=[]
                detailsDriver=details[j]
                if(len(detailsDriver)):
                    coeffAndInterval.append(detailsDriver[0]) #Name of Driver at index 0
                    coeffAndInterval.append(detailsDriver[1]) #Coeff of Driver at index 1
                    if(detailsDriver[-1]=="***" or detailsDriver[-1]=="**" or detailsDriver[-1]=="*" or detailsDriver[-1]=="."):#significant code of Driver at index -1 if present otherwise
                        _11s=detailsDriver[-1]
                    else:
                        _11s=" "
                    coeffAndInterval.append(_11s)
                    coeffAndIntervalForAllDrivers.append(coeffAndInterval)
                #print(coeffAndInterval)
            self.__confidenceinterval.append(coeffAndIntervalForAllDrivers)
        #print(self.__confidenceinterval)

    @pyqtSlot(int,int)
    def on_twViewModelCoefficint_ModelAnalysis_cellChanged(self,row,col):
        if(col==0):
            item=self.ui.twViewModelCoefficint_ModelAnalysis.item(row,col)
            item1=self.ui.twMigrationOrder_ModelAnalysis.horizontalHeaderItem(row)
            item2=self.ui.twMigrationOrder_ModelAnalysis.item(row,0)

            if(item1):
                item1.setText(item.text())
            if(item2):
                item2.setText(item.text())
            self.ui.twSelectModelTypeAndDrivers_DriverSelectionT1.item(row,0).setText(item.text())
            self.ui.twPolicies_DemandAllocation.item(row,0).setText(item.text())
            if(self.ui.twColorTable_ViewMaps):
                self.ui.twColorTable_ViewMaps.item(row,1).setText(item.text())
    
    @pyqtSlot(bool)
    def on_twMigrationOrder_ModelAnalysis_toggled(self,checked):
        self.buildtwMigrationOrder_ModelAnalysis(self.ui.twMigrationOrder_ModelAnalysis)
        if(checked):
            self.ui.twMigrationOrder_ModelAnalysis.setEnabled(False)
            
    @pyqtSlot(bool)
    def on_rbUserDefined_ModelAnalysisMigrationOrder_toggled(self,checked):
        self.buildtwMigrationOrder_ModelAnalysis(self.ui.twMigrationOrder_ModelAnalysis)
        if(checked):
            self.__conversionOrder='UD'
            self.ui.twMigrationOrder_ModelAnalysis.setEnabled(True)
            for i in list(range(0,self.__noOfClasses,1)):
                for j in list(range(0,self.__noOfClasses,1)):
                    num=str(j+1)
                self.ui.twMigrationOrder_ModelAnalysis.item(i,j+1).setText(num)

    @pyqtSlot(QTableWidgetItem)
    def on_twMigrationOrder_ModelAnalysis_itemChanged(self,item):
        #if(self.ui.twMigrationOrder_ModelAnalysis.isSelected(item)):
        if(item.isSelected()):
            row=self.ui.twMigrationOrder_ModelAnalysis.currentRow()
            col=self.ui.twMigrationOrder_ModelAnalysis.currentColumn()
            if(col>=1):#First Column is Class Names
                strvalue=str(item.text())
                if(strvalue.isdigit()):
                    newvalue=int(strvalue)
                    for j in list(range(0,self.__noOfClasses,1)):
                        flag=1
                        for k in list(range(0,self.__noOfClasses,1)):
                            checkitem=int(str(self.ui.twMigrationOrder_ModelAnalysis.item(row,k+1).text()))
                            if((j+1)==checkitem):
                                flag=0
                                break
                        if(flag!=0):
                            oldvalue=str(j+1)
                            break

                    if(0<newvalue<=int(str(self.__noOfClasses))):
                        for i in list(range(0,self.__noOfClasses,1)):
                            if((i+1)!=col):
                                checkitem=int(str(self.ui.twMigrationOrder_ModelAnalysis.item(row,i+1).text()))
                                if(checkitem==newvalue):
                                    newcol=i+1
                                    self.ui.twMigrationOrder_ModelAnalysis.item(row,newcol).setText(oldvalue)
                    else:
                        print ("please enter value within range:1-",self.__noOfClasses)
                        self.ui.twMigrationOrder_ModelAnalysis.item(row,col).setText(oldvalue)
                else:
                    print ("Wrong Input")
                    item.setText('0')



######
    def buildtwMigrationOrder_ModelAnalysis(self,twMigrationOrder):
        twMigrationOrder.setRowCount(self.__noOfClasses)
        twMigrationOrder.setColumnCount(self.__noOfClasses+1)
        #Setting Individual tabelitems
        row=twMigrationOrder.rowCount()
        col=twMigrationOrder.columnCount()
        for i in list(range(0, row, 1)):
            for j in list((range(0, col, 1))):
                if(twMigrationOrder.item(i,j)!=0): #0 is return if item is not set
                    item = QTableWidgetItem()
                    item.setText(str(j))
                    if(col==1):
                        item.flags()^QtCore.Qt.ItemIsEditable
                    twMigrationOrder.setItem(i, j, item)
                
        #Setting Table Header
        stringlist2 = list()#QtCore.QStringList()
        stringlist2.append("Class")
        for i in list(range(0,self.ui.twViewModelCoefficint_ModelAnalysis.rowCount(),1)):
            classname=self.ui.twViewModelCoefficint_ModelAnalysis.item(i,0).text()
            stringlist2.append(classname)
            twMigrationOrder.item(i,0).setText(classname)
            item = self.ui.twMigrationOrder_ModelAnalysis.item(i,0)
            item.setFlags(QtCore.Qt.ItemIsSelectable|QtCore.Qt.ItemIsEnabled)
            #item.setFlags(item.flags()^QtCore.Qt.ItemIsEnabled)

        twMigrationOrder.setHorizontalHeaderLabels(stringlist2)

    def buildtwPolicies_DemandAllocation(self,twPolicies):
        #twPolicies.setRowCount(self.__noOfClasses)
        twPolicies.setRowCount((self.__noOfClasses)+1)
        twPolicies.setColumnCount(4)
        #Setting Individual tabelitems
        row=twPolicies.rowCount()
        col=twPolicies.columnCount()
        for i in list(range(0, row, 1)):
            for j in list(range(0, col, 1)):
                item = QTableWidgetItem()
                item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                twPolicies.setItem(i, j, item)
        #Setting Table Header
        stringlist2 = list()#QtCore.QStringList()
        stringlist2.append('Class')
        stringlist2.append('Allocation')
        stringlist2.append('Demand')
        stringlist2.append('Class Inertia')
        twPolicies.setHorizontalHeaderLabels(stringlist2)
        for i in list(range(0,self.ui.twViewModelCoefficint_ModelAnalysis.rowCount(),1)):
            classname=self.ui.twViewModelCoefficint_ModelAnalysis.item(i,0).text()
            twPolicies.item(i,0).setText(classname)
        #label="Total"
        #twPolicies.item(row-1,0).setText(label)
        self.ui.twPolicies_DemandAllocation.item(row-1,0).setText('Total')

    def buildtwDetailed_AccuracyAssesment(self,twClasswiseAccuracy,forConfusionmatrix):
        if(forConfusionmatrix):
            twClasswiseAccuracy.setColumnCount(len(self.__transitionMatrix))
            twClasswiseAccuracy.setRowCount(len(self.__transitionMatrix))
        else:
            twClasswiseAccuracy.setColumnCount(1)
            twClasswiseAccuracy.setRowCount(len(self.__transitionMatrix)-1)
          
        #Setting Individual tabelitems
        row=twClasswiseAccuracy.rowCount()
        col=twClasswiseAccuracy.columnCount()
        for i in list(range(0, row, 1)):
            for j in list(range(0, col, 1)):
                item = QTableWidgetItem()
                #if(col==1):
                item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                twClasswiseAccuracy.setItem(i, j, item)
        #Setting Table Header
        stringlistv = []#QtCore.QStringList()
        for i in list(range(0,row-1,1)):
            if(self.ui.twViewModelCoefficint_ModelAnalysis.item(i,0)):
                classname=self.ui.twViewModelCoefficint_ModelAnalysis.item(i,0).text()
            else:
                classname="class"+str(i+1)
            stringlistv.append(classname)
        if(forConfusionmatrix):
            stringlistv.append("Total")
            stringlisth=stringlistv
        else:
            stringlisth=["Accuracy"]
            #if(self.ui.twViewModelCoefficint_ModelAnalysis.item(row,0)):
            #    self.ui.twViewModelCoefficint_ModelAnalysis.removeRow(row)
        twClasswiseAccuracy.setHorizontalHeaderLabels(stringlisth)   
        twClasswiseAccuracy.setVerticalHeaderLabels(stringlistv)

    @pyqtSlot()
    def on_pbShow_ViewMaps_clicked(self):
        self.prepareR()
        fileName=self.prepareDataForPlot()
        file=QFile(fileName);
        image = QtGui.QImage(fileName)
        self.ui.lbCanvas_ViewMaps.setPixmap(QtGui.QPixmap.fromImage(image))
        #self.releaseR()

    @pyqtSlot()
    def on_pbExport_ViewMaps_clicked(self):
        fileLocation = QFileDialog.getSaveFileName(self, "Save Output File", self.__currentDirectory,"Image (*.png )")[0];
        #print(fileLocation)
        (dirName, fileName) = os.path.split(str(fileLocation))
        fileType = splitext(fileName)[1]
        if(fileType == ".png"):
            newfileLocation=str(fileLocation)
        else:
            newfileLocation=str(fileLocation)+".png"
        print(newfileLocation)
        self.plot="A4"
        self.prepareR()
        fileName=self.prepareDataForPlot()
        file=QFile(fileName)
        file.copy(str(newfileLocation))
        #self.releaseR()

    def buildtwColorTable_ViewMaps(self,twColorTable_ViewMaps):
        twColorTable_ViewMaps.setRowCount(self.__noOfClasses)
        twColorTable_ViewMaps.setColumnCount(3)
        row=twColorTable_ViewMaps.rowCount()
        col=twColorTable_ViewMaps.columnCount()
        m=int(255/(row-1))
        for i in list(range(0, row)):
            for j in list(range(0, col)):
                item = QTableWidgetItem()
                if j == 0:
                    item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                    item.setText(str(self.__className[i]))
                elif j== 1 :
                    item.setText('Class-'+str(i+1))
                else:
                    item.setFlags(item.flags()^QtCore.Qt.ItemIsEditable)
                    item.setBackground(QBrush(QColor(255-m*i,255-m*i,255-m*i)))
                    self.SelectColour_pushed();
                twColorTable_ViewMaps.setItem(i, j, item)

        stringlist1 = []#QtCore.QStringList()
        stringlist1.append('DN Value')
        stringlist1.append('Class Name')
        stringlist1.append('Colour')
        twColorTable_ViewMaps.setHorizontalHeaderLabels(stringlist1)
        twColorTable_ViewMaps.resizeColumnsToContents()

    def on_twColorTable_ViewMaps_cellDoubleClicked(self,row,col):
        if(col==2):
            color = QColorDialog.getColor()#(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)");
            item=self.ui.twColorTable_ViewMaps.item(row,col)
            item.setBackground(QBrush(color))
        self.ui.twColorTable_ViewMaps.resizeColumnsToContents()

    def SelectColour_pushed(self):
        #col = QtGui.QColor(0, 0, 0)
        self.btn = QPushButton('Dialog', self)
        #self.btn.move(20, 20)
        self.btn.clicked.connect(self.showDialog)

    def getFileToPlot(self):
        ClassifiedRasterFile=str(self.ui.cbFile_ViewMaps.currentText())
        if(ClassifiedRasterFile=="" or not QFileInfo(ClassifiedRasterFile).isReadable()):
            reply=QMessageBox.question(self, "File doesn't Exist?","Do you want to add a file?",QMessageBox.Yes | QMessageBox.No, QMessageBox.No);
            if(reply==QMessageBox.Yes):
                ClassifiedRasterFile,_ = QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.tif *.img)")
                lineEdit=QLineEdit()
                lineEdit.setText(ClassifiedRasterFile)
                self.ui.cbFile_ViewMaps.setLineEdit(lineEdit)
                self.ui.cbFile_ViewMaps.addItem(ClassifiedRasterFile)
                #self.ui.cbFile_ViewMaps.setCurrentIndex(1)#(self.ui.cbFile_ViewMaps.maxCount()-1)
                ClassifiedRasterFile = str(ClassifiedRasterFile)
            else:
                ClassifiedRasterFile = ""
        return ClassifiedRasterFile

    def prepareDataForPlot(self):
        pF=self.getFileToPlot()
        if(not pF):
            return;

        pT=str(self.ui.leTitle_ViewMaps.text())
        if(not pT):
            pT="Predicted Map"

        pL=str(self.ui.leLegendHeading_ViewMaps.text())
        if(not pL):
            pL="LULC Class"

        cNo=self.getLegendClassValue()

        tableRowCount=self.ui.twColorTable_ViewMaps.rowCount()
        classCount=len(cNo)
        if(classCount !=0 and tableRowCount!=classCount):
            self.buildtwColorTable_ViewMaps(self.ui.twColorTable_ViewMaps)
        cN=self.getLegendClassName()
        cC=self.getColorPallet()

        self.plot = "onscreen"
        wTP=self.plot
        canvusFile=self.prepareTempPng(pF,pT,pL,cNo,cN,cC,wTP)
        return(canvusFile);

    def getLegendClassName(self):
        names=[]
        row=self.ui.twColorTable_ViewMaps.rowCount()
        for i in list(range(0, row)):
            item=self.ui.twColorTable_ViewMaps.item(i,1)
            classLabel=item.text()
            names.append(str(classLabel))
        return(R.StrVector(names))

    def getLegendClassValue(self):
       # if (self.__noOfClasses==0):
        pF=str(self.ui.cbFile_ViewMaps.currentText())
        getClassNum = R.r['getClassNumber']
        classes = getClassNum(pF)
        classDN=[int(i) for i in classes]
        self.__noOfClasses=len(classes)
        self.__className=classDN
        #else:
        #   classDN=[int(i) for i in self.__className]
        #    self.__className=classDN
        return(R.IntVector(classDN))

    def prepareTempPng(self,pF,pT,pL,cNo,cN,cC,wTP):
        import tempfile
        from rpy2.robjects.packages import importr
        genMapp = R.r['genrateMap']
        self.temp = tempfile.NamedTemporaryFile(mode='w+t', delete=False,suffix='.png')
        filepng=self.temp.name
        self.temp.close()
        grdevices = importr('grDevices')
        if wTP=="A4":
            grdevices.png(filepng ,width=2480,height=3508,res=300)
        else:
            grdevices.png(filepng ,width=546,height=520)
        genMapp(pF,pT,pL,cNo,cN,cC,wTP)
        grdevices.dev_off()
        print( filepng)
        return(filepng)

    def showDialog(self):
        col = QtGui.QColorDialog.getColor()
        if col.isValid():
            self.frm.setStyleSheet("QWidget { background-color: %s }"% col.name())

    def prepareExecutionEnv(self):
        installed_dir = self.get_main_dir()
        installed_dir=installed_dir.replace(os.sep,"/")
        self.__projectDirectory = installed_dir
        self.ui.leProjectDirectory_DataPreparationInputSectionProjectSection.setText(str(self.__projectDirectory))
        self.__installedDir=installed_dir

    def prepareR(self):
        self.rcode="source('"+self.__installedDir+"/Rasterise_dev_68akj.r')"
        R.r(self.rcode)
        #libs=["png","grid","raster","maptools","rgeos","rgdal","parallel","doParallel","nnet","data.table"]
        libs=['data.table','raster','maptools','parallel','doParallel','nnet','maptools','rgeos','rgdal','randomForest','expm','Matrix','png']
        CheckInstallPackage = R.r['CheckInstallPackage']
        #code="packages="+libs
        CheckInstallPackage(libs)

    def releaseR(self):
        rquit=R.r['quit']
        rquit("no",0)

    def getColorPallet(self):
        newp=[]
        row=self.ui.twColorTable_ViewMaps.rowCount()
        col=self.ui.twColorTable_ViewMaps.columnCount()
        for i in list(range(0, row)):
            item=self.ui.twColorTable_ViewMaps.item(i,2)
            color=item.background().color().name()+"FF"
            newp.append(str(color).upper())
        newp= R.StrVector(newp)
        return(newp)

    def closeEvent(self, event):
        quit_msg = "Are you sure you want to exit the program?"
        reply = QtWidgets.QMessageBox.question(self, 'OpenLDM Message',
                                                   quit_msg, QtWidgets.QMessageBox.Yes, QtWidgets.QMessageBox.No)

        if reply == QtWidgets.QMessageBox.Yes:
            self.statusMessage.exit(0)
            self.releaseR()
            event.accept()
        else:
            event.ignore()

    def _result_available(self, ok):
        frame = self.page().mainFrame()

    def main_is_frozen(self):
        return (hasattr(sys, "frozen") or # new py2exe
                    hasattr(sys, "importers") # old py2exe
                    or imp.is_frozen("__main__")) # tools/freeze

    def get_main_dir(self):
        if self.main_is_frozen():
            return os.path.dirname(os.path.abspath(sys.executable))
        if len(sys.argv)==1:
            return os.path.dirname(os.path.abspath(sys.argv[0]))
        return os.path.dirname(os.path.abspath(sys.argv[len(sys.argv)-1]))
    
    def filldebugInput(self):
        self.ui.leT0File_DataPreparationInputSectionDataInput.setText(str("../examples/LULC/1985.tif"))   
        self.__T0File=str(self.ui.leT0File_DataPreparationInputSectionDataInput.text())
        
        self.ui.leT0Year_DataPreparationInputSectionDataInput.setText(str("1985"))
        self.__T0Year = int(str(self.ui.leT0Year_DataPreparationInputSectionDataInput.text()))
        
        self.ui.leT1File_DataPreparationInputSectionDataInput.setText(str("../examples/LULC/1995.tif"))
        self.__T1File=str(self.ui.leT1File_DataPreparationInputSectionDataInput.text())
        
        self.ui.leT1Year_DataPreparationInputSectionDataInput.setText(str("1995"))
        self.__T1Year = int(str(self.ui.leT1Year_DataPreparationInputSectionDataInput.text()))
        
        self.ui.leOutputFile_DataPreparationOutputSection.setText(str("../examples/outputdata/modeloutput.tif"))
        self.__OutputFile= str(self.ui.leOutputFile_DataPreparationOutputSection.text())
        self.ui.lePredictedFile_AccuracyAssesment.setText(self.__OutputFile)
        
        
        
    def filldebugAdddriver(self):
        self.filldebugSelectDriver(1,"../examples/Drivers/commonDrivers/elevation.img")
        self.filldebugSelectDriver(2,"../examples/Drivers/drivers_85/dist_stream.img")
        self.filldebugSelectDriver(3,"../examples/Drivers/drivers_85/Dist_urban.img")
        self.filldebugSelectDriver(4,"../examples/Drivers/drivers_85/road_final.img")
        
        #self.SelectDriver_pushed()
    def filldebugSelectDriver(self,row,filename):
        self.on_pbAddDriver_DriverSelectionT0SelectDrivers_clicked()
        disp=self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(row-1, 1)
        disp.setText(filename)
        disp=self.ui.twSelectDrivers_DriverSelectionT0.cellWidget(row-1, 0)
        (dirName, OnlyFilename) = os.path.split(filename.strip())
        disp.setText(OnlyFilename.replace(".","_"))

        
    def filldebug(self):
        self.filldebugInput()
        self.on_pbNextDataPreparation_clicked()

        self.filldebugAdddriver()
        self.fillModelType()
        self.on_pbViewModelStatistics_DriverSelectionT0DoModelFitting_clicked()
        self.on_pbNext_DriverSelectionT0_clicked()

        self.fillClassNames()
        self.on_pbNext_ModelAnalysis_clicked()
        
        #self.printParameter()
    
    def fillClassNames(self):
        twViewModelCoefficint=self.ui.twViewModelCoefficint_ModelAnalysis 
        row=twViewModelCoefficint.rowCount()
        #col=twViewModelCoefficint_ModelAnalysis.columnCount()
        j=0
        classnames=['BU','AG','DF','FL','GL','MF','PL','SL','WB']
        for i in list(range(0, row, 1)):
            twViewModelCoefficint.item(i,j).setText(classnames[i])

    def fillModelType(self):
        self.ui.rbLogisticRegression_DriverSelectionT0DoModelFitting.setChecked(True)
        #self.ui.rbRandomForest_DriverSelectionT0DoModelFitting.setChecked(True)
        
        
        
    def printParameter(self):
        print("model.type-[[ "+str(self.__modeltype)+ " ]] ")
        print("T0File-[[ "+self.__T0File+ " ]] ")
        print("T1File-[[ "+self.__T1File+ " ]] ")
        print("T0Year-[[ "+str(self.__T0Year)+ " ]] ")
        print("T1Year-[[ "+str(self.__T1Year)+ " ]] ")        
        print("T1drivers-[[ "+str(self.__DriverDictionaryT1)+ " ]] ")
        print("T2drivers-[[ "+str(self.__DriverDictionaryT2)+ " ]] ")
        print("withNAvalue-[[ "+str(self.__modelNAValue)+ " ]] ")
        #print("with.class.name-[[ "+str(self.__className)+ " ]] ")
        #print("demand-[[ "+str(self.__demand)+ " ]] ")
        #print("restrictSpatialMigration-[[ "+str(self.__restricSpatial) + " ]] ")
        #print("AllowedClassMigration-[[ "+"TODO" + " ]] ")
        #print("conversionOrder-[[ "+str(self.__conversionOrder)+ " ]] ")
        #print("classAllocationOrder - [["+str(self.__classAllocationOrder)+ " ]] ")
        #print("neighbour-[[ "+str(self.__neughbourl)+ " ]] ")
        print("modelformula-[[ "+str(self.__modelformula)+ " ]] ")
        print("outputfile-[[ "+self.__OutputFile+ " ]] ")
        print("modelNAValue-[[ "+str(self.__modelNAValue)+ " ]] ")
        print("MASKFile-[[ "+str(self.__MASKFile)+ " ]] ")
        print("AOIFile-[[ "+str(self.__AOIFile)+ " ]] ")
        print("suitabilityFileDirectory-[[ "+str(self.__suitabilityFileDirectory)+ " ]] ")        
if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    myapp = MyForm()
    myapp.show()
    #myapp.filldebug()   
    # icon = QtGui.QIcon()
    # icon.addPixmap(QtGui.QPixmap(":/images/ilulc/icon.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
    # myapp.setWindowIcon(icon)
    myapp.setWindowIcon(QtGui.QIcon(":/images/ilulc/icon.png"))
    r=app.exec_()
    #myapp.statusMessage.exit(0)
    print(r)
    sys.exit(r)
