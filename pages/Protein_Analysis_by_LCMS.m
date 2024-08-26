(* CreatedFrom Object[EmeraldCloudFile, "id:kEJ9mqJ1xwvL"] on Wed 24 Jul 2024 16:21:02 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Title:: *)
Intact Protein Analysis by LCMS

(* ::Subsection:: *)
License

(* ::Text:: *)
This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Dirk Schild & Waseem Vali
E.mail: waseem@alignbio.org
Requested citation: https://doi.org/10.5281/zenodo.13375207

(* ::Section:: *)
Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
This protocol is used to determine the intact mass of protein using LCMS with a QToF detector. 

(* ::Item:: *)
The sample is filtered in in-line using a desalting column

(* ::Item:: *)
Mass spectrum is collected

(* ::Item:: *)
Downsampling is performed to reduce the size of the datasets

(* ::Item:: *)
Data is exported for analysis

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
Broad signals will be observed if the concentration of the protein is too high due to overloading of the column. A 50 - 100 pMol injection is recommended, which can be achieved through a combination of Injection Volumes and Concentrations, e.g. 10 Microliter of a 10 Micromolar solution, or 2 Microliter of a 50 Micromolar solution.
Set the AutoSamplerTemperature to 7 Celsius if samples are temperature sensitive.
If this protocol is ran as a script, pause after the Downsampling to make sure the the computations have finished (each computation takes up to an hour or Manifold).

(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup

(* ::Text:: *)
Make sure that the protein is at the right concentration as no dilutions are performed in the protocol.

(* ::Code:: *)
inputSamples = Object[Sample,"id:L8kPEjkLJo8V"];(*input samples here*)
injectionVolumes = 10 Microliter;(*needs to be between 2 and 50 Microliter. The suyggested injection amount is 100 pmol*)
sampleTemperature = Ambient; (*5 - 40 Celsius*)
massRange = ;;; (*Set to expected m/z range in which the charged intact protein is epected (this is lower than the mass of the protein if it has more than one charge.*)

(* ::Text:: *)
If you need, you can calculate the recommended injection volume using the following function.

(* ::Code:: *)
recommendedInjectionVolume[proteinConcentration_] := UnitScale[100 Picomole/proteinConcentration//N]; 

(* ::Input:: *)
recommendedInjectionVolume[10Micromolar]

(* ::Output:: *)


(* ::Section:: *)
Running Code

(* ::Text:: *)
Step 1. LCMS Protocol

(* ::Code:: *)
lcmsProtocol = ExperimentLCMS[
	inputSamples,
	SampleTemperature -> sampleTemperature, (*check if this is desirable for a generalized method*)
	InjectionVolume -> injectionVolumes,
	
	Blank -> Model[Sample,"Milli-Q water"],

		(*Gradient and Buffer settings*)
	Gradient -> Object[Method,Gradient,"OpenNano Protein LCMS Gradient"],
	BufferA -> Model[Sample,"0.1% Formic acid in Water"],
	BufferB -> Model[Sample,"0.1% Formic acid in Acetonirile for LCMS"],
	BufferC -> Model[Sample,StockSolution,"20% Methanol in MilliQ Water"],
	
	(*HPLC Settings*)
	GuardColumn -> Model[Item,Column,"High Performance ZORBAX Guard Column Cartridge Holder"],
	ColumnSelector -> {Model[Item,Column,"High Performance ZORBAX Guard Column Cartridge Holder"], Null,Null,Null},
	Column->Null,	
	NeedleWashSolution -> Model[Sample,StockSolution,"20% Methanol in MilliQ Water"],
	
	(*Mass Spec Settings*)
	Analytes -> {Model[Molecule,"Water"],Model[Molecule,"Lysozyme"]},
	MassAnalyzer -> QTOF,
	IonMode -> Positive,
	ESICapillaryVoltage -> 4000 Volt,
	DeclusteringVoltage -> 40 Volt,
	StepwaveVoltage -> 40 Volt,
	SourceTemperature -> 150 Celsius,
	DesolvationTemperature -> 600 Celsius,
	DesolvationGasFlow -> 1200 Liter/Hour,
	ConeGasFlow -> 75 Liter/Hour,
	Fragment -> False,
	MassDetection -> massRange,
	ScanTime -> 1 Second,
	AcquisitionWindow ->{0 Minute;;4 Minute},
	
	BlankIonMode -> Positive,
	BlankESICapillaryVoltage -> 4000 Volt,
	StandardDeclusteringVoltage -> 40 Volt,
	BlankStepwaveVoltage -> 40 Volt,
	BlankSourceTemperature -> 150 Celsius,
	BlankDesolvationTemperature -> 600 Celsius,
	BlankDesolvationGasFlow -> 1200 Liter/Hour,
	BlankConeGasFlow -> 75 Liter/Hour,
	BlankFragment -> False,
	BlankMassDetection -> massRange,
	BlankScanTime -> 1 Second,
	BlankAcquisitionWindow ->{0 Minute;;4 Minute}
]

(* ::Text:: *)
Step 2. Downsampling

(* ::Text:: *)
Downsampling is performed to reduce the size of the datasets. If needed replace lcmsProtocol with your Protocol. This is run on Manifold using the Compute function.

(* ::Code:: *)
Compute[AnalyzeDownsampling[
		#,
		IonAbundance3D,
		LoadingBars->False
	],
	HardwareConfiguration -> HighRAM
	]&/@ lcmsProtocol[Data]

(* ::Text:: *)
Step 3. Exporting for Deconvolution

(* ::Text:: *)
https://www.enovatia.com/our-products/promass/promass-web/

(* ::Text:: *)
In the stand-alone mode you should either drag/drop your experimental spectrum as a tab-delimited text file. The exportForDeconvolution function below takes a data object and the time of interest (in minute) and exports the mass spectrum to a .txt file.

(* ::Code:: *)
exportForDeconvolution[dataObject_,targetTime_] := Module[{ionAbundance3DData,timePoints,nearestTimePoint,ionAbudanceDataAtTimePoint},
	ionAbundance3DData = QuantityMagnitude[dataObject[IonAbundance3D]];
	timePoints=ionAbundance3DData[[All,1]]//DeleteDuplicates;
	nearestTimePoint =Nearest[timePoints,Unitless[targetTime]];
	ionAbudanceDataAtTimePoint = #[[{2,3}]]&/@Cases[ionAbundance3DData,{nearestTimePoint[[1]],_ ,_}];
	Export[FileNameJoin[$TemporaryDirectory, StringDrop[dataObject[ID],3] <> (*ToString[targetTime] <> *)".txt"],ionAbudanceDataAtTimePoint,"TSV"]
]

(* ::Section:: *)
Inventory and discarding unwanted samples

(* ::Subsubsection:: *)
All samples in script

(* ::Code:: *)
scriptStoredObjects = {}

(* ::Subsubsection:: *)
Sample discarding

(* ::Text:: *)
Set the samples to keep throughout the script

(* ::Code:: *)
(*containersToKeep = {} *)

(* ::Code:: *)
containersToDiscard = Complement[scriptStoredObjects,containersToKeep]

(* ::Input:: *)
(*These are recommended samples to discard*)

(* ::Code:: *)
PauseScript[]

(* ::Code:: *)
DiscardSamples[containersToDiscard]

(* ::Section:: *)
Conclusions and Future Directions