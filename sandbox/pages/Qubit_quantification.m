(* CreatedFrom Object[EmeraldCloudFile, "id:GmzlKjzOlZO4"] on Tue 30 Apr 2024 15:28:16 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Section:: *)
Qubit Quantification

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Michael Crone
E.mail: crone.michael@gmail.com
Requested citation: None

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
This code can be used for the quantification of DNA or RNA with the Qubit BR and HS kits.

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
None


(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup Code

(* ::Subsection:: *)
Plate Reader Settings for different kits

(* ::Section:: *)
Running Code

(* ::Subsubsection:: *)
Specify the kit being used

(* ::Text:: *)
Specify the kit being used, which determines the dye and the standards that are used.
DNA BR Kit

(* ::Code:: *)
highconcentrationstandard = Model[Sample,"id:54n6evn1X5rB"]
lowconcentrationstandard = Model[Sample,"id:J8AY5jAmq0kD"]
premixeddyebuffer = Model[Sample,"id:R8e1Pje9LqXd"]
kitexcitationwavelength = 483 Nanometer
kitemissionwavelength = 530 Nanometer

(* ::Output:: *)
Model[Sample,"Qubit dsDNA BR Working Solution (Component C) (Qubit 1X dsDNA BR Kit)"]

(* ::Output:: *)
Model[Sample,"Qubit dsDNA BR Working Solution (Component B) (Qubit 1X dsDNA BR Kit)"]

(* ::Output:: *)
Model[Sample,"Qubit dsDNA BR Working Solution (Component A) (Qubit 1X dsDNA BR Kit)"]

(* ::Output:: *)


(* ::Output:: *)


(* ::Subsubsection:: *)
Specify the samples and the name of the plate

(* ::Code:: *)
dnaplatename = "DNA plate_240424_1"
samples = {highconcentrationstandard}
numsamples = Length[samples]
transfers = ConstantArray[2 * Microliter, numsamples]

(* ::Output:: *)
"DNA plate_240424_1"

(* ::Output:: *)
{Model[Sample,"Qubit dsDNA BR Working Solution (Component C) (Qubit 1X dsDNA BR Kit)"]}

(* ::Output:: *)
1

(* ::Output:: *)
{}

(* ::Subsection:: *)
Wells to use

(* ::Code:: *)
allpossiblewells = Flatten[Transpose[AllWells[]]]
totalnumwells = numsamples + 6
wellsused = Take[allpossiblewells,totalnumwells]
controlwells = wellsused[[;;6]]
controlwells[[;;-2]]
samplewells = wellsused[[7;;]]

(* ::Output:: *)
{"A1","B1","C1","D1","E1","F1","G1","H1","A2","B2","C2","D2","E2","F2","G2","H2","A3","B3","C3","D3","E3","F3","G3","H3","A4","B4","C4","D4","E4","F4","G4","H4","A5","B5","C5","D5","E5","F5","G5","H5","A6","B6","C6","D6","E6","F6","G6","H6","A7","B7","C7","D7","E7","F7","G7","H7","A8","B8","C8","D8","E8","F8","G8","H8","A9","B9","C9","D9","E9","F9","G9","H9","A10","B10","C10","D10","E10","F10","G10","H10","A11","B11","C11","D11","E11","F11","G11","H11","A12","B12","C12","D12","E12","F12","G12","H12"}

(* ::Output:: *)
7

(* ::Output:: *)
{"A1","B1","C1","D1","E1","F1","G1"}

(* ::Output:: *)
{"A1","B1","C1","D1","E1","F1"}

(* ::Output:: *)
{"A1","B1","C1","D1","E1"}

(* ::Output:: *)
{"G1"}

(* ::Subsection:: *)
Creating identity models

(* ::Code:: *)
Qubitexperiment = ExperimentRoboticSamplePreparation[
	{
LabelContainer[
     Label->dnaplatename,
Container->Model[Container, Plate, "96-well Black Wall Greiner Plate"]],
LabelContainer[
     Label->"LowConcentrationStandardContainer",
Container-> Model[Container,Vessel,"New 0.5mL Tube with 2mL Tube Skirt"]],
Transfer[
	Source -> lowconcentrationstandard,
			Destination -> "LowConcentrationStandardContainer",
				Amount -> 60 * Microliter
		],
LabelContainer[
     Label->"HighConcentrationStandardContainer",
Container-> Model[Container,Vessel,"New 0.5mL Tube with 2mL Tube Skirt"]],
Transfer[
	Source -> highconcentrationstandard,
			Destination -> "HighConcentrationStandardContainer",
				Amount -> 60 * Microliter
		],
LabelContainer[
     Label->"NucleaseFreeWaterContainer",
Container-> Model[Container,Vessel,"New 0.5mL Tube with 2mL Tube Skirt"]],
Transfer[
	Source -> Model[Sample, "Nuclease-free Water"],
			Destination -> "NucleaseFreeWaterContainer",
				Amount -> 500 * Microliter
		],
Transfer[
	Source -> "LowConcentrationStandardContainer",
			Destination -> dnaplatename,
				DestinationWell -> controlwells[[2;;]],
				Amount -> {2 * Microliter, 4 * Microliter,6 * Microliter, 8 * Microliter, 10 * Microliter}
(*AspirationPosition -> Bottom,
AspirationPositionOffset ->  0.5 * Millimeter*)
		],
Transfer[
	Source -> "HighConcentrationStandardContainer",
			Destination -> dnaplatename,
				DestinationWell -> controlwells[[;;-2]],
				Amount -> {10 * Microliter, 8 * Microliter,6 * Microliter, 4 * Microliter, 2 * Microliter}
(*AspirationPosition -> Bottom,
AspirationPositionOffset ->  0.5 * Millimeter*)
		],
Transfer[
	Source -> "NucleaseFreeWaterContainer",
			Destination -> dnaplatename,
				DestinationWell -> samplewells,
				Amount -> 8 * Microliter
(*AspirationPosition -> Bottom,
AspirationPositionOffset ->  0.5 * Millimeter*)
		],
Transfer[
	Source -> samples,
			Destination -> dnaplatename,
				DestinationWell -> samplewells,
				Amount -> transfers
(*AspirationPosition -> Bottom,
AspirationPositionOffset ->  0.5 * Millimeter*)
		],
Transfer[
	Source -> premixeddyebuffer,
			Destination -> dnaplatename,
				DestinationWell -> wellsused,
				Amount -> 190 Microliter,
				DispenseMixVolume -> 190 Microliter,
				NumberOfDispenseMixes -> 20
		],
	FluorescenceIntensity[
	Sample -> dnaplatename,
	WavelengthSelection -> Monochromators,
	ReadLocation -> Bottom,
AdjustmentSample ->FullPlate,
	ExcitationWavelength -> kitexcitationwavelength,
	EmissionWavelength -> kitemissionwavelength,
	Instrument -> Model[Instrument, PlateReader, "CLARIOstar"],
	EquilibrationTime -> 5 Minute
	]
	}
	
	]

(* ::Print:: *)


(* ::Output:: *)


(* ::Print:: *)
"New objects added to Constellation:"FalseTrue

(* ::Output:: *)
Object[Protocol,RoboticSamplePreparation,"id:xRO9n3OJvoYY"]

(* ::Subsection:: *)
Create standard curve and plot concentration

(* ::Code:: *)
orderDataByWell[data_]:=Module[{wells,wellDataPairs,wellOrderingList,sortedWellDataPairs},wells=data[Well];
(*Pair up the data and the wells*)wellDataPairs=Transpose[{wells,data}];
(*Make a well ordering list*)wellOrderingList=Flatten[Transpose[AllWells[]]];
(*Sort the paired {well,data} list by well*)sortedWellDataPairs=SortBy[wellDataPairs,Position[wellOrderingList,First[#]]&]]


(* ::Code:: *)
Qubitexperiment = Object[Protocol,RoboticSamplePreparation,"id:xRO9n3OJvoYY"]
qubitData=orderDataByWell[Qubitexperiment[Data]]
qubitComposition = orderDataByWell[Qubitexperiment[SamplesOut]]
standardcurveIntensities = Flatten[Cases[qubitData,{Alternatives@@controlwells,data_}:>data][Intensities]]
standardcurveComposition = Cases[qubitComposition,{Alternatives@@controlwells,data_}:>data][Composition]
standardCurveConcs = ReplaceAll[standardcurveComposition,{{Null,Null}}->{{Null,Null},{,}}][[All, 2, 1]]
scriptfit = AnalyzeFit[Transpose[{standardCurveConcs, standardcurveIntensities}],Linear]

(* ::Output:: *)
Object[Protocol,RoboticSamplePreparation,"id:xRO9n3OJvoYY"]

(* ::Output:: *)
{{"A1",},{"B1",},{"C1",},{"D1",},{"E1",},{"F1",},{"G1",}}

(* ::Output:: *)
{{Null,},{Null,},{"A1",},{"B1",},{"C1",},{"D1",},{"E1",},{"F1",},{"G1",}}

(* ::Output:: *)
{,,,,,}

(* ::Output:: *)
{{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null}}}

(* ::Output:: *)
{,,,,,}

(* ::Print:: *)
"New objects added to Constellation:"FalseTrue

(* ::Output:: *)
Object[Analysis,Fit,"id:xRO9n3OJqo6w"]

(* ::Code:: *)
qubitData=orderDataByWell[Qubitexperiment[Data]]
sampleIntensities = Flatten[Cases[qubitData,{Alternatives@@samplewells,data_}:>data][Intensities]]


(*Concentration = InversePrediction[Object[Analysis,Fit,"id:R8e1Pje3Rdad"],sampleIntensities[[1,1]]]/2*20*)
Concentration = InversePrediction[Object[Analysis,Fit,"id:xRO9n3OJqo6w"],#]/2*20& /@ sampleIntensities

(* ::Output:: *)
{{"A1",},{"B1",},{"C1",},{"D1",},{"E1",},{"F1",},{"G1",}}

(* ::Output:: *)
{}

(* ::Output:: *)
{}

(* ::Input:: *)
PlotPrediction[scriptfit, 2.5 Nanogram/Microliter]

(* ::Output:: *)


(* ::Code:: *)
headers={"Sample Name","Qubit Fluorescence","Concentration"};

PlotTable[Table[{samples[[i]],sampleIntensities[[i]],Concentration[[i]]},{i,Length[samples]}],TableHeadings->{None,headers}]

(* ::Output:: *)


(* ::Subsection:: *)
Calculate concentration of sample

(* ::Code:: *)
Concentration = InversePrediction[scriptfit,intensitiessample[[1,1]]/2*20 ]

(* ::Subsection:: *)
Troubleshooting Info

(* ::Subsection:: *)
Validation/Control/Sufficiency/Robustness checks

(* ::Text:: *)


(* ::Section:: *)
Inventory and discarding unwanted samples

(* ::Subsubsection:: *)
All samples in script

(* ::Input:: *)
(*StoredObjectsFromScript[notebookPage,Output->Table]*°

(* ::Subsubsection:: *)
Sample discarding

(* ::Text:: *)
Set the samples to keep throughout the script

(* ::Input:: *)
(*containersToKeep = {}

(* ::Output:: *)
{}

(* ::Input:: *)
containersToDiscard = Complement[scriptStoredObjects,containersToKeep]

(* ::Input:: *)
(*These are recommended samples to discard*)

(* ::Input:: *)
PauseScript[]

(* ::Input:: *)
DiscardSamples[containersToDiscard]

(* ::Section:: *)
Conclusions and Future Directions