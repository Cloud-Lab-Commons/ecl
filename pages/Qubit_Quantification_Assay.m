(* CreatedFrom Object[EmeraldCloudFile, "id:1ZA60vApKYEP"] on Thu 9 May 2024 10:31:06 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Section:: *)
Qubit Quantification

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Michael Crone and Waseem Vali
E.mail: crone.michael@gmail.com , waseem@alignbio.org
Requested citation: https://doi.org/10.5281/zenodo.13375207

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Schematic of the experiment

(* ::Input:: *)


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

(* ::Item:: *)
Specify the kit samples being used, which determines the dye and the standards that are used.
DNA BR Kit

(* ::Item:: *)
Sometimes the sample can be expired, however DNA and Kit samples are stables for much longer than the expiration date. In order to use the \[OpenCurlyDoubleQuote]Expired\[CloseCurlyDoubleQuote] samples, use Object[Sample] instead of the Models to specify using the correct samples. 

(* ::Item:: *)
CAUTION: make sure you have enough volume when specifying Object[Sample] if not pool two or more samples and then use it in the protocol.

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
(*transfers = ConstantArray[2 * Microliter, numsamples]*)

(* ::Output:: *)
"DNA plate_240424_1"

(* ::Output:: *)
{Model[Sample,"Qubit dsDNA BR Working Solution (Component C) (Qubit 1X dsDNA BR Kit)"]}

(* ::Output:: *)
1

(* ::Subsection:: *)
Wells to use

(* ::Code:: *)
allpossiblewells = Flatten[Transpose[AllWells[]]]
totalnumwells = numsamples + 6
wellsused = Take[allpossiblewells,totalnumwells]
controlwells = wellsused[[;;6]]
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
{"G1"}

(* ::Subsection:: *)
Run Experiment

(* ::Code:: *)
Qubitexperiment = ExperimentRoboticSamplePreparationInputs[
	{
		(*Label the plate in which the reaction will be carried out*)
		LabelContainer[
			Label->dnaplatename,
			Container->Model[Container, Plate, "96-well Black Wall Greiner Plate"]
		],
		(*Label the tubes for high Conc. Standard, low Conc. Standard, and Nuclease Free Water*)
		LabelContainer[
			Label->{"LowConcentrationStandardContainer","HighConcentrationStandardContainer","NucleaseFreeWaterContainer"},
			Container-> Model[Container,Vessel,"New 0.5mL Tube with 2mL Tube Skirt"]
		],
		(*Transfer the standards and water in the tubes*)
		Transfer[
			Source -> {lowconcentrationstandard, highconcentrationstandard, Model[Sample, "Nuclease-free Water"]},
			Destination -> {"LowConcentrationStandardContainer", "HighConcentrationStandardContainer", "NucleaseFreeWaterContainer"},
			Amount -> {60 Microliter, 60 Microliter, 500 Microliter}
		],
		(*Transfer the low standard into the control reaction wells*)
		Transfer[
			Source -> "LowConcentrationStandardContainer",
			Destination -> dnaplatename,
			DestinationWell -> controlwells[[2;;]],
			Amount -> {2 Microliter, 4 Microliter,6 Microliter, 8 Microliter, 10 Microliter}
		],
		(*Transfer the high standard into the control reaction wells*)
		Transfer[
			Source -> "HighConcentrationStandardContainer",
			Destination -> dnaplatename,
			DestinationWell -> controlwells[[;;-2]],
			Amount -> {10 Microliter, 8 Microliter,6 Microliter, 4 Microliter, 2 Microliter}
		],
		(*Transfer the water into unknown sample wells*)
		Transfer[
			Source -> "NucleaseFreeWaterContainer",
			Destination -> dnaplatename,
			DestinationWell -> samplewells,
			Amount -> 8 Microliter
		],
		(*Transfer the samples ino the unknow sample wells*)
		Transfer[
			Source -> samples,
			Destination -> dnaplatename,
			DestinationWell -> samplewells,
			Amount -> 2 Microliter
		],
		(*Transfer the eddy buffer into all the reaction wells including the controls*)
		Transfer[
			Source -> premixeddyebuffer,
			Destination -> dnaplatename,
			DestinationWell -> wellsused,
			Amount -> 190 Microliter,
			DispenseMixVolume -> 190 Microliter,
			NumberOfDispenseMixes -> 20
		],
		(*Check the flurescent intensity*)
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

(* ::Message:: *)


(* ::Output:: *)
{[],[],[],[]}

(* ::Input:: *)


(* ::Subsection:: *)
Create standard curve and plot concentration

(* ::Code:: *)
qbitProtocol = Object[Protocol,RoboticSamplePreparation,"id:xRO9n3OJvoYY"]

(*Extract the standard curve data and intensity*)
standardData = Cases[SortBy[Transpose[{qbitProtocol[Data][Well], qbitProtocol[Data]}], First], {Alternatives@@controlwells,_}]
standardSamples = Cases[SortBy[Transpose[{qbitProtocol[SamplesOut][Well], qbitProtocol[SamplesOut]}], First], {Alternatives@@controlwells,_}]
standardCompositions = standardSamples[[All, 2]][Composition]
standardConcentrations = Append[Take[standardCompositions, 5][[All, 2, 1]], 0.0 Nanogram/Microliter]
standardIntensities =Flatten[standardData[[All,2]][Intensities]]

(* ::Output:: *)
Object[Protocol,RoboticSamplePreparation,"id:xRO9n3OJvoYY"]

(* ::Output:: *)
{{"A1",},{"B1",},{"C1",},{"D1",},{"E1",},{"F1",}}

(* ::Output:: *)
{{"A1",},{"B1",},{"C1",},{"D1",},{"E1",},{"F1",}}

(* ::Output:: *)
{{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null},{,}},{{Null,Null}}}

(* ::Output:: *)
{,,,,,}

(* ::Output:: *)
{,,,,,}

(* ::Subsubsection:: *)
Fit the concentration vs intensity data to obtain a fit

(* ::Code:: *)
standardFit = AnalyzeFit[Transpose[{standardConcentrations, standardIntensities}], Linear]

(* ::Print:: *)
"New objects added to Constellation:"FalseTrue

(* ::Output:: *)
Object[Analysis,Fit,"id:o1k9jAkL7ZLm"]

(* ::Subsection:: *)
Extract Sample Data for concentration determination

(* ::Code:: *)
sampleData = Cases[SortBy[Transpose[{exp[Data][Well], exp[Data]}], First], {Alternatives@@samplewells,_}]
sampleIntensities = Flatten[ sampleData[[All, 2,1]][Intensities]]

(* ::Output:: *)
{{"G1",Link[Object[Data,FluorescenceIntensity,"id:qdkmxzkYWOkx"],Protocol,"8qZ1Vb9ee1dP"]}}

(* ::Output:: *)
{}

(* ::Text:: *)
Determining Concentrations of the unknown samples using the fit performed earlier

(* ::Code:: *)
sampleConcentrations= Map[InversePrediction[standardFit,#]/2*20&, sampleIntensities]

(* ::Output:: *)
{}

(* ::Text:: *)
Plot a table of unknown samples and concentrations

(* ::Code:: *)
headers={"Sample Name","Qubit Fluorescence","Concentration"};

PlotTable[Table[{samples[[i]],sampleIntensities[[i]],sampleConcentrations[[i]]},{i,Length[samples]}],TableHeadings->{None,headers}]

(* ::Output:: *)


(* ::Text:: *)
Predict an unknown concentration

(* ::Code:: *)
PlotPrediction[standardFit, 4 Nanogram/Microliter]

(* ::Output:: *)


(* ::Subsection:: *)
Calculate concentration of sample

(* ::Code:: *)
Concentration = InversePrediction[standardFit,Intensity]/2*20

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
(*StoredObjectsFromScript[notebookPage,Output->Table]*)

(* ::Subsubsection:: *)
Sample discarding

(* ::Text:: *)
Set the samples to keep throughout the script

(* ::Input:: *)
(*containersToKeep = {}*)

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