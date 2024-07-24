(* CreatedFrom Object[EmeraldCloudFile, "id:01G6nvGJG3Am"] on Wed 24 Jul 2024 16:27:46 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Title:: *)
Pure Express

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Waseem Vali
E.mail: waseem@alignbio.org
Requested citation: None

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
To express the protein from DNA using cell free expression PureExpress reaction and then measure fluorescence intensity. 

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
1. Make sure to have the samples and PureExpress components in a Hamilton Compatible container 
2. Also make sure you have enough volumes in the input samples to use
3. In order for the legends in the plot to appear, make sure your samples have names


(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
1) Input Samples 

(* ::Code:: *)
inputSamples = {} (*provide DNA samples that you want to express *)

(* ::Input:: *)
(*this is the sameple in the above list that will be used to zero the FluorescenceIntensity *)

(* ::Code:: *)
adjustmentSample = 1;

(* ::Section:: *)
2) Define quantities for Protein production

(* ::Input:: *)
(*Pure express recommends using 250 ng template DNA per 25 \[Mu]l reaction)

(* ::Text:: *)
1. Water (Nuclease free water)
2. PureExpress Solution A - 40 % by volume, 24 ul per 60
3. PureExpress Solution B - 30 % by volume, 18 ul per 60
4. RNasin Plus Rnase Inhibitor - 2 % by volume, 1.2 ul per 60
5. DNA : 8 ng/ul which corresponds to 200 ng per 25 ul or 480 ng per 60 - Not sure how much RNA
5 B. ligated RNA should be 1750 pm 
@MBC : We ligate 500 pmol and purify it and resuspend in 30 ul and add 3.5 ul out of it for the translation reaction.
     
    @ 30 ml volume :
  1.72 ul Nuclease free water
9.6 ul solution A
7.2 ul solution B
0.48 RNasin Plus
7 ul Ligated mixture (added last)

@ 15 ml volume
0.86 ul Nuclease free water
4.8 ul solution A
3.6 ul solution B
0.24 RNasin Plus
3.5 ul Ligated mixture (added last)

(* ::Code:: *)
xTimesBaseVolumes = 2.03  (*The extra 3% is for sample sample loss in pipetting*)

(* ::Code:: *)
numbSamplesToPrepPerWell = 7 (*this indicates that the max volume is 7*30ul or 240ul per setup well when XTimesBaseVolume is set to 2*)

(* ::Code:: *)
solutionAPerWell =4.8Microliter*xTimesBaseVolumes

(* ::Code:: *)
solutionBPerWell =3.6Microliter*xTimesBaseVolumes

(* ::Code:: *)
rnasinPerWell = 0.24Microliter*xTimesBaseVolumes

(* ::Code:: *)
dnaPerWell = 3.5 Microliter*xTimesBaseVolumes

(* ::Code:: *)
nucleaseFreeWaterPerWell = 2.86 Microliter*xTimesBaseVolumes

(* ::Code:: *)
dnaPerWellAdjusted = dnaPerWell + nucleaseFreeWaterPerWell

(* ::Code:: *)
solutionATotalVolume =solutionAPerWell*Length[inputSamples]

(* ::Code:: *)
solutionBTotalVolume =solutionBPerWell*Length[inputSamples]

(* ::Code:: *)
rnasinTotalVolume = rnasinPerWell*Length[inputSamples]

(* ::Code:: *)
solutionPerWell = solutionAPerWell+solutionBPerWell+rnasinPerWell+dnaPerWell+nucleaseFreeWaterPerWell (*should be 30ul*)

(* ::Subsection:: *)
3 A) setup 1well water mote

(* ::Code:: *)
moteWells =Experiment`Private`getMoatWells[AllWells[],1]

(* ::Code:: *)
moteWellsInPlate={#,"protein plate"}&/@moteWells

(* ::Code:: *)
complmentMote = Complement[Flatten[AllWells[]],Experiment`Private`getMoatWells[AllWells[],1]]

(* ::Code:: *)
complmentMoteOrdered=SortBy[complmentMote,PositionIndex[Flatten[AllWells[]]]]

(* ::Subsection:: *)
3 B) setup solution distribution wells

(* ::Code:: *)
numbPrepWells = If[QuotientRemainder[Length[inputSamples],numbSamplesToPrepPerWell][[2]]>0,QuotientRemainder[Length[inputSamples],numbSamplesToPrepPerWell][[1]]+1,QuotientRemainder[Length[inputSamples],numbSamplesToPrepPerWell][[1]]]

(* ::Code:: *)
pureExpressDistribute[solutionPerWell_,sampleCt_]:=Append[
	Repeat[solutionPerWell*numbSamplesToPrepPerWell,QuotientRemainder[sampleCt,numbSamplesToPrepPerWell][[1]]],
	solutionPerWell*QuotientRemainder[sampleCt,numbSamplesToPrepPerWell][[2]]
]

(* ::Input:: *)
(*these are the prep wells*)

(* ::Code:: *)
prepWells={#,"protein plate"}&/@complmentMoteOrdered[[1;;numbPrepWells]]

(* ::Output:: *)
{{"B2","protein plate"}}

(* ::Input:: *)
(*these are the experimental wells*)

(* ::Code:: *)
distributionWellsTmp = {#,"protein plate"}&/@complmentMoteOrdered[[numbPrepWells+1;;numbPrepWells+Length[inputSamples]]]

(* ::Output:: *)
{{"B3","protein plate"},{"B4","protein plate"},{"B5","protein plate"},{"B6","protein plate"},{"B7","protein plate"},{"B8","protein plate"}}

(* ::Code:: *)
distributionWellsOld = {#,"protein plate old"}&/@complmentMoteOrdered[[numbPrepWells+1;;numbPrepWells+5]]

(* ::Output:: *)
{{"B3","protein plate old"},{"B4","protein plate old"},{"B5","protein plate old"},{"B6","protein plate old"},{"B7","protein plate old"}}

(* ::Code:: *)
adjustmentSampleWell = distributionWellsTmp[[adjustmentSample]]

(* ::Output:: *)
{"B3","protein plate"}

(* ::Input:: *)
(*distribution wells is a list of list because we need to trakc which of the prep wells goes where*)

(* ::Code:: *)
distributionWells = Partition[distributionWellsTmp,numbSamplesToPrepPerWell,numbSamplesToPrepPerWell,1 ,{}]

(* ::Output:: *)
{{{"B3","protein plate"},{"B4","protein plate"},{"B5","protein plate"},{"B6","protein plate"},{"B7","protein plate"},{"B8","protein plate"}}}

(* ::Code:: *)
distributionWellsflattened = Flatten[distributionWells,1](*this is flattened because we need a list for mixing*)

(* ::Output:: *)
{{"B3","protein plate"},{"B4","protein plate"},{"B5","protein plate"},{"B6","protein plate"},{"B7","protein plate"},{"B8","protein plate"}}

(* ::Code:: *)
volumeToDistributeFromPrepWells =  solutionAPerWell+solutionBPerWell+rnasinPerWell(*+nucleaseFreeWaterPerWell*)

(* ::Output:: *)


(* ::Code:: *)
(*The following sets up the distribution amounts to distribute, it would be a longer list of different volumes with more input samples*)
solutionAVolumes =pureExpressDistribute[solutionAPerWell,Length[inputSamples]]

(* ::Output:: *)
{}

(* ::Code:: *)
solutionBVolumes =pureExpressDistribute[solutionBPerWell,Length[inputSamples]]

(* ::Output:: *)
{}

(* ::Code:: *)
rnasinVolumes=pureExpressDistribute[rnasinPerWell,Length[inputSamples]]

(* ::Output:: *)
{}

(* ::Code:: *)
nucleaseFreeWaterVolumes = pureExpressDistribute[nucleaseFreeWaterPerWell,Length[inputSamples]]

(* ::Output:: *)
{}

(* ::Input:: *)
(*Double check print out of total volume, it should be number of input samples * 30ul*)

(* ::Code:: *)
solutionAVolumes+solutionBVolumes+rnasinVolumes(*+nucleaseFreeWaterVolumes*)+ dnaPerWell*Length[inputSamples]

(* ::Output:: *)
{}

(* ::Subsection:: *)
3c) Run PureExpress

(* ::Code:: *)
setupPureExpressExperiment=Flatten[
{
	RoboticSamplePreparation[
		LabelContainer[
			Label->"protein plate",
			Container-> Model[Container, Plate, "96-well PCR Plate"]
		],
(*		LabelContainer[
			Label -> "protein plate old",
			Container -> Object[Container,Plate,"id:8qZ1VW0pNrBp"]
		],*)
		(*Setup Mote wells*)
		Transfer[
			Source ->  Model[Sample,"Milli-Q water"],
			Destination -> moteWellsInPlate,
			Amount -> 175 Microliter
		],
		(*cool off protein plate*)
		Incubate[
			Sample -> "protein plate",
			Time -> 5Minute,
			Temperature -> 4 Celsius,
			ResidualIncubation->True,
			ResidualTemperature->4 Celsius 
		],
		Transfer[
			Source->Model[Sample, "PURExpress Solution A"],
			Destination -> prepWells,
			Amount -> solutionAVolumes,
			Tips -> Model[Item,Tips,"300 uL Hamilton barrier tips, sterile"]
		],
		Transfer[
			Source -> Model[Sample, "PURExpress Solution B"] ,
			Destination -> prepWells,
			Amount -> solutionBVolumes,
			Tips->Model[Item,Tips,"300 uL Hamilton barrier tips, sterile"]
		],
		(*RNasin doesn't seem to be in stock yet*)
		Transfer[
			Source -> Model[Sample, "RNasin Plus RNase Inhibitor"],
			Destination -> prepWells,
			Amount -> rnasinVolumes,
			Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		],
		(*Transfer[
		Source -> Model[Sample, "Nuclease-free Water"],
		Destination -> prepWells,
		Amount -> nucleaseFreeWaterVolumes,
		Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		],*)
		Mix[
			Sample -> prepWells,
			NumberOfMixes -> 8,
			MixVolume -> 10 Microliter,
			MixType -> Pipette,
			Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		],
		(*distribute mixture to wells*)
		Sequence@@MapThread[
			Transfer[
				Source ->#1,
				Destination -> #2,
				Amount -> volumeToDistributeFromPrepWells,
		        Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		    ]&,
		    {prepWells,distributionWells}
		],
		(* Transfer DNA into the wells *)
		Transfer[
			Source->inputSamples,
			Destination->distributionWellsflattened,
			Amount->dnaPerWellAdjusted,
			Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		],
		(*Mix*)
		Mix[
			Sample -> distributionWellsflattened,
			NumberOfMixes -> 4,
			MixVolume -> 5 Microliter,
			MixType -> Pipette,
			Tips->Model[Item,Tips,"50 uL Hamilton barrier tips, sterile"]
		],
		LabelSample[
			Sample->adjustmentSampleWell,
			Label-> "adjustment sample" 
		],
		Cover[
			Sample->"protein plate"
		],
		Incubate[
			Sample ->  "protein plate",
			Time -> 240 Minute,
			Temperature -> 36 Celsius(*,
			SamplesInStorageCondition -> Freezer*)
		],
		FluorescenceIntensity[
			Sample->distributionWellsTmp,
			SamplesInStorageCondition -> Freezer,
			ExcitationWavelength -> 390 Nanometer,
			EmissionWavelength -> 510 Nanometer,
			AdjustmentSample ->  {"adjustment sample", "adjustment sample"}
		]
	]
}
];

(* ::Code:: *)
pureExpressExperiment = ExperimentSamplePreparationInputs[setupPureExpressExperiment,
MeasureVolume -> False,
MeasureWeight->False,
ImageSample->True]

(* ::Section:: *)
4) Analysis

(* ::Code:: *)
dataObjs = pureExpressExperiment[Data]

(* ::Code:: *)
PlotObject[pureExpressExperiment[Data][Intensities],
	Legend-> inputSamples[Name],  (* provide Labels for your samples if you desire*)
	ChartStyle -> 75,
	PlotLabel -> "390 Nanometer"
]

(* ::Section:: *)
Inventory and discarding unwanted samples

(* ::Subsubsection:: *)
All samples in script

(* ::Code:: *)
scriptStoredObjects = StoredObjectsFromScript[notebookPage,Output->Table]

(* ::Output:: *)
StoredObjectsFromScript[notebookPage,Output->Table]

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