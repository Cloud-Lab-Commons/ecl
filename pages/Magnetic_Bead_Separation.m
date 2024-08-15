(* CreatedFrom Object[EmeraldCloudFile, "id:kEJ9mq8od30E"] on Thu 15 Aug 2024 14:08:38 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Chapter:: *)
Magnetic Bead Separation

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
To purify DNA using AmpureXP beads and ExperimentMagneticBeadSeparation

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
1. Make sure you have the beads and your samples in a hamilton compatible container
2. It would be a good idea to centrifuge down the sample post Magnetic Bead Purification protocol to use them further in another experiment since the liquid could be stuck to the sides of the well. 

(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup Code

(* ::Code:: *)
inputSamples = {} (*Provide your samples here*)

(* ::Code:: *)
aliquotedBeads = {} (* Provide the bead you would want to use for this purpose *)

(* ::Section:: *)
Running Code

(* ::Code:: *)
beadPurificationProtocol = ExperimentMagneticBeadSeparation[
	inputSamples, 
	PreparatoryUnitOperations -> {
		Mix[
			Sample -> aliquotedBeads,
			MixType -> Invert,
			NumberOfMixes -> 10
		]
	},
	Volume -> 30 Microliter,
	MagneticBeads -> aliquotedBeads,
	MagneticBeadVolume -> 54 Microliter,
	Preparation -> Robotic,
	PreWash -> False,
	
	(* Adding sample and mix followed by mixing *)
	LoadingMix -> True,
	LoadingMixType -> Pipette,
	LoadingMixVolume -> 40 Microliter,
	NumberOfLoadingMixes -> 10,
	LoadingMagnetizationTime -> 4 Minute,
	LoadingAspirationVolume -> All,
	LoadingAspirationPosition -> Bottom,
	LoadingAspirationPositionOffset -> 2 Millimeter,
	LoadingCollectionContainer -> Model[Container,Plate,"96-well 2mL Deep Well Plate"],
	LoadingCollectionStorageCondition -> Refrigerator,
	
	(*Washing*)
	Wash -> True,
	WashBuffer -> Model[Sample, StockSolution, "70% Ethanol"],
	WashBufferVolume -> 200 Microliter,
	WashMix -> True,
	WashMixType -> Pipette,
	WashMixVolume -> 100 Microliter,
	NumberOfWashMixes -> 5,
	WashMagnetizationTime -> 2 Minute,
	WashCollectionContainer -> Model[Container,Plate,"96-well 2mL Deep Well Plate"],
	WashCollectionStorageCondition -> Refrigerator,
	WashAspirationPosition -> Bottom,
	WashAspirationPositionOffset -> 0.5 Millimeter,
	NumberOfWashes -> 2,
	WashAirDry -> True,
	WashAirDryTime -> 4 Minute,
	
	(*Elution*)
	ElutionBufferVolume -> 50 Microliter,
	ElutionBuffer -> Model[Sample,"Nuclease-free Water"],
	ElutionMixType -> Pipette,
	ElutionMixVolume -> 25 Microliter,
	NumberOfElutionMixes -> 10,
	ElutionMagnetizationTime -> 2 Minute,
	ElutionAspirationVolume -> All,
	ElutionAspirationPosition -> Bottom,
	ElutionAspirationPositionOffset -> 0.5 Millimeter,
	ElutionCollectionContainer -> Model[Container,Plate,"96-well PCR Plate"],
	ElutionCollectionStorageCondition -> Refrigerator,
	
	(*Post Completion Protocols*)
	MeasureWeight -> False,
	MeasureVolume -> False
]

(* ::Section:: *)
Inventory and discarding unwanted samples

(* ::Subsubsection:: *)
Sample discarding

(* ::Text:: *)
Set the samples to keep throughout the script

(* ::Code:: *)
(*containersToKeep = {} *)

(* ::Code:: *)
containersToDiscard = {}

(* ::Code:: *)
PauseScript[]

(* ::Code:: *)
DiscardSamples[containersToDiscard]

(* ::Section:: *)
Conclusions and Future Directions