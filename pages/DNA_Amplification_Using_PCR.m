(* CreatedFrom Object[EmeraldCloudFile, "id:3em6ZvmbXklL"] on Wed 24 Jul 2024 16:39:44 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Chapter:: *)
PCR 

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Waseem Vali
E.mail: waseem@alignbio.org
Requested citation: https://doi.org/10.5281/zenodo.13375207

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
To Amplify the double stranded DNA using Polymerase Chain Reaction (PCR)

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
1. Make sure to have the samples in a Hamilton Compatible container 
2. Also make sure you have enough volumes in the input samples to use


(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup Code

(* ::Section:: *)
1. Setup naming / check volume of samples

(* ::Code:: *)
samplesInput= {} (* Provide a list of samples you want to perform PCR on *)

(* ::Code:: *)
sampleVolumes = Table[2 Microliter, Length[samplesInput]]

(* ::Subsubsection:: *)
Check volumes

(* ::Code:: *)
UnitScale[samplesInput[Volume]]

(* ::Code:: *)
primer1=Object[Sample,"id:wqW9BPWe6JwG"] (*provide primers here *) 
primer2=Object[Sample,"id:J8AY5jA9d6o9"]

(* ::Code:: *)
UnitScale[{primer1[Volume],primer2[Volume]}]

(* ::Code:: *)
primersInput = Table[{{primer1, primer2}}, Length[samplesInput]]

(* ::Section:: *)
2. Run PCR

(* ::Input:: *)
(*Buffer volume 17.5ul (Milli-Q qater), Sample volume 2ul, Forward/reverse primers 2.5 ul,  MasterMix 25ul*) 

(* ::Code:: *)
myPCR = ExperimentPCR[
	samplesInput,
	primersInput,
	LidTemperature -> 105 Celsius,
	
	(*Volumes*)
	SampleVolume -> sampleVolumes,
	ReactionVolume -> 50 Microliter,
	MasterMix -> Model[Sample,"DreamTaq PCR Master Mix"],
	ForwardPrimerVolume -> 2.5Microliter,
	ReversePrimerVolume -> 2.5Microliter,
	
	(*Activation*)
	Activation -> True,
	ActivationTime -> 3 Minute,
	ActivationTemperature -> 98 Celsius,
	
	(*Denaturation*)
	DenaturationTime -> 30 Second,
	DenaturationTemperature -> 98 Celsius,
	
	(*Annealing*)
	PrimerAnnealing -> True,
	PrimerAnnealingTime -> 30 Second,
	PrimerAnnealingTemperature -> 55 Celsius,  (* Change the values here*)
	
	(*Extention*)
	ExtensionTime -> 75 Second,  (* Change this to account for the size of your DNA*)
	FinalExtension -> True,
	FinalExtensionTime -> 5 Minute,
	HoldTemperature -> 4 Celsius,
	NumberOfCycles -> 30,
	
	(*Post Experiment*)
	SamplesOutStorageCondition -> Freezer,
	ImageSample -> False,
	MeasureVolume -> False,
	MeasureWeight -> False
]

(* ::Code:: *)
samplesHaveDNA[myPCR[SamplesOut]];

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