(* CreatedFrom Object[EmeraldCloudFile, "id:kEJ9mq8oXWPp"] on Thu 15 Aug 2024 13:01:55 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Chapter:: *)
Golden Gate Assembly

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
To perform Golden Gate Assembly on two Gblock DNA followed by PCR amplification and size verification using Gel Electrophoresis. 

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
1. Make sure to have the samples in a Hamilton Compatible container.
2. Also make sure you have enough volumes (atleast 10 Microliter) in the input samples to use.
3. Remember to modify the incubation protocol depending on the number of constructs that you are trying to assemble.
4. Do not forget the check the volumes of reaction components and make sure there is enough in Hamilton Compatible Containers
5. If carrying out multiple Golden Gate Reactions you would have to modify the code for additional wells in the reaction plate and modify ggReactionWells and gBlockVolumes.
6. Make sure to provide your own primers and modify the PCR parameters for your use case.
7. Choose the Loading Dye of different size from the size of the DNA you are trying to analyze.

(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup Code

(* ::Subsubsection:: *)
Materials needed

(* ::Item:: *)
Golden Gate

(* ::Subitem:: *)
Gene Fragments

(* ::Subitem:: *)
NEB T4 DNA Ligase  

(* ::Subitem:: *)
NEB T4 DNA Ligase Buffer 

(* ::Subitem:: *)
NEB BsaI 

(* ::Subitem:: *)
Nuclease-Free water

(* ::Subsubsection:: *)
Set up input samples

(* ::Code:: *)
gBlocks = {Object[Sample,"id:dORYzZR6o1qA"], Object[Sample,"id:Z1lqpMlJen1V"]} 
(* Provide a list of samples you want to perform PCR on *)

(* ::Code:: *)
gBlockVolumes = {{4 Microliter, 4 Microliter}}

(* ::Text:: *)
To Set up Golden Gate take the muGFP and T7 gene fragments and set up a 30 uL Golden Gate Reaction.

Golden Gate Components 
Gene Fragments  -- gBlocks
NEB T4 DNA Ligase -- Object[Sample,\[CloseCurlyDoubleQuote]id:D8KAEvK49VL3\[CloseCurlyDoubleQuote]]
NEB T4 DNA Ligase Buffer -- Object[Sample,\[CloseCurlyDoubleQuote]id:zGj91a73lw76\[CloseCurlyDoubleQuote]]
NEB BsaI -- Object[Sample,\[CloseCurlyDoubleQuote]id:aXRlGnRPvD4j\[CloseCurlyDoubleQuote]]
Nuclease Free Water  -- Model[Sample, \[OpenCurlyDoubleQuote]Nuclease-free Water\[CloseCurlyDoubleQuote]]

(* ::Code:: *)
T4LigaseBuffer = Object[Sample,"id:zGj91a73lw76"];
T4Ligase = Object[Sample,"id:D8KAEvK49VL3"];
BsaI = Object[Sample,"id:aXRlGnRPvD4j"];
nucFreeWater = Model[Sample, "Nuclease-free Water"];

(* ::Code:: *)
T4LigaseVolume = 3 Microliter;
T4LigaseBufferVolume = 3 Microliter;
BSAIVolume = 3 Microliter;

(* ::Code:: *)
nucFreeWaterVolume =(30 Microliter - 
(Total[#]+ T4LigaseVolume + T4LigaseBufferVolume+BSAIVolume))& /@ gBlockVolumes

(* ::Code:: *)
reactionMix=MapThread[ Function[{u, v},
	{{"Fragment1",u[[1]]},
	{"Fragment2",u[[2]]},
	{"NEB T4 DNA Ligase Buffer",T4LigaseBufferVolume},
	{"NEB T4 DNA Ligase",T4LigaseVolume},
	{"BsaI",BSAIVolume},
	{"Nuclease Free Water",v}}] ,{gBlockVolumes, nucFreeWaterVolume}]

(* ::Code:: *)
PlotTable[reactionMix, TableHeadings -> {Automatic, {"Component", "Volume"}}]

(* ::Code:: *)
reactionMixVolumes = Transpose[reactionMix[[All, #, 2]]& /@ Range[6]]

(* ::Code:: *)
reactionComponents = Join[gBlocks, {T4LigaseBuffer, T4Ligase, BsaI, nucFreeWater}]

(* ::Code:: *)
ggReactionWells = {{"B2", "GG Mix Plate"}} (*Add more wells if you want to conduct more than one reaction*)

(* ::Subsubsection:: *)
Check Volumes

(* ::Code:: *)
UnitScale[gBlocks[Volume]]

(* ::Code:: *)
UnitScale[Download[{T4LigaseBuffer, T4Ligase, BsaI}, Volume]]

(* ::Section:: *)
Running Code

(* ::Subsection:: *)
Mix Components on Hamilton for Golden Gate Reaction

(* ::Code:: *)
ggPrepProtocol = ExperimentRoboticSamplePreparation[
{
		LabelContainer[
			Label -> "GG Mix Plate",
			Container -> Model[Container, Plate, "96-well PCR Plate"]
		],
		Transfer[
			Source -> reactionComponents,
			Destination -> ggReactionWells,
			Amount -> reactionMixVolumes[[1]],
			DestinationLabel -> "GG Reaction 1",
			SamplesOutStorageCondition -> Refrigerator
		],
		Mix[
			Sample -> "GG Reaction 1",
			MixType -> Pipette,
			NumberOfMixes -> 10,
			MixVolume -> 15 Microliter
		]
},
MeasureVolume -> False,
MeasureWeight -> False
]

(* ::Text:: *)
Get the Plate from the preparation Protocol

(* ::Subsection:: *)
Incubate on the Thermocycler

(* ::Code:: *)
ggReactionPlate = ggPrepProtocol[ContainersOut]

(* ::Code:: *)
ggReactionSample = ggPrepProtocol[SamplesOut]

(* ::Code:: *)
incubationProtocol = ExperimentManualSamplePreparation[
{
	Incubate[
		Sample -> ggReactionPlate,
		Instrument -> Model[Instrument,Thermocycler,"Automated Thermal Cycler"],
		TemperatureProfile -> {
			{0 Minute, 37 Celsius}, 
			{60 Minute, 37 Celsius},
			{61 Minute, 60 Celsius},
			{65 Minute, 60 Celsius}
		},
		Mix -> False
	],
	Wait[
		Duration -> 30 Minute
	],
	Centrifuge[
		Sample -> ggReactionPlate,
		Intensity -> 2000 RPM,
		Time -> 2 Minute
	]
},
MeasureVolume -> False,
MeasureWeight -> False
]

(* ::Subsection:: *)
PCR after Golden Gate Assembly

(* ::Text:: *)
Use the Golden Gate Reaction to perform PCR

(* ::Code:: *)
ggReactionSamples = ggPrepProtocol[SamplesOut][Object]

(* ::Code:: *)
samplesInput = ggReactionSamples

(* ::Code:: *)
primers = {Object[Sample,"id:Y0lXejlkDlYm"],Object[Sample,"id:zGj91ajzYjaJ"]} 
(*Provide your own Primers*)

(* ::Code:: *)
ggReactionWells = {"B2", "GG Mix Plate"}

(* ::Code:: *)
ggReactionPlate = First[ggPrepProtocol[ContainersOut][Object]]

(* ::Code:: *)
pcrPrepProtocol = ExperimentRoboticSamplePreparation[
{
		LabelContainer[
			Label -> "GG Mix Plate",
			Container -> ggReactionPlate
		],
		(* Put in the DreamTaq PCR Master Mix 2X*)
		Transfer[
			Source -> Model[Sample,"DreamTaq PCR Master Mix"],
			Destination -> ggReactionWells,
			Amount -> 40 Microliter
		],
		(* Transfer forward primer into the reaction*)
		Transfer[
			Source -> primers[[1]],
			Destination -> ggReactionWells,
			Amount -> 4 Microliter
		],
		(* Transfer reverse primer into the reaction*)
		Transfer[
			Source -> primers[[2]],
			Destination -> ggReactionWells,
			Amount -> 4 Microliter
		]
},
MeasureWeight -> False,
ImageSample -> False,
MeasureVolume -> False
]

(* ::Code:: *)
pcrProtocol = ExperimentManualSamplePreparation[
{
		LabelContainer[
			Label -> "GG Mix Plate",
			Container -> ggReactionPlate
		],
		Centrifuge[
			Sample -> "GG Mix Plate",
			Intensity -> 1000 RPM,
			Time -> 2 Minute
		],
		PCR[
			Sample -> "GG Mix Plate",
			SampleVolume -> 78 Microliter,
			ReactionVolume -> 78 Microliter,
			MasterMix -> Null,
			Buffer -> Null,
			(* Activation *)
			Activation-> True,
			ActivationTemperature -> 95 Celsius,
			ActivationTime -> 3 Minute,
			(* Denaturation *)
			DenaturationTemperature -> 95 Celsius,
			DenaturationTime -> 30 Second,
			(* Annealing *)
			PrimerAnnealing -> True,
			PrimerAnnealingTime -> 30 Second,
			PrimerAnnealingTemperature -> 56 Celsius,
			(* Extention *)
			ExtensionTime ->  75 Second,
			ExtensionTemperature -> 72 Celsius,
			(* Number of Cycles *)
			NumberOfCycles -> 30,
			(* Final Extension *)
			FinalExtensionTime -> 10 Minute,
			FinalExtensionTemperature -> 72 Celsius,
			(*Post Experiment*)
			MeasureVolume -> False,
			MeasureWeight -> False
		],
		Wait[
			Duration -> 30 Minute
		],
		Centrifuge[
			Sample -> "GG Mix Plate",
			Intensity -> 2000 RPM,
			Time -> 3 Minute
		]
},
MeasureWeight -> False,
ImageSample -> False,
MeasureVolume -> False,
SamplesOutStorageCondition -> Refrigerator
]

(* ::Section:: *)
Gel Electrophoresis

(* ::Code:: *)
gelInputSamples = pcrProtocol[SamplesOut]

(* ::Code:: *)
gelInputSamplesContainer = gelInputSamples[Container]

(* ::Code:: *)
gelElectophoresisProtocol=
ExperimentAgaroseGelElectrophoresis[
	gelInputSamples,
	Scale -> Analytical,
	Gel->Model[Item,Gel,"Analytical 1.0% agarose cassette, 24 channel"],
	SampleVolume-> 4 Microliter,
	Centrifuge->False,
	LoadingDye->{Model[Sample,"300 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"2000 bp dyed loading buffer for agarose gel electrophoresis"]},
	LoadingDyeVolume -> 3 Microliter,
	LoadingDilutionBufferVolume -> 10 Microliter,
	MeasureWeight->False,
	MeasureVolume->False,
	ImageSample->False,
	SamplesInStorageCondition -> Refrigerator
]

(* ::Subsection:: *)
Visualize the Data

(* ::Input:: *)
PlotObject[gelElectophoresisProtocol[Data]]

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