(* CreatedFrom Object[EmeraldCloudFile, "id:KBL5DvLkbqJj"] on Mon 12 Aug 2024 16:23:58 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Chapter:: *)
DNA Gel

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
To perform Agarose Gel Electrophoresis on the given amplified/ assembled DNA to confirm the size. 

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
1. Make sure to have the samples in a Hamilton Compatible container. If the samples are in a PCR plate then it will not be an issue
2. Also make sure you have enough volumes (atleast 4 Microliter) in the input samples to use
3. Maximum number of samples that can be run on 1 Gel is 24. If you are running more than 24 samples, it will run on two gels. 
4. Choose the Loading Dye of different size from the size of the DNA you are trying to analyze. Following are the list of Loading Dyes available at ECL

(* ::Code:: *)
Search[Model[Sample], 
StringContainsQ[Name, "dyed loading buffer for agarose gel electrophoresis"]]

(* ::Output:: *)
{Model[Sample,"25 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"50 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"75 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"100 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"200 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"300 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"500 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"1000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"1500 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"2000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"3000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"5000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"7000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"10000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"20000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"100 bp dyed loading buffer for agarose gel electrophoresis from kit"]}

(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup Code

(* ::Subsubsection:: *)
Set up input samples

(* ::Code:: *)
samples= {} (* Provide a list of samples you want to perform PCR on *)

(* ::Code:: *)
controlGelSample = Object[Sample,"id:1ZA60vA1wbb0"] (*provide your control sample*)

(* ::Code:: *)
inputSamples = Prepend[samples, controlGelSample]

(* ::Subsubsection:: *)
Check Volumes

(* ::Code:: *)
UnitScale[inputSamples[Volume]]

(* ::Section:: *)
Run Agarose Gel Electrophoresis

(* ::Text:: *)
Make sure to use the right Loading Dye you would want to use

(* ::Code:: *)
Search[Model[Sample], StringContainsQ[Name, "dyed loading buffer for agarose gel electrophoresis"]]

(* ::Output:: *)
{Model[Sample,"25 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"50 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"75 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"100 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"200 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"300 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"500 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"1000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"1500 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"2000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"3000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"5000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"7000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"10000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"20000 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"100 bp dyed loading buffer for agarose gel electrophoresis from kit"]}

(* ::Text:: *)
Following are the 4 different Analytical Gels you can choose from. There are other types of Gel available in ECL\[CloseCurlyQuote]s Catalog as well. 

(* ::Code:: *)
Search[Model[Item, Gel], StringContainsQ[Name, "Analytical"]]

(* ::Output:: *)
{Model[Item,Gel,"Analytical 0.5% agarose cassette, 24 channel"],Model[Item,Gel,"Analytical 1.0% agarose cassette, 24 channel"],Model[Item,Gel,"Analytical 1.5% agarose cassette, 24 channel"],Model[Item,Gel,"Analytical 2.0% agarose cassette, 24 channel"],Model[Item,Gel,"Analytical 3.0% agarose cassette, 24 channel"]}

(* ::Code:: *)
gelElectophoresisProtocol = ExperimentAgaroseGelElectrophoresis[
	inputSamples,
	Scale -> Analytical,
	Gel -> Model[Item, Gel, "Analytical 1.0% agarose cassette, 24 channel"],
	SampleVolume -> 4 Microliter,
	(*Centrifuge Steps after sample preparation*)
	Centrifuge -> True,
	CentrifugeTime -> 1 Minute,
	LoadingDye->{Model[Sample,"300 bp dyed loading buffer for agarose gel electrophoresis"],Model[Sample,"2000 bp dyed loading buffer for agarose gel electrophoresis"]},
	LoadingDyeVolume -> 3 Microliter,
	LoadingDilutionBufferVolume -> 10 Microliter,
	MeasureWeight -> False,
	MeasureVolume -> False,
	ImageSample -> False
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