(* CreatedFrom Object[EmeraldCloudFile, "id:8qZ1VWZGZJGx"] on Wed 8 May 2024 11:39:58 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Section:: *)
Setting up IDT reagents for PCR in ECL v 1.0

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Dana Cortade
E.mail: dana@alignbio.org
Requested citation: https://doi.org/10.5281/zenodo.13375207

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
This code is meant to enable you to quickly go from an IDT order number to automatically creating identity, fulfilment and sample objects for your oligos and template for a qPCR when purchased in tubes.

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
Constants

(* ::Text:: *)
BEFORE YOU USE THIS CODE:
1) You must run the \[OpenCurlyQuote]Buffers for DNA and RNA\[CloseCurlyQuote] Github code, specifically the IDTE Buffer code block
2) You must have an IDT order number.
3) The order at which reagents appear and their names should be identical between your IDT order and your .csv file. 

Experiment structure:
This code first creates an identity model and fulfilment model for each reagent. It then connects the fulfilment models to the received goods shipped from IDT to ECL via your IDT order number, generating sample object IDs which can be used in ECL experiments.

(* ::Subsection:: *)
Functions

(* ::Text:: *)
Optional: You can validate that your oligomers will upload before running the code by running the code block in the Analysis section before running the code block in the Running Code section.

(* ::Subsection:: *)
Inputs 

(* ::Text:: *)
Here is a link to the pre-made google sheets template: https://docs.google.com/spreadsheets/d/1cDc1s1Ox1_W98IibB9UIE_P09UQFGtpKjrDSp-RRtks/edit?usp=sharing

Remember:
- download as a .csv
- the Name of each reagent must be unique to anything you have ever created in ECL - using this code the final fulfillment model will automatically include your Name, State, Concentration Number and Concentration Unit.
- the Sequences must be all capital letters without spaces. For DNA - they can only contain A, T, G, and C. For RNA they can only contain A, U, G, and C.
 - primers are single stranded DNA


(* ::Text:: *)
Entering your inputs using a .csv file. Using the code block below, enter your IDT order number and .csv file name. To make sure your reagents are matched with the correct physical items, make sure the IDT order matches the .csv in both order and name. Once you have filled in the code block, run it. 

(* ::Code:: *)

IDTorderNumber= "IDTorderNumber";
IDTorderInfo = Import["C:\\ProgramData\\Microsoft\\Windows\\Start Menu\\Programs\\ECL Command Center\\Data\\Inputs to ECL template.csv"]

(* ::Output:: *)
{{"Name","Sequence","State","Concentration Number","Concentration Units","Number Of Strands","Polymer Type"},{"Dana's next test34","ATTCTATAT","Lyophilized",100,"MassPercent","Single","RNA"},{"Dana's next test35","GGGGCGGG","in Nuclease Free Water",200,"Millimolar","Double","DNA"},{"Dana's next test36","AGCGAGTAGACAG","Lyophilized",100,"MassPercent","Double","DNA"},{"Dana's next test37","CGGTGGCG","in IDTE Buffer",20,"Microgram / Microliter","Single","RNA"}}

(* ::Subsection:: *)
Variables

(* ::Text:: *)
(Inputs that have a usual default value but are sometimes changed)


(* ::Section:: *)
Running Code

(* ::Subsection:: *)
Creating identity models

(* ::Code:: *)
(*Removes the header and associates the header nemes with each column's data*)
IDTHeads = First[IDTorderInfo];
IDTData = Rest[IDTorderInfo];
IDTInfoAssociations = AssociationThread[IDTHeads, #]& /@ IDTData;

(*Determining if each molecule is DNA or RNA*)
myPolymerType = If[Lookup[#,"Polymer Type"]=="DNA",
	DNA,
	RNA
]& /@ IDTInfoAssociations;

 (*Creating each molecule depedning on if it is double stranded or single stranded*)
myMolecule = Which[
	Lookup[#,"Number Of Strands"]=="Double" && myPolymerType==DNA,
	Structure[{Strand[DNA[Lookup[#,"Sequence"]]], Strand[DNA[ReverseComplementSequence[Lookup[#,"Sequence"]]]]},{Bond[{1,1},{2,1}]}],
	Lookup[#,"Number Of Strands"]=="Single" && myPolymerType==DNA,
	Strand[DNA[Lookup[#,"Sequence"]]],
	Lookup[#,"Number Of Strands"]=="Double" && myPolymerType==RNA,
	Structure[{Strand[RNA[Lookup[#,"Sequence"]]], Strand[RNA[ReverseComplementSequence[Lookup[#,"Sequence"]]]]},{Bond[{1,1},{2,1}]}],
	Lookup[#,"Number Of Strands"]=="Single" && myPolymerType==RNA,
	Strand[RNA[Lookup[#,"Sequence"]]]
]& /@ IDTInfoAssociations;

(* Creating an identity model for each temple*)
IDTorderIDModels = UploadOligomer[
	IDTData[[All, 1]],
	Molecule -> myMolecule,
	PolymerType -> myPolymerType
	];

ListedIDTorderIDModels= ToList[IDTorderIDModels]

(* ::Subsection:: *)
Creating fulfilment models

(* ::Code:: *)
(* Creating fulfillment model names*)
IDTorderFModelNames= MapThread[
#1 <> " " <> #2 <> " at " <> ToString[#3] <> " " <>#4&,
{IDTData[[All, 1]], IDTData[[All, 3]], IDTData[[All, 4]], IDTData[[All, 5]]}
];

(*settign compositions*)
ConcentrationNumber = IDTData[[All, 4]];
(* Note: 'Percent' is handled in an unintuitive way by Mathematica, so ECl's VolumePercent is different from the
Mathematica version of VolumePercent. We'll go through and replace those units below to ensure that things work correctly. *)
ConcentrationUnits = ReplaceAll[
	Quantity/@IDTData[[All, 5]], 
	{
		"VolumePercent"->IndependentUnit["VolumePercent"],
		"MassPercent"->IndependentUnit["MassPercent"]
	}
];
IDTFModelCompositions= {#}&/@Transpose[{ConcentrationNumber * ConcentrationUnits, IDTorderIDModels}];

(*setting solvents*)
IDTSolvent = Which[
#[[3]] == "in IDTE Buffer pH 8.0" , Model[Sample, "IDTE Buffer pH 8.0"], 
#[[3]] == "in IDTE Buffer pH 7.5" , Model[Sample, "IDTE Buffer pH 7.5"], 
#[[3]] == "in Nuclease Free Water", Model[Sample, "Nuclease-free Water"] ,
True, Null
]& /@ IDTData;

(*setting state*)
IDTState = Which[
#[[3]] == "in IDTE Buffer" , Liquid, 
#[[3]] ==  "in Nuclease Free Water", Liquid,
True, Solid
]& /@ IDTData;

(*Creates the final fulfillment model for each oligo and sets storage conditions*)
UploadSampleModel[
	IDTorderFModelNames,
	Composition-> IDTFModelCompositions,
	State-> IDTState,
	DefaultStorageCondition->Model[StorageCondition,"Freezer"],
	Expires->True,
	ShelfLife->2 Year,
	UnsealedShelfLife->2 Year,
	MSDSRequired -> False, 
	Solvent ->  IDTSolvent
]

(* ::Subsection:: *)
Connecting fulfilment models to incoming orders from IDT

(* ::Code:: *)
(*This connects each reagent in your IDT order to its corresponding fulfillment models, 
which in turn create samples object IDs allowing you to use the reagents in experiments at ECL. 
It is very important that your .csv file macthes your IDT order in both:
- the order at which regaents appear
- and their individual names
After using this code your samples can be searched by their names*)
OrderObjectFromIDT = DropShipSamples[
IDTorderFModelNames,
orderNumberIDT,
Provider->Object[Company,Service,"Integrated DNA Technologies"],
Mass->ProductDocumentation
]


(* ::Subsection:: *)
Naming samples

(* ::Code:: *)
(*Naming all incoming sample objects based on the columns in the .csv. Each name will follow the form:
Name State at ConcentrationNumber ConcentrationUnits Original Tube YYYY-MM-DD
Example: GAPDH F Primer in IDTE Buffer at 100�g/�L Original Tube 2023-09-05 *)
IDTorderSampleNames= IDTorderFModelNames <> " Original Tube " <> DateString["ISODate"];
samplesFromIDT = OrderObjectFromIDT[SamplesOut];
UploadName[
samplesFromIDT,
IDTorderSampleNames]

(* ::Section:: *)
Analysis Code

(* ::Subsection:: *)
Validate that your oligomers will upload before running the code:

(* ::Code:: *)
(* Runs a test to see if the oligomer will successfully upload*)	
ValidTest = ValidUploadOligomerQ[
	IDTtemplateInfo[1,1],
	Molecule -> (Structure[{Strand[DNA[#]], Strand[DNA[ReverseComplementSequence[#]]]}, {Bond[{1,1},{2,1}]}]& /@ IDTtemplateInfo[[All, 2]]),
	PolymerType -> DNA]

(* ::Subsection:: *)
Troubleshooting Info

(* ::Subsection:: *)
Validation/Control/Sufficiency/Robustness checks

(* ::Text:: *)
- Before starting, verify IDT order matched the requirements and formatting in the Constants and Inputs sections.

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

(* ::Output:: *)
{Object[Container,Plate,"id:6V0npv0mXV7w"]}

(* ::Input:: *)
(*These are recommended samples to discard*)

(* ::Input:: *)
PauseScript[]

(* ::Input:: *)
DiscardSamples[containersToDiscard]

(* ::Output:: *)
{Object[Container,Plate,"id:6V0npv0mXV7w"],Object[Sample,"id:dORYzZR38xq5"],Object[Sample,"id:eGakldavbERo"]}

(* ::Section:: *)
Conclusions and Future Directions