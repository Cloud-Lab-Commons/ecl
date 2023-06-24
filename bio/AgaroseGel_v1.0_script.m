(* ::Package:: *)

(* ::Section:: *)
Agarose gel v 1.0


(* ::Subsection:: *)
License


(* ::Text:: *)

This software is being released under the Apace 2.0 license
https://www.apache.org/licenses/LICENSE-2.0

When publishing please cite the following:

Author(s): TJ Brunette
E.mail: tjbrunette@gmail.com
Requested citation: None


(* ::Section:: *)
 Overview


(* ::Item:: *)
WARNING until ECL comes online this has not been fully tested


(* ::Subsection:: *)
Purpose and Scope


(* ::Text:: *)
Agarose gels can be finicky at low volumes. This provides an advised set of volumes that run robustly 


(* ::Subsection:: *)
List of Updates from Previous Version 


(* ::Text:: *)
v1.0 volumes increased for improved gel loading


(* ::Subsection:: *)
List of known bugs


(* ::Text:: *)
v 1.0 the ladders seem to not always run


(* ::Section:: *)
Web link


(* ::Input:: *)
Publish[$Notebook]


(* ::Section:: *)
Setup Code


(* ::Subsection:: *)
Constants


(* ::Text:: *)
Values that never change from run-to-run


(* ::Subsection:: *)
Functions


(* ::Text:: *)
Internal helper functions used only in this template

Requires
scripts/universal_scripts.m 


(* ::Section:: *)
Inputs


(* ::Text:: *)
(First, list inputs that will likely be different every time you run)


(* ::Text:: *)
For agarose the normal input would be a list of samples. This example code preps water that runs within the gel code via PreparatoryUnitOperations.


(* ::Code:: *)
setupBlankAgarose={RoboticSamplePreparation[
LabelContainer[
	Label->"agarose plate",
	Container->Model[Container,Plate,"96-well 2mL Deep Well Plate"]],
Transfer[
	Source->Model[Sample,"Nuclease-free Water"],
	Destination->"agarose plate",
	DestinationWell->Flatten[Transpose[AllWells[]]][[1;;10]],
	Amount->10 Microliter,
	SamplesInStorageCondition->AmbientStorage,
	SamplesOutStorageCondition->AmbientStorage]]};


(* ::Text:: *)
alternate input is a list of samples


(* ::Input:: *)
agaroseInputSamples = Object[Protocol, RoboticSamplePreparation, "id:54n6evnGJOpN"][SamplesOut];


(* ::Subsection:: *)
Variables


(* ::Text:: *)
(Inputs that have a usual default value but are sometimes changed)


(* ::Section:: *)
Running Code


(* ::Code:: *)
ExperimentAgaroseGelElectrophoresis[
 "agarose plate", Scale -> Analytical,
 LoadingDyeVolume -> 2 Microliter,
 SampleVolume -> 2.5 Microliter,
 AgarosePercentage -> 1.5 Percent, 
 LoadingDilutionBufferVolume -> 8 Microliter, 
 SampleLoadingVolume -> 8 Microliter, 
 PreparatoryUnitOperations -> setupBlankAgarose, 
 LoadingDye -> {Model[Sample, "100 bp dyed loading buffer for agarose gel electrophoresis"],
  Model[Sample, "1000 bp dyed loading buffer for agarose gel electrophoresis"]},
LoadingDyeVolume->4Microliter,
 Centrifuge -> True,
 CentrifugeTime -> 30 Second,
 MeasureWeight -> False,
 MeasureVolume -> False,
 ImageSample -> False]


(* ::Code:: *)
(*alternate code with samples as input,
note that SamplesInStorageCondition\[Rule]Disposal is not compatible with the PrepUnitOperations above*)
 (*ExperimentAgaroseGelElectrophoresis[
	agaroseInputSamples,
	Scale -> Analytical,
	LoadingDyeVolume\[Rule]2 Microliter,
	SampleVolume -> 2.5 Microliter,
       AgarosePercentage -> 1.5 Percent, 
	LoadingDilutionBufferVolume -> 8 Microliter,
	SampleLoadingVolume -> 8 Microliter,
	LoadingDye -> {Model[Sample, "100 bp dyed loading buffer for agarose gel electrophoresis"],
  Model[Sample, "1000 bp dyed loading buffer for agarose gel electrophoresis"]},
	LoadingDyeVolume\[Rule]4Microliter,
 Centrifuge -> True,
 CentrifugeTime -> 30 Second,
 MeasureWeight -> False,
 MeasureVolume -> False,
 ImageSample -> False,
SamplesInStorageCondition\[Rule]Disposal]*)



(* ::Section:: *)
Analysis Code


(* ::Subsection:: *)
Results


(* ::Text:: *)
Figure captions should include conclusions about what the figure is telling you/what argument the data is making


(* ::Input:: *)
(*Normally this code would come from what was executed in this notebook but with ECL down I have skipped that*)


(* ::Input:: *)
data1 = Object[Protocol, AgaroseGelElectrophoresis, "id:jLq9jXqrAVZR"][Data][[1]];


(* ::Input:: *)
data1[BlueFluorescenceGelImages][[4]]


(* ::Output:: *)



(* ::Input:: *)
data1[RedFluorescenceGelImages][[4]]


(* ::Output:: *)



(* ::Text:: *)
Animation of gel running not shown due to memory


(* ::Input:: *)
(*PlotGelAnimation[data1]*)


(* ::Subsection:: *)
Troubleshooting Info


(* ::Subsubsection:: *)
Gantt Chart


(* ::Text:: *)
The gantt chart gives an understanding on how long it took a series of experiments to run on a script page. Not this does not currently work on a page run experiment.


(* ::Text:: *)
Normally the notebookpage would be defined as $NotebookPage as opposed to preset notebookPage but with ECL down this page references a previous run page to test the Gantt chart code


(* ::Text:: *)



(* ::Input:: *)
notebookPage = Object[Notebook, Script, "id:wqW9BPWJ0DeM"];


(* ::Input:: *)
(*notebookPage = $NotebookPage*)


(* ::Input:: *)
statusLogGanttChart[Join[notebookPage[Protocols][Object], {}]]


(* ::Output:: *)



(* ::Subsection:: *)
Validation/Control/Sufficiency/Robustness checks


(* ::Text:: *)
Where advice should go to improve the robustness of the method


(* ::Section:: *)
Inventory and discarding unwanted samples


(* ::Subsubsection:: *)
All samples in script


(* ::Text:: *)
Again, this points to an alternate page that was run as a script


(* ::Input:: *)
notebookPage =  Object[Notebook, Script, "id:wqW9BPWJ0DeM"];


(* ::Input:: *)
(*notebookPage = $NotebookPage*)


(* ::Input:: *)
StoredObjectsFromScript[notebookPage, Output -> Table]


(* ::Output:: *)
\!\(
PaneBox["",
ImageSize->{
UpTo[
Scaled[1]]},
ScrollPosition->{0., 0.},
Scrollbars->{True, False}]\)


(* ::Subsubsection:: *)
Sample discarding


(* ::Text:: *)
Set the samples to keep throughout the script


(* ::Input:: *)
containersToKeep = {}


(* ::Output:: *)
{}


(* ::Input:: *)
containersToDiscard = Complement[scriptStoredObjects, containersToKeep]


(* ::Output:: *)
{Object[Container, Plate, "id:6V0npv0mXV7w"]}


(* ::Input:: *)
(*These are recommended samples to discard*)


(* ::Input:: *)
PauseScript[]


(* ::Input:: *)
DiscardSamples[containersToDiscard]


(* ::Output:: *)
{Object[Container, Plate, "id:6V0npv0mXV7w"], Object[Sample, "id:dORYzZR38xq5"], Object[Sample, "id:eGakldavbERo"]}


(* ::Section:: *)
Conclusions and Future Directions


(* ::Text:: *)
Observed failure cases


