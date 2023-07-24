(* ::Package:: *)

(* ::Section:: *)
Agarose gel


(* ::Subsection:: *)
License


(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): TJ Brunette
E.mail: tjbrunette@gmail.com
Requested citation: None


(* ::Section:: *)
 Overview


(* ::Subsection:: *)
Purpose and Scope


(* ::Text:: *)
Agarose gels can be finicky at low volumes. This provides an advised set of volumes that run robustly 


(* ::Subsection:: *)
Type


(* ::Text:: *)
script


(* ::Subsection:: *)
List of Updates from Previous Version 


(* ::Text:: *)
volumes increased for improved gel loading


(* ::Subsection:: *)
List of known bugs


(* ::Text:: *)
The ladders seem to not always run


(* ::Section:: *)
Web link


(* ::Input:: *)
Block[{$Notebook = Null}, Publish[$NotebookPage]]


(* ::Output:: *)
URL["https://www.emeraldcloudlab.com/documentation/publish/notebookPage?id=id:jLq9jXqwNEVa"]


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


(* ::Subsubsection:: *)
Gantt Chart


(* ::Code:: *)
Clear[ganttChart];
Clear[barMaker];
Clear[statusLogGanttChart];
Clear[statusLogBarMaker];
Clear[checkpointProgressBarMaker];
Clear[checkpointStatusComparer];


barMaker[prot:ObjectP[Object[Protocol]],startDate_?DateObjectQ,check:(True|False)]:=Module[
	{allDates,preProtocol,inCart,backlogged,enqueued,processingStart,processingEnd,tstStarts,tstEnds,tstBlocks,preTicketTime,lastTicketEnd,postTicketTime,
	ticketTime,processing,bar},

	allDates = Download[prot,{DateCreated,DateConfirmed,DateEnqueued,DateStarted,TroubleshootingTickets[DateCreated],TroubleshootingTickets[DateResolved],TroubleshootingTickets[BlockedLog],DateCompleted,DateCanceled}];

	preProtocol = Style[DateDifference[startDate,allDates[[1]]],GrayLevel[1,0]];
	inCart = Style[DateDifference@@(allDates[[1;;2]]),Darker[Purple]];
	backlogged = Style[DateDifference@@(allDates[[2;;3]]),Darker[Blue]];
	enqueued = Style[DateDifference@@(allDates[[3;;4]]),ColorFade[{Blue,Green},3][[2]]];

	processingStart = allDates[[4]];
	processingEnd = If[NullQ[allDates[[8]]],If[NullQ[allDates[[9]]],Now,allDates[[9]]],allDates[[8]]];

	tstStarts = allDates[[5]];
	tstEnds= allDates[[6]];
	tstBlocks= allDates[[7]];


	processing = If[MatchQ[tstStarts,{}],
		Style[DateDifference[processingStart,processingEnd],Darker[Green]],

		preTicketTime = Style[DateDifference[processingStart,First[tstStarts]],Darker[Green]];
		lastTicketEnd = If[NullQ[tstEnds],processingEnd,First[Sort[{processingEnd,Last[Sort@tstEnds]}]]];
		postTicketTime = Style[DateDifference[lastTicketEnd,processingEnd],Darker[Green]];

		ticketTime = 
			Table[Module[{ticketStartToBlock,ticketEndDate,ticketBlock},
				ticketStartToBlock = If[i==1,
					Style[DateDifference[First[tstStarts],First[tstBlocks[[i]]][[1]]],Darker[Yellow]],
					If[Last[tstBlocks[[i-1]]][[1]]<tstStarts[[i]],
						{
							Style[DateDifference[Last[tstBlocks[[i-1]]][[1]],tstStarts[[i]]],Darker[Green]],
							Style[DateDifference[tstStarts[[i]],First[tstBlocks[[i]]][[1]]],Darker[Yellow]]
						},
						Style[DateDifference[tstStarts[[i]],First[tstBlocks[[i]]][[1]]],Darker[Yellow]]
					]
				];
				ticketEndDate = If[i==Length[tstStarts],
					lastTicketEnd,
					Min[Last[tstBlocks[[i]]][[1]],tstStarts[[i+1]]]
				];
				ticketBlock = If[tstBlocks[[i]][[1,3]],
					Style[DateDifference[First[tstBlocks[[i]]][[1]],ticketEndDate],Darker[Red]],
					Style[DateDifference[First[tstBlocks[[i]]][[1]],ticketEndDate],Darker[Yellow]]
				];
				Flatten[{ticketStartToBlock,ticketBlock}]
			],{i,Length[tstStarts]}];

		Flatten[{preTicketTime,ticketTime,postTicketTime}]
	];

	bar = Unitless[Flatten[{preProtocol,inCart,backlogged,enqueued,processing}]];

	If[check,
		(Total[Flatten[{inCart,backlogged,enqueued,processing}][[All,1]]])==DateDifference[allDates[[1]],processingEnd],
		bar
	]

];


ganttChart[protocolList:{(ObjectP[Object[Protocol]]|ObjectP[Object[Troubleshooting]])...}]:=Module[
	{protocols,troubleshootings,unabortedProtocols,created,confirmed,enqueued,started,completed,canceled,completionDates,startDate,finshedDate,dateRanges,
	tsCreated,tsResolved,tsDateRanges,sortedObjects,sortedDateRanges,tsStyles,legend},
	
	(* input is a mix of protocols and troubleshootings and we need to handle these differently *)
	(* separate protocols and troubleshootings into separate lists *)
	protocols = Cases[protocolList,ObjectP[Object[Protocol]]];
	troubleshootings = Cases[protocolList,ObjectP[Object[Troubleshooting]]];
	
	(* we'll deal with protocols first *)
	
	(* remove any failed protocols from the list *)
	unabortedProtocols = Select[protocols,!MatchQ[#[Status],Failed]&];
	
	(* pull all the relavent dates from the protocol objects *)
	{created,confirmed,enqueued,started,completed,canceled} = Transpose[Download[unabortedProtocols,{DateCreated,DateConfirmed,DateEnqueued,DateStarted,DateCompleted,DateCanceled}]];
	
	(* sort out completion dates *)
	(* if the protocol was canceled, we'll use that as the completion date *)
	(* if the protocol is not yet finished, we'll use Now as the completion date*)
	completionDates = MapThread[Switch[{#1,#2},
		{Null,Null},Now,
		{Null,_},#2,
		{_,Null},#1
	]&,{completed,canceled}];
	
	
	(* now we deal with the troubleshootings *)
	(* pull the relevant dates from the objects *)
	{tsCreated,tsResolved} = If[MatchQ[troubleshootings,{}],
		{{},{}},
		Transpose[troubleshootings[[{DateCreated,DateResolved}]]/.{Null->Now}]
	];

	startDate = First[Sort[Join[created,tsCreated]]];	
	finshedDate = Last[Sort[Join[completionDates,tsResolved]]];
	
	(* make list of rules with protocol objects and styled date ranges, so we can sort these later by date enqueued when we deal with the troubleshootings too*)
	dateRanges = (#->barMaker[#,startDate,False])&/@unabortedProtocols;
	
	(* make the list of styled date range rules for the troubleshootings as well *)
	tsStyles = {GrayLevel[1,0],Darker[Orange]};
	tsDateRanges = If[MatchQ[troubleshootings,{}],
		{},
		MapThread[#1->{
			Style[DateDifference[startDate,#2],tsStyles[[1]]],
			Style[DateDifference[#2,#3],tsStyles[[2]]]
		}&,{troubleshootings,tsCreated,tsResolved}]
	];

	(* now sort all the protocols and troubleshootings by DateEnqueued/DateCreated *)
	sortedObjects = Sort[Join[Transpose[{unabortedProtocols,enqueued/.{Null->Now}}],Transpose[{troubleshootings,tsCreated}]],(#1[[2]])<(#2[[2]])&][[All,1]];
	
	(* and get the list of date ranges all in the correctly sorted order *)
	sortedDateRanges = sortedObjects/.Join[dateRanges,tsDateRanges];
	
			
	legend = Placed[SwatchLegend[
		{Darker[Purple],Darker[Blue],Darker[ColorFade[{Blue,Green},3][[2]]],Darker[Green],Darker[Orange],Darker[Yellow],Darker[Red]},
		Style[#,Bold,20]&/@{"InCart","Backlogged","Enqueued","Processing","Troubleshooting Report","Troubleshooting Ticket","Blocked Troubleshooting Ticket"},
		LegendLayout->"Row",
		LegendMarkerSize->{{30,30}}
	],Bottom];
	
	Legended[BarChart[
		Reverse[Unitless[sortedDateRanges]],
		ChartLayout->"Stacked",
		BarOrigin->Left,
		ChartBaseStyle->EdgeForm[],
		ChartLabels->{Placed[Reverse[Style[ToString[#],12,Bold]&/@sortedObjects],Axis],None},
		GridLines->{Range[Unitless[Ceiling[DateDifference[startDate,Now]]]],None},
		FrameLabel->{"Protocols","Protocols","Days","Days"},
		ImageSize->1200,
		PlotLabel->("Total Pipeline Time = "<>ToString[Ceiling[DateDifference[startDate,finshedDate]]])
	],legend]
	
];

colorRules = {
		InCart->Darker[Purple],
		Backlogged->Darker[Blue],
		OperatorStart->Darker[ColorFade[{Blue,Green},3][[2]]],
		OperatorProcessing->Darker[Darker[Green]],
		OperatorReady->Darker[Yellow],
		Troubleshooting->Darker[Red],
		AwaitingMaterials->Darker[Orange],
		InstrumentProcessing->Darker[Green]
	};
	
statusLogInfoGenerator[prot:ObjectP[Object[Protocol]],startDate_?DateObjectQ]:=Module[
	{statusLog,doneBool,startTimes,endTimes,dateDifs,statuses,colors,bar},
	
	statusLog = prot[StatusLog][[All,1;;2]];
	
	doneBool = MatchQ[prot[Status],Completed];
	
	startTimes = Prepend[If[doneBool,
		Most[statusLog[[All,1]]],
		statusLog[[All,1]]
	],startDate];
		
	endTimes = If[doneBool,
		statusLog[[All,1]],
		Append[statusLog[[All,1]],First[DeleteCases[Join[prot[[{DateCompleted,DateCanceled}]],{Now}],Null]]]
	];

	dateDifs = MapThread[DateDifference[#1,#2]&,{startTimes,endTimes}];
	
	statuses = If[doneBool,
		Most[statusLog[[All,2]]],
		statusLog[[All,2]]
	];
	
	{dateDifs,statuses}
	
];


statusLogBarMaker[prot:ObjectP[Object[Protocol]],startDate_?DateObjectQ]:=Module[
	{dateDifs,statuses,colors,bar},

	{dateDifs,statuses} = statusLogInfoGenerator[prot,startDate];
	
	colors = Prepend[Switch[#,
		InCart,Darker[Purple],
		Backlogged,Darker[Blue],
		OperatorStart,Darker[ColorFade[{Blue,Green},3][[2]]],
		OperatorProcessing,Darker[Darker[Green]],
		OperatorReady,Darker[Yellow],
		Troubleshooting,Darker[Red],
		AwaitingMaterials,Darker[Orange],
		InstrumentProcessing,Darker[Green]
	]&/@statuses,GrayLevel[1,0]];

	bar = Unitless[MapThread[Style[#1,#2]&,{dateDifs,colors}]]

];

protocolStatusPieChart[prot:ObjectP[Object[Protocol]]]:=Module[
	{startDate,dateDifs,statuses,statusTimes,gatheredTimes,totaledTimes},
	
	startDate = prot[DateCreated];

	{dateDifs,statuses} = statusLogInfoGenerator[prot,startDate];
	
	statusTimes = Transpose[{statuses,Rest[dateDifs]}];
	
	gatheredTimes = Gather[statusTimes,MatchQ[#1[[1]],#2[[1]]]&];
	
	totaledTimes ={#[[1,1]],Total[#[[All,2]]]}&/@gatheredTimes;
	
	PieChart[totaledTimes[[All,2]],ChartLabels->(totaledTimes[[All,1]]),ChartStyle->((totaledTimes[[All,1]])/.colorRules)]

];


statusLogGanttChart[protocolList:{(ObjectP[Object[Protocol]]|ObjectP[Object[Troubleshooting]])...}]:=Module[
	{protocols,troubleshootings,unabortedProtocols,created,enqueued,completed,canceled,completionDates,startDate,finshedDate,dateRanges,
	tsCreated,tsResolved,tsDateRanges,sortedObjects,sortedDateRanges,tsStyles,legend},
	
	(* input is a mix of protocols and troubleshootings and we need to handle these differently *)
	(* separate protocols and troubleshootings into separate lists *)
	protocols = Cases[protocolList,ObjectP[Object[Protocol]]];
	troubleshootings = Cases[protocolList,ObjectP[Object[Troubleshooting]]];
	
	(* we'll deal with protocols first *)
	
	(* remove any failed protocols from the list *)
	unabortedProtocols = Select[protocols,!MatchQ[#[Status],Failed]&];
	
	(* pull all the relavent dates from the protocol objects *)
	{created,enqueued,completed,canceled} = Transpose[Download[unabortedProtocols,{DateCreated,DateEnqueued,DateCompleted,DateCanceled}]];
	
	(* sort out completion dates *)
	(* if the protocol was canceled, we'll use that as the completion date *)
	(* if the protocol is not yet finished, we'll use Now as the completion date*)
	completionDates = MapThread[Switch[{#1,#2},
		{Null,Null},Now,
		{Null,_},#2,
		{_,Null},#1
	]&,{completed,canceled}];
	
	
	(* now we deal with the troubleshootings *)
	(* pull the relevant dates from the objects *)
	{tsCreated,tsResolved} = If[MatchQ[troubleshootings,{}],
		{{},{}},
		Transpose[troubleshootings[[{DateCreated,DateResolved}]]/.{Null->Last[Sort[completionDates]]}]
	];

	startDate = First[Sort[Join[created,tsCreated]]];	
	finshedDate = Last[Sort[Join[completionDates,tsResolved]]];
	
	(* make list of rules with protocol objects and styled date ranges, so we can sort these later by date enqueued when we deal with the troubleshootings too*)
	dateRanges = (#->statusLogBarMaker[#,startDate])&/@unabortedProtocols;
	
	(* make the list of styled date range rules for the troubleshootings as well *)
	tsStyles = {GrayLevel[1,0],Darker[Red]};
	tsDateRanges = If[MatchQ[troubleshootings,{}],
		{},
		MapThread[#1->{
			Style[DateDifference[startDate,#2],tsStyles[[1]]],
			Style[DateDifference[#2,#3],tsStyles[[2]]]
		}&,{troubleshootings,tsCreated,tsResolved}]
	];

	(* now sort all the protocols and troubleshootings by DateEnqueued/DateCreated *)
	sortedObjects = Sort[Join[Transpose[{unabortedProtocols,enqueued/.{Null->Now}}],Transpose[{troubleshootings,tsCreated}]],(#1[[2]])<(#2[[2]])&][[All,1]];
	
	(* and get the list of date ranges all in the correctly sorted order *)
	sortedDateRanges = sortedObjects/.Join[dateRanges,tsDateRanges];
	
			
	legend = Placed[SwatchLegend[
		{Darker[Purple],Darker[Blue],Darker[ColorFade[{Blue,Green},3][[2]]],Darker[Darker[Green]],Darker[Yellow],Darker[Red],Darker[Orange],Darker[Green]},
		Style[#,Bold,20]&/@{"In Cart","Backlogged","Operator Start","Operator Processing","Operator Ready","Troubleshooting","Awaiting Materials","Instrument Processing"},
		LegendLayout->"Row",
		LegendMarkerSize->{{30,30}}
	],Bottom];
	
	Legended[BarChart[
		Reverse[Unitless[sortedDateRanges]],
		ChartLayout->"Stacked",
		BarOrigin->Left,
		ChartBaseStyle->EdgeForm[],
		ChartLabels->{Placed[Reverse[Style[ToString[#],12,Bold]&/@sortedObjects],Axis],None},
		GridLines->{Range[Unitless[Ceiling[DateDifference[startDate,Now]]]],None},
		FrameLabel->{"Protocols","Protocols","Days","Days"},
		ImageSize->1200,
		PlotLabel->("Total Pipeline Time = "<>ToString[Ceiling[DateDifference[startDate,finshedDate]]])
	],legend]
	
];


checkpointProgressBarMaker[prot:ObjectP[Object[Protocol]],startDate_?DateObjectQ]:=Module[
	{checkpointProgress,doneBool,initTime,dateDifs,labels,labeledBars,bar},
	
	checkpointProgress = prot[CheckpointProgress];
	
	doneBool = MatchQ[prot[Status],Completed];
	
	initTime = DateDifference[startDate,checkpointProgress[[1,2]]];

	dateDifs = If[doneBool,
		DateDifference@@@(checkpointProgress[[All,2;;3]]),
		Append[
			DateDifference@@@Most[checkpointProgress[[All,2;;3]]],
			DateDifference[Last[checkpointProgress][[2]],First[DeleteCases[Join[prot[[{DateCompleted,DateCanceled}]],{Now}],Null]]]
		]
	];
	
	labels = checkpointProgress[[All,1]];
	
	labeledBars = MapThread[Labeled[#1,#2,Center]&,{Unitless[dateDifs],labels}];
	
	bar = Prepend[labeledBars,Style[Unitless[initTime,Day],GrayLevel[1,0]]]
	
];


DefineOptions[checkpointStatusComparer,
	Options :> {
		{DisplaySubprotocols -> False, BooleanP, 
		"If True, the StatusLogs of all subprotocols associated with the inputted protocol will be displayed in a gantt chart.  If False, only the StatusLog and the Checkpoints for the protocol will be displayed."}
}];

checkpointStatusComparer[prot:ObjectP[Object[Protocol]],ops:OptionsPattern[checkpointStatusComparer]]:=Module[
	{resolvedOps,displaySubs,startDate,checkpointBar,statusLogBar,subprotocols,subprotocolBars,legend,chartLabels},
	
	resolvedOps = ECL`OptionsHandling`SafeOptions[checkpointStatusComparer,{ops}];
	displaySubs = DisplaySubprotocols/.resolvedOps;
	
	startDate = prot[DateCreated];
	checkpointBar =checkpointProgressBarMaker[prot,startDate];
	statusLogBar= statusLogBarMaker[prot,prot[DateCreated]];
	
	subprotocolBars = If[displaySubs,
		subprotocols = Flatten[Download[prot,Subprotocols..]][Object];
		If[MatchQ[subprotocols,{}],{},checkpointProgressBarMaker[#,prot[DateCreated]]&/@subprotocols],
		subprotocols = {};
		{}
	];
			
	legend = Placed[SwatchLegend[
		{Darker[Purple],Darker[Blue],Darker[ColorFade[{Blue,Green},3][[2]]],Darker[Darker[Green]],Darker[Yellow],Darker[Red],Darker[Orange],Darker[Green]},
		Style[#,Bold,20]&/@{"In Cart","Backlogged","Operator Start","Operator Processing","Operator Ready","Troubleshooting","Awaiting Materials","Instrument Processing"},
		LegendLayout->"Row",
		LegendMarkerSize->{{30,30}}
	],Bottom];
	
	chartLabels = If[MatchQ[subprotocols,{}],
		{"Status Log","Checkpoint Progress"},
		Join[{"Status Log"},Reverse[ToString/@subprotocols],{"Checkpoint Progress"}]
	];
	
	Legended[BarChart[
		Reverse[Flatten[{{checkpointBar},subprotocolBars,{statusLogBar}},1]],
		ChartStyle->97,
		ChartLayout->"Stacked",
		BarOrigin->Left,
		ChartBaseStyle->EdgeForm[],
		ChartLabels->{Placed[Style[#,12,Bold]&/@chartLabels,Axis],None},
		GridLines->{Range[Unitless[Ceiling[DateDifference[startDate,Now]]]],None},
		ImageSize->1200
	],legend]
	
];


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


