(* CreatedFrom Object[EmeraldCloudFile, "id:7X104v1qNwvd"] on Tue 28 May 2024 16:38:14 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Title:: *)
Script enhancement functions, buffers and constants

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Waseem Vali
E.mail:  waseem@alignbio.org
Requested citation: None

(* ::Section:: *)
 Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
To be included included in all scripts and notebooks. This code contains useful functions, helpful constants and commonly used buffers that. 

(* ::Subsection:: *)
Functions

(* ::Item:: *)
plotProtocolWatcher

(* ::Item:: *)
plotOrderStatus

(* ::Item:: *)
ganttChart

(* ::Item:: *)
barMaker

(* ::Item:: *)
statusLogGanttChart

(* ::Item:: *)
statusLogBarMaker

(* ::Item:: *)
checkpoint ProgressBarMaker

(* ::Item:: *)
checkpointStatusComparer

(* ::Item:: *)
nameSearch

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
 Running the Code

(* ::Text:: *)
To begin using these functions in your workflow:
1. Copy the following code block
2. Open your notebook on ECL\[CloseCurlyQuote]s Command Center in the Notebook Library
3. Under the Functions section, create a new function and add the copied code block
4. Make sure to click \[OpenCurlyDoubleQuote]Turn on Preload\[CloseCurlyDoubleQuote] to ensure the code loads each you open the notebook

(* ::Section:: *)
Code

(* ::Subsection:: *)
ProtocolWatcher

(* ::Text:: *)
To see the status of all the protocol running in the lab for the current financing team

(* ::Code:: *)
Clear[plotProtocolWatcher];

plotProtocolWatcher[]:=Module[{financingTeam ,notebooks , allProtocols,allDownloads,myData,headers,highlightIfTroubleshooting,highlightedData,rowColors,formattedData},
	financingTeam = Object[Team,Financing,"Schmidt-Futures"];
	notebooks = Object[Team,Financing,"Schmidt-Futures"][NotebooksFinanced][Object];
	
	allProtocols=Search[Object[Protocol],And[Or[OperationStatus==(OperatorReady|OperatorProcessing|Troubleshooting|OperatorStart|InstrumentProcessing),Status==ShippingMaterials],ParentProtocol==Null,Notebook==Alternatives@@notebooks]];
	allDownloads=Download[allProtocols,{Object,Notebook[Financers][Name],Notebook[Name],ReadyCheckLog[[-1]][[2]],Status,OperationStatus}];

	headers={"Protocol","Financer","Notebook","ReadyCheck","Status","Op Status"};
	rowColors=Map[
		Which[
			MemberQ[#,Troubleshooting],LightRed,
			MemberQ[#,ShippingMaterials],LightOrange,
			MemberQ[#,Processing],LightGreen,
			True,None
		]&,
	allDownloads];

	PlotTable[allDownloads,TableHeadings->{Range[Length[allDownloads]],headers},Background->{None,Prepend[rowColors,LightGray]}]
]

(* ::Section:: *)
OrderStatus

(* ::Text:: *)
To Check the status of the various types of orders across the financing team

(* ::Code:: *)
Clear[plotOrderStatus];

plotOrderStatus[orderType_String]:= plotOrderStatus[orderType, 6 Month]
plotOrderStatus[orderType_String, timeFrame_Quantity]:= Module[{financingTeam, notebooks, plottingData, plottingHeaders, plottingColors},
	financingTeam = Object[Team,Financing,"Schmidt-Futures"];
	notebooks = Object[Team,Financing,"Schmidt-Futures"][NotebooksFinanced][Object];

	{plottingData, plottingHeaders, plottingColors} = Switch[orderType, 
	"DropShip",
	Module[{dropShipTransactions, sortedDropShipTransactions, dropShipData, dropShipHeaders, dropShipColors},
		dropShipTransactions = Search[Object[Transaction,DropShipping], Notebook==Alternatives@@notebooks&&Status!=Canceled&&DateCreated>Now-timeFrame];
		sortedDropShipTransactions = ReverseSortBy[dropShipTransactions, #[DateCreated]&];
		dropShipData = Quiet[Download[sortedDropShipTransactions, {Object, DateCreated, Notebook, Status, Provider, OrderedItems, Receiving}], {Download::FieldDoesntExist}];
		dropShipHeaders = {"Transaction", "DateCreated","Notebook", "Status", "Provider", "OrderedItems", "Receiving"};
		dropShipColors = Map[Which[
			MemberQ[#,Ordered],LightOrange,
			MemberQ[#,Ordered]&&Length[#[[7]]>=1],LightYellow,
			MemberQ[#,Received],LightGreen,
			True,None]&,
			dropShipData];
		{dropShipData, dropShipHeaders, dropShipColors}
	],
	"Order",
	Module[{orderTransactions, sortedorderTransactions, orderData, orderHeaders, orderColors},
		orderTransactions = Search[Object[Transaction, Order], Notebook==Alternatives@@notebooks&&Status!=Canceled&&DateCreated>Now-timeFrame];
		sortedorderTransactions = ReverseSortBy[orderTransactions, #[DateCreated]&];
		orderData = Quiet[Download[sortedorderTransactions, {Object, DateOrdered, Notebook, Status, Supplier, Products, ItemUnitDescriptions}], {Download::FieldDoesntExist}];
		orderHeaders = {"Transaction", "DateOrdered","Notebook", "Status", "Supplier", "OrderedProducts", "ItemUnitDescriptions"};
		orderColors = Map[Which[
			MemberQ[#,(Pending|Ordered|Shipped)],LightYellow,
			MemberQ[#,Backordered],LightOrange,
			MemberQ[#,Received],LightGreen,
			True,None]&,
			orderData];
		{orderData, orderHeaders, orderColors}
	],
	"ShipToECL",
	Module[{shipToECLTransactions, sortedshipToECLTransactions, shipToECLData, shipToECLHeaders, shipToECLColors},
		shipToECLTransactions = Search[Object[Transaction, ShipToECL], Notebook==Alternatives@@notebooks&&Status!=Canceled&&DateCreated>Now-timeFrame];
		sortedshipToECLTransactions = ReverseSortBy[shipToECLTransactions, #[DateCreated]&];
		shipToECLData = Quiet[Download[sortedshipToECLTransactions, {Object, Source, Notebook, Status, Creator, Shipper, Receiving}], {Download::FieldDoesntExist}];
		shipToECLHeaders = {"Transaction", "Source", "Notebook", "Status", "Creator", "Shipper", "ReceivingProtocols"};
		shipToECLColors = Map[Which[
			MemberQ[#,(Pending|Shipped)],LightYellow,
			MemberQ[#,Received],LightGreen,
			True,None]&,
			shipToECLData];
		{shipToECLData, shipToECLHeaders, shipToECLColors}
	],
	"ShipToUser",
	Module[{shipToUserTransactions, sortedshipToUserTransactions, shipToUserData, shipToUserHeaders, shipToUserColors},
		shipToUserTransactions = Search[Object[Transaction, ShipToUser], Notebook==Alternatives@@notebooks&&Status!=Canceled&&DateCreated>Now-timeFrame];
		sortedshipToUserTransactions = ReverseSortBy[shipToUserTransactions, #[DateCreated]&];
		shipToUserData = Quiet[Download[sortedshipToUserTransactions, {Object, Notebook, Status, Source, Destination, Creator, SamplesIn}], {Download::FieldDoesntExist}];
		shipToUserHeaders = {"Transaction", "Notebook", "Status", "Source", "Destination", "Creator", "Samples"};
		shipToUserColors = Map[Which[
			MemberQ[#,Pending],LightYellow,
			MemberQ[#,Shipped],LightGreen,
			True,None]&,
			shipToUserData];
		{shipToUserData, shipToUserHeaders, shipToUserColors}
	]
	];

	PlotTable[plottingData,TableHeadings->{Automatic,plottingHeaders},Background->{None,Prepend[plottingColors,LightGray]}]
]

(* ::Section:: *)
GANTT Chart 

(* ::Text:: *)
To plot the GANTT chart for a particular protocol set of protocols/troubleshooting objects

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
SamplesHaveDNA

(* ::Text:: *)
Add a DNA component to the composition field of the sample

(* ::Code:: *)
Clear[samplesHaveDNA];

samplesHaveDNA[mySample:ObjectP[Object[Sample]]]:=samplesHaveDNA[{mySample}]
samplesHaveDNA[mySamples:{ObjectP[Object[Sample]]..}]:=Module[{sampleObjs,changePackets,validQresults},
	sampleObjs=Download[mySamples,Object];
	changePackets=<|Object->#,Append[Composition]->{{Null,Link[Model[Molecule,Oligomer,"default_DNA_extinction_coef"]]}}|>&/@sampleObjs;
	validQresults=ValidUploadQ[changePackets];
	If[
		validQresults==True, Upload[changePackets],
		Print["ValidUploadQ error, please check the sample input"]
	]
]

(* ::Section:: *)
nameSearch

(* ::Text:: *)
To Search for a an object with a particular name

(* ::Code:: *)
nameSearch[myType:ListableP[TypeP[]], myName_String]:=
	If[KeyExistsQ[Lookup[LookupTypeDefinition[myType], Fields], Synonyms],
		Search[myType, StringContainsQ[Synonyms, myName, IgnoreCase -> True]],
		Search[myType, StringContainsQ[Name, myName, IgnoreCase -> True]]
];
nameSearch[myType:ListableP[TypeP[]]]:=Search[myType];

(* ::Section:: *)
Constants

(* ::Code:: *)
(* shortcut for the placeholder model container for new products *)
placeholder = Model[Container,Vessel,"id:6V0npvK611wE"];

(* shortcut for 96 well deep well plate *)
dwp = Model[Container,Plate,"id:E8zoYveRll17"];

(* shortcut for a 2mL and 50mL tube *)
tube2 = Model[Container, Vessel, "2mL Tube"];
tube15 = Model[Container, Vessel, "15mL Tube"];
tube50 = Model[Container, Vessel, "50mL Tube"];

(* shortcut for water and acetone *)
water = Model[Sample, "Milli-Q water"];

(* shortcut for common storage conditions *)
ambient = Model[StorageCondition, "Ambient Storage"];
fridge = Model[StorageCondition, "Refrigerator"];
freezer = Model[StorageCondition, "Freezer"];

(* ::Section:: *)
Buffers

(* ::Section:: *)
Conclusions and Future Direction

(* ::Text:: *)
Any more useful functions, constants or Buffers will be added as they are encountered 