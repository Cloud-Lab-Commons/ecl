(* CreatedFrom Object[EmeraldCloudFile, "id:vXl9j5WbNzEk"] on Tue 27 Aug 2024 11:39:33 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Chapter:: *)
Protocol Troubleshooting Guide

(* ::Section:: *)
Types of protocols

(* ::Text:: *)
Protocols can be composed of individual unit operations such as ExperimentRoboticSamplePreparation, ExperimentManualSamplePreparation, ExperimentMagneticBead Separation Etc, or they can be standalone such as ExperimentPCR, ExperimentGelElectrophoresis. 

(* ::Text:: *)

First step to troubleshoot is to view the information that was captured in the protocol object using the Inspect function.

(* ::Code:: *)
Inspect[Object[Protocol,PCR,"id:N80DNj0qMGBE"]]

(* ::Output:: *)


(* ::Text:: *)
As you can notice a lot of information is stored in the protocol object once the protocol is completed running on ECL\[CloseCurlyQuote]s platform.

(* ::Section:: *)
Protocols Composed of Unit Operations

(* ::Text:: *)
Protocols composed of Unit Operations have a field that contain the Unit Operations that were performed during execution in the order they were performed with pertinent details for individual Unit Operations. 

(* ::Text:: *)
Let\[CloseCurlyQuote]s take an example. As you can see, you can download the information from about the Unit Operations from the OutputUnitOperations field for a protocol.

(* ::Code:: *)
Object[Protocol,RoboticSamplePreparation,"id:Z1lqpMlVM8N4"][OutputUnitOperations]

(* ::Output:: *)
{[],[],[],[]}

(* ::Code:: *)
Object[Protocol,ManualSamplePreparation,"id:3em6Zvmzw5zM"][OutputUnitOperations]

(* ::Output:: *)
{[],[],[]}

(* ::Text:: *)
Within these Unit Operations you can click on the + icon to reveal more useful information like Source, SoruceContainer, Tips etc. The information depends on the type of unit operation.

(* ::Input:: *)
[]

(* ::Subsubsection:: *)
Visualizing Pressure Traces

(* ::Text:: *)
If the protocol was performed on a Hamilton, then you can view the pressure traces for each transfer performed. Lets take a look at one MageticBeadSeparation Protocol. Use the function PlotTADM to plot the pressure traces. To better under how the pressure traces should ideally look like here is a guide. https://download.hamiltonsupport.com/wl/?id=PJiHhJhjHojiW8cUzdLgyo85o4OiakDs&mode=grid&download=1

(* ::Input:: *)


(* ::Text:: *)
Please note the the above trace shapes can be used as a guideline to investigate any issues during aspiration or dispense steps. 

(* ::Code:: *)
PlotTADM[Object[Protocol,RoboticSamplePreparation,"id:Z1lqpMlVM8N4"]]

(* ::Output:: *)


(* ::Input:: *)
Object[Sample,"id:3em6ZvmzvMbo"][SampleHistory]

(* ::Output:: *)
{[],[],[],[],[],[],[],[],[]}

(* ::Code:: *)
Object[Protocol,RoboticSamplePreparation,"id:Z1lqpMlVM8N4"][OutputUnitOperations]

(* ::Output:: *)
{[],[],[],[]}

(* ::Text:: *)
Also note that the function PlotTADM can also be used on individual Transfer Unit Operations.

(* ::Code:: *)
PlotTADM[Object[UnitOperation,Transfer,"id:1ZA60vAOvKGE"]]

(* ::Output:: *)


(* ::Subsubsection:: *)
Magnetic Bead Separation Protocol

(* ::Text:: *)
The individual Unit Operations are hidden within the MagneticBeadSeparation Unit Operation. As you can see you can get the MagneticBeadSeparation 

(* ::Code:: *)
Object[Protocol,RoboticSamplePreparation,"id:qdkmxzkwkJA3"][OutputUnitOperations]

(* ::Output:: *)
{[],[]}

(* ::Code:: *)
Object[UnitOperation,MagneticBeadSeparation,"id:WNa4ZjanaA8D"][RoboticUnitOperations]

(* ::Output:: *)
{[],[],[],[],[],[],[],[],[],[],[],[],[],[]}

(* ::Section:: *)
Standalone Protocols

(* ::Text:: *)
For most standalone protocols the best place to start is to Inspect the protocol to get the relevant information. As you can see some relevant fields investigating are Instrument, SamplesIn, ContainersIn, SamplePreparationProtocols, PostProcessingProtocols, ResolvedOptions etc. Depending on where you believe the issue would have occurred, those particular fields can be investigated in depth.

(* ::Item:: *)
SamplesIn - Contains information about the Samples that were taken as input for the experiment along with any reagents that the experiment needed

(* ::Item:: *)
SamplesOut - Samples that were generated during the protocol run time

(* ::Item:: *)
SamplePreparation Protocols - Protocols generated to prepare the samples for the experiment

(* ::Item:: *)
PostProcessingProtocols - Protocols generated after the experiment is completed to collect additional data about the samples generated, like taking a picture, measuring volume, measuring weight etc

(* ::Item:: *)
ResolvedOptions - contains a list of options that were generated using ECL\[CloseCurlyQuote]s internal logic 

(* ::Code:: *)
Inspect[Object[Protocol,AgaroseGelElectrophoresis,"id:M8n3rxnErWLl"]]

(* ::Output:: *)


(* ::Text:: *)
If you are unsure about what kind of information a particular field stores, you can Inspect the parent protocol type as shown below to get the definitions of the individual fields

(* ::Code:: *)
Inspect[Object[Protocol, AgaroseGelElectrophoresis]]

(* ::Output:: *)


(* ::Section:: *)
Watching Protocols

(* ::Text:: *)
For protocols performed on the Hamilton you can watch the video of protocol being carried out using the function WatchProtocol. Run the following line of code to open a browser tab with the video of the protocol. Using the timestamps within the Individual Unit Operations, you can go the timestamp within to video to troubleshoot any issues that you might noticed in the Pressure Traces.

(* ::Code:: *)
WatchProtocol[Object[Protocol,RoboticSamplePreparation,"id:9RdZXvdOVLd6"]]

(* ::Section:: *)
Visualizing Data

(* ::Text:: *)
The data if any collected during the protocol like Fluorescence and Absorbance readings etc, are stored in the Data field of the protocol object

(* ::Code:: *)
data = Object[Protocol,AgaroseGelElectrophoresis,"id:M8n3rxnErWLl"][Data]

(* ::Output:: *)
{,,,,,,,,,,}

(* ::Text:: *)
Most data can be visualized using PlotObject function and a host of other data visualizing tools available in Command Center
https://www.emeraldcloudlab.com/guides/visualizingconstellationobjects?toggles=open

(* ::Code:: *)
PlotObject[data]

(* ::Output:: *)


(* ::Input:: *)
PlotGelAnimation[Object[Protocol,AgaroseGelElectrophoresis,"id:M8n3rxnErWLl"]]

(* ::Output:: *)
{1"Plate_1 Red"2"Blue"3"Combined"}