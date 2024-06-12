(* CreatedFrom Object[EmeraldCloudFile, "id:rea9jlaVe803"] on Wed 12 Jun 2024 15:53:15 - please remove this line if you make manual changes to the file. *)
(* ::Package:: *)


(* ::Title:: *)
Resuspension of Lyophilized Protein

(* ::Subsection:: *)
License

(* ::Text:: *)

This software is being released under the terms defined by the Apache 2.0 license.
https://opensource.org/license/apache-2-0/

When publishing please cite the following:

Author(s): Amanda Kohler
E.mail: amanda@tyralaconsulting.com
Requested citation: None

(* ::Section:: *)
Overview

(* ::Subsection:: *)
Purpose and Scope

(* ::Text:: *)
This protocol creates a 5mg/mL stock solution of a 2mg lyophilized protein. Resuspension volume should be calculated based on the desired final stock concentration and the amount of protein to be resuspended - this will vary based on the specific requirements of each experiment.

Step 1. Resuspend lyophilized protein
Step 2. Centrifuge the sample
Step 3. Measure Absorbance to estimate concentration

(* ::Subsection:: *)
List of Updates from Previous Version 

(* ::Text:: *)
Not available

(* ::Subsection:: *)
List of known bugs

(* ::Text:: *)
Make sure your sample is well re-suspended without any bubbles by checking it\[CloseCurlyQuote]s appearance after resuspension before measuring Absorbance

In order to estimate the protein concentration extinction coefficient must be populated for the protein that you are re-suspending and estimating concentration for

(* ::Section:: *)
Web link

(* ::Input:: *)
Publish[$NotebookPage]

(* ::Section:: *)
Setup

(* ::Text:: *)
None

(* ::Section:: *)
Running Code

(* ::Text:: *)
Step 1. Resuspend lyophilized protein. Provide the 

(* ::Code:: *)
inputsSample = Object[Sample, "id:zGj91aj6JEqE"]

(* ::Code:: *)
ExperimentResuspend[
	inputSample,
	Volume -> 400 Microliter,
	Diluent -> Model[Sample, StockSolution, "Filtered PBS, Sterile"],
	MixType -> Pipette,
	MixVolume -> 200 Microliter, 
	NumberOfMixes -> 25,
	MixUntilDissolved -> True
]

(* ::Text:: *)
Step 2. Transfer sample to a centrifuge compatible vessel and centrifuge sample to remove any bubbles or particulate and ensure more accurate concentration reading.

(* ::Code:: *)
transferProtocol = ExperimentManualSamplePreparation[{
	Transfer[
		Source->inputSample, 
		(*You need the same sample as for Experiment Resuspend since it is resuspended in the same container*)
		Destination->Model[Container,Vessel,"1.5mL Tube with 2mL Tube Skirt"],
		Amount->800 Microliter,
		DestinationTemperature->4 Celsius,
		ReplaceDestinationCover->True,
		DestinationLabel->"Aliquoted sample"
	],
	Centrifuge[
		Sample->"Aliquoted sample",
		Intensity->12000.00 GravitationalAcceleration,
		Time->10 Minute,
		Temperature->10 Celsius, 
		SamplesInStorageCondition -> Refrigerator
	]
}]

(* ::Code:: *)
transferredSample= transferProtocol[SamplesOut]

(* ::Text:: *)

Step 3. Take A280 reading of protein sample to confirm the concentration of the prepared stock solution. This script makes two replicate readings.

(* ::Code:: *)
absorbanceProtocol = ExperimentAbsorbanceSpectroscopy[
	transferredSample,
	QuantifyConcentration -> True,
	Blanks->Model[Sample,StockSolution,"Filtered PBS, Sterile"],
	NumberOfReplicates->2
]

(* ::Text:: *)
Estimated protein concentration 

(* ::Code:: *)
concentration= absorbanceProtocol[QuantificationAnalyses][ConcentrationDistribution]

(* ::Section:: *)
Conclusions and Future Directions