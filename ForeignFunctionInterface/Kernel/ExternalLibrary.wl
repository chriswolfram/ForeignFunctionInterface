BeginPackage["ChristopherWolfram`ForeignFunctionInterface`ExternalLibrary`"]

$DefaultExternalLibrary

Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


$DefaultExternalLibrary := $DefaultExternalLibrary = GetDefaultExternalLibrary[];


(*
	ExternalLibrary objects have the form:

	ExternalLibrary[name, OpaqueRawPointer[...]]
*)

(* Summary box *)

ExternalLibrary /: MakeBoxes[expr:ExternalLibrary[name_, ptr_OpaqueRawPointer], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		ExternalLibrary,
		expr,
		None,
		{"name: ", FileNameTake[name]},
		{"path: ", name},
		form
	]



End[] (* End `Private` *)

EndPackage[]
