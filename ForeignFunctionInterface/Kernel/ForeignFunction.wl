BeginPackage["ChristopherWolfram`ForeignFunctionInterface`ForeignFunction`"]

Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`ExternalLibrary`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


(*
	ForeignFunction objects have the form:

	ForeignFunction[name, type, ForeignFunctionObject[...]]
*)

(* Constructors *)

ForeignFunction[lib_ExternalLibrary, name_String, type_] :=
	With[{ff = CreateForeignFunction[lib, name, type]},
		If[!MatchQ[ff, _DataStructure],
			ff,
			ForeignFunction[name, type, ff]
		]
	]

ForeignFunction[name_String, type_] :=
	ForeignFunction[$DefaultExternalLibrary, name, type]


(* Calling *)

ForeignFunction[name_String, type_, ff_DataStructure][args___] :=
	CallForeignFunction[ff, {args}]


(* Summary box *)

ForeignFunction /: MakeBoxes[expr:ForeignFunction[name_String, type_, ff_DataStructure], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		ForeignFunction,
		expr,
		None,
		{
			{"name: ", name},
			{"type: ", type}
		},
		{},
		form
	]



End[] (* End `Private` *)

EndPackage[]
