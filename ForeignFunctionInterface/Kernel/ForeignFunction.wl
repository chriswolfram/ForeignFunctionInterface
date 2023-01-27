BeginPackage["ChristopherWolfram`ForeignFunctionInterface`ForeignFunction`"]

Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`ExternalLibrary`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`BaseTypeConversion`"]


(*
	ForeignFunction objects have the form:

	ForeignFunction[name, type, ForeignFunctionObject[...]]
*)

(* Constructors *)

ForeignFunction[lib_ExternalLibrary, name_String, typeI_] :=
	With[{type = canonicalizeType[typeI]},
		If[FailureQ[type],
			type,
			With[{ff = CreateForeignFunction[lib, name, functionBaseType[type]]},
				If[!MatchQ[ff, _DataStructure],
					ff,
					ForeignFunction[name, type, ff]
				]
			]
		]
	]

ForeignFunction[name_String, type_] :=
	ForeignFunction[$DefaultExternalLibrary, name, type]



(*
	canonicalizeType[type]
		converts a function type into {arg1, arg2, ...} -> out
*)

canonicalizeType[ty:({___} -> _)] := ty

canonicalizeType[TypeSpecifier[ty:({___} -> _)]] := ty

canonicalizeType[ty_] :=
	(
		Message[ForeignFunction::invtype, ty];
		Failure["InvalidFunctionType", <|
			"MessageTemplate" :> ForeignFunction::invtype,
			"MessageParameters" -> {ty},
			"Type" -> ty
		|>]
	)


(*
	functionBaseType[canonicalType]
		converts arguments and output types into their base type representations.
*)

functionBaseType[args_List -> out_] :=
	ToBaseType/@args -> ToBaseType[out]



(* Calling *)

ForeignFunction[name_String, {___} -> outTy_, ff_DataStructure][args___] :=
	With[{res = CallForeignFunction[ff, ToBaseValue/@{args}]},
		If[FailureQ[res],
			res,
			FromBaseValue[res, outTy]
		]
	]


(* Summary box *)

ForeignFunction /: MakeBoxes[expr:ForeignFunction[name_String, type:({___} -> _), ff_DataStructure], form:StandardForm]:=
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
