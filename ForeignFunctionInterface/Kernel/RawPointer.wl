BeginPackage["ChristopherWolfram`ForeignFunctionInterface`RawPointer`"]

Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


(* Constructors *)

HoldPattern[RawPointer][HoldPattern[OpaqueRawPointer][addr_Integer], type_] :=
	RawPointer[addr, type]

HoldPattern[RawPointer][HoldPattern[RawPointer][addr_Integer, _], type_] :=
	RawPointer[addr, type]


(* Validators *)

HoldPattern[RawPointer][expr:Except[_Integer], type_] :=
	(
		Message[RawPointer::invaddress, expr];
		Failure["InvalidPointerAddress", <|
			"MessageTemplate" :> RawPointer::invaddress,
			"MessageParameters" -> {expr},
			"Address" -> expr
		|>]
	)

HoldPattern[RawPointer][addr_, type_, args__] :=
	ArgumentsOptions[RawPointer[addr, type, args], 2]


(* Summary box *)

RawPointer /: MakeBoxes[expr:HoldPattern[RawPointer][addr_Integer, type_], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		RawPointer,
		expr,
		None,
		{
			{"type: ", type},
			{"address: ", addr}
		},
		{},
		form
	]


End[] (* End `Private` *)

EndPackage[]
