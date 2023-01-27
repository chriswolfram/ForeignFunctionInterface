BeginPackage["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]


ExpressionToPointer
PointerToExpression


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

	(* Expression conversion *)

	FunctionDeclaration[PointerToExpression,
		Typed[{"OpaqueRawPointer"} -> "InertExpression"]@
		Function[ptr,
			Construct[
				InertExpression[OpaqueRawPointer],
				(* TODO: is this safe on 32-bit platforms? *)
				Cast[Cast[ptr, "UnsignedInteger64", "BitCast"], "InertExpression"]
			]
		]
	],

	FunctionDeclaration[ExpressionToPointer,
		Typed[{"InertExpression"} -> "OpaqueRawPointer"]@
		Function[expr,
			If[Head[expr] =!= InertExpression[OpaqueRawPointer] || Length[expr] =!= 1,
				Native`ThrowWolframExceptionCode["Argument"]
			];
			Cast[Cast[First[expr], "UnsignedInteger64"], "OpaqueRawPointer", "BitCast"]
		]
	]

}];


(* Constructor *)

HoldPattern[OpaqueRawPointer][RawPointer[addr_Integer, type_]] :=
	OpaqueRawPointer[addr]


(* Validator *)

HoldPattern[OpaqueRawPointer][expr:Except[_Integer]] :=
	(
		Message[OpaqueRawPointer::invaddress, expr];
		Failure["InvalidPointerAddress", <|
			"MessageTemplate" :> OpaqueRawPointer::invaddress,
			"MessageParameters" -> {expr},
			"Address" -> expr
		|>]
	)


(* Summary box *)

OpaqueRawPointer /: MakeBoxes[expr:HoldPattern[OpaqueRawPointer][addr_Integer], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		OpaqueRawPointer,
		expr,
		None,
		{"address: ", addr},
		{},
		form
	]


End[] (* End `Private` *)

EndPackage[]
