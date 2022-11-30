BeginPackage["ForeignFunctionInterface`OpaqueRawPointer`"]


ExpressionToPointer
PointerToExpression


Begin["`Private`"]

Needs["ForeignFunctionInterface`"]



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



(* Summary box *)

OpaqueRawPointer /: MakeBoxes[expr:OpaqueRawPointer[addr_Integer], form:StandardForm]:=
	BoxForm`ArrangeSummaryBox[
		OpaqueRawPointer,
		expr,
		None,
		(*the next argument is the always visisble properties*)
		{"address: ", addr},
		{},
		form
	];


End[] (* End `Private` *)

EndPackage[]
