BeginPackage["ForeignFunctionInterface`OpaqueRawPointer`"]


ExpressionToPointer
PointerToExpression


Begin["`Private`"]

Needs["ForeignFunctionInterface`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

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



End[] (* End `Private` *)

EndPackage[]
