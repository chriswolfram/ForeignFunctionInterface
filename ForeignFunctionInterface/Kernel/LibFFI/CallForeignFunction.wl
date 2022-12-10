BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]

ExpressionIntoPointer

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"] (* for GetCallbackPointer *)



DeclareCompiledComponent["ForeignFunctionInterface", {

	FunctionDeclaration[ExpressionIntoPointer,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::[type], "BitCast"],
				Cast[init, type]
			];
		]
	],

	FunctionDeclaration[ExpressionIntoPointer,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"],
				(* TODO: This could be more efficient. GetCallbackPointer turns a pointer to an expression, and this turns it back. *)
				ExpressionToPointer[GetCallbackPointer[GetManagedExpression[init]]]
			];
		]
	],

	FunctionDeclaration[ExpressionIntoPointer,
		Typed[{"OpaqueRawPointer", "FFIType", "InertExpression"} -> "Null"]@
		Function[{ptr, type, init},
			Switch[type,

				(* "Void" is removed *)
				FFIType[LiteralType["UnsignedInteger8"]],		ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger8"], init],
				FFIType[LiteralType["Integer8"]],						ExpressionIntoPointer[ptr, TypeSpecifier["Integer8"], init],
				FFIType[LiteralType["UnsignedInteger16"]],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger16"], init],
				FFIType[LiteralType["Integer16"]],					ExpressionIntoPointer[ptr, TypeSpecifier["Integer16"], init],
				FFIType[LiteralType["UnsignedInteger32"]],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger32"], init],
				FFIType[LiteralType["Integer32"]],					ExpressionIntoPointer[ptr, TypeSpecifier["Integer32"], init],
				FFIType[LiteralType["UnsignedInteger64"]],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger64"], init],
				FFIType[LiteralType["Integer64"]],					ExpressionIntoPointer[ptr, TypeSpecifier["Integer64"], init],
				FFIType[LiteralType["CFloat"]],							ExpressionIntoPointer[ptr, TypeSpecifier["CFloat"], init],
				FFIType[LiteralType["CDouble"]],						ExpressionIntoPointer[ptr, TypeSpecifier["CDouble"], init],
				FFIType[LiteralType["CUnsignedChar"]],			ExpressionIntoPointer[ptr, TypeSpecifier["CUnsignedChar"], init],
				FFIType[LiteralType["CSignedChar"]],				ExpressionIntoPointer[ptr, TypeSpecifier["CSignedChar"], init],
				FFIType[LiteralType["CUnsignedShort"]],			ExpressionIntoPointer[ptr, TypeSpecifier["CUnsignedShort"], init],
				FFIType[LiteralType["CShort"]],							ExpressionIntoPointer[ptr, TypeSpecifier["CShort"], init],
				FFIType[LiteralType["CUnsignedInt"]],				ExpressionIntoPointer[ptr, TypeSpecifier["CUnsignedInt"], init],
				FFIType[LiteralType["CInt"]],								ExpressionIntoPointer[ptr, TypeSpecifier["CInt"], init],
				FFIType[LiteralType["CUnsignedLong"]],			ExpressionIntoPointer[ptr, TypeSpecifier["CUnsignedLong"], init],
				FFIType[LiteralType["CLong"]],							ExpressionIntoPointer[ptr, TypeSpecifier["CLong"], init],
				FFIType[LiteralType["OpaqueRawPointer"]],		ExpressionIntoPointer[ptr, TypeSpecifier["OpaqueRawPointer"], init],
				_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

			]
		]
	],


	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "InertExpression"} -> "InertExpression"]@
		Function[{ff, args},
			Module[{argCount},
				argCount = Cast[ff["CallInterface"]["ArgumentCount"], "MachineInteger", "CCast"];

				If[Head[args] =!= InertExpression[List] || Length[args] =!= argCount,
					Native`ThrowWolframExceptionCode["Argument"]
				];

				Do[
					ExpressionIntoPointer[
						FromRawPointer[ff["ArgumentPointers"], i-1],
						FromRawPointer[ff["CallInterface"]["ArgumentTypes"], i-1],
						args[[i]]
					],
					{i, argCount}
				];

				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], ff["OutputPointer"], ff["ArgumentPointers"]];

				DereferenceBuffer[ff["OutputPointer"], ff["CallInterface"]["OutputType"]]
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CallForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
