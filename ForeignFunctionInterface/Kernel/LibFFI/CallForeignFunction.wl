BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"] (* for GetCallbackPointer *)



DeclareCompiledComponent["ForeignFunctionInterface", {

	FunctionDeclaration[populateArgumentPointer,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::[type], "BitCast"],
				Cast[init, type]
			];
		]
	],

	FunctionDeclaration[populateArgumentPointer,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"],
				(* TODO: This could be more efficient. GetCallbackPointer turns a pointer to an expression, and this turns it back. *)
				ExpressionToPointer[GetCallbackPointer[GetManagedExpression[init]]]
			];
		]
	],

	FunctionDeclaration[populateArgumentPointer,
		Typed[{"OpaqueRawPointer", "FFIType", "InertExpression"} -> "Null"]@
		Function[{ptr, type, init},
			Switch[type,

				(* "Void" is removed *)
				FFIType[LiteralType["UnsignedInteger8"]],		populateArgumentPointer[ptr, TypeSpecifier["UnsignedInteger8"], init],
				FFIType[LiteralType["Integer8"]],						populateArgumentPointer[ptr, TypeSpecifier["Integer8"], init],
				FFIType[LiteralType["UnsignedInteger16"]],	populateArgumentPointer[ptr, TypeSpecifier["UnsignedInteger16"], init],
				FFIType[LiteralType["Integer16"]],					populateArgumentPointer[ptr, TypeSpecifier["Integer16"], init],
				FFIType[LiteralType["UnsignedInteger32"]],	populateArgumentPointer[ptr, TypeSpecifier["UnsignedInteger32"], init],
				FFIType[LiteralType["Integer32"]],					populateArgumentPointer[ptr, TypeSpecifier["Integer32"], init],
				FFIType[LiteralType["UnsignedInteger64"]],	populateArgumentPointer[ptr, TypeSpecifier["UnsignedInteger64"], init],
				FFIType[LiteralType["Integer64"]],					populateArgumentPointer[ptr, TypeSpecifier["Integer64"], init],
				FFIType[LiteralType["CFloat"]],							populateArgumentPointer[ptr, TypeSpecifier["CFloat"], init],
				FFIType[LiteralType["CDouble"]],						populateArgumentPointer[ptr, TypeSpecifier["CDouble"], init],
				FFIType[LiteralType["CUnsignedChar"]],			populateArgumentPointer[ptr, TypeSpecifier["CUnsignedChar"], init],
				FFIType[LiteralType["CSignedChar"]],				populateArgumentPointer[ptr, TypeSpecifier["CSignedChar"], init],
				FFIType[LiteralType["CUnsignedShort"]],			populateArgumentPointer[ptr, TypeSpecifier["CUnsignedShort"], init],
				FFIType[LiteralType["CShort"]],							populateArgumentPointer[ptr, TypeSpecifier["CShort"], init],
				FFIType[LiteralType["CUnsignedInt"]],				populateArgumentPointer[ptr, TypeSpecifier["CUnsignedInt"], init],
				FFIType[LiteralType["CInt"]],								populateArgumentPointer[ptr, TypeSpecifier["CInt"], init],
				FFIType[LiteralType["CUnsignedLong"]],			populateArgumentPointer[ptr, TypeSpecifier["CUnsignedLong"], init],
				FFIType[LiteralType["CLong"]],							populateArgumentPointer[ptr, TypeSpecifier["CLong"], init],
				FFIType[LiteralType["OpaqueRawPointer"]],		populateArgumentPointer[ptr, TypeSpecifier["OpaqueRawPointer"], init],
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
					populateArgumentPointer[
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
