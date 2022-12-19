BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CallForeignFunction`"]

ExpressionIntoPointer

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
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
			Switch[type["Type"],

				NameFFITypeID["UINT8"][],		ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger8"], init],
				NameFFITypeID["SINT8"][],		ExpressionIntoPointer[ptr, TypeSpecifier["Integer8"], init],
				NameFFITypeID["UINT16"][],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger16"], init],
				NameFFITypeID["SINT16"][],	ExpressionIntoPointer[ptr, TypeSpecifier["Integer16"], init],
				NameFFITypeID["UINT32"][],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger32"], init],
				NameFFITypeID["SINT32"][],	ExpressionIntoPointer[ptr, TypeSpecifier["Integer32"], init],
				NameFFITypeID["UINT64"][],	ExpressionIntoPointer[ptr, TypeSpecifier["UnsignedInteger64"], init],
				NameFFITypeID["SINT64"][],	ExpressionIntoPointer[ptr, TypeSpecifier["Integer64"], init],
				NameFFITypeID["INT"][],			ExpressionIntoPointer[ptr, TypeSpecifier["CInt"], init],
				NameFFITypeID["FLOAT"][],		ExpressionIntoPointer[ptr, TypeSpecifier["CFloat"], init],
				NameFFITypeID["DOUBLE"][],	ExpressionIntoPointer[ptr, TypeSpecifier["CDouble"], init],
				NameFFITypeID["POINTER"][],	ExpressionIntoPointer[ptr, TypeSpecifier["OpaqueRawPointer"], init],
				NameFFITypeID["STRUCT"][],	expressionIntoStructPointer[ptr, type, init],
				_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

			]
		]
	],

	FunctionDeclaration[expressionIntoStructPointer,
		Typed[{"OpaqueRawPointer", "FFIType", "InertExpression"} -> "Null"]@
		Function[{ptr, type, init},
			Module[{elementCount, elementOffsets},

				elementCount = FFITypeElementCount[type];

				If[Head[init] =!= InertExpression[List] || Length[init] =!= elementCount,
					Native`ThrowWolframExceptionCode["Argument"]
				];

				(* TODO: Check error code *)
				elementOffsets = CreateTypeInstance["Managed"::["CArray"::["CSizeT"]], elementCount];
				LibraryFunction["ffi_get_struct_offsets"][
					LibraryFunction["get_FFI_DEFAULT_ABI"][],
					type,
					elementOffsets
				];

				Do[
					expressionIntoStructPointer[
						Cast[Cast[ptr,"UnsignedInteger64","BitCast"] + FromRawPointer[elementOffsets,i-1], "OpaqueRawPointer","BitCast"],
						FromRawPointer[type["Elements"], i-1],
						init[[i]]
					],
					{i, elementCount}
				]
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
