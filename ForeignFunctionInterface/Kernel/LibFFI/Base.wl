BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Base`"]


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]


(* TEMPORARY DECLARATIONS *)
(*
	These declarations should probably be added to the C types that are included with the compiler by default
*)
DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Macro", "CUnsignedChar", "UnsignedInteger8"],
	TypeDeclaration["Macro", "CSignedChar", "Integer8"]

}];


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Product", "ForeignFunctionObject",
		<|
			"ArgumentTypes" -> "CArray"::["FFIType"],
			"ArgumentPointers" -> "CArray"::["OpaqueRawPointer"],
			"ArgumentCount" -> "MachineInteger",
			"OutputType" -> "FFIType",
			"OutputPointer" -> "OpaqueRawPointer",
			"CallInterface" -> "FFICallInterface",
			"FunctionPointer" -> "OpaqueRawPointer"
		|>,
		"AbstractTypes" -> {"DataStructures"}
	],

	FunctionDeclaration[CompilerCallback["OnFree"],
		Typed[{"ForeignFunctionObject"} -> "Null"]@
		Function[ff,
			DeleteObject[ff["CallInterface"]];
			DeleteObject[ff["ArgumentTypes"]];
			Do[
				DeleteObject@Cast[FromRawPointer[ff["ArgumentPointers"], i-1], "CArray"::["Integer8"], "BitCast"],
				{i, ff["ArgumentCount"]}
			];
			DeleteObject[ff["ArgumentPointers"]];
			DeleteObject[Cast[ff["OutputPointer"], "CArray"::["Integer8"], "BitCast"]];
		]
	],

	FunctionDeclaration[CreateForeignFunctionWithLibrary,
		Typed[{"ExternalLibraryHandle", "String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, outputType},
			Module[{cif, argCount, argTypesArray, argValuesArray, outputValue, fun},

				argCount = Length[argTypes];

				argTypesArray = CreateTypeInstance["CArray"::["FFIType"], argTypes];

				argValuesArray = CreateTypeInstance["CArray"::["OpaqueRawPointer"], argCount];
				Do[
					ToRawPointer[argValuesArray, i-1,
						Cast[CreateTypeInstance["CArray"::["Integer8"], typeSize[argTypes[[i]]]], "OpaqueRawPointer", "BitCast"]
					],
					{i, argCount}
				];

				(* TODO: This should be at least as big as the ffi_arg type *)
				outputValue =
					Cast[
						CreateTypeInstance["CArray"::["Integer8"], Max[Native`SizeOf[TypeSpecifier["OpaqueRawPointer"]], typeSize[outputType]]],
						"OpaqueRawPointer", "BitCast"
					];

				(* TODO: check error code *)
				cif = CreateTypeInstance["FFICallInterface", <||>];
				LibraryFunction["ffi_prep_cif"][
					cif,
					LibraryFunction["get_FFI_DEFAULT_ABI"][],
					Cast[Length[argTypes],"CUnsignedInt","CCast"],
					outputType,
					argTypesArray
				];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argTypesArray,
					"ArgumentPointers" -> argValuesArray,
					"ArgumentCount" -> argCount,
					"OutputType" -> outputType,
					"OutputPointer" -> outputValue,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],

	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{funName, argTypes, outputType},
			CreateForeignFunctionWithLibrary[
				LibraryFunction["get_RTLD_DEFAULT"][],
				funName,
				argTypes,
				outputType
			]
		]
	],


	FunctionDeclaration[typeSize,
		Typed[{"FFIType"} -> "MachineInteger"]@
		Function[type,
			Cast[type["Size"], "MachineInteger", "CCast"]
		]
	],


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
				ExpressionToPointer[GetManagedExpression[init]]
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
		Typed[{"ForeignFunctionObject", "ListVector"::["InertExpression"]} -> "InertExpression"]@
		Function[{ff, args},
			Do[
				populateArgumentPointer[
					FromRawPointer[ff["ArgumentPointers"], i-1],
					FromRawPointer[ff["ArgumentTypes"], i-1],
					args[[i]]
				],
				{i, ff["ArgumentCount"]}
			];

			LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], ff["OutputPointer"], ff["ArgumentPointers"]];

			DereferenceBuffer[ff["OutputPointer"], ff["OutputType"]]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunctionWithLibrary,
	CreateForeignFunction,
	CallForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
