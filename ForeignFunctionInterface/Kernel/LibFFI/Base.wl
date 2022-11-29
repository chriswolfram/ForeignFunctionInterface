BeginPackage["ForeignFunctionInterface`LibFFI`Base`"]


Begin["`Private`"]


Needs["ForeignFunctionInterface`"]
Needs["ForeignFunctionInterface`LibFFI`"]
Needs["ForeignFunctionInterface`RawObject`"]


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
			Switch[type,

					(* "Void" is removed *)
					FFIType[LiteralType["UnsignedInteger8"]],		Native`SizeOf[TypeSpecifier["UnsignedInteger8"]],
					FFIType[LiteralType["Integer8"]],						Native`SizeOf[TypeSpecifier["Integer8"]],
					FFIType[LiteralType["UnsignedInteger16"]],	Native`SizeOf[TypeSpecifier["UnsignedInteger16"]],
					FFIType[LiteralType["Integer16"]],					Native`SizeOf[TypeSpecifier["Integer16"]],
					FFIType[LiteralType["UnsignedInteger32"]],	Native`SizeOf[TypeSpecifier["UnsignedInteger32"]],
					FFIType[LiteralType["Integer32"]],					Native`SizeOf[TypeSpecifier["Integer32"]],
					FFIType[LiteralType["UnsignedInteger64"]],	Native`SizeOf[TypeSpecifier["UnsignedInteger64"]],
					FFIType[LiteralType["Integer64"]],					Native`SizeOf[TypeSpecifier["Integer64"]],
					FFIType[LiteralType["CFloat"]],							Native`SizeOf[TypeSpecifier["CFloat"]],
					FFIType[LiteralType["CDouble"]],						Native`SizeOf[TypeSpecifier["CDouble"]],
					FFIType[LiteralType["CUnsignedChar"]],			Native`SizeOf[TypeSpecifier["CUnsignedChar"]],
					FFIType[LiteralType["CSignedChar"]],				Native`SizeOf[TypeSpecifier["CSignedChar"]],
					FFIType[LiteralType["CUnsignedShort"]],			Native`SizeOf[TypeSpecifier["CUnsignedShort"]],
					FFIType[LiteralType["CShort"]],							Native`SizeOf[TypeSpecifier["CShort"]],
					FFIType[LiteralType["CUnsignedInt"]],				Native`SizeOf[TypeSpecifier["CUnsignedInt"]],
					FFIType[LiteralType["CInt"]],								Native`SizeOf[TypeSpecifier["CInt"]],
					FFIType[LiteralType["CUnsignedLong"]],			Native`SizeOf[TypeSpecifier["CUnsignedLong"]],
					FFIType[LiteralType["CLong"]],							Native`SizeOf[TypeSpecifier["CLong"]],
					FFIType[LiteralType["OpaqueRawPointer"]],		Native`SizeOf[TypeSpecifier["OpaqueRawPointer"]],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
		]
	],


	FunctionDeclaration[typePointer,
		Typed[ForAllType[ty, {"TypeSpecifier"::[ty]} -> "OpaqueRawPointer"]]@
		Function[type,
			Cast[Typed[ToRawPointer[], "RawPointer"::[type]], "OpaqueRawPointer", "BitCast"]
		]
	],

	FunctionDeclaration[typePointer,
		Typed[{"TypeSpecifier"::["Void"]} -> "OpaqueRawPointer"]@
		Function[type,
			Cast[0, "OpaqueRawPointer", "BitCast"]
		]
	],

	FunctionDeclaration[typePointer,
		Typed[{"FFIType"} -> "OpaqueRawPointer"]@
		Function[type,
			Switch[type,

					FFIType[LiteralType["Void"]],								typePointer[TypeSpecifier["Void"]],
					FFIType[LiteralType["UnsignedInteger8"]],		typePointer[TypeSpecifier["UnsignedInteger8"]],
					FFIType[LiteralType["Integer8"]],						typePointer[TypeSpecifier["Integer8"]],
					FFIType[LiteralType["UnsignedInteger16"]],	typePointer[TypeSpecifier["UnsignedInteger16"]],
					FFIType[LiteralType["Integer16"]],					typePointer[TypeSpecifier["Integer16"]],
					FFIType[LiteralType["UnsignedInteger32"]],	typePointer[TypeSpecifier["UnsignedInteger32"]],
					FFIType[LiteralType["Integer32"]],					typePointer[TypeSpecifier["Integer32"]],
					FFIType[LiteralType["UnsignedInteger64"]],	typePointer[TypeSpecifier["UnsignedInteger64"]],
					FFIType[LiteralType["Integer64"]],					typePointer[TypeSpecifier["Integer64"]],
					FFIType[LiteralType["CFloat"]],							typePointer[TypeSpecifier["CFloat"]],
					FFIType[LiteralType["CDouble"]],						typePointer[TypeSpecifier["CDouble"]],
					FFIType[LiteralType["CUnsignedChar"]],			typePointer[TypeSpecifier["CUnsignedChar"]],
					FFIType[LiteralType["CSignedChar"]],				typePointer[TypeSpecifier["CSignedChar"]],
					FFIType[LiteralType["CUnsignedShort"]],			typePointer[TypeSpecifier["CUnsignedShort"]],
					FFIType[LiteralType["CShort"]],							typePointer[TypeSpecifier["CShort"]],
					FFIType[LiteralType["CUnsignedInt"]],				typePointer[TypeSpecifier["CUnsignedInt"]],
					FFIType[LiteralType["CInt"]],								typePointer[TypeSpecifier["CInt"]],
					FFIType[LiteralType["CUnsignedLong"]],			typePointer[TypeSpecifier["CUnsignedLong"]],
					FFIType[LiteralType["CLong"]],							typePointer[TypeSpecifier["CLong"]],
					FFIType[LiteralType["OpaqueRawPointer"]],		typePointer[TypeSpecifier["OpaqueRawPointer"]],
					_, 																					Native`ThrowWolframExceptionCode["Unimplemented"]

				]
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
				UnwrapRawObject[Cast[init, "RawObject"::["OpaqueRawPointer"]]]
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


	FunctionDeclaration[pointerExpression,
		Typed[ForAllType[ty, {"TypeSpecifier"::[ty], "OpaqueRawPointer"} -> "InertExpression"]]@
		Function[{type, ptr},
			Cast[FromRawPointer@Cast[ptr, "RawPointer"::[type], "BitCast"], "InertExpression"]
		]
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"TypeSpecifier"::["Void"], "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			InertExpression[Null]
		]
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"TypeSpecifier"::["OpaqueRawPointer"], "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			Cast[CreateRawObject@FromRawPointer@Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"], "InertExpression"]
		]
	],

	FunctionDeclaration[pointerExpression,
		Typed[{"FFIType", "OpaqueRawPointer"} -> "InertExpression"]@
		Function[{type, ptr},
			Switch[type,

					FFIType[LiteralType["Void"]],								pointerExpression[TypeSpecifier["Void"], ptr],
					FFIType[LiteralType["UnsignedInteger8"]],		pointerExpression[TypeSpecifier["UnsignedInteger8"], ptr],
					FFIType[LiteralType["Integer8"]],						pointerExpression[TypeSpecifier["Integer8"], ptr],
					FFIType[LiteralType["UnsignedInteger16"]],	pointerExpression[TypeSpecifier["UnsignedInteger16"], ptr],
					FFIType[LiteralType["Integer16"]],					pointerExpression[TypeSpecifier["Integer16"], ptr],
					FFIType[LiteralType["UnsignedInteger32"]],	pointerExpression[TypeSpecifier["UnsignedInteger32"], ptr],
					FFIType[LiteralType["Integer32"]],					pointerExpression[TypeSpecifier["Integer32"], ptr],
					FFIType[LiteralType["UnsignedInteger64"]],	pointerExpression[TypeSpecifier["UnsignedInteger64"], ptr],
					FFIType[LiteralType["Integer64"]],					pointerExpression[TypeSpecifier["Integer64"], ptr],
					FFIType[LiteralType["CFloat"]],							pointerExpression[TypeSpecifier["CFloat"], ptr],
					FFIType[LiteralType["CDouble"]],						pointerExpression[TypeSpecifier["CDouble"], ptr],
					FFIType[LiteralType["CUnsignedChar"]],			pointerExpression[TypeSpecifier["CUnsignedChar"], ptr],
					FFIType[LiteralType["CSignedChar"]],				pointerExpression[TypeSpecifier["CSignedChar"], ptr],
					FFIType[LiteralType["CUnsignedShort"]],			pointerExpression[TypeSpecifier["CUnsignedShort"], ptr],
					FFIType[LiteralType["CShort"]],							pointerExpression[TypeSpecifier["CShort"], ptr],
					FFIType[LiteralType["CUnsignedInt"]],				pointerExpression[TypeSpecifier["CUnsignedInt"], ptr],
					FFIType[LiteralType["CInt"]],								pointerExpression[TypeSpecifier["CInt"], ptr],
					FFIType[LiteralType["CUnsignedLong"]],			pointerExpression[TypeSpecifier["CUnsignedLong"], ptr],
					FFIType[LiteralType["CLong"]],							pointerExpression[TypeSpecifier["CLong"], ptr],
					FFIType[LiteralType["OpaqueRawPointer"]],		pointerExpression[TypeSpecifier["OpaqueRawPointer"], ptr],
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

			pointerExpression[ff["OutputType"], ff["OutputPointer"]]
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
