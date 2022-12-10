BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]


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
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunctionWithLibrary,
	CreateForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
