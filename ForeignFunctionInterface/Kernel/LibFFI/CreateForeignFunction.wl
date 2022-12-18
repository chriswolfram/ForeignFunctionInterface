BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]

CreateFFICallInterface
DeleteFFICallInterface

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
			"ArgumentPointers" -> "CArray"::["OpaqueRawPointer"],
			"OutputPointer" -> "OpaqueRawPointer",
			"CallInterface" -> "FFICallInterface",
			"FunctionPointer" -> "OpaqueRawPointer"
		|>,
		"AbstractTypes" -> {"DataStructures"}
	],


	FunctionDeclaration[CompilerCallback["OnFree"],
		Typed[{"ForeignFunctionObject"} -> "Null"]@
		Function[ff,
			Do[
				DeleteObject@Cast[FromRawPointer[ff["ArgumentPointers"], i-1], "CArray"::["Integer8"], "BitCast"],
				{i, ff["CallInterface"]["ArgumentCount"]}
			];
			DeleteObject[ff["ArgumentPointers"]];
			DeleteObject[Cast[ff["OutputPointer"], "CArray"::["Integer8"], "BitCast"]];
			DeleteFFICallInterface[ff["CallInterface"]];
		]
	],


	(*
		CreateFFICallInterface[argTypes, outputType]
			creates an FFICallInterface with the specified argument and output types.

		CreateFFICallInterface[argTypes, argCount, outputType]
			takes a CArray of FFITypes for the arguments, and an FFIType in the output. Takes ownership of argument memory.

		If the types are passed as FFITypes, then ownership of the types themselves and the array
		containing the arguments will be passed to CreateFFICallInterface.
	*)
	FunctionDeclaration[CreateFFICallInterface,
		Typed[{"ListVector"::["InertExpression"], "InertExpression"} -> "FFICallInterface"]@
		Function[{argTypes, outputType},
			Module[{argFFITypes},

				argFFITypes = CreateTypeInstance["CArray"::["FFIType"], Length[argTypes]];
				Do[
					ToRawPointer[argFFITypes, i-1, CreateFFIType[argTypes[[i]]]],
					{i, Length[argTypes]}
				];

				CreateFFICallInterface[argFFITypes, Length[argTypes], CreateFFIType[outputType]]
			]
		]
	],

	FunctionDeclaration[CreateFFICallInterface,
		Typed[{"CArray"::["FFIType"], "MachineInteger", "FFIType"} -> "FFICallInterface"]@
		Function[{argFFITypes, argCount, outputFFIType},
			Module[{cif},

				(* TODO: check error code *)
				cif = CreateTypeInstance["FFICallInterface", <||>];
				LibraryFunction["ffi_prep_cif"][
					cif,
					LibraryFunction["get_FFI_DEFAULT_ABI"][],
					Cast[argCount,"CUnsignedInt","CCast"],
					outputFFIType,
					argFFITypes
				];

				cif
			]
		]
	],


	FunctionDeclaration[DeleteFFICallInterface,
		Typed[{"FFICallInterface"} -> "Null"]@
		Function[cif,
			Do[DeleteFFIType[FromRawPointer[cif["ArgumentTypes"], i-1]], {i, cif["ArgumentCount"]}];
			DeleteFFIType[cif["OutputType"]];
			DeleteObject[cif["ArgumentTypes"]];
			DeleteObject[cif];
		]
	],


	FunctionDeclaration[CreateForeignFunctionWithLibrary,
		Typed[{"ExternalLibraryHandle", "String", "ListVector"::["InertExpression"], "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, outputType},
			Module[{cif, argCount, argFFITypes, outputFFIType, argValuesArray, outputValue, fun},

				argCount = Length[argTypes];

				argFFITypes = CreateTypeInstance["CArray"::["FFIType"], Length[argTypes]];
				Do[
					ToRawPointer[argFFITypes, i-1, CreateFFIType[argTypes[[i]]]],
					{i, Length[argTypes]}
				];

				outputFFIType = CreateFFIType[outputType];

				argValuesArray = CreateTypeInstance["CArray"::["OpaqueRawPointer"], argCount];
				Do[
					ToRawPointer[argValuesArray, i-1,
						Cast[CreateTypeInstance["CArray"::["Integer8"], FFITypeByteCount[FromRawPointer[argFFITypes,i-1]]], "OpaqueRawPointer", "BitCast"]
					],
					{i, argCount}
				];

				(* TODO: This should be at least as big as the ffi_arg type *)
				outputValue =
					Cast[
						CreateTypeInstance["CArray"::["Integer8"], Max[Native`SizeOf[TypeSpecifier["OpaqueRawPointer"]], FFITypeByteCount[outputFFIType]]],
						"OpaqueRawPointer", "BitCast"
					];

				cif = CreateFFICallInterface[argFFITypes, argCount, outputFFIType];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentPointers" -> argValuesArray,
					"OutputPointer" -> outputValue,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],


	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "ListVector"::["InertExpression"], "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{funName, argTypes, outputType},
			CreateForeignFunctionWithLibrary[
				LibraryFunction["get_RTLD_DEFAULT"][],
				funName,
				argTypes,
				outputType
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunctionWithLibrary,
	CreateForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
