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
			"ArgumentTypes" -> "ListVector"::["Managed"::["FFIType"]],
			"OutputType" -> "Managed"::["FFIType"],
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

		Note: The caller is responsible for ensuring that the FFITypes passed in will not be freed
		while the CallInterface is in use.
	*)
	FunctionDeclaration[CreateFFICallInterface,
		Typed[{"ListVector"::["Managed"::["FFIType"]], "Managed"::["FFIType"]} -> "FFICallInterface"]@
		Function[{argTypes, outputType},
			Module[{cif, argTypesArray},

				argTypesArray = CreateTypeInstance["CArray"::["FFIType"], Length[argTypes]];
				Do[
					ToRawPointer[argTypesArray, i-1, Compile`BorrowManagedObject[argTypes[[i]]]],
					{i, Length[argTypes]}
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

				cif
			]
		]
	],


	FunctionDeclaration[DeleteFFICallInterface,
		Typed[{"FFICallInterface"} -> "Null"]@
		Function[cif,
			DeleteObject[cif["ArgumentTypes"]];
			DeleteObject[cif];
		]
	],


	FunctionDeclaration[CreateForeignFunctionWithLibrary,
		Typed[{"ExternalLibraryHandle", "String", "ListVector"::["Managed"::["FFIType"]], "Managed"::["FFIType"]} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, outputType},
			Module[{cif, argCount, argValuesArray, outputValue, fun},

				argCount = Length[argTypes];

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

				cif = CreateFFICallInterface[argTypes, outputType];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argTypes,
					"OutputType" -> outputType,
					"ArgumentPointers" -> argValuesArray,
					"OutputPointer" -> outputValue,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],


	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "ListVector"::["Managed"::["FFIType"]], "Managed"::["FFIType"]} -> "ForeignFunctionObject"]@
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

	FunctionDeclaration[typeSize,
		Typed[{"Managed"::["FFIType"]} -> "MachineInteger"]@
		Function[type,
			typeSize[Compile`BorrowManagedObject[type]]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunctionWithLibrary,
	CreateForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
