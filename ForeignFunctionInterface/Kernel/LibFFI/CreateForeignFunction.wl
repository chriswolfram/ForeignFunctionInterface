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
		CreateFFICallInterface[funcType]
			creates an FFICallInterface with the specified type signature.

		The caller is responsible for freeing the call interface once it is no longer needed.
	*)
	FunctionDeclaration[CreateFFICallInterface,
		Typed[{"InertExpression"} -> "FFICallInterface"]@
		Function[funcType,
			Module[{argTypes, outputType, argCount, argFFITypes, outputFFIType, cif},

				(* Check that the input type is well-formed *)
				If[
					Head[funcType] =!= InertExpression[Rule] ||
					Length[funcType] =!= 2 ||
					Head[First[funcType]] =!= InertExpression[List],
					Native`ThrowWolframExceptionCode["Argument"]
				];

				argTypes = First[funcType];
				outputType = Last[funcType];
				argCount = Length[argTypes];

				argFFITypes = CreateTypeInstance["CArray"::["FFIType"], argCount];
				Do[
					ToRawPointer[argFFITypes, i-1, CreateFFIType[argTypes[[i]]]],
					{i, Length[argTypes]}
				];

				outputFFIType = CreateFFIType[outputType];

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
		Typed[{"ExternalLibraryHandle", "String", "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, funcType},
			Module[{cif, argValuesArray, outputValue, fun},

				cif = CreateFFICallInterface[funcType];

				argValuesArray = CreateTypeInstance["CArray"::["OpaqueRawPointer"], cif["ArgumentCount"]];
				Do[
					ToRawPointer[argValuesArray, i-1,
						Cast[CreateTypeInstance["CArray"::["Integer8"], FFITypeByteCount[FromRawPointer[cif["ArgumentTypes"],i-1]]], "OpaqueRawPointer", "BitCast"]
					],
					{i, cif["ArgumentCount"]}
				];

				(* TODO: This should be at least as big as the ffi_arg type *)
				outputValue =
					Cast[
						CreateTypeInstance["CArray"::["Integer8"], Max[Native`SizeOf[TypeSpecifier["OpaqueRawPointer"]], FFITypeByteCount[cif["OutputType"]]]],
						"OpaqueRawPointer", "BitCast"
					];

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
		Typed[{"String", "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{funName, funcType},
			CreateForeignFunctionWithLibrary[
				LibraryFunction["get_RTLD_DEFAULT"][],
				funName,
				funcType
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
