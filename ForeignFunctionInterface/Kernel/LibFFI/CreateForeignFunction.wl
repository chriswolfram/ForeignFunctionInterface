BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]

CreateFFICallInterface
DeleteFFICallInterface

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]


(* TODO: TEMPORARY DECLARATIONS *)
(*
	These declarations should probably be added to the C types that are included with the compiler by default
*)
DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Macro", "CUnsignedChar", "UnsignedInteger8"],
	TypeDeclaration["Macro", "CSignedChar", "Integer8"]

}];


(*********************************************************************************)
(**************************** getFunctionPointer *********************************)
(*********************************************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

	(* GetDLLFunctionPointer is defined in codeDLLTools.mc and used in the RTL *)

	LibraryFunctionDeclaration["GetDLLFunctionPointer",
		{"CString", "CString"} -> "OpaqueRawPointer"],


	(*
		getFunctionPointer[libPath, funcName]
			uses the kernel to get a function pointer from a library.

			Both inputs must be UTF8 encoded.

			libPath must be a full path (as returned by FindLibrary).
	*)

	FunctionDeclaration[getFunctionPointer,
		Typed[{"String", "String"} -> "OpaqueRawPointer"]@
		Function[{libPath, funcName},
			LibraryFunction["GetDLLFunctionPointer"][
				Cast[funcName, "Managed"::["CString"]],
				Cast[libPath, "Managed"::["CString"]]
			]
		]
	]

}];


(*********************************************************************************)
(*************************** ForeignFunctionObjects ******************************)
(*********************************************************************************)


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
		Function[funcTypeI,
			Module[{funcType, argTypes, outputType, argCount, argFFITypes, outputFFIType, cif},

				funcType = unwrapTypeSpecifier[funcTypeI];

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

	FunctionDeclaration[unwrapTypeSpecifier,
		Typed[{"InertExpression"} -> "InertExpression"]@
		Function[expr,
			If[Head[expr] === InertExpression[TypeSpecifier] && Length[expr] === 1,
				unwrapTypeSpecifier[First[expr]],
				expr
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


	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "String", "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{libPath, funName, funcType},
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

				(* TODO: Should return library error *)
				fun = getFunctionPointer[libPath, funName];
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
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateForeignFunction
}];



End[] (* End `Private` *)

EndPackage[]
