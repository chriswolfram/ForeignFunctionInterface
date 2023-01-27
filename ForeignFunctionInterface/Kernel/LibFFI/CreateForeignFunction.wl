BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`CreateForeignFunction`"]

CreateFFICallInterface
DeleteFFICallInterface

CanonicalizeFunctionType
FunctionTypeArguments
FunctionTypeOutput

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


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
			"FunctionPointer" -> "OpaqueRawPointer",
			"ArgumentTypeExpressions" -> "ListVector"::["InertExpression"]
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
		Typed[{"ListVector"::["InertExpression"], "InertExpression"} -> "FFICallInterface"]@
		Function[{argTypes, outputType},
			Module[{argCount, argFFITypes, outputFFIType, cif},

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

	(* Unwraps a function type and checks that it is well-formed.
	Does not check that all of the subtypes are valid. *)
	FunctionDeclaration[CanonicalizeFunctionType,
		Typed[{"InertExpression"} -> "InertExpression"]@
		Function[expr,
			If[
				Head[expr] =!= InertExpression[Rule] ||
				Length[expr] =!= 2 ||
				Head[First[expr]] =!= InertExpression[List],
				Native`ThrowWolframExceptionCode["Argument"]
			];
			unwrapTypeSpecifier[expr]
		]
	],

	FunctionDeclaration[FunctionTypeArguments,
		Typed[{"InertExpression"} -> "ListVector"::["InertExpression"]]@
		Function[expr,
			With[{argTypesExpr = First[expr]},
				Map[argTypesExpr[[#]]&, Cast[Range[Length[argTypesExpr]], "ListVector"::[_]]]
			]
		]
	],

	FunctionDeclaration[FunctionTypeOutput,
		Typed[{"InertExpression"} -> "InertExpression"]@
		Function[expr,
			Last[expr]
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
		Typed[{"ExternalLibrary", "String", "InertExpression"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, funcTypeI},
			Module[{funcType, argTypes, outputType, cif, argValuesArray, outputValue, fun},

				funcType = CanonicalizeFunctionType[funcTypeI];
				argTypes = FunctionTypeArguments[funcType];
				outputType = FunctionTypeOutput[funcType];

				cif = CreateFFICallInterface[argTypes, outputType];

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

				fun = LibraryFunction["dlsym"][GetExternalLibraryHandle[lib], Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentPointers" -> argValuesArray,
					"OutputPointer" -> outputValue,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun,
					"ArgumentTypeExpressions" -> argTypes
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
