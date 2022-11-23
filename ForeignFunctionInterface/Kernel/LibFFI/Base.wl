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
			"ArgumentTypes" -> "Managed"::["CArray"::["FFIType"]],
			"OutputType" -> "FFIType",
			"CallInterface" -> "Managed"::["FFICallInterface"],
			"FunctionPointer" -> "OpaqueRawPointer"
		|>,
		"AbstractTypes" -> {"DataStructures"}
	],

	FunctionDeclaration[CreateForeignFunctionWithLibrary,
		Typed[{"ExternalLibraryHandle", "String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, returnType},
			Module[{cif, argArray, fun},
				argArray = CreateTypeInstance["Managed"::["CArray"::["FFIType"]], argTypes];

				cif = CreateTypeInstance["Managed", LibraryFunction["create_ffi_cif"][]];
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","CCast"], returnType, argArray];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				If[fun === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"]
				];

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argArray,
					"OutputType" -> returnType,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],

	FunctionDeclaration[CreateForeignFunction,
		Typed[{"String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{funName, argTypes, returnType},
			(* TODO: The value for RTLD_DEFAULT might be platform specific *)
			CreateForeignFunctionWithLibrary[
				Cast[0,"ExternalLibraryHandle","BitCast"] (*RTLD_DEFAULT*),
				funName,
				argTypes,
				returnType
			]
		]
	],

	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "ListVector"::["InertExpression"]} -> "InertExpression"]@
		Function[{ff, args},
			Module[{out, argArray},

				out = Switch[ff["OutputType"],

					FFIType["Void"][],							Cast[0, "OpaqueRawPointer", "BitCast"],
					FFIType["UnsignedInteger8"][],	Cast[Typed[ToRawPointer[], "RawPointer"::["UnsignedInteger8"]], "OpaqueRawPointer", "BitCast"],
					FFIType["Integer8"][],					Cast[Typed[ToRawPointer[], "RawPointer"::["Integer8"]], "OpaqueRawPointer", "BitCast"],
					FFIType["UnsignedInteger16"][],	Cast[Typed[ToRawPointer[], "RawPointer"::["UnsignedInteger16"]], "OpaqueRawPointer", "BitCast"],
					FFIType["Integer16"][],					Cast[Typed[ToRawPointer[], "RawPointer"::["Integer16"]], "OpaqueRawPointer", "BitCast"],
					FFIType["UnsignedInteger32"][],	Cast[Typed[ToRawPointer[], "RawPointer"::["UnsignedInteger32"]], "OpaqueRawPointer", "BitCast"],
					FFIType["Integer32"][],					Cast[Typed[ToRawPointer[], "RawPointer"::["Integer32"]], "OpaqueRawPointer", "BitCast"],
					FFIType["UnsignedInteger64"][],	Cast[Typed[ToRawPointer[], "RawPointer"::["UnsignedInteger64"]], "OpaqueRawPointer", "BitCast"],
					FFIType["Integer64"][],					Cast[Typed[ToRawPointer[], "RawPointer"::["Integer64"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CFloat"][],						Cast[Typed[ToRawPointer[], "RawPointer"::["CFloat"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CDouble"][],						Cast[Typed[ToRawPointer[], "RawPointer"::["CDouble"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CUnsignedChar"][],			Cast[Typed[ToRawPointer[], "RawPointer"::["CUnsignedChar"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CSignedChar"][],				Cast[Typed[ToRawPointer[], "RawPointer"::["CSignedChar"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CUnsignedShort"][],		Cast[Typed[ToRawPointer[], "RawPointer"::["CUnsignedShort"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CShort"][],						Cast[Typed[ToRawPointer[], "RawPointer"::["CShort"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CUnsignedInt"][],			Cast[Typed[ToRawPointer[], "RawPointer"::["CUnsignedInt"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CInt"][],							Cast[Typed[ToRawPointer[], "RawPointer"::["CInt"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CUnsignedLong"][],			Cast[Typed[ToRawPointer[], "RawPointer"::["CUnsignedLong"]], "OpaqueRawPointer", "BitCast"],
					FFIType["CLong"][],							Cast[Typed[ToRawPointer[], "RawPointer"::["CLong"]], "OpaqueRawPointer", "BitCast"],
					FFIType["OpaqueRawPointer"][],	Cast[Typed[ToRawPointer[], "RawPointer"::["OpaqueRawPointer"]], "OpaqueRawPointer", "BitCast"],
					_, 															Native`ThrowWolframExceptionCode["Unimplemented"]

				];

				argArray = CreateTypeInstance["Managed"::["CArray"::["OpaqueRawPointer"]], Length[args]];
				Do[
					Module[{arg},

						arg = Switch[FromRawPointer[ff["ArgumentTypes"],i-1],

							(* "Void" is removed *)
							FFIType["UnsignedInteger8"][],	Cast[ToRawPointer[Cast[args[[i]], "UnsignedInteger8"]], "OpaqueRawPointer", "BitCast"],
							FFIType["Integer8"][],					Cast[ToRawPointer[Cast[args[[i]], "Integer8"]], "OpaqueRawPointer", "BitCast"],
							FFIType["UnsignedInteger16"][],	Cast[ToRawPointer[Cast[args[[i]], "UnsignedInteger16"]], "OpaqueRawPointer", "BitCast"],
							FFIType["Integer16"][],					Cast[ToRawPointer[Cast[args[[i]], "Integer16"]], "OpaqueRawPointer", "BitCast"],
							FFIType["UnsignedInteger32"][],	Cast[ToRawPointer[Cast[args[[i]], "UnsignedInteger32"]], "OpaqueRawPointer", "BitCast"],
							FFIType["Integer32"][],					Cast[ToRawPointer[Cast[args[[i]], "Integer32"]], "OpaqueRawPointer", "BitCast"],
							FFIType["UnsignedInteger64"][],	Cast[ToRawPointer[Cast[args[[i]], "UnsignedInteger64"]], "OpaqueRawPointer", "BitCast"],
							FFIType["Integer64"][],					Cast[ToRawPointer[Cast[args[[i]], "Integer64"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CFloat"][],						Cast[ToRawPointer[Cast[args[[i]], "CFloat"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CDouble"][],						Cast[ToRawPointer[Cast[args[[i]], "CDouble"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CUnsignedChar"][],			Cast[ToRawPointer[Cast[args[[i]], "CUnsignedChar"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CSignedChar"][],				Cast[ToRawPointer[Cast[args[[i]], "CSignedChar"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CUnsignedShort"][],		Cast[ToRawPointer[Cast[args[[i]], "CUnsignedShort"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CShort"][],						Cast[ToRawPointer[Cast[args[[i]], "CShort"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CUnsignedInt"][],			Cast[ToRawPointer[Cast[args[[i]], "CUnsignedInt"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CInt"][],							Cast[ToRawPointer[Cast[args[[i]], "CInt"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CUnsignedLong"][],			Cast[ToRawPointer[Cast[args[[i]], "CUnsignedLong"]], "OpaqueRawPointer", "BitCast"],
							FFIType["CLong"][],							Cast[ToRawPointer[Cast[args[[i]], "CLong"]], "OpaqueRawPointer", "BitCast"],
							FFIType["OpaqueRawPointer"][],	Cast[ToRawPointer[UnwrapRawObject[Cast[args[[i]], "RawObject"::["OpaqueRawPointer"]]]], "OpaqueRawPointer", "BitCast"],
							_,															Native`ThrowWolframExceptionCode["Unimplemented"]

						];

						ToRawPointer[argArray, i-1, arg]

						],
					{i, Length[args]}
				];

				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], Cast[out,"OpaqueRawPointer","BitCast"], argArray];

				Switch[ff["OutputType"],

					FFIType["Void"][],							InertExpression[Null],
					FFIType["UnsignedInteger8"][],	Cast[FromRawPointer@Cast[out, "RawPointer"::["UnsignedInteger8"], "BitCast"], "InertExpression"],
					FFIType["Integer8"][],					Cast[FromRawPointer@Cast[out, "RawPointer"::["Integer8"], "BitCast"], "InertExpression"],
					FFIType["UnsignedInteger16"][],	Cast[FromRawPointer@Cast[out, "RawPointer"::["UnsignedInteger16"], "BitCast"], "InertExpression"],
					FFIType["Integer16"][],					Cast[FromRawPointer@Cast[out, "RawPointer"::["Integer16"], "BitCast"], "InertExpression"],
					FFIType["UnsignedInteger32"][],	Cast[FromRawPointer@Cast[out, "RawPointer"::["UnsignedInteger32"], "BitCast"], "InertExpression"],
					FFIType["Integer32"][],					Cast[FromRawPointer@Cast[out, "RawPointer"::["Integer32"], "BitCast"], "InertExpression"],
					FFIType["UnsignedInteger64"][],	Cast[FromRawPointer@Cast[out, "RawPointer"::["UnsignedInteger64"], "BitCast"], "InertExpression"],
					FFIType["Integer64"][],					Cast[FromRawPointer@Cast[out, "RawPointer"::["Integer64"], "BitCast"], "InertExpression"],
					FFIType["CFloat"][],						Cast[FromRawPointer@Cast[out, "RawPointer"::["CFloat"], "BitCast"], "InertExpression"],
					FFIType["CDouble"][],						Cast[FromRawPointer@Cast[out, "RawPointer"::["CDouble"], "BitCast"], "InertExpression"],
					FFIType["CUnsignedChar"][],			Cast[FromRawPointer@Cast[out, "RawPointer"::["CUnsignedChar"], "BitCast"], "InertExpression"],
					FFIType["CSignedChar"][],				Cast[FromRawPointer@Cast[out, "RawPointer"::["CSignedChar"], "BitCast"], "InertExpression"],
					FFIType["CUnsignedShort"][],		Cast[FromRawPointer@Cast[out, "RawPointer"::["CUnsignedShort"], "BitCast"], "InertExpression"],
					FFIType["CShort"][],						Cast[FromRawPointer@Cast[out, "RawPointer"::["CShort"], "BitCast"], "InertExpression"],
					FFIType["CUnsignedInt"][],			Cast[FromRawPointer@Cast[out, "RawPointer"::["CUnsignedInt"], "BitCast"], "InertExpression"],
					FFIType["CInt"][],							Cast[FromRawPointer@Cast[out, "RawPointer"::["CInt"], "BitCast"], "InertExpression"],
					FFIType["CUnsignedLong"][],			Cast[FromRawPointer@Cast[out, "RawPointer"::["CUnsignedLong"], "BitCast"], "InertExpression"],
					FFIType["CLong"][],							Cast[FromRawPointer@Cast[out, "RawPointer"::["CLong"], "BitCast"], "InertExpression"],
					FFIType["OpaqueRawPointer"][],	Cast[CreateRawObject@FromRawPointer@Cast[out, "RawPointer"::["OpaqueRawPointer"], "BitCast"], "InertExpression"],
					_, 															Native`ThrowWolframExceptionCode["Unimplemented"]

				]

			]
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
