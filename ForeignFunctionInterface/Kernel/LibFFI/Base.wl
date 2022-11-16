BeginPackage["ForeignFunctionInterface`LibFFI`Base`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`LibFFI`"
}]


Begin["`Private`"]



DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "FFICallInterface", "OpaqueRawPointer"],

	LibraryFunctionDeclaration["create_ffi_cif", $LibFFIPaths,
		{} -> "FFICallInterface"],

	LibraryFunctionDeclaration["free_ffi_cif", $LibFFIPaths,
		{"FFICallInterface"} -> "Void"],

	LibraryFunctionDeclaration["prepare_ffi_cif", $LibFFIPaths,
		{"FFICallInterface", "CUnsignedInt", "FFIType", "CArray"::["FFIType"]} -> "CInt"],

	LibraryFunctionDeclaration["ffi_call", $LibFFIPaths,
		{"FFICallInterface", "OpaqueRawPointer", "OpaqueRawPointer", "CArray"::["OpaqueRawPointer"]} -> "Void"],

	LibraryFunctionDeclaration["get_fun_pointer", $LibFFIPaths,
		{} -> "OpaqueRawPointer"],


	FunctionDeclaration[DeleteObject,
		Typed[{"FFICallInterface"} -> "Null"]@
		Function[cif, LibraryFunction["free_ffi_cif"][cif];]
	]

}];



DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "ExternalLibraryHandle", "OpaqueRawPointer"],

	LibraryFunctionDeclaration["dlopen",
		{"CString", "CInt"} -> "ExternalLibraryHandle"],

	LibraryFunctionDeclaration["dlclose",
		{"ExternalLibraryHandle"} -> "CInt"],

	LibraryFunctionDeclaration["dlsym",
		{"ExternalLibraryHandle", "CString"} -> "OpaqueRawPointer"],

	FunctionDeclaration[DeleteObject,
		Typed[{"ExternalLibraryHandle"} -> "Null"]@
		Function[arg, Null(*LibraryFunction["dlclose"][arg];*)]
	]

}]



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

	FunctionDeclaration[LoadExternalLibrary,
		Typed[{"String"} -> "Managed"::["ExternalLibraryHandle"]]@
		Function[libName,
			CreateTypeInstance["Managed",
				LibraryFunction["dlopen"][Cast[libName, "Managed"::["CString"]], Typed[1,"CInt"](*RTLD_LAZY*)]
			]
			(* TODO: check for NULL *)
		]
	],

	FunctionDeclaration[CreateForeignFunction,
		Typed[{"Managed"::["ExternalLibraryHandle"], "String", "ListVector"::["FFIType"], "FFIType"} -> "ForeignFunctionObject"]@
		Function[{lib, funName, argTypes, returnType},
			Module[{cif, argArray, fun},
				argArray = CreateTypeInstance["Managed"::["CArray"::["FFIType"]], argTypes];

				cif = CreateTypeInstance["Managed", LibraryFunction["create_ffi_cif"][]];
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","CCast"], returnType, argArray];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				(* TODO: check for NULL *)

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argArray,
					"OutputType" -> returnType,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],

	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "ListVector"::["InertExpression"]} -> "InertExpression"]@
		Function[{ff, args},
			Module[{out, argArray},

				Echo[0];

				out = Switch[ff["OutputType"],

					GetFFIType["CInt"], Cast[Typed[ToRawPointer[], "RawPointer"::["CInt"]], "OpaqueRawPointer", "BitCast"],
					GetFFIType["CDouble"], Cast[Typed[ToRawPointer[], "RawPointer"::["CDouble"]], "OpaqueRawPointer", "BitCast"],
					GetFFIType["CLong"], Cast[Typed[ToRawPointer[], "RawPointer"::["CLong"]], "OpaqueRawPointer", "BitCast"],
					_, Native`ThrowWolframExceptionCode["Unimplemented"]

				];
				Echo[1];
				argArray = CreateTypeInstance["Managed"::["CArray"::["OpaqueRawPointer"]], Length[args]];
				Do[
					Module[{arg},

						arg = Switch[FromRawPointer[ff["ArgumentTypes"],i-1],

							GetFFIType["CInt"], Cast[ToRawPointer[Cast[args[[i]], "CInt"]], "OpaqueRawPointer", "BitCast"],
							GetFFIType["CDouble"], Cast[ToRawPointer[Cast[args[[i]], "CDouble"]], "OpaqueRawPointer", "BitCast"],
							GetFFIType["CLong"], Cast[ToRawPointer[Cast[args[[i]], "CLong"]], "OpaqueRawPointer", "BitCast"],
							_, Native`ThrowWolframExceptionCode["Unimplemented"]

						];

						ToRawPointer[argArray, i-1, arg]

						],
					{i, Length[args]}
				];

				Echo[2];

				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], Cast[out,"OpaqueRawPointer","BitCast"], argArray];

				Switch[ff["OutputType"],

					GetFFIType["CInt"], Cast[FromRawPointer@Cast[out, "RawPointer"::["CInt"], "BitCast"], "InertExpression"],
					GetFFIType["CDouble"], Cast[FromRawPointer@Cast[out, "RawPointer"::["CDouble"], "BitCast"], "InertExpression"],
					GetFFIType["CLong"], Cast[FromRawPointer@Cast[out, "RawPointer"::["CLong"], "BitCast"], "InertExpression"],
					_, Native`ThrowWolframExceptionCode["Unimplemented"]

				]

			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	LoadExternalLibrary,
	CreateForeignFunction,
	CallForeignFunction
}];



DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "FFIType", "OpaqueRawPointer", "AbstractTypes" -> {"DataStructures"}],

	LibraryFunctionDeclaration[GetFFIType["Void"] -> "get_ffi_type_void", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["UnsignedInteger8"] -> "get_ffi_type_uint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["Integer8"] -> "get_ffi_type_sint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["UnsignedInteger16"] -> "get_ffi_type_uint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["Integer16"] -> "get_ffi_type_sint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["UnsignedInteger32"] -> "get_ffi_type_uint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["Integer32"] -> "get_ffi_type_sint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["UnsignedInteger64"] -> "get_ffi_type_uint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["Integer64"] -> "get_ffi_type_sint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CFloat"] -> "get_ffi_type_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CDouble"] -> "get_ffi_type_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CUnsignedChar"] -> "get_ffi_type_uchar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CSignedChar"] -> "get_ffi_type_schar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CUnsignedShort"] -> "get_ffi_type_ushort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CShort"] -> "get_ffi_type_sshort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CUnsignedInt"] -> "get_ffi_type_uint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CInt"] -> "get_ffi_type_sint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CUnsignedLong"] -> "get_ffi_type_ulong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["CLong"] -> "get_ffi_type_slong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_longdouble", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[GetFFIType["OpaqueRawPointer"] -> "get_ffi_type_pointer", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_longdouble", $LibFFIPaths,
		{} -> "FFIType"],


	FunctionDeclaration[SameQ,
		Typed[{"FFIType", "FFIType"} -> "Boolean"]@
		Function[{ty1, ty2},
			Cast[ty1,"OpaqueRawPointer","BitCast"] === Cast[ty2,"OpaqueRawPointer","BitCast"]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	GetFFIType2 ->
		Function[Typed[name, "String"],
			Switch[name,
				"Void", 							GetFFIType["Void"][],
				"UnsignedInteger8", 	GetFFIType["UnsignedInteger8"][],
				"Integer8", 					GetFFIType["Integer8"][],
				"UnsignedInteger16", 	GetFFIType["UnsignedInteger16"][],
				"Integer16", 					GetFFIType["Integer16"][],
				"UnsignedInteger32", 	GetFFIType["UnsignedInteger32"][],
				"Integer32", 					GetFFIType["Integer32"][],
				"UnsignedInteger64", 	GetFFIType["UnsignedInteger64"][],
				"Integer64", 					GetFFIType["Integer64"][],
				"CFloat", 						GetFFIType["CFloat"][],
				"CDouble", 						GetFFIType["CDouble"][],
				"CUnsignedChar", 			GetFFIType["CUnsignedChar"][],
				"CSignedChar", 				GetFFIType["CSignedChar"][],
				"CUnsignedShort", 		GetFFIType["CUnsignedShort"][],
				"CShort", 						GetFFIType["CShort"][],
				"CUnsignedInt", 			GetFFIType["CUnsignedInt"][],
				"CInt", 							GetFFIType["CInt"][],
				"CUnsignedLong", 			GetFFIType["CUnsignedLong"][],
				"CLong", 							GetFFIType["CLong"][],
				"OpaqueRawPointer", 	GetFFIType["OpaqueRawPointer"][],
				_,										Native`ThrowWolframExceptionCode["Unimplemented"]
			]
		]
|>];



End[] (* End `Private` *)

EndPackage[]
