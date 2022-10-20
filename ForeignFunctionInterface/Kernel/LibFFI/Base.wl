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
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","ReinterpretCast"], returnType, argArray];

				fun = LibraryFunction["dlsym"][lib, Cast[funName, "Managed"::["CString"]]];
				(* TODO: check for NULL *)

				CreateTypeInstance["ForeignFunctionObject", <|
					"ArgumentTypes" -> argArray,
					"CallInterface" -> cif,
					"FunctionPointer" -> fun
				|>]
			]
		]
	],

	FunctionDeclaration[CallForeignFunction,
		Typed[{"ForeignFunctionObject", "ListVector"::["CInt"]} -> "CInt"]@
		Function[{ff, args},
			Module[{out, argArray},
				out = Typed[ToRawPointer[], "RawPointer"::["CInt"]];
				argArray = CreateTypeInstance["Managed"::["CArray"::["OpaqueRawPointer"]], Length[args]];
				Do[
					ToRawPointer[argArray, i-1,
						Cast[ToRawPointer[args[[i]]], "OpaqueRawPointer", "BitCast"]],
					{i, Length[args]}
				];
				LibraryFunction["ffi_call"][ff["CallInterface"], ff["FunctionPointer"], Cast[out,"OpaqueRawPointer","BitCast"], argArray];
				FromRawPointer[out]
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

	LibraryFunctionDeclaration["get_ffi_type_sint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_double", $LibFFIPaths,
		{} -> "FFIType"],

	FunctionDeclaration[GetFFITypeSignedInt32,
		Typed[{} -> "FFIType"]@
		Function[{}, LibraryFunction["get_ffi_type_sint32"][]]
	],

	FunctionDeclaration[GetFFITypeDouble,
		Typed[{} -> "FFIType"]@
		Function[{}, LibraryFunction["get_ffi_type_double"][]]
	]	

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	GetFFITypeSignedInt32,
	GetFFITypeDouble
}];



End[] (* End `Private` *)

EndPackage[]
