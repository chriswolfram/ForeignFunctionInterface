BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


(***************************************************)
(***************** dlopen / dlsym ******************)
(***************************************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "ExternalLibraryHandle", "OpaqueRawPointer", "AbstractTypes" -> {"DataStructures"}],

	LibraryFunctionDeclaration["dlopen",
		{"CString", "CInt"} -> "ExternalLibraryHandle"],

	LibraryFunctionDeclaration["dlclose",
		{"ExternalLibraryHandle"} -> "CInt"],

	LibraryFunctionDeclaration["dlsym",
		{"ExternalLibraryHandle", "CString"} -> "OpaqueRawPointer"],


	FunctionDeclaration[LoadExternalLibrary,
		Typed[{"String"} -> "ExternalLibraryHandle"]@
		Function[libName,
			With[{ptr = LibraryFunction["dlopen"][Cast[libName, "Managed"::["CString"]], LibraryFunction["get_RTLD_LAZY"][]]},
				If[Cast[ptr, "OpaqueRawPointer", "BitCast"] === Cast[0, "OpaqueRawPointer", "BitCast"],
					Native`ThrowWolframExceptionCode["System"],
					ptr
				]
			]
		]
	],

	FunctionDeclaration[UnloadExternalLibrary,
		Typed[{"ExternalLibraryHandle"} -> "Null"]@
		Function[lib,
			LibraryFunction["dlclose"][lib];
		]
	]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	LoadExternalLibrary,
	UnloadExternalLibrary
}];


End[] (* End `Private` *)

EndPackage[]
