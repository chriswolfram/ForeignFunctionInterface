BeginPackage["ForeignFunctionInterface`LibFFI`RawFunctionLoading`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`LibFFI`"
}]


Begin["`Private`"]


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
			With[{ptr = LibraryFunction["dlopen"][Cast[libName, "Managed"::["CString"]], Typed[1,"CInt"](*RTLD_LAZY*)]},
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
