BeginPackage["ForeignFunctionInterface`LibFFI`RawFunctionLoading`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`LibFFI`"
}]


Begin["`Private`"]


(***************************************************)
(***************** dlopen / dlsym ******************)
(***************************************************)

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
	],
	(* TODO: dlclose never gets called... *)


		FunctionDeclaration[LoadExternalLibrary,
		Typed[{"String"} -> "Managed"::["ExternalLibraryHandle"]]@
		Function[libName,
			CreateTypeInstance["Managed",
				LibraryFunction["dlopen"][Cast[libName, "Managed"::["CString"]], Typed[1,"CInt"](*RTLD_LAZY*)]
			]
			(* TODO: check for NULL *)
		]
	]

}]


End[] (* End `Private` *)

EndPackage[]
