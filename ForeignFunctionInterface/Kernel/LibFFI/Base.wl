BeginPackage["ForeignFunctionInterface`LibFFI`Base`", {
	"ForeignFunctionInterface`"
}]

Begin["`Private`"]


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "FFICIF", "OpaqueRawPointer"],

	LibraryFunctionDeclaration["create_ffi_cif",
		{} -> "FFICIF"],

	LibraryFunctionDeclaration["free_ffi_cif",
		{"FFICIF"} -> "Void"],

	LibraryFunctionDeclaration["prepare_ffi_cif",
		{"FFICIF", "CUnsignedInt", "FFIType", "CArray"::["FFIType"]} -> "CInt"],


	FunctionDeclaration[DeleteObject,
		Typed[{"FFICIF"} -> "Null"]@
		Function[cif, LibraryFunction["free_ffi_cif"][cif];]
	],


	FunctionDeclaration[CreateCallInterface,
		Typed[{} -> "CInt"]@
		Function[{},
			Module[{argTypes, returnType, cif, argArray},
				argTypes = {LibraryFunction["get_ffi_type_sint32"][], LibraryFunction["get_ffi_type_sint32"][]};
				returnType = LibraryFunction["get_ffi_type_sint32"][];
				argArray = CreateTypeInstance["Managed"::["CArray"::["FFIType"]], argTypes];

				cif = LibraryFunction["create_ffi_cif"][];
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","ReinterpretCast"], returnType, argArray]
			]
		]
	]

	(* FunctionDeclaration[CreateCallInterface,
		Typed[{"ListVector"::["FFIType"], "FFIType"} -> "Managed"::["FFICIF"]]@
		Function[{argTypes, returnType},
			Module[{cif, argArray},
				argArray = CreateTypeInstance["Managed"::["CArray"::["FFIType"]], argTypes];

				cif = LibraryFunction["create_ffi_cif"][];
				LibraryFunction["prepare_ffi_cif"][cif, Cast[Length[argTypes],"CUnsignedInt","ReinterpretCast"], returnType, argArray];

				CreateTypeInstance["Managed", cif]
			]
		]
	] *)

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	CreateCallInterface
}];


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "FFIType", "OpaqueRawPointer", "AbstractTypes" -> {"DataStructures"}],

	LibraryFunctionDeclaration["get_ffi_type_sint32",
		{} -> "FFIType"],

	FunctionDeclaration[GetFFITypeSignedInt32,
		Typed[{} -> "FFIType"]@
		Function[{}, LibraryFunction["get_ffi_type_sint32"][]]
	]	

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	GetFFITypeSignedInt32
}];


DeclareCompiledComponent["ForeignFunctionInterface", "Prologs" -> {
	LibraryLoad["/usr/lib/x86_64-linux-gnu/libffi.so"]&,
	LibraryLoad["libFFIInterface.so"]&
}];


End[] (* End `Private` *)

EndPackage[]
