BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`BaseTypes`"]


(***************************************************)
(****************** libffi types *******************)
(***************************************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(* Based on ffi_cif from ffi.h *)
	TypeDeclaration["Product", "FFICallInterface", <|
			"ABI" -> "CInt",
			"ArgumentCount" -> "CUnsignedInt",
			"ArgumentTypes" -> "CArray"::["FFIType"],
			"OutputType" -> "FFIType",
			"Bytes" -> "CUnsignedInt",
			"Flags" -> "CUnsignedInt"
		|>,
		"AbstractTypes" -> {"DataStructures"},
		"ReferenceSemantics" -> True,
		"MemoryManaged" -> False
	],

	(* Based on ffi_type from ffi.h *)
	TypeDeclaration["Product", "FFIType", <|
			"Size" -> "CSizeT",
			"Alignment" -> "CUnsignedShort",
			"Type" -> "CUnsignedShort",
			"Elements" -> "CArray"::["FFIType"]
		|>,
		"AbstractTypes" -> {"DataStructures"},
		"ReferenceSemantics" -> True,
		"MemoryManaged" -> False
	],

	LibraryFunctionDeclaration["ffi_prep_cif", $LibFFIPaths,
		{"FFICallInterface", "CInt", "CUnsignedInt", "FFIType", "CArray"::["FFIType"]} -> "CInt"],

	LibraryFunctionDeclaration["ffi_call", $LibFFIPaths,
		{"FFICallInterface", "OpaqueRawPointer", "OpaqueRawPointer", "CArray"::["OpaqueRawPointer"]} -> "Void"],

	LibraryFunctionDeclaration["ffi_closure_alloc", $LibFFIPaths,
		{"CSizeT", "RawPointer"::["OpaqueRawPointer"]} -> "OpaqueRawPointer"],

	LibraryFunctionDeclaration["ffi_closure_free", $LibFFIPaths,
		{"OpaqueRawPointer"} -> "Void"],

	LibraryFunctionDeclaration["ffi_prep_closure_loc", $LibFFIPaths,
		{
			"OpaqueRawPointer", (* ffi_closure* closure *)
			"FFICallInterface", (* ffi_cif* cif *)
			"OpaqueRawPointer",(*"RawFunction"::[{"FFICallInterface", "OpaqueRawPointer", "CArray"::["OpaqueRawPointer"], "OpaqueRawPointer"} -> "Void"],*) (* void ( *fun) (ffi_cif* cif, void* ret, void** args, void* user_data) *)
			"OpaqueRawPointer", (* void* user_data *)
			"OpaqueRawPointer" (* void* codeloc *)
		} -> "CInt"
	],

	LibraryFunctionDeclaration["ffi_get_struct_offsets", $LibFFIPaths,
		{"CInt", "FFIType", "CArray"::["CSizeT"]} -> "CInt"],


	FunctionDeclaration[SameQ,
		Typed[{"FFIType", "FFIType"} -> "Boolean"]@
		Function[{ty1, ty2},
			Cast[ty1,"OpaqueRawPointer","BitCast"] === Cast[ty2,"OpaqueRawPointer","BitCast"]
		]
	]

}];



End[] (* End `Private` *)

EndPackage[]
