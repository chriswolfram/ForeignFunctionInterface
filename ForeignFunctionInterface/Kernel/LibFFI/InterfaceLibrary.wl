BeginPackage["ForeignFunctionInterface`LibFFI`InterfaceLibrary`", {
	"ForeignFunctionInterface`",
	"ForeignFunctionInterface`LibFFI`"
}]


FFIType


Begin["`Private`"]


(***************************************************)
(********** Interface Library Functions ************)
(***************************************************)

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



(***************************************************)
(********************* Types ***********************)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

	TypeDeclaration["Alias", "FFIType", "OpaqueRawPointer", "AbstractTypes" -> {"DataStructures"}],

	FunctionDeclaration[SameQ,
		Typed[{"FFIType", "FFIType"} -> "Boolean"]@
		Function[{ty1, ty2},
			Cast[ty1,"OpaqueRawPointer","BitCast"] === Cast[ty2,"OpaqueRawPointer","BitCast"]
		]
	],

	LibraryFunctionDeclaration[FFIType["Void"] -> "get_ffi_type_void", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["UnsignedInteger8"] -> "get_ffi_type_uint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["Integer8"] -> "get_ffi_type_sint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["UnsignedInteger16"] -> "get_ffi_type_uint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["Integer16"] -> "get_ffi_type_sint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["UnsignedInteger32"] -> "get_ffi_type_uint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["Integer32"] -> "get_ffi_type_sint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["UnsignedInteger64"] -> "get_ffi_type_uint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["Integer64"] -> "get_ffi_type_sint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CFloat"] -> "get_ffi_type_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CDouble"] -> "get_ffi_type_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CUnsignedChar"] -> "get_ffi_type_uchar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CSignedChar"] -> "get_ffi_type_schar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CUnsignedShort"] -> "get_ffi_type_ushort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CShort"] -> "get_ffi_type_sshort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CUnsignedInt"] -> "get_ffi_type_uint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CInt"] -> "get_ffi_type_sint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CUnsignedLong"] -> "get_ffi_type_ulong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["CLong"] -> "get_ffi_type_slong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_longdouble", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[FFIType["OpaqueRawPointer"] -> "get_ffi_type_pointer", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_longdouble", $LibFFIPaths,
		{} -> "FFIType"]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	GetFFIType ->
		Function[Typed[name, "String"],
			Switch[name,
				"Void", 							FFIType["Void"][],
				"UnsignedInteger8", 	FFIType["UnsignedInteger8"][],
				"Integer8", 					FFIType["Integer8"][],
				"UnsignedInteger16", 	FFIType["UnsignedInteger16"][],
				"Integer16", 					FFIType["Integer16"][],
				"UnsignedInteger32", 	FFIType["UnsignedInteger32"][],
				"Integer32", 					FFIType["Integer32"][],
				"UnsignedInteger64", 	FFIType["UnsignedInteger64"][],
				"Integer64", 					FFIType["Integer64"][],
				"CFloat", 						FFIType["CFloat"][],
				"CDouble", 						FFIType["CDouble"][],
				"CUnsignedChar", 			FFIType["CUnsignedChar"][],
				"CSignedChar", 				FFIType["CSignedChar"][],
				"CUnsignedShort", 		FFIType["CUnsignedShort"][],
				"CShort", 						FFIType["CShort"][],
				"CUnsignedInt", 			FFIType["CUnsignedInt"][],
				"CInt", 							FFIType["CInt"][],
				"CUnsignedLong", 			FFIType["CUnsignedLong"][],
				"CLong", 							FFIType["CLong"][],
				"OpaqueRawPointer", 	FFIType["OpaqueRawPointer"][],
				_,										Native`ThrowWolframExceptionCode["Unimplemented"]
			]
		]
|>];


End[] (* End `Private` *)

EndPackage[]
