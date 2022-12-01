BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]


Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]



(***************************************************)
(**************** dlfcn.h constants ****************)
(***************************************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	LibraryFunctionDeclaration["get_RTLD_DEFAULT", $LibFFIPaths,
		{} -> "ExternalLibraryHandle"],

	LibraryFunctionDeclaration["get_RTLD_LAZY", $LibFFIPaths,
		{} -> "CInt"]

}];



(***************************************************)
(***************** libffi constants ****************)
(***************************************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(* ABIs *)
	LibraryFunctionDeclaration["get_FFI_DEFAULT_ABI", $LibFFIPaths,
		{} -> "CInt"],

	(* ffi_status values *)
	LibraryFunctionDeclaration["get_FFI_OK", $LibFFIPaths,
		{} -> "CInt"],

	LibraryFunctionDeclaration["get_FFI_BAD_TYPEDEF", $LibFFIPaths,
		{} -> "CInt"],

	LibraryFunctionDeclaration["get_FFI_BAD_ARGTYPE", $LibFFIPaths,
		{} -> "CInt"],

	LibraryFunctionDeclaration["get_FFI_BAD_ABI", $LibFFIPaths,
		{} -> "CInt"]

}];



(***************************************************)
(********************* Types ***********************)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

	LibraryFunctionDeclaration[rawFFIType["Void"] -> "get_ffi_type_void", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["UnsignedInteger8"] -> "get_ffi_type_uint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["Integer8"] -> "get_ffi_type_sint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["UnsignedInteger16"] -> "get_ffi_type_uint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["Integer16"] -> "get_ffi_type_sint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["UnsignedInteger32"] -> "get_ffi_type_uint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["Integer32"] -> "get_ffi_type_sint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["UnsignedInteger64"] -> "get_ffi_type_uint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["Integer64"] -> "get_ffi_type_sint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CFloat"] -> "get_ffi_type_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CDouble"] -> "get_ffi_type_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CUnsignedChar"] -> "get_ffi_type_uchar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CSignedChar"] -> "get_ffi_type_schar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CUnsignedShort"] -> "get_ffi_type_ushort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CShort"] -> "get_ffi_type_sshort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CUnsignedInt"] -> "get_ffi_type_uint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CInt"] -> "get_ffi_type_sint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CUnsignedLong"] -> "get_ffi_type_ulong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["CLong"] -> "get_ffi_type_slong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_longdouble", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[rawFFIType["OpaqueRawPointer"] -> "get_ffi_type_pointer", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_longdouble", $LibFFIPaths,
		{} -> "FFIType"],


	FunctionDeclaration[FFIType, Typed[{LiteralType["Void"]} -> "FFIType"]@Function[ty, rawFFIType["Void"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["UnsignedInteger8"]} -> "FFIType"]@Function[ty, rawFFIType["UnsignedInteger8"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["Integer8"]} -> "FFIType"]@Function[ty, rawFFIType["Integer8"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["UnsignedInteger16"]} -> "FFIType"]@Function[ty, rawFFIType["UnsignedInteger16"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["Integer16"]} -> "FFIType"]@Function[ty, rawFFIType["Integer16"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["UnsignedInteger32"]} -> "FFIType"]@Function[ty, rawFFIType["UnsignedInteger32"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["Integer32"]} -> "FFIType"]@Function[ty, rawFFIType["Integer32"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["UnsignedInteger64"]} -> "FFIType"]@Function[ty, rawFFIType["UnsignedInteger64"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["Integer64"]} -> "FFIType"]@Function[ty, rawFFIType["Integer64"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CFloat"]} -> "FFIType"]@Function[ty, rawFFIType["CFloat"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CDouble"]} -> "FFIType"]@Function[ty, rawFFIType["CDouble"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CUnsignedChar"]} -> "FFIType"]@Function[ty, rawFFIType["CUnsignedChar"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CSignedChar"]} -> "FFIType"]@Function[ty, rawFFIType["CSignedChar"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CUnsignedShort"]} -> "FFIType"]@Function[ty, rawFFIType["CUnsignedShort"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CShort"]} -> "FFIType"]@Function[ty, rawFFIType["CShort"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CUnsignedInt"]} -> "FFIType"]@Function[ty, rawFFIType["CUnsignedInt"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CInt"]} -> "FFIType"]@Function[ty, rawFFIType["CInt"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CUnsignedLong"]} -> "FFIType"]@Function[ty, rawFFIType["CUnsignedLong"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["CLong"]} -> "FFIType"]@Function[ty, rawFFIType["CLong"][]]],
	FunctionDeclaration[FFIType, Typed[{LiteralType["OpaqueRawPointer"]} -> "FFIType"]@Function[ty, rawFFIType["OpaqueRawPointer"][]]],

	FunctionDeclaration[FFIType,
		Typed[{"String"} -> "FFIType"]@
		Function[name,
			Switch[name,
				"Void", 							rawFFIType["Void"][],
				"UnsignedInteger8", 	rawFFIType["UnsignedInteger8"][],
				"Integer8", 					rawFFIType["Integer8"][],
				"UnsignedInteger16", 	rawFFIType["UnsignedInteger16"][],
				"Integer16", 					rawFFIType["Integer16"][],
				"UnsignedInteger32", 	rawFFIType["UnsignedInteger32"][],
				"Integer32", 					rawFFIType["Integer32"][],
				"UnsignedInteger64", 	rawFFIType["UnsignedInteger64"][],
				"Integer64", 					rawFFIType["Integer64"][],
				"CFloat", 						rawFFIType["CFloat"][],
				"CDouble", 						rawFFIType["CDouble"][],
				"CUnsignedChar", 			rawFFIType["CUnsignedChar"][],
				"CSignedChar", 				rawFFIType["CSignedChar"][],
				"CUnsignedShort", 		rawFFIType["CUnsignedShort"][],
				"CShort", 						rawFFIType["CShort"][],
				"CUnsignedInt", 			rawFFIType["CUnsignedInt"][],
				"CInt", 							rawFFIType["CInt"][],
				"CUnsignedLong", 			rawFFIType["CUnsignedLong"][],
				"CLong", 							rawFFIType["CLong"][],
				"OpaqueRawPointer", 	rawFFIType["OpaqueRawPointer"][],
				_,										Native`ThrowWolframExceptionCode["Unimplemented"]
			]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	FFIType -> Typed[{"String"} -> "FFIType"]@FFIType
|>];


End[] (* End `Private` *)

EndPackage[]
