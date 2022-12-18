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
		{} -> "CInt"],

	(* Closures *)
	LibraryFunctionDeclaration["get_ffi_closure_size", $LibFFIPaths,
		{} -> "CSizeT"]

}];



(***************************************************)
(******************** Type IDs *********************)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

	LibraryFunctionDeclaration[NameFFITypeID["VOID"] -> "get_id_FFI_TYPE_VOID", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["INT"] -> "get_id_FFI_TYPE_INT", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["FLOAT"] -> "get_id_FFI_TYPE_FLOAT", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["DOUBLE"] -> "get_id_FFI_TYPE_DOUBLE", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["LONGDOUBLE"] -> "get_id_FFI_TYPE_LONGDOUBLE", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["UINT8"] -> "get_id_FFI_TYPE_UINT8", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["SINT8"] -> "get_id_FFI_TYPE_SINT8", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["UINT16"] -> "get_id_FFI_TYPE_UINT16", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["SINT16"] -> "get_id_FFI_TYPE_SINT16", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["UINT32"] -> "get_id_FFI_TYPE_UINT32", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["SINT32"] -> "get_id_FFI_TYPE_SINT32", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["UINT64"] -> "get_id_FFI_TYPE_UINT64", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["SINT64"] -> "get_id_FFI_TYPE_SINT64", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["STRUCT"] -> "get_id_FFI_TYPE_STRUCT", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["POINTER"] -> "get_id_FFI_TYPE_POINTER", $LibFFIPaths,
		{} -> "CUnsignedShort"],

	LibraryFunctionDeclaration[NameFFITypeID["COMPLEX"] -> "get_id_FFI_TYPE_COMPLEX", $LibFFIPaths,
		{} -> "CUnsignedShort"]

}];


End[] (* End `Private` *)

EndPackage[]
