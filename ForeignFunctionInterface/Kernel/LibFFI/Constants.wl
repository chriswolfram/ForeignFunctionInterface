BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"]

RawFFIType

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"] (* for the symbol NameFFITypeID *)



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
(********************* Types ***********************)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

	LibraryFunctionDeclaration[RawFFIType["Void"] -> "get_ffi_type_void", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["UnsignedInteger8"] -> "get_ffi_type_uint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["Integer8"] -> "get_ffi_type_sint8", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["UnsignedInteger16"] -> "get_ffi_type_uint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["Integer16"] -> "get_ffi_type_sint16", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["UnsignedInteger32"] -> "get_ffi_type_uint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["Integer32"] -> "get_ffi_type_sint32", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["UnsignedInteger64"] -> "get_ffi_type_uint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["Integer64"] -> "get_ffi_type_sint64", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CFloat"] -> "get_ffi_type_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CDouble"] -> "get_ffi_type_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CUnsignedChar"] -> "get_ffi_type_uchar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CSignedChar"] -> "get_ffi_type_schar", $LibFFIPaths, (*Not a supported type*)
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CUnsignedShort"] -> "get_ffi_type_ushort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CShort"] -> "get_ffi_type_sshort", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CUnsignedInt"] -> "get_ffi_type_uint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CInt"] -> "get_ffi_type_sint", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CUnsignedLong"] -> "get_ffi_type_ulong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["CLong"] -> "get_ffi_type_slong", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_longdouble", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration[RawFFIType["OpaqueRawPointer"] -> "get_ffi_type_pointer", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_float", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_double", $LibFFIPaths,
		{} -> "FFIType"],

	LibraryFunctionDeclaration["get_ffi_type_complex_longdouble", $LibFFIPaths,
		{} -> "FFIType"]

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
