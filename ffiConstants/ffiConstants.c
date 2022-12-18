#define _GNU_SOURCE
// The #define above adds support for RTLD_DEFAULT

#include <ffi.h>
#include <dlfcn.h>


// dlfcn.h constants

void* get_RTLD_DEFAULT() {
	return RTLD_DEFAULT;
}

int get_RTLD_LAZY() {
	return RTLD_LAZY;
}


// libffi constants

// ABIs
int get_FFI_DEFAULT_ABI() {
	return FFI_DEFAULT_ABI;
}

// ffi_status values
int get_FFI_OK() {
	return FFI_OK;
}

int get_FFI_BAD_TYPEDEF() {
	return FFI_BAD_TYPEDEF;
}

int get_FFI_BAD_ARGTYPE() {
	return FFI_BAD_ARGTYPE;
}

int get_FFI_BAD_ABI() {
	return FFI_BAD_ABI;
}

// Closures
size_t get_ffi_closure_size() {
	return sizeof(ffi_closure);
}


// libffi types

ffi_type* get_ffi_type_void() {
	return &ffi_type_void;
}

ffi_type* get_ffi_type_uint8() {
	return &ffi_type_uint8;
}

ffi_type* get_ffi_type_sint8() {
	return &ffi_type_sint8;
}

ffi_type* get_ffi_type_uint16() {
	return &ffi_type_uint16;
}

ffi_type* get_ffi_type_sint16() {
	return &ffi_type_sint16;
}

ffi_type* get_ffi_type_uint32() {
	return &ffi_type_uint32;
}

ffi_type* get_ffi_type_sint32() {
	return &ffi_type_sint32;
}

ffi_type* get_ffi_type_uint64() {
	return &ffi_type_uint64;
}

ffi_type* get_ffi_type_sint64() {
	return &ffi_type_sint64;
}

ffi_type* get_ffi_type_float() {
	return &ffi_type_float;
}

ffi_type* get_ffi_type_double() {
	return &ffi_type_double;
}

ffi_type* get_ffi_type_uchar() {
	return &ffi_type_uchar;
}

ffi_type* get_ffi_type_schar() {
	return &ffi_type_schar;
}

ffi_type* get_ffi_type_ushort() {
	return &ffi_type_ushort;
}

ffi_type* get_ffi_type_sshort() {
	return &ffi_type_sshort;
}

ffi_type* get_ffi_type_uint() {
	return &ffi_type_uint;
}

ffi_type* get_ffi_type_sint() {
	return &ffi_type_sint;
}

ffi_type* get_ffi_type_ulong() {
	return &ffi_type_ulong;
}

ffi_type* get_ffi_type_slong() {
	return &ffi_type_slong;
}

ffi_type* get_ffi_type_longdouble() {
	return &ffi_type_longdouble;
}

ffi_type* get_ffi_type_pointer() {
	return &ffi_type_pointer;
}

ffi_type* get_ffi_type_complex_float() {
	return &ffi_type_complex_float;
}

ffi_type* get_ffi_type_complex_double() {
	return &ffi_type_complex_double;
}

ffi_type* get_ffi_type_complex_longdouble() {
	return &ffi_type_complex_longdouble;
}


// Type IDs

unsigned short get_id_FFI_TYPE_VOID() {
	return FFI_TYPE_VOID;
}

unsigned short get_id_FFI_TYPE_INT() {
	return FFI_TYPE_INT;
}

unsigned short get_id_FFI_TYPE_FLOAT() {
	return FFI_TYPE_FLOAT;
}

unsigned short get_id_FFI_TYPE_DOUBLE() {
	return FFI_TYPE_DOUBLE;
}

unsigned short get_id_FFI_TYPE_LONGDOUBLE() {
	return FFI_TYPE_LONGDOUBLE;
}

unsigned short get_id_FFI_TYPE_UINT8() {
	return FFI_TYPE_UINT8;
}

unsigned short get_id_FFI_TYPE_SINT8() {
	return FFI_TYPE_SINT8;
}

unsigned short get_id_FFI_TYPE_UINT16() {
	return FFI_TYPE_UINT16;
}

unsigned short get_id_FFI_TYPE_SINT16() {
	return FFI_TYPE_SINT16;
}

unsigned short get_id_FFI_TYPE_UINT32() {
	return FFI_TYPE_UINT32;
}

unsigned short get_id_FFI_TYPE_SINT32() {
	return FFI_TYPE_SINT32;
}

unsigned short get_id_FFI_TYPE_UINT64() {
	return FFI_TYPE_UINT64;
}

unsigned short get_id_FFI_TYPE_SINT64() {
	return FFI_TYPE_SINT64;
}

unsigned short get_id_FFI_TYPE_STRUCT() {
	return FFI_TYPE_STRUCT;
}

unsigned short get_id_FFI_TYPE_POINTER() {
	return FFI_TYPE_POINTER;
}

unsigned short get_id_FFI_TYPE_COMPLEX() {
	return FFI_TYPE_COMPLEX;
}
