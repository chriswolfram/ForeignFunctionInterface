#include <stdlib.h>
#include <ffi.h>

#include <math.h>

// clang libFFIInterface.c -L/usr/local/opt/libffi/lib/ -I/usr/local/opt/libffi/include -lffi -shared -o libFFIInterface.dylib

//CIFs

ffi_cif* create_ffi_cif() {
	return (ffi_cif*)malloc(sizeof(ffi_cif));
}

void free_ffi_cif(ffi_cif* cif) {
	free(cif);
}

int prepare_ffi_cif(ffi_cif* cif, unsigned int nargs, ffi_type* rtype, ffi_type** argtypes) {
	switch(ffi_prep_cif(cif, FFI_DEFAULT_ABI, nargs, rtype, argtypes)) {
		case FFI_OK:
			return 1;
		case FFI_BAD_TYPEDEF:
			return -1;
		case FFI_BAD_ARGTYPE:
			return -2;
		case FFI_BAD_ABI:
			return -3;
	}
}


void* get_fun_pointer() {
	return (void*)sin;
}



// Types

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


// Old experiments

// int main()
// {
//   ffi_cif cif;
//   ffi_type *args[1];
//   void *values[1];
//   char *s;
//   ffi_arg rc;
  
//   /* Initialize the argument info vectors */    
//   args[0] = &ffi_type_pointer;
//   values[0] = &s;
  
//   /* Initialize the cif */
//   if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
// 		       &ffi_type_sint, args) == FFI_OK)
//     {
//       s = "Hello World!";
//       ffi_call(&cif, puts, &rc, values);
//       /* rc now holds the result of the call to puts */
      
//       /* values holds a pointer to the function's arg, so to 
//          call puts() again all we need to do is change the 
//          value of s */
//       s = "This is cool!";
//       ffi_call(&cif, puts, &rc, values);
//     }
  
//   return 0;
// }
