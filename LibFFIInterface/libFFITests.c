#include <stdlib.h>
// #include <ffi.h>

#include <math.h>
#include <stdio.h>

#include <gnu/lib-names.h>

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

ffi_type* get_ffi_type_sint32() {
	return &ffi_type_sint32;
}

ffi_type* get_ffi_type_double() {
	return &ffi_type_double;
}

int main2()
{
  ffi_cif cif;
  ffi_type *args[1];
  void *values[1];
  double s = 3.0;
  double rc;
  
  /* Initialize the argument info vectors */    
  args[0] = &ffi_type_double;
  values[0] = &s;
  
  /* Initialize the cif */
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, 
		       &ffi_type_double, args) == FFI_OK) 
  {
      ffi_call(&cif, get_fun_pointer(), &rc, values);

      printf("%lf\n", rc);

    }
  
  return 0;
}

int main3() {
	double out;
	void* in[1];
	ffi_type* argTys[1];
	double inVal = 3.0;
	in[0] = &inVal;

	ffi_type* argTy = get_ffi_type_double();
	argTys[0] = argTy;

	ffi_cif* cif = create_ffi_cif();
	printf("%d\n", prepare_ffi_cif(cif, 1, get_ffi_type_double(), argTys));
	ffi_call(cif, sin, &out, in);

	printf("%lf, %lf\n", out, sin(inVal));

	return 0;
}

int main1() {
	printf("%s\n", LIB_SO);
}