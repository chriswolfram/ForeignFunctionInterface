#include <ffi.h>
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>


// clang -I/usr/include/x86_64-linux-gnu/ -lffi -Wall --pedantic -o stackBuffersError stackBuffersError.c; ./stackBuffersError

int withHeapBuffers() {
	ffi_type* arg_types[3];

	arg_types[0] = &ffi_type_pointer;
	arg_types[1] = &ffi_type_uint64;
	arg_types[2] = &ffi_type_pointer;

	ffi_cif cif;
	int prep_res = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 3, &ffi_type_pointer, arg_types);
	printf("prep_res is OK: %d\n", prep_res == FFI_OK);

	void* open_res = dlopen("/usr/lib/x86_64-linux-gnu/libcrypto.so.3", RTLD_NOW | RTLD_GLOBAL);
	printf("open_res: %ld\n", (size_t)open_res);

	char* error;
	error = dlerror();
	if (error != NULL) {
		printf("%s\n", error);
	}

	void* sha256_ptr = dlsym(open_res, "SHA256");
	printf("sha256_ptr: %ld\n", (size_t)sha256_ptr);

	error = dlerror();
	if (error != NULL) {
		printf("%s\n", error);
	}

	unsigned char* in = (unsigned char*)malloc(sizeof(unsigned char)*14);
	in = "Hello, World!";
	unsigned char* out = (unsigned char*)malloc(sizeof(unsigned char)*32);
	size_t size = 13;
	void* arg_values[3];
	arg_values[0] = &in;
	arg_values[1] = &size;
	arg_values[2] = &out;

	ffi_arg rc;

	ffi_call(&cif, sha256_ptr, &rc, arg_values);

	for(int i = 0; i < 32; i++)
		printf("%d, ", out[i]);

	printf("\n");

	return 0;
}

int withStackBuffers() {
	ffi_type* arg_types[3];

	arg_types[0] = &ffi_type_pointer;
	arg_types[1] = &ffi_type_uint64;
	arg_types[2] = &ffi_type_pointer;

	ffi_cif cif;
	int prep_res = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 3, &ffi_type_pointer, arg_types);
	printf("prep_res is OK: %d\n", prep_res == FFI_OK);

	void* open_res = dlopen("/usr/lib/x86_64-linux-gnu/libcrypto.so.3", RTLD_NOW | RTLD_GLOBAL);
	printf("open_res: %ld\n", (size_t)open_res);

	char* error;
	error = dlerror();
	if (error != NULL) {
		printf("%s\n", error);
	}

	void* sha256_ptr = dlsym(open_res, "SHA256");
	printf("sha256_ptr: %ld\n", (size_t)sha256_ptr);

	error = dlerror();
	if (error != NULL) {
		printf("%s\n", error);
	}

	unsigned char in[14];
	strcpy(in, "Hello, World!");
	unsigned char out[32];
	size_t size = 13;
	void* arg_values[3];
	arg_values[0] = &in;
	arg_values[1] = &size;
	arg_values[2] = &out;

	ffi_arg rc;

	ffi_call(&cif, sha256_ptr, &rc, arg_values);

	for(int i = 0; i < 32; i++)
		printf("%d, ", out[i]);

	printf("\n");

	return 0;
}

int main() {
	printf("Heap buffers:\n");
	withHeapBuffers();

	printf("\n\nStack buffers:\n");
	withStackBuffers();

	return 0;
}