BeginPackage["ForeignFunctionInterface`LibFFI`"]


$LibFFIPaths = {"/lib/x86_64-linux-gnu/libffi.so", "/home/christopher/git/ForeignFunctionInterface/LibFFIInterface/libFFIInterface.dylib"};

FFIType


Needs["ForeignFunctionInterface`LibFFI`Base`"]
Needs["ForeignFunctionInterface`LibFFI`InterfaceLibrary`"]
Needs["ForeignFunctionInterface`LibFFI`RawFunctionLoading`"]


DeclareCompiledComponent["ForeignFunctionInterface", "ExternalLibraries" -> $LibFFIPaths];


EndPackage[]