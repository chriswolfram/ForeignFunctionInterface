BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]

FFITypeID
NameFFITypeID

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"] (* for RawFFIType *)


DeclareCompiledComponent["ForeignFunctionInterface", {


	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["Void"]} -> "FFIType"]@Function[ty, RawFFIType["Void"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["UnsignedInteger8"]} -> "FFIType"]@Function[ty, RawFFIType["UnsignedInteger8"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["Integer8"]} -> "FFIType"]@Function[ty, RawFFIType["Integer8"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["UnsignedInteger16"]} -> "FFIType"]@Function[ty, RawFFIType["UnsignedInteger16"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["Integer16"]} -> "FFIType"]@Function[ty, RawFFIType["Integer16"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["UnsignedInteger32"]} -> "FFIType"]@Function[ty, RawFFIType["UnsignedInteger32"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["Integer32"]} -> "FFIType"]@Function[ty, RawFFIType["Integer32"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["UnsignedInteger64"]} -> "FFIType"]@Function[ty, RawFFIType["UnsignedInteger64"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["Integer64"]} -> "FFIType"]@Function[ty, RawFFIType["Integer64"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CFloat"]} -> "FFIType"]@Function[ty, RawFFIType["CFloat"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CDouble"]} -> "FFIType"]@Function[ty, RawFFIType["CDouble"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CUnsignedChar"]} -> "FFIType"]@Function[ty, RawFFIType["CUnsignedChar"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CSignedChar"]} -> "FFIType"]@Function[ty, RawFFIType["CSignedChar"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CUnsignedShort"]} -> "FFIType"]@Function[ty, RawFFIType["CUnsignedShort"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CShort"]} -> "FFIType"]@Function[ty, RawFFIType["CShort"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CUnsignedInt"]} -> "FFIType"]@Function[ty, RawFFIType["CUnsignedInt"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CInt"]} -> "FFIType"]@Function[ty, RawFFIType["CInt"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CUnsignedLong"]} -> "FFIType"]@Function[ty, RawFFIType["CUnsignedLong"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["CLong"]} -> "FFIType"]@Function[ty, RawFFIType["CLong"][]]],
	FunctionDeclaration[unmanagedFFIType, Typed[{LiteralType["OpaqueRawPointer"]} -> "FFIType"]@Function[ty, RawFFIType["OpaqueRawPointer"][]]],

	FunctionDeclaration[unmanagedFFIType,
		Typed[{"String"} -> "FFIType"]@
		Function[name,
			Switch[name,
				"Void", 							RawFFIType["Void"][],
				"UnsignedInteger8", 	RawFFIType["UnsignedInteger8"][],
				"Integer8", 					RawFFIType["Integer8"][],
				"UnsignedInteger16", 	RawFFIType["UnsignedInteger16"][],
				"Integer16", 					RawFFIType["Integer16"][],
				"UnsignedInteger32", 	RawFFIType["UnsignedInteger32"][],
				"Integer32", 					RawFFIType["Integer32"][],
				"UnsignedInteger64", 	RawFFIType["UnsignedInteger64"][],
				"Integer64", 					RawFFIType["Integer64"][],
				"CFloat", 						RawFFIType["CFloat"][],
				"CDouble", 						RawFFIType["CDouble"][],
				"CUnsignedChar", 			RawFFIType["CUnsignedChar"][],
				"CSignedChar", 				RawFFIType["CSignedChar"][],
				"CUnsignedShort", 		RawFFIType["CUnsignedShort"][],
				"CShort", 						RawFFIType["CShort"][],
				"CUnsignedInt", 			RawFFIType["CUnsignedInt"][],
				"CInt", 							RawFFIType["CInt"][],
				"CUnsignedLong", 			RawFFIType["CUnsignedLong"][],
				"CLong", 							RawFFIType["CLong"][],
				"OpaqueRawPointer", 	RawFFIType["OpaqueRawPointer"][],
				_,										Native`ThrowWolframExceptionCode["Unimplemented"]
			]
		]
	],


	FunctionDeclaration[FFIType,
		Typed[ForAllType[ty, {ty} -> "Managed"::["FFIType"]]]@
		Function[type,
			CreateTypeInstance["Managed", unmanagedFFIType[type], freeFFIType]
		]
	],

	FunctionDeclaration[FFIType,
		Typed[{"String"} -> "Managed"::["FFIType"]]@
		Function[name,
			CreateTypeInstance["Managed", unmanagedFFIType[name], freeFFIType]
		]
	],


	(*
		Free an FFIType if it needs to be freed. This does not free FFITypes that have a NULL as their elements array (which includes
		all of the built-in types from libffi). Otherwise, it recursively frees all elements.
	*)
	FunctionDeclaration[freeFFIType,
		Typed[{"FFIType"} -> "Null"]@
		Function[ty,
			If[ty["Elements"] =!= Cast[0,"OpaqueRawPointer","BitCast"],
				Module[{i = 0, elem},
					While[elem = FromRawPointer[ty["Elements"], i]; Cast[elem,"OpaqueRawPointer","BitCast"] =!= Cast[0,"OpaqueRawPointer","BitCast"],
						freeFFIType[elem]
					]
				];
				DeleteObject[ty["Elements"]];
				DeleteObject[ty];,
				Null
			]
		]
	],


	(************************* FFITypeByteCount ***************************)

	FunctionDeclaration[FFITypeByteCount,
		Typed[{"FFIType"} -> "MachineInteger"]@
		Function[ty,
			Cast[ty["Size"], "MachineInteger", "CCast"]
		]
	],

	FunctionDeclaration[FFITypeByteCount,
		Typed[{"Managed"::["FFIType"]} -> "MachineInteger"]@
		Function[ty,
			FFITypeByteCount[Compile`BorrowManagedObject[ty]]
		]
	],


	(************************* FFITypeID ***************************)

	FunctionDeclaration[FFITypeID,
		Typed[{"FFIType"} -> "CUnsignedShort"]@
		Function[ty,
			ty["Type"]
		]
	],

	FunctionDeclaration[FFITypeID,
		Typed[{"Managed"::["FFIType"]} -> "CUnsignedShort"]@
		Function[ty,
			FFITypeID[Compile`BorrowManagedObject[ty]]
		]
	]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	FFIType -> Typed[{"String"} -> "Managed"::["FFIType"]]@FFIType,
	FFITypeByteCount -> Typed[{"Managed"::["FFIType"]} -> "MachineInteger"]@FFITypeByteCount
|>];


End[] (* End `Private` *)

EndPackage[]
