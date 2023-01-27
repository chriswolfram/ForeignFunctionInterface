BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


(************************* CreateFFIType ***************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		CreateFFIType["typeName"]
			creates the FFI type analogous to the compiler type "typeName".

		CreateFFIType[{ty1, ty2, ...}]
			creates a struct type with the specified field type IDs.

		All types and fields are owned by the caller, which is responsible for freeing them.
	*)
	FunctionDeclaration[CreateFFIType,
		Typed[{"InertExpression"} -> "FFIType"]@
		Function[typeSpec,
			Which[

				StringQ[typeSpec],
					typeFromString[Cast[typeSpec, "String"]],

				Head[typeSpec] === InertExpression[TypeSpecifier["RawPointer"]] ||
				Head[typeSpec] === InertExpression["RawPointer"],
					typeFromRawPointer[typeSpec],

				Head[typeSpec] === InertExpression[TypeSpecifier["ListTuple"]] ||
				Head[typeSpec] === InertExpression["ListTuple"] ||
				ListQ[typeSpec],
					typeFromList[typeSpec],

				Head[typeSpec] === InertExpression[TypeSpecifier] && Length[typeSpec] === 1,
					CreateFFIType[First[typeSpec]],

				True,
					Native`ThrowWolframExceptionCode["Unimplemented"]

			]
		]
	],

	FunctionDeclaration[typeFromString,
		Typed[{"String"} -> "FFIType"]@
		Function[typeName,
			Switch[typeName,

				"Void",								RawFFIType["Void"][],
				"UnsignedInteger8",		RawFFIType["UnsignedInteger8"][],
				"Integer8",						RawFFIType["Integer8"][],
				"UnsignedInteger16",	RawFFIType["UnsignedInteger16"][],
				"Integer16",					RawFFIType["Integer16"][],
				"UnsignedInteger32",	RawFFIType["UnsignedInteger32"][],
				"Integer32",					RawFFIType["Integer32"][],
				"UnsignedInteger64",	RawFFIType["UnsignedInteger64"][],
				"Integer64",					RawFFIType["Integer64"][],
				"CFloat",							RawFFIType["CFloat"][],
				"CDouble",						RawFFIType["CDouble"][],
				"CChar",							RawFFIType["CUnsignedChar"][],
				"CUnsignedChar",			RawFFIType["CUnsignedChar"][],
				"CSignedChar",				RawFFIType["CSignedChar"][],
				"CUnsignedShort",			RawFFIType["CUnsignedShort"][],
				"CShort",							RawFFIType["CShort"][],
				"CUnsignedInt",				RawFFIType["CUnsignedInt"][],
				"CInt",								RawFFIType["CInt"][],
				"CUnsignedLong",			RawFFIType["CUnsignedLong"][],
				"CLong",							RawFFIType["CLong"][],
				"OpaqueRawPointer",		RawFFIType["OpaqueRawPointer"][],
				_, 										Native`ThrowWolframExceptionCode["Unimplemented"]

			]
		]
	],

	FunctionDeclaration[typeFromRawPointer,
		Typed[{"InertExpression"} -> "FFIType"]@
		Function[typeSpec,
			If[Length[typeSpec] =!= 1,
				Native`ThrowWolframExceptionCode["Argument"],
				RawFFIType["OpaqueRawPointer"][]
			]
		]
	],

	FunctionDeclaration[typeFromList,
		Typed[{"InertExpression"} -> "FFIType"]@
		Function[typeList,
			Module[{elementCount, elements, type},

				elementCount = Length[typeList];

				(* TODO: This is potentially a memory leak if it fails after allocating a few types *)
				elements = CreateTypeInstance["CArray"::["FFIType"], elementCount+1];
				Do[ToRawPointer[elements, i-1, CreateFFIType[typeList[[i]]]], {i, elementCount}];
				ToRawPointer[elements, elementCount, Cast[0, "FFIType", "BitCast"]];

				type = CreateTypeInstance["FFIType", <|
					"Size" -> Typed[0, "CSizeT"],
					"Alignment" -> TypeHint[0,"CUnsignedShort"],
					"Type" -> NameFFITypeID["STRUCT"][],
					"Elements" -> elements
				|>];

				(* TODO: Check error code *)
				LibraryFunction["ffi_get_struct_offsets"][
					LibraryFunction["get_FFI_DEFAULT_ABI"][],
					type,
					Cast[0, "CArray"::["CSizeT"], "BitCast"]
				];

				type
			]
		]
	]

}];



(************************* DeleteFFIType ***************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		DeleteFFIType[ffiType]
			frees an FFIType and along with its fields (if it has any).
	*)
	FunctionDeclaration[DeleteFFIType,
		Typed[{"FFIType"} -> "Null"]@
		Function[type,

			If[type["Type"] === NameFFITypeID["STRUCT"][],

				If[type["Elements"] =!= Cast[0, "CArray"::["FFIType"], "BitCast"],
					Module[{i = 0},
						While[FromRawPointer[type["Elements"], i] =!= Cast[0, "FFIType", "BitCast"],
							DeleteFFIType[FromRawPointer[type["Elements"], i]];
							i++
						];
						DeleteObject[type["Elements"]]
					]
				];

				DeleteObject[type]

			];
		]
	]

}];



(*********************** FFITypeElementCount *************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		FFITypeElementCount[ffiType]
			gives the number of elements (struct fields) in a type.
	*)
	FunctionDeclaration[FFITypeElementCount,
		Typed[{"FFIType"} -> "MachineInteger"]@
		Function[type,
			If[type["Elements"] === Cast[0, "CArray"::["FFIType"], "BitCast"],
				0,
				Module[{i = 0},
					While[FromRawPointer[type["Elements"], i] =!= Cast[0, "FFIType", "BitCast"],
						i++
					];
					i
				]
			]
		]
	]

}];



(************************* FFITypeByteCount ***************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		FFITypeByteCount[ffiType]
			gives the size of ffiType in bytes.
	*)
	FunctionDeclaration[FFITypeByteCount,
		Typed[{"FFIType"} -> "MachineInteger"]@
		Function[type,
			Cast[type["Size"], "MachineInteger", "CCast"]
		]
	],

	FunctionDeclaration[FFITypeByteCount,
		Typed[{"InertExpression"} -> "MachineInteger"]@
		Function[type,
			Module[{ty, size},
				(* TODO: Maybe use a managed object instead of manually freeing? Might have better behavior in exceptions. *)
				ty = CreateFFIType[type];
				size = FFITypeByteCount[ty];
				DeleteFFIType[ty];
				size
			]
		]
	]

}];



End[] (* End `Private` *)

EndPackage[]
