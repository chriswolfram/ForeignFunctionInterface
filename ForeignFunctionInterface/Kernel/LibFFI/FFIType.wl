BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]


(************************* CreateFFIType ***************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		CreateFFIType[typeID_Integer]
			creates an CreateFFIType with the type specified by the libffi type ID.

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

				IntegerQ[typeSpec],
					typeFromIntegerID[Cast[typeSpec, "CUnsignedShort"]],

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
			Module[{template},

				template =
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

					];

				CreateTypeInstance["FFIType", <|
					"Size" -> template["Size"],
					"Alignment" -> TypeHint[0,"CUnsignedShort"],
					"Type" -> template["Type"],
					"Elements" -> Cast[0, "CArray"::["FFIType"], "BitCast"]
				|>]
			]
		]
	],

	FunctionDeclaration[typeFromIntegerID,
		Typed[{"CUnsignedShort"} -> "FFIType"]@
		Function[typeID,
			Module[{size},

				size =
					Switch[typeID,

						NameFFITypeID["UINT8"][],		Native`SizeOf["UnsignedInteger8"],
						NameFFITypeID["SINT8"][],		Native`SizeOf["Integer8"],
						NameFFITypeID["UINT16"][],	Native`SizeOf["UnsignedInteger16"],
						NameFFITypeID["SINT16"][],	Native`SizeOf["Integer16"],
						NameFFITypeID["UINT32"][],	Native`SizeOf["UnsignedInteger32"],
						NameFFITypeID["SINT32"][],	Native`SizeOf["Integer32"],
						NameFFITypeID["UINT64"][],	Native`SizeOf["UnsignedInteger64"],
						NameFFITypeID["SINT64"][],	Native`SizeOf["Integer64"],
						NameFFITypeID["INT"][],			Native`SizeOf["CInt"],
						NameFFITypeID["FLOAT"][],		Native`SizeOf["CFloat"],
						NameFFITypeID["DOUBLE"][],	Native`SizeOf["CDouble"],
						NameFFITypeID["POINTER"][],	Native`SizeOf["OpaqueRawPointer"],
						_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

					];

				CreateTypeInstance["FFIType", <|
					"Size" -> Cast[size, "CSizeT", "CCast"],
					"Alignment" -> TypeHint[0,"CUnsignedShort"],
					"Type" -> typeID,
					"Elements" -> Cast[0, "CArray"::["FFIType"], "BitCast"]
				|>]
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
			(* If[type["Elements"] =!= Cast[0, "CArray"::["FFIType"], "BitCast"],
				Module[{}]
			]; *)
			DeleteObject[type];
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
