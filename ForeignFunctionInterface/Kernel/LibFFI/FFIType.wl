BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`FFIType`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Constants`"] (* for RawFFIType *)


(************************* CreateFFIType ***************************)

DeclareCompiledComponent["ForeignFunctionInterface", {

	(*
		CreateFFIType[typeID_Integer]
			creates an CreateFFIType with the type specified by the libffi type ID.

		CreateFFIType[{ty1, ty2, ...}]
			creates a struct type with the specified field type IDs.

		All types and fields are owned by the caller, which is responsible for freeing them.
	*)
	FunctionDeclaration[CreateFFIType,
		Typed[{"InertExpression"} -> "FFIType"]@
		Function[typeIDSpec,
			Which[
				IntegerQ[typeIDSpec], typeFromIntegerID[Cast[typeIDSpec, "CUnsignedShort"]],
				True,									Native`ThrowWolframExceptionCode["Unimplemented"]
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
