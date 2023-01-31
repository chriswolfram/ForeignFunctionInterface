BeginPackage["ChristopherWolfram`ForeignFunctionInterface`LibFFI`ExpressionConversion`"]

Begin["`Private`"]


Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`Callback`"] (* for GetCallbackPointer *)



(****************************************************************************)
(***************************** ExpressionToC ********************************)
(****************************************************************************)

(*
	ExpressionToC is the main function for converting expressions to various C-compatible types. It takes a pointer
	into which to write the results, a type, and an expression.
*)

DeclareCompiledComponent["ForeignFunctionInterface", {

	FunctionDeclaration[ExpressionToC,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::[type], "BitCast"],
				Cast[init, type]
			];
		]
	],

	FunctionDeclaration[ExpressionToC,
		Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"], "InertExpression"} -> "Null"]]@
		Function[{ptr, type, init},
			ToRawPointer[
				Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"],
				(* TODO: This could be more efficient. GetCallbackPointer turns a pointer to an expression, and this turns it back. *)
				ExpressionToPointer[GetCallbackPointer[init]]
			];
		]
	],

	FunctionDeclaration[ExpressionToC,
		Typed[{"OpaqueRawPointer", "FFIType", "InertExpression"} -> "Null"]@
		Function[{ptr, type, init},
			Switch[type["Type"],

				NameFFITypeID["UINT8"][],		ExpressionToC[ptr, TypeSpecifier["UnsignedInteger8"], init],
				NameFFITypeID["SINT8"][],		ExpressionToC[ptr, TypeSpecifier["Integer8"], init],
				NameFFITypeID["UINT16"][],	ExpressionToC[ptr, TypeSpecifier["UnsignedInteger16"], init],
				NameFFITypeID["SINT16"][],	ExpressionToC[ptr, TypeSpecifier["Integer16"], init],
				NameFFITypeID["UINT32"][],	ExpressionToC[ptr, TypeSpecifier["UnsignedInteger32"], init],
				NameFFITypeID["SINT32"][],	ExpressionToC[ptr, TypeSpecifier["Integer32"], init],
				NameFFITypeID["UINT64"][],	ExpressionToC[ptr, TypeSpecifier["UnsignedInteger64"], init],
				NameFFITypeID["SINT64"][],	ExpressionToC[ptr, TypeSpecifier["Integer64"], init],
				NameFFITypeID["INT"][],			ExpressionToC[ptr, TypeSpecifier["CInt"], init],
				NameFFITypeID["FLOAT"][],		ExpressionToC[ptr, TypeSpecifier["CFloat"], init],
				NameFFITypeID["DOUBLE"][],	ExpressionToC[ptr, TypeSpecifier["CDouble"], init],
				NameFFITypeID["POINTER"][],	ExpressionToC[ptr, TypeSpecifier["OpaqueRawPointer"], init],
				NameFFITypeID["STRUCT"][],	expressionToCStruct[ptr, type, init],
				_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

			]
		]
	],

	FunctionDeclaration[expressionToCStruct,
		Typed[{"OpaqueRawPointer", "FFIType", "InertExpression"} -> "Null"]@
		Function[{ptr, type, init},
			Module[{elementCount, elementOffsets},

				elementCount = FFITypeElementCount[type];

				If[Head[init] =!= InertExpression[List] || Length[init] =!= elementCount,
					Native`ThrowWolframExceptionCode["Argument"]
				];

				(* TODO: Check error code *)
				elementOffsets = CreateTypeInstance["Managed"::["CArray"::["CSizeT"]], elementCount];
				LibraryFunction["ffi_get_struct_offsets"][
					LibraryFunction["get_FFI_DEFAULT_ABI"][],
					type,
					elementOffsets
				];

				Do[
					ExpressionToC[
						Cast[Cast[ptr,"UnsignedInteger64","BitCast"] + FromRawPointer[elementOffsets,i-1], "OpaqueRawPointer","BitCast"],
						FromRawPointer[type["Elements"], i-1],
						init[[i]]
					],
					{i, elementCount}
				]
			]
		]
	]

}];



(****************************************************************************)
(****************************** CToExpression *******************************)
(****************************************************************************)

(*
	CToExpression is the main function for converting C-compatible types into expressions. It takes
	a pointer where the value is stored and the type of the value, and it returns the contained expression.
*)

DeclareCompiledComponent["ForeignFunctionInterface", {

	FunctionDeclaration[CToExpression,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty]} -> "InertExpression"]]@
			Function[{ptr, type},
				Cast[FromRawPointer[Cast[ptr, "RawPointer"::[type], "BitCast"]], "InertExpression"]
			]
		],

		FunctionDeclaration[CToExpression,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["Void"]} -> "InertExpression"]@
			Function[{ptr, type},
				InertExpression[Null]
			]
		],

		FunctionDeclaration[CToExpression,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"]} -> "InertExpression"]@
			Function[{ptr, type},
				PointerToExpression[FromRawPointer[Cast[ptr, "RawPointer"::["OpaqueRawPointer"], "BitCast"]]]
			]
		],

		FunctionDeclaration[CToExpression,
			Typed[{"OpaqueRawPointer", "InertExpression"} -> "InertExpression"]@
			Function[{ptr, type},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					CToExpression[ptr, Compile`BorrowManagedObject[ffiType]]
				]
			]
		],

		FunctionDeclaration[CToExpression,
			Typed[{"OpaqueRawPointer", "FFIType"} -> "InertExpression"]@
			Function[{ptr, type},
				Switch[type["Type"],

					NameFFITypeID["VOID"][],		CToExpression[ptr, TypeSpecifier["Void"]],
					NameFFITypeID["UINT8"][],		CToExpression[ptr, TypeSpecifier["UnsignedInteger8"]],
					NameFFITypeID["SINT8"][],		CToExpression[ptr, TypeSpecifier["Integer8"]],
					NameFFITypeID["UINT16"][],	CToExpression[ptr, TypeSpecifier["UnsignedInteger16"]],
					NameFFITypeID["SINT16"][],	CToExpression[ptr, TypeSpecifier["Integer16"]],
					NameFFITypeID["UINT32"][],	CToExpression[ptr, TypeSpecifier["UnsignedInteger32"]],
					NameFFITypeID["SINT32"][],	CToExpression[ptr, TypeSpecifier["Integer32"]],
					NameFFITypeID["UINT64"][],	CToExpression[ptr, TypeSpecifier["UnsignedInteger64"]],
					NameFFITypeID["SINT64"][],	CToExpression[ptr, TypeSpecifier["Integer64"]],
					NameFFITypeID["INT"][],			CToExpression[ptr, TypeSpecifier["CInt"]],
					NameFFITypeID["FLOAT"][],		CToExpression[ptr, TypeSpecifier["CFloat"]],
					NameFFITypeID["DOUBLE"][],	CToExpression[ptr, TypeSpecifier["CDouble"]],
					NameFFITypeID["POINTER"][],	CToExpression[ptr, TypeSpecifier["OpaqueRawPointer"]],
					NameFFITypeID["STRUCT"][],	cStructToExpression[ptr, type],
					_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		],

		(******* Structs *******)

		FunctionDeclaration[cStructToExpression,
			Typed[{"OpaqueRawPointer", "FFIType"} -> "InertExpression"]@
			Function[{ptr, type},
				Module[{elementCount, elementOffsets, expr},

					elementCount = FFITypeElementCount[type];

					(* TODO: Check error code *)
					elementOffsets = CreateTypeInstance["Managed"::["CArray"::["CSizeT"]], elementCount];
					LibraryFunction["ffi_get_struct_offsets"][
						LibraryFunction["get_FFI_DEFAULT_ABI"][],
						type,
						elementOffsets
					];

					expr = Native`PrimitiveFunction["CreateHeaded_IE_E"][elementCount, InertExpression[List]];
					Do[
						Native`PrimitiveFunction["SetElement_EIE_Void"][
							expr,
							i,
							CToExpression[
								Cast[Cast[ptr,"UnsignedInteger64","BitCast"] + FromRawPointer[elementOffsets,i-1], "OpaqueRawPointer","BitCast"],
								FromRawPointer[type["Elements"],i-1]
							]
						],
						{i, elementCount}
					];

					expr

				]
			]
		],


		(******* Expression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"InertExpression", ty} -> "InertExpression"]]@
			Function[{ptr, type},
				DereferenceBuffer[ExpressionToPointer[ptr], type]
			]
		]

}];



End[] (* End `Private` *)

EndPackage[]
