BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]



(***************************************************)
(********** Creating and freeing buffers ***********)
(***************************************************)


(******* FreeBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[FreeBuffer,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	FreeBuffer
}];



(******* CreateBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[CreateBuffer,
			Typed[{"InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{type, len},
				PointerToExpression@Cast[
					CreateTypeInstance["CArray"::["Integer8"], Cast[FFITypeByteCount[type]*len, "MachineInteger", "CCast"]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iCreateBuffer -> Typed[CreateBuffer, {"InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

CreateBuffer[ty_, len_] :=
	iCreateBuffer[ty, len]

CreateBuffer[ty_] :=
	iCreateBuffer[ty, 1]



(******* BufferToNumericArray / NumericArrayToBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[BufferToNumericArray,
			Typed[ForAllType[elemType, {"InertExpression", "TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, len},
				
				Cast[
					CreateTypeInstance[
						"NumericArray"::[type,1],
						Cast[ExpressionToPointer[GetManagedExpression[ptr]],"CArray"::[type],"BitCast"],
						len
					],
					"InertExpression"
				]

			]
		],

		FunctionDeclaration[BufferToNumericArray,
			Typed[{"InertExpression", "CUnsignedShort", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, typeID, len},

				Switch[typeID,

					NameFFITypeID["UINT8"][],		BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger8"], len],
					NameFFITypeID["SINT8"][],		BufferToNumericArray[ptr, TypeSpecifier["Integer8"], len],
					NameFFITypeID["UINT16"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger16"], len],
					NameFFITypeID["SINT16"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer16"], len],
					NameFFITypeID["UINT32"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger32"], len],
					NameFFITypeID["SINT32"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer32"], len],
					NameFFITypeID["UINT64"][],	BufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger64"], len],
					NameFFITypeID["SINT64"][],	BufferToNumericArray[ptr, TypeSpecifier["Integer64"], len],
					NameFFITypeID["INT"][],			BufferToNumericArray[ptr, TypeSpecifier["CInt"], len],
					NameFFITypeID["FLOAT"][],		BufferToNumericArray[ptr, TypeSpecifier["CFloat"], len],
					NameFFITypeID["DOUBLE"][],	BufferToNumericArray[ptr, TypeSpecifier["CDouble"], len],
					_, 												Native`ThrowWolframExceptionCode["Unimplemented"]

				]
				
			]
		],


		FunctionDeclaration[NumericArrayToBuffer,
			Typed[ForAllType[ty, {"NumericArray"::[ty,1]} -> "InertExpression"]]@
			Function[arr,
				PointerToExpression@Cast[
					CreateTypeInstance["CArray", Array`GetData[arr], Array`NumberOfElements[arr]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[NumericArrayToBuffer,
			Typed[ForAllType[ty, {"InertExpression", "TypeSpecifier"::[ty]} -> "InertExpression"]]@
			Function[{expr, elemTy},
				NumericArrayToBuffer[Cast[expr, "NumericArray"::[elemTy,1]]]
			]
		],

		FunctionDeclaration[NumericArrayToBuffer,
			Typed[{"InertExpression", "CUnsignedShort"} -> "InertExpression"]@
			Function[{expr, typeID},

				Switch[typeID,

					NameFFITypeID["UINT8"][],		NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger8"]],
					NameFFITypeID["SINT8"][],		NumericArrayToBuffer[expr, TypeSpecifier["Integer8"]],
					NameFFITypeID["UINT16"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger16"]],
					NameFFITypeID["SINT16"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer16"]],
					NameFFITypeID["UINT32"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger32"]],
					NameFFITypeID["SINT32"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer32"]],
					NameFFITypeID["UINT64"][],	NumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger64"]],
					NameFFITypeID["SINT64"][],	NumericArrayToBuffer[expr, TypeSpecifier["Integer64"]],
					NameFFITypeID["INT"][],			NumericArrayToBuffer[expr, TypeSpecifier["CInt"]],
					NameFFITypeID["FLOAT"][],		NumericArrayToBuffer[expr, TypeSpecifier["CFloat"]],
					NameFFITypeID["DOUBLE"][],	NumericArrayToBuffer[expr, TypeSpecifier["CDouble"]],
					_, 												Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	BufferToNumericArray -> Typed[BufferToNumericArray, {"InertExpression", "CUnsignedShort", "MachineInteger"} -> "InertExpression"],
	NumericArrayToBuffer -> Typed[NumericArrayToBuffer, {"InertExpression", "CUnsignedShort"} -> "InertExpression"]
|>];



(******* StringToBuffer / BufferToString *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[StringToBuffer,
			Typed[{"String"} -> "InertExpression"]@
			Function[str,
				PointerToExpression@Cast[CreateTypeInstance["CString", str], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[BufferToString,
			Typed[{"InertExpression"} -> "String"]@
			Function[carr,
				CreateTypeInstance["String", Cast[ExpressionToPointer[GetManagedExpression[carr]], "CString", "BitCast"]]
			]
		]
}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	StringToBuffer,
	BufferToString
}];



(***************************************************)
(******* Extracting expressions from buffers *******)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Dereferencing / indexing *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "TypeSpecifier"::[ty], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				Cast[FromRawPointer[Cast[ptr, "CArray"::[type], "BitCast"], offset], "InertExpression"]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["Void"], "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				InertExpression[Null]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "TypeSpecifier"::["OpaqueRawPointer"], "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				PointerToExpression[FromRawPointer[Cast[ptr, "CArray"::["OpaqueRawPointer"], "BitCast"], offset]]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				Module[{ffiType, out},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					DereferenceBuffer[ptr, Compile`BorrowManagedObject[ffiType], offset]
				]
			]
		],

		FunctionDeclaration[DereferenceBuffer,
			Typed[{"OpaqueRawPointer", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
				Switch[type["Type"],

					NameFFITypeID["VOID"][],		DereferenceBuffer[ptr, TypeSpecifier["Void"], offset],
					NameFFITypeID["UINT8"][],		DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger8"], offset],
					NameFFITypeID["SINT8"][],		DereferenceBuffer[ptr, TypeSpecifier["Integer8"], offset],
					NameFFITypeID["UINT16"][],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger16"], offset],
					NameFFITypeID["SINT16"][],	DereferenceBuffer[ptr, TypeSpecifier["Integer16"], offset],
					NameFFITypeID["UINT32"][],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger32"], offset],
					NameFFITypeID["SINT32"][],	DereferenceBuffer[ptr, TypeSpecifier["Integer32"], offset],
					NameFFITypeID["UINT64"][],	DereferenceBuffer[ptr, TypeSpecifier["UnsignedInteger64"], offset],
					NameFFITypeID["SINT64"][],	DereferenceBuffer[ptr, TypeSpecifier["Integer64"], offset],
					NameFFITypeID["INT"][],			DereferenceBuffer[ptr, TypeSpecifier["CInt"], offset],
					NameFFITypeID["FLOAT"][],		DereferenceBuffer[ptr, TypeSpecifier["CFloat"], offset],
					NameFFITypeID["DOUBLE"][],	DereferenceBuffer[ptr, TypeSpecifier["CDouble"], offset],
					NameFFITypeID["POINTER"][],	DereferenceBuffer[ptr, TypeSpecifier["OpaqueRawPointer"], offset],
					NameFFITypeID["STRUCT"][],	dereferenceStruct[ptr, type, offset],
					_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		],

		(******* Structs *******)

		FunctionDeclaration[dereferenceStruct,
			Typed[{"OpaqueRawPointer", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, offset},
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
							DereferenceBuffer[
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

		(******* ManagedExpression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"ManagedExpression", ty, "MachineInteger"} -> "InertExpression"]]@
			Function[{man, type, offset},
				DereferenceBuffer[GetManagedExpression[man], type, offset]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[ty, {"InertExpression", ty, "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				DereferenceBuffer[ExpressionToPointer[GetManagedExpression[ptr]], type, offset]
			]
		],

		(******* 2-argument form *******)

		FunctionDeclaration[DereferenceBuffer,
			Typed[ForAllType[{ptrTy, valTy}, {ptrTy, valTy} -> "InertExpression"]]@
			Function[{ptr, type},
				DereferenceBuffer[ptr, type, 0]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iDereferenceBuffer -> Typed[DereferenceBuffer, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

DereferenceBuffer[ptr_, ty_, offset_] :=
	iDereferenceBuffer[ptr, ty, offset]

DereferenceBuffer[ptr_, ty_] :=
	iDereferenceBuffer[ptr, ty, 0]



End[] (* End `Private` *)

EndPackage[]
