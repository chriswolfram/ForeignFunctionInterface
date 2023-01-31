BeginPackage["ChristopherWolfram`ForeignFunctionInterface`Buffer`"]


Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer`"]
Needs["ChristopherWolfram`ForeignFunctionInterface`LibFFI`"]



(***************************************************)
(********** Creating and freeing buffers ***********)
(***************************************************)


(******* DeleteBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[DeleteBuffer,
			Typed[{"InertExpression"} -> "Null"]@
			Function[ptr,
				(* TODO: This ignores the element type of the array. Confirm that this isn't a problem. *)
				DeleteObject[Cast[ExpressionToPointer[ptr], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	DeleteBuffer
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
	With[{res = iCreateBuffer[ty, len]},
		If[FailureQ[res],
			res,
			RawPointer[res, ty]
		]
	]

CreateBuffer[ty_] :=
	iCreateBuffer[ty, 1]



(******* BufferToNumericArray / NumericArrayToBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[iBufferToNumericArray,
			Typed[ForAllType[elemType, {"InertExpression", "TypeSpecifier"::[elemType], "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, len},
				
				Cast[
					CreateTypeInstance[
						"NumericArray"::[type,1],
						Cast[ExpressionToPointer[ptr],"CArray"::[type],"BitCast"],
						len
					],
					"InertExpression"
				]

			]
		],

		FunctionDeclaration[iBufferToNumericArray,
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, type, len},

				Switch[type["Type"],

					NameFFITypeID["UINT8"][],		iBufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger8"], len],
					NameFFITypeID["SINT8"][],		iBufferToNumericArray[ptr, TypeSpecifier["Integer8"], len],
					NameFFITypeID["UINT16"][],	iBufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger16"], len],
					NameFFITypeID["SINT16"][],	iBufferToNumericArray[ptr, TypeSpecifier["Integer16"], len],
					NameFFITypeID["UINT32"][],	iBufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger32"], len],
					NameFFITypeID["SINT32"][],	iBufferToNumericArray[ptr, TypeSpecifier["Integer32"], len],
					NameFFITypeID["UINT64"][],	iBufferToNumericArray[ptr, TypeSpecifier["UnsignedInteger64"], len],
					NameFFITypeID["SINT64"][],	iBufferToNumericArray[ptr, TypeSpecifier["Integer64"], len],
					NameFFITypeID["INT"][],			iBufferToNumericArray[ptr, TypeSpecifier["CInt"], len],
					NameFFITypeID["FLOAT"][],		iBufferToNumericArray[ptr, TypeSpecifier["CFloat"], len],
					NameFFITypeID["DOUBLE"][],	iBufferToNumericArray[ptr, TypeSpecifier["CDouble"], len],
					_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

				]
				
			]
		],

		FunctionDeclaration[iBufferToNumericArray,
			Typed[{"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, typeExpr, len},
				Module[{type = CreateTypeInstance["Managed", CreateFFIType[typeExpr], DeleteFFIType]},
					iBufferToNumericArray[ptr, Compile`BorrowManagedObject[type], len]		
				]		
			]
		],


		FunctionDeclaration[iNumericArrayToBuffer,
			Typed[ForAllType[ty, {"NumericArray"::[ty,1]} -> "InertExpression"]]@
			Function[arr,
				PointerToExpression@Cast[
					CreateTypeInstance["CArray", Array`GetData[arr], Array`NumberOfElements[arr]],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[iNumericArrayToBuffer,
			Typed[ForAllType[ty, {"InertExpression", "TypeSpecifier"::[ty]} -> "InertExpression"]]@
			Function[{expr, elemTy},
				iNumericArrayToBuffer[Cast[expr, "NumericArray"::[elemTy,1]]]
			]
		],

		FunctionDeclaration[iNumericArrayToBuffer,
			Typed[{"InertExpression", "FFIType"} -> "InertExpression"]@
			Function[{expr, type},

				Switch[type["Type"],

					NameFFITypeID["UINT8"][],		iNumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger8"]],
					NameFFITypeID["SINT8"][],		iNumericArrayToBuffer[expr, TypeSpecifier["Integer8"]],
					NameFFITypeID["UINT16"][],	iNumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger16"]],
					NameFFITypeID["SINT16"][],	iNumericArrayToBuffer[expr, TypeSpecifier["Integer16"]],
					NameFFITypeID["UINT32"][],	iNumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger32"]],
					NameFFITypeID["SINT32"][],	iNumericArrayToBuffer[expr, TypeSpecifier["Integer32"]],
					NameFFITypeID["UINT64"][],	iNumericArrayToBuffer[expr, TypeSpecifier["UnsignedInteger64"]],
					NameFFITypeID["SINT64"][],	iNumericArrayToBuffer[expr, TypeSpecifier["Integer64"]],
					NameFFITypeID["INT"][],			iNumericArrayToBuffer[expr, TypeSpecifier["CInt"]],
					NameFFITypeID["FLOAT"][],		iNumericArrayToBuffer[expr, TypeSpecifier["CFloat"]],
					NameFFITypeID["DOUBLE"][],	iNumericArrayToBuffer[expr, TypeSpecifier["CDouble"]],
					_, 													Native`ThrowWolframExceptionCode["Unimplemented"]

				]
			]
		],

		FunctionDeclaration[iNumericArrayToBuffer,
			Typed[{"InertExpression", "InertExpression"} -> "InertExpression"]@
			Function[{expr, typeExpr},
				Module[{type = CreateTypeInstance["Managed", CreateFFIType[typeExpr], DeleteFFIType]},
					iNumericArrayToBuffer[expr, Compile`BorrowManagedObject[type]]		
				]		
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iBufferToNumericArray -> Typed[iBufferToNumericArray, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"],
	iNumericArrayToBuffer -> Typed[iNumericArrayToBuffer, {"InertExpression", "InertExpression"} -> "InertExpression"]
|>];


(* DownValues *)

(* TODO: Add fallthrough *)
BufferToNumericArray[man_ManagedExpression, len_] :=
	BufferToNumericArray[GetManagedExpression[man], len]

BufferToNumericArray[HoldPattern[RawPointer][addr_, ty_], len_] :=
	iBufferToNumericArray[OpaqueRawPointer[addr], ty, len]


NumericArrayToBuffer[arr_, ty_] :=
	With[{res = iNumericArrayToBuffer[arr, ty]},
		If[FailureQ[res],
			res,
			RawPointer[res, ty]
		]
	]



(******* StringToBuffer / BufferToString *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[iStringToBuffer,
			Typed[{"String"} -> "InertExpression"]@
			Function[str,
				PointerToExpression@Cast[CreateTypeInstance["CString", str], "OpaqueRawPointer", "BitCast"]
			]
		],

		FunctionDeclaration[iBufferToString,
			Typed[{"InertExpression"} -> "String"]@
			Function[carr,
				CreateTypeInstance["String", Cast[ExpressionToPointer[carr], "CString", "BitCast"]]
			]
		]
}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> {
	iStringToBuffer,
	iBufferToString
}];


(* DownValues *)

(* TODO: Add fallthrough *)
StringToBuffer[str_String] :=
	RawPointer[iStringToBuffer[str], "UnsignedInteger8"]


BufferToString[ptr_] :=
	iBufferToString[OpaqueRawPointer[ptr]]



(******* BufferToList / ListToBuffer *******)
DeclareCompiledComponent["ForeignFunctionInterface", {

		FunctionDeclaration[iBufferToList,
			Typed[{"InertExpression", "FFIType", "MachineInteger"} -> "InertExpression"]@
			Function[{ptrExpr, type, len},

				Module[{ptr, elemSize, expr},
					ptr = ExpressionToPointer[ptrExpr];
					elemSize = FFITypeByteCount[type];

					expr = Native`PrimitiveFunction["CreateHeaded_IE_E"][len, InertExpression[List]];
					Do[
						Native`PrimitiveFunction["SetElement_EIE_Void"][
							expr,
							i,
							CToExpression[
								Cast[Cast[ptr, "UnsignedInteger64", "BitCast"] + elemSize * (i-1), "OpaqueRawPointer", "BitCast"],
								type
							]
						],
						{i, len}
					];

					expr
				]
			]
		],

		FunctionDeclaration[iBufferToList,
			Typed[{"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{ptr, typeExpr, len},
				Module[{type = CreateTypeInstance["Managed", CreateFFIType[typeExpr], DeleteFFIType]},
					iBufferToList[ptr, Compile`BorrowManagedObject[type], len]		
				]		
			]
		],


		FunctionDeclaration[iListToBuffer,
			Typed[{"InertExpression", "FFIType"} -> "InertExpression"]@
			Function[{list, type},

				Module[{len, ptr},

					If[Head[list] =!= InertExpression[List],
						Native`ThrowWolframExceptionCode["Argument"]
					];

					len = Length[list];

					ptr = Cast[
						CreateTypeInstance["CArray"::["Integer8"], Cast[FFITypeByteCount[type]*len, "MachineInteger", "CCast"]],
						"OpaqueRawPointer", "BitCast"
					];

					Do[
						iPopulateBuffer[ptr, type, list[[i]], i-1],
						{i, len}
					];

					PointerToExpression[ptr]

				]
			]
		],

		FunctionDeclaration[iListToBuffer,
			Typed[{"InertExpression", "InertExpression"} -> "InertExpression"]@
			Function[{list, type},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					iListToBuffer[list, Compile`BorrowManagedObject[ffiType]]
				]
			]
		]

}];


DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iBufferToList -> Typed[iBufferToList, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"],
	iListToBuffer -> Typed[iListToBuffer, {"InertExpression", "InertExpression"} -> "InertExpression"]
|>];


(* DownValues *)

(* TODO: Add fallthrough *)
BufferToList[man_ManagedExpression, len_] :=
	BufferToList[GetManagedExpression[man], len]

BufferToList[ptr:HoldPattern[RawPointer][addr_Integer, ty_], len_Integer] :=
	iBufferToList[OpaqueRawPointer[ptr], ty, len]


ListToBuffer[list_, type_] :=
	With[{res = iListToBuffer[list, type]},
		If[FailureQ[res],
			res,
			RawPointer[res, type]
		]
	]



(***************************************************)
(******* Extracting expressions from buffers *******)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Dereferencing / indexing *******)

		FunctionDeclaration[iDereferenceBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "FFIType", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				CToExpression[
					Cast[Cast[ptr, "UnsignedInteger64", "BitCast"] + FFITypeByteCount[type] * offset, "OpaqueRawPointer", "BitCast"],
					type
				]
			]
		],

		FunctionDeclaration[iDereferenceBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, offset},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					iDereferenceBuffer[ptr, Compile`BorrowManagedObject[ffiType], offset]
				]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[iDereferenceBuffer,
			Typed[{"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{expr, type, offset},
				iDereferenceBuffer[ExpressionToPointer[expr], type, offset]
			]
		],

		(******* 2-argument form *******)

		FunctionDeclaration[iDereferenceBuffer,
			Typed[ForAllType[p, (*Element[p, {"InertExpression", "OpaqueRawPointer"}],*) {p, "InertExpression"} -> "InertExpression"]]@
			Function[{ptr, type},
				iDereferenceBuffer[ptr, type, 0]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iDereferenceBuffer -> Typed[iDereferenceBuffer, {"InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

DereferenceBuffer[man_ManagedExpression, offset_] :=
	DereferenceBuffer[GetManagedExpression[man], offset]

DereferenceBuffer[ptr:HoldPattern[RawPointer][addr_Integer, ty_], offset_] :=
	iDereferenceBuffer[OpaqueRawPointer[ptr], ty, offset]

DereferenceBuffer[ptr_] :=
	DereferenceBuffer[ptr, 0]



(***************************************************)
(********* Writing expressions into buffers ********)
(***************************************************)


DeclareCompiledComponent["ForeignFunctionInterface", {

		(******* Populating *******)

		FunctionDeclaration[iPopulateBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "FFIType", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, val, offset},
				ExpressionToC[
					Cast[Cast[ptr, "UnsignedInteger64", "BitCast"] + FFITypeByteCount[type] * offset, "OpaqueRawPointer", "BitCast"],
					type,
					val
				];
				PointerToExpression[ptr]
			]
		],

		FunctionDeclaration[iPopulateBuffer,
			Typed[ForAllType[ty, {"OpaqueRawPointer", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]]@
			Function[{ptr, type, val, offset},
				Module[{ffiType},
					ffiType = CreateTypeInstance["Managed", CreateFFIType[type], DeleteFFIType];
					iPopulateBuffer[ptr, Compile`BorrowManagedObject[ffiType], val, offset]
				]
			]
		],

		(******* Expression case *******)

		FunctionDeclaration[iPopulateBuffer,
			Typed[{"InertExpression", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]@
			Function[{expr, type, val, offset},
				iPopulateBuffer[ExpressionToPointer[expr], type, val, offset]
			]
		],

		(******* 3-argument form *******)

		FunctionDeclaration[iPopulateBuffer,
			Typed[ForAllType[p, (*Element[p, {"InertExpression", "OpaqueRawPointer"}],*) {p, "InertExpression", "InertExpression"} -> "InertExpression"]]@
			Function[{ptr, val, type},
				iPopulateBuffer[ptr, type, val, 0]
			]
		]

}];

DeclareCompiledComponent["ForeignFunctionInterface", "InstalledFunctions" -> <|
	iPopulateBuffer -> Typed[iPopulateBuffer, {"InertExpression", "InertExpression", "InertExpression", "MachineInteger"} -> "InertExpression"]
|>];


(* Down values *)

PopulateBuffer[man_ManagedExpression, val_, offset_] :=
	PopulateBuffer[GetManagedExpression[man], val, offset]

PopulateBuffer[ptr:HoldPattern[RawPointer][addr_Integer, ty_], val_, offset_] :=
	With[{res = iPopulateBuffer[OpaqueRawPointer[ptr], ty, val, offset]},
		If[FailureQ[res],
			res,
			RawPointer[res, ty]
		]
	]

PopulateBuffer[ptr_, val_] :=
	PopulateBuffer[ptr, val, 0]



End[] (* End `Private` *)

EndPackage[]
