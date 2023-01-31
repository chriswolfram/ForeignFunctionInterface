BeginPackage["ChristopherWolfram`ForeignFunctionInterface`BaseTypeConversion`"]

ToBaseType

ToBaseValue
FromBaseValue

Begin["`Private`"]

Needs["ChristopherWolfram`ForeignFunctionInterface`"]


(*
	libffi supports a fixed set of types. These are the "base types". However, there are
	other types which are sugar on top of these base types. For example, libffi only has
	OpaqueRawPointer, so RawPointer is converted to an OpaqueRawPointer before being passed
	to libffi, and are converted back when returned.
*)


(*
	ToBaseType[type]
		gets the base type representation of a type.

		For example, RawPointer types are represented with OpaqueRawPointer (a base type).
*)

ToBaseType["Void"] := "Void"
ToBaseType["UnsignedInteger8"] := "UnsignedInteger8"
ToBaseType["Integer8"] := "Integer8"
ToBaseType["UnsignedInteger16"] := "UnsignedInteger16"
ToBaseType["Integer16"] := "Integer16"
ToBaseType["UnsignedInteger32"] := "UnsignedInteger32"
ToBaseType["Integer32"] := "Integer32"
ToBaseType["UnsignedInteger64"] := "UnsignedInteger64"
ToBaseType["Integer64"] := "Integer64"
ToBaseType["CFloat"] := "CFloat"
ToBaseType["CDouble"] := "CDouble"
ToBaseType["CChar"] := "CChar"
ToBaseType["CUnsignedChar"] := "CUnsignedChar"
ToBaseType["CSignedChar"] := "CSignedChar"
ToBaseType["CUnsignedShort"] := "CUnsignedShort"
ToBaseType["CShort"] := "CShort"
ToBaseType["CUnsignedInt"] := "CUnsignedInt"
ToBaseType["CInt"] := "CInt"
ToBaseType["CUnsignedLong"] := "CUnsignedLong"
ToBaseType["CLong"] := "CLong"
ToBaseType["OpaqueRawPointer"] := "OpaqueRawPointer"

ToBaseType["RawPointer"::[_]] := "OpaqueRawPointer"
ToBaseType["RawPointer"[_]] := "OpaqueRawPointer"

(* TODO: Arguably this should at least print a message *)
ToBaseType[type_] := type



(* TODO: Arguably both ToBaseValue and FromBaseValue should do some error checking *)
(*
	ToBaseValue[val]
		gets the base value representation of a value.

		For example, RawPointer[...] types are represented with OpaqueRawPointer[...] (a base value).
*)

ToBaseValue[man_ManagedExpression] := ToBaseValue[GetManagedExpression[man]]

ToBaseValue[ptr_RawPointer] := OpaqueRawPointer[ptr]

ToBaseValue[val_] := val



(*
	FromBaseValue[baseVal, type]
		converts a base value to a full value.

		For example, the base value OpaqueRawPointer[...] might have a full value of RawPointer[...].
*)

FromBaseValue[ptr_OpaqueRawPointer, "RawPointer"::[type_] | "RawPointer"[type_]] := RawPointer[ptr, type]

FromBaseValue[val_, type_] := val



End[] (* End `Private` *)

EndPackage[]
