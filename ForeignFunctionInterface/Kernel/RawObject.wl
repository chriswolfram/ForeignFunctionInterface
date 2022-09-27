BeginPackage["ForeignFunctionInterface`RawObject`", {
	"ForeignFunctionInterface`",
	"Compile`Utilities`Components`"
}]


CreateRawObject


Begin["`Private`"]



CompiledComponentAppendTo["ForeignFunctionInterface", {

	(******* Base RawObject declarations *******)

	TypeDeclaration["Abstract", "DirectBoxables"],

	TypeDeclaration[TypeFramework`TypeInstance["DirectBoxables", "String"]],
	TypeDeclaration[TypeFramework`TypeInstance["DirectBoxables", {Element["t", "DataStructures"]}, "t"]],
	TypeDeclaration[TypeFramework`TypeInstance["DirectBoxables", {Element["t", "Numbers"]}, "t"]],


	TypeDeclaration["Product", "RawObject"::[t],
			<|
				"Object" -> t
			|>,
			"AbstractTypes" -> {"DataStructures"}
		],

		FunctionDeclaration[CreateRawObject,
			Typed[ForAllType[t, NotElement[t, "DirectBoxables"], {t} -> "RawObject"::[t]]]@
			Function[obj,
				CreateTypeInstance["RawObject", <|"Object" -> obj|>]
			]
		],

		FunctionDeclaration[CreateRawObject,
			Typed[ForAllType[t, Element[t, "DirectBoxables"], {t} -> t]]@
			Function[obj,
				obj
			]
		],


		(******* CArray *******)

		FunctionDeclaration[CreateCArray,
			Typed[{"MachineInteger"} -> "RawObject"::["OpaqueRawPointer"]]@
			Function[len,
				CreateRawObject@Cast[
					CreateTypeInstance["CArray"::["Integer8"], len],
					"OpaqueRawPointer", "BitCast"
				]
			]
		],

		FunctionDeclaration[FreeMemory,
			Typed[{"RawObject"::["OpaqueRawPointer"]} -> "Null"]@
			Function[pointer,
				DeleteObject[Cast[pointer["Object"], "CArray"::["Integer8"], "BitCast"]]
			]
		]

}];


CompiledComponentAppendTo["ForeignFunctionInterface", "InstalledFunctions", {
	CreateCArray,
	FreeMemory
}];



End[] (* End `Private` *)

EndPackage[]
