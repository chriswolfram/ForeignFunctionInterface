(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "ChristopherWolfram/ForeignFunctionInterface",
    "Description" -> "Efficiently connect to C-compatible libraries",
    "Creator" -> "Christopher Wolfram",
    "License" -> "MIT",
    "PublisherID" -> "ChristopherWolfram",
    "Version" -> "1.0.0",
    "WolframVersion" -> "13.2+",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> "ChristopherWolfram`ForeignFunctionInterface`",
        "Symbols" -> {
          "ChristopherWolfram`ForeignFunctionInterface`BufferToList",
          "ChristopherWolfram`ForeignFunctionInterface`BufferToNumericArray",
          "ChristopherWolfram`ForeignFunctionInterface`BufferToString",
          "ChristopherWolfram`ForeignFunctionInterface`CallForeignFunction",
          "ChristopherWolfram`ForeignFunctionInterface`CreateBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`CreateCallback",
          "ChristopherWolfram`ForeignFunctionInterface`CreateForeignFunction",
          "ChristopherWolfram`ForeignFunctionInterface`CreateManagedExpression",
          "ChristopherWolfram`ForeignFunctionInterface`DereferenceBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`DeleteBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`DeleteCallback",
          "ChristopherWolfram`ForeignFunctionInterface`GetManagedExpression",
          "ChristopherWolfram`ForeignFunctionInterface`ListToBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`NumericArrayToBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`OpaqueRawPointer",
          "ChristopherWolfram`ForeignFunctionInterface`PopulateBuffer",
          "ChristopherWolfram`ForeignFunctionInterface`StringToBuffer"
        }
      },
      {"Compiler", "Components" -> {"ForeignFunctionInterface"}},
      {"LibraryLink"},
      {"Documentation", "Language" -> "English"}
    },
    "SystemID" -> {"Linux", "Linux-x86-64", "MacOSX-x86-64"}
  |>
]
