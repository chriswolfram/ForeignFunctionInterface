cmake_minimum_required(VERSION 3.16.0)

set(PACLET_NAME ForeignFunctionInterface)

# setup WRICMakeModules_DIR which is git clone of repo
# https://stash.wolfram.com/projects/SHAR/repos/wricmakemodules
if(NOT WRICMakeModules_DIR)
	if(DEFINED ENV{WRICMakeModules_DIR})
		Set(WRICMakeModules_DIR "$ENV{WRICMakeModules_DIR}" CACHE PATH "")
	elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../WRICMakeModules")
		Set(WRICMakeModules_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../WRICMakeModules" CACHE PATH "")
	elseif(EXISTS "${CMAKE_BINARY_DIR}/WRICMakeModules")
		Set(WRICMakeModules_DIR "${CMAKE_BINARY_DIR}/WRICMakeModules" CACHE PATH "")
	else()
		execute_process(
			COMMAND git clone ssh://git@stash.wolfram.com:7999/shar/WRICMakeModules.git
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
			RESULT_VARIABLE GIT_STATUS
		)
		if(GIT_STATUS AND NOT GIT_STATUS EQUAL 0)
			message(FATAL_ERROR "git clone repo WRICMakeModules FAILED")
		endif()
		Set(WRICMakeModules_DIR "${CMAKE_BINARY_DIR}/WRICMakeModules" CACHE PATH "")
	endif()
endif()

include(${WRICMakeModules_DIR}/Paclets/Paclets.cmake)

# os_default_settings called before project(...)
os_default_settings()

project(ForeignFunctionInterface)

include(${CMAKE_CURRENT_SOURCE_DIR}/scripts/cvs.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/scripts/wl.cmake)

set(ffiConstants_SRC
	${CMAKE_CURRENT_SOURCE_DIR}/ffiConstants/ffiConstants.c
)

add_library(ffiConstants SHARED ${ffiConstants_SRC})

target_include_directories(ffiConstants PRIVATE 
	${LIBFFI_DIR}/include
)

message("LIBFFI_DIR: ${LIBFFI_DIR}")
find_library(FFI_LIBRARY NAMES "ffi-8" "ffi.8" "ffi" PATHS "${LIBFFI_DIR}/lib" "${LIBFFI_DIR}/lib64" NO_DEFAULT_PATH)

message("FFI_LIBRARY: ${FFI_LIBRARY}")
target_link_libraries(ffiConstants PRIVATE ${FFI_LIBRARY})
set_target_properties(ffiConstants PROPERTIES PREFIX "")
set_paclet_library_properties(ffiConstants)

if(RE_BUILD)
		wri_strip(ffiConstants)
		wri_install_symbols(ffiConstants ${DEBUG_FILES_INSTALL_DIR})
endif()

install(TARGETS ffiConstants DESTINATION ${WRI_LIB_INSTALL_DIR})


if(NOT DEFINED WOLFRAM_KERNEL_PATH)
	set(MATHEMATICA_VERSION 13.3)
	get_wolfram_kernel(${MATHEMATICA_VERSION} WOLFRAM_KERNEL_PATH)
	if(NOT EXISTS ${WOLFRAM_KERNEL_PATH})
		message(FATAL_ERROR "Failed to find Wolfram Kernel")
	endif()
endif()

set(WL_MODULE_SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/${WL_MODULE}")
set(ForeignFunctionInterface_DLL ${CMAKE_BINARY_DIR}/ForeignFunctionInterface${CMAKE_SHARED_LIBRARY_SUFFIX})

add_custom_command(
	OUTPUT ${ForeignFunctionInterface_DLL}
	COMMAND
	${WOLFRAM_KERNEL_PATH} -noprompt -script ${CMAKE_CURRENT_SOURCE_DIR}/scripts/BuildForeignFunctionInterfaceLibrary.wl -basedir ${CMAKE_CURRENT_SOURCE_DIR} -outputdir ${CMAKE_BINARY_DIR} -libffi ${FFI_LIBRARY}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts
	DEPENDS ffiConstants
	VERBATIM
	COMMENT	"Built target ForeignFunctionInterface"
)

add_custom_target(ForeignFunctionInterface ALL
 DEPENDS ${ForeignFunctionInterface_DLL}
)

install(FILES 
	${ForeignFunctionInterface_DLL}
	DESTINATION ${WRI_LIB_INSTALL_DIR}
)


