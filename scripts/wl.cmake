# Get default location of Mathematica #########################################

function(get_default_mathematica_dir MATHEMATICA_VERSION DEFAULT_MATHEMATICA_INSTALL_DIR)
	set(_M_INSTALL_DIR NOTFOUND)
	if(APPLE)
	 	find_path(_M_INSTALL_DIR "Contents" PATHS 
			"/Applications/Mathematica ${MATHEMATICA_VERSION}.app"
			"/Applications/Mathematica.app"
		)
		set(_M_INSTALL_DIR "${_M_INSTALL_DIR}/Contents")
	elseif(WIN32)
		set(_M_INSTALL_DIR "C:/Program\ Files/Wolfram\ Research/Mathematica/${MATHEMATICA_VERSION}")
	else()
		set(_M_INSTALL_DIR "/usr/local/Wolfram/Mathematica/${MATHEMATICA_VERSION}")
	endif()
	if(NOT IS_DIRECTORY "${_M_INSTALL_DIR}" AND IS_DIRECTORY "$ENV{MATHEMATICA_HOME}")
		set(_M_INSTALL_DIR "$ENV{MATHEMATICA_HOME}")
	endif()
	set(${DEFAULT_MATHEMATICA_INSTALL_DIR} "${_M_INSTALL_DIR}" PARENT_SCOPE)
endfunction()

# Get default location of Mathematica #########################################

function(get_wolfram_kernel MATHEMATICA_VERSION DEFAULT_WOLFRAM_KERNEL_PATH)
	get_default_mathematica_dir(${MATHEMATICA_VERSION} _M_INSTALL_DIR)
	if(WIN32)
		set(${DEFAULT_WOLFRAM_KERNEL_PATH} "${_M_INSTALL_DIR}/wolfram.exe" PARENT_SCOPE)
	elseif (APPLE)
		set(${DEFAULT_WOLFRAM_KERNEL_PATH} "${_M_INSTALL_DIR}/MacOS/WolframKernel" PARENT_SCOPE)
	else()
		set(${DEFAULT_WOLFRAM_KERNEL_PATH} "${_M_INSTALL_DIR}/Executables/WolframKernel" PARENT_SCOPE)
	endif()	
endfunction()
