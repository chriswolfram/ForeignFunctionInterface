
find_program(CVS cvs PATHS /usr/bin /usr/local/bin)
if(CVS_NOTFOUND)
	message("program cvs is not found")
endif()

set(CVS_QUIET "-Q")
set(CHDIRCOMMAND ${CMAKE_COMMAND} -E chdir)

# cvs_checkout(cvs_path) invoke 'cvs co ${cvs_path}'
# cvs_checkout(cvs_path local_path) invoke 'cvs co -d ${local_path} ${cvs_path}'
# cvs_checkout(cvs_path local_path work_dir) does cvs checkout in directory ${work_dir}
# The default value of work_dir is ${CMAKE_BINARY_DIR}
function(cvs_checkout)
	if(ARGC GREATER_EQUAL 1)
		set(cvs_path "${ARGV0}")
	endif()
	unset(local_path)
	if(ARGC GREATER_EQUAL 2)
		set(local_path "-d${ARGV1}")
	endif()
	if(ARGC GREATER_EQUAL 3)
		set(work_dir "${ARGV2}")
	else()
		set(work_dir ${CMAKE_BINARY_DIR})
	endif()
	if(local_path)
		set(${ARGV1}_DIR "${work_dir}/${ARGV1}" PARENT_SCOPE)
	endif()
	message("${CHDIRCOMMAND} ${work_dir} ${CVS} ${CVS_QUIET} co ${local_path} ${cvs_path}")
	execute_process(
		COMMAND ${CHDIRCOMMAND} ${work_dir} ${CVS} ${CVS_QUIET} co ${local_path} ${cvs_path}
	)
endfunction(cvs_checkout)

if(${SYSTEMID} STREQUAL "MacOSX-ARM64")
	cvs_checkout("Components/LIBFFI/3.4.4/MacOSX-ARM64/libcxx-min11.0" "LIBFFI")
endif()

if(${SYSTEMID} STREQUAL "MacOSX-x86-64")
	cvs_checkout("Components/LIBFFI/3.4.4/MacOSX-x86-64/libcxx-min10.15" "LIBFFI")
endif()

if(${SYSTEMID} STREQUAL "Windows-x86-64")
	cvs_checkout("Components/LIBFFI/3.4.4/Windows-x86-64/vc143" "LIBFFI")
endif()

if(${SYSTEMID} STREQUAL "Linux-x86-64")
	cvs_checkout("Components/LIBFFI/3.4.4/Linux-x86-64/nocona-glibc2.17" "LIBFFI")
endif()

if(${SYSTEMID} STREQUAL "Linux-ARM64")
	cvs_checkout("Components/LIBFFI/3.4.4/Linux-ARM64/aarch64-glibc2.31-gcc10.2" "LIBFFI")
endif()

if(${SYSTEMID} STREQUAL "Linux-ARM")
	cvs_checkout("Components/LIBFFI/3.4.4/Linux-ARM/armv6-glibc2.28-gcc8.3" "LIBFFI")
endif()








