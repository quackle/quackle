include_guard()

message("-- Including settings")
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
  add_compile_options(/MP)
endif()

# Sanitizers. All off by default; enable with e.g.
#   cmake -DQUACKLE_SANITIZE_ADDRESS=ON ...
# in a fresh build directory (sanitizers change codegen and the runtime
# libraries linked in, so don't flip them on in an existing build tree).
#
# Address and Thread cannot be combined; Undefined pairs with either.
option(QUACKLE_SANITIZE_ADDRESS
  "Build with AddressSanitizer: detects use-after-free, buffer overflow, leaks" OFF)
option(QUACKLE_SANITIZE_UNDEFINED
  "Build with UndefinedBehaviorSanitizer: detects UB such as signed overflow, bad shifts, misaligned access" OFF)
option(QUACKLE_SANITIZE_THREAD
  "Build with ThreadSanitizer: detects data races (incompatible with ASan)" OFF)

set(_quackle_sanitizers "")
set(_quackle_sanitizer_flags "")

if(QUACKLE_SANITIZE_ADDRESS AND QUACKLE_SANITIZE_THREAD)
  message(FATAL_ERROR
    "QUACKLE_SANITIZE_ADDRESS and QUACKLE_SANITIZE_THREAD are mutually exclusive; enable only one.")
endif()

if(MSVC)
  # MSVC ships AddressSanitizer only. The compiler driver tells the linker
  # which sanitizer runtime to pull in, so no separate link flag is needed.
  if(QUACKLE_SANITIZE_ADDRESS)
    list(APPEND _quackle_sanitizers "address")
    add_compile_options(/fsanitize=address)
    # ASan is incompatible with edit-and-continue debug info and with the
    # runtime checks MSVC enables in Debug builds.
    add_compile_options($<$<CONFIG:Debug>:/Zi>)
    add_link_options(/INCREMENTAL:NO)
  endif()
  if(QUACKLE_SANITIZE_UNDEFINED)
    message(FATAL_ERROR "QUACKLE_SANITIZE_UNDEFINED is not supported by MSVC (no UBSan implementation).")
  endif()
  if(QUACKLE_SANITIZE_THREAD)
    message(FATAL_ERROR "QUACKLE_SANITIZE_THREAD is not supported by MSVC (no TSan implementation).")
  endif()
else()
  if(QUACKLE_SANITIZE_ADDRESS)
    list(APPEND _quackle_sanitizers "address")
  endif()
  if(QUACKLE_SANITIZE_UNDEFINED)
    list(APPEND _quackle_sanitizers "undefined")
  endif()
  if(QUACKLE_SANITIZE_THREAD)
    list(APPEND _quackle_sanitizers "thread")
  endif()

  if(_quackle_sanitizers)
    string(REPLACE ";" "," _quackle_sanitizer_list "${_quackle_sanitizers}")
    set(_quackle_sanitizer_flags "-fsanitize=${_quackle_sanitizer_list}")
    # Frame pointers and debug info: without them sanitizer reports have no
    # usable stack traces, which is the whole point of turning these on.
    add_compile_options(${_quackle_sanitizer_flags} -fno-omit-frame-pointer -g)
    add_link_options(${_quackle_sanitizer_flags})
  endif()
endif()

message(STATUS "Sanitizers (all default OFF; set in a fresh build dir):")
message(STATUS "    QUACKLE_SANITIZE_ADDRESS   = ${QUACKLE_SANITIZE_ADDRESS}    (AddressSanitizer)")
message(STATUS "    QUACKLE_SANITIZE_UNDEFINED = ${QUACKLE_SANITIZE_UNDEFINED}    (UndefinedBehaviorSanitizer)")
message(STATUS "    QUACKLE_SANITIZE_THREAD    = ${QUACKLE_SANITIZE_THREAD}    (ThreadSanitizer, not combinable with ASan)")
if(_quackle_sanitizers)
  string(REPLACE ";" ", " _quackle_sanitizers_pretty "${_quackle_sanitizers}")
  message(STATUS "    -> enabled: ${_quackle_sanitizers_pretty}")
else()
  message(STATUS "    -> none enabled")
endif()

# Commented out so that automated builds can work
# but we'll need to set this and set up matching Qt libs
# for building a final app bundle
#
# Note that Qt6 uses std::filesystem which requires minimum of 10.15
# Qt5 can be set lower (was working fine with 10.10)
#
# set(CMAKE_OSX_DEPLOYMENT_TARGET 10.15)
