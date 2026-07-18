cmake_minimum_required (VERSION 3.16)

project (quackle-tests)

message("-- Configuring test apps")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

set(QT_VERSION "6" CACHE STRING "Version of Qt (anything past the first digit is ignored)")

if("${QT_VERSION}" MATCHES "^6")
	find_package(Qt6 REQUIRED COMPONENTS Core)
else()
	find_package(Qt5 REQUIRED COMPONENTS Core)
endif()

if("${QT_VERSION}" MATCHES "^6")
	set(QUACKLE_TEST_QT_CORE Qt6::Core)
else()
	set(QUACKLE_TEST_QT_CORE Qt5::Core)
endif()

# test.pro: game-driving test harness (positions, selfplay, reports, etc.)
add_executable(test
	testharness.cpp
	testharness.h
	testmain.cpp
	trademarkedboards.cpp
	trademarkedboards.h
)
target_link_libraries(test quackleio libquackle ${QUACKLE_TEST_QT_CORE})

# quackle.pro's excluded quackletest.cpp: standalone libquackle smoke test,
# no Qt dependency.
add_executable(quackletest
	"${CMAKE_CURRENT_SOURCE_DIR}/../quackletest.cpp"
)
target_link_libraries(quackletest libquackle)

# quackleio/iotest/iotest.pro: GCG read/write round-trip test.
add_executable(iotest
	"${CMAKE_CURRENT_SOURCE_DIR}/../quackleio/iotest/iotest.cpp"
	"${CMAKE_CURRENT_SOURCE_DIR}/../quackleio/iotest/trademarkedboards.cpp"
	"${CMAKE_CURRENT_SOURCE_DIR}/../quackleio/iotest/trademarkedboards.h"
)
target_include_directories(iotest PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../quackleio")
target_link_libraries(iotest quackleio libquackle ${QUACKLE_TEST_QT_CORE})
