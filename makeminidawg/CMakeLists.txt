cmake_minimum_required (VERSION 3.16)

project (makeminidawg)

message("-- Configuring makeminidawg")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

set(QT_VERSION "6" CACHE STRING "Version of Qt (anything past the first digit is ignored)")

if("${QT_VERSION}" MATCHES "^6")
	find_package(Qt6 REQUIRED COMPONENTS Core)
else()
	find_package(Qt5 REQUIRED COMPONENTS Core)
endif()

# makeminidawg.pro: standalone console utility for building Quackle dictionaries.
add_executable(makeminidawg
	makeminidawgmain.cpp
)
if("${QT_VERSION}" MATCHES "^6")
	target_link_libraries(makeminidawg quackleio quackle Qt6::Core)
else()
	target_link_libraries(makeminidawg quackleio quackle Qt5::Core)
endif()
