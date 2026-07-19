cmake_minimum_required (VERSION 3.16)

project (makegaddag)

message("-- Configuring makegaddag")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

set(QT_VERSION "6" CACHE STRING "Version of Qt (anything past the first digit is ignored)")

if("${QT_VERSION}" MATCHES "^6")
	find_package(Qt6 REQUIRED COMPONENTS Core)
else()
	find_package(Qt5 REQUIRED COMPONENTS Core)
endif()

# makegaddag.pro: standalone console utility for building gaddag files.
add_executable(makegaddag
	makegaddag.cpp
)
if("${QT_VERSION}" MATCHES "^6")
	target_link_libraries(makegaddag quackleio quackle Qt6::Core)
else()
	target_link_libraries(makegaddag quackleio quackle Qt5::Core)
endif()
