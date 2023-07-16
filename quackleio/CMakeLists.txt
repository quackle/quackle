cmake_minimum_required (VERSION 3.10)

project (quackleio)

message("-- Configuring libquackleio")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

set(QT_VERSION "5" CACHE STRING "Version of Qt (anything past the first digit is ignored)")

if("${QT_VERSION}" MATCHES "^6")
	find_package(Qt6 REQUIRED COMPONENTS Core Widgets)
else()
	find_package(Qt5 REQUIRED COMPONENTS Core Widgets)
endif()

set(QUACKLEIO_SOURCES
	dawgfactory.cpp
	dict.cpp
	dictfactory.cpp
	dictimplementation.cpp
	flexiblealphabet.cpp
	froggetopt.cpp
	gaddagfactory.cpp
	gcgio.cpp
	queenie.cpp
	streamingreporter.cpp
	util.cpp
)

set(QUACKLEIO_HEADERS
	dawgfactory.h
	dict.h
	dictfactory.h
	dictimplementation.h
	flexiblealphabet.h
	froggetopt.h
	gaddagfactory.h
	gcgio.h
	logania.h
	queenie.h
	streamingreporter.h
	util.h
)

add_library(quackleio
	${QUACKLEIO_SOURCES} ${QUACKLEIO_HEADERS}
)
target_include_directories(quackleio PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/..")
if("${QT_VERSION}" MATCHES "^6")
	target_link_libraries(quackleio Qt6::Core)
else()
	target_link_libraries(quackleio Qt5::Core)
endif()
