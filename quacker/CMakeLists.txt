cmake_minimum_required (VERSION 3.10)

project (Quackle VERSION 1.0.4)

message("-- Configuring Quackle")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

option(BUNDLE "Build app bundle on macOS; does not populate with resources" OFF)
set(QT_VERSION "6" CACHE STRING "Version of Qt (anything past the first digit is ignored)")


add_subdirectory(.. libquackle)
add_subdirectory(../quackleio quackleio)
add_subdirectory(../data data)

if("${QT_VERSION}" MATCHES "^6")
	# See quackleio/CMakeLists.txt for why this is set.
	set(QT_SKIP_AUTO_PLUGIN_INCLUSION ON)
	find_package(Qt6 REQUIRED COMPONENTS Core Widgets)
else()
	find_package(Qt5 REQUIRED COMPONENTS Core Widgets)
endif()

set(CMAKE_AUTOMOC ON)

set(QUACKLE_SOURCES
	bagdisplay.cpp
	boarddisplay.cpp
	boardsetup.cpp
	boardsetupdialog.cpp
	brb.cpp
	configdialog.cpp
	configpages.cpp
	dashboard.cpp
	geometry.cpp
	graphicalboard.cpp
	graphicalreporter.cpp
	history.cpp
	letterbox.cpp
	letterboxsettings.cpp
	lexicondialog.cpp
	lister.cpp
	main.cpp
	movebox.cpp
	newgame.cpp
	noteeditor.cpp
	oppothread.cpp
	oppothreadprogressbar.cpp
	quacker.cpp
	quackersettings.cpp
	rackdisplay.cpp
	settings.cpp
	simviewer.cpp
	view.cpp
	widgetfactory.cpp
)

set(QUACKLE_HEADERS
	bagdisplay.h
	boarddisplay.h
	boardsetup.h
	boardsetupdialog.h
	brb.h
	configdialog.h
	configpages.h
	customqsettings.h
	dashboard.h
	geometry.h
	graphicalboard.h
	graphicalreporter.h
	history.h
	letterbox.h
	letterboxsettings.h
	lexicondialog.h
	lister.h
	movebox.h
	newgame.h
	noteeditor.h
	oppothread.h
	oppothreadprogressbar.h
	quacker.h
	quackersettings.h
	rackdisplay.h
	settings.h
	simviewer.h
	view.h
	widgetfactory.h
)

set(QUACKLE_RESOURCES
	"${PROJECT_SOURCE_DIR}/quacker.icns"
	# ${DATA_ALPHABETS}
)

if(APPLE AND BUNDLE)
	add_executable(Quackle
		MACOSX_BUNDLE
		${QUACKLE_SOURCES} ${QUACKLE_HEADERS} quacker.icns
	)
else()
	add_executable(Quackle
		WIN32
		${QUACKLE_SOURCES} ${QUACKLE_HEADERS} quacker.rc
	)
endif()

target_link_libraries(Quackle quackleio libquackle)
if("${QT_VERSION}" MATCHES "^6")
	target_link_libraries(Quackle Qt6::Core Qt6::Widgets)
	if(APPLE)
		# Qt6's FindWrapOpenGL.cmake (used by Qt6Gui through 6.4ish) links
		# -framework AGL unconditionally, even though nothing actually calls
		# into it. Xcode 15+ dropped AGL.framework from the SDK entirely, so
		# the link fails there even though the framework is still resolvable
		# at runtime via the dyld shared cache. Point the linker at any
		# installed SDK that still ships the (unused) stub, if one is
		# present. Don't hardcode a specific SDK version here: it varies by
		# machine (and by whichever Xcode/CLT happens to be installed on a
		# given CI runner).
		if(NOT QUACKLE_AGL_FRAMEWORK_DIR)
			file(GLOB _quackle_agl_sdk_roots
				"/Library/Developer/CommandLineTools/SDKs/MacOSX*.sdk"
				"/Applications/Xcode*.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX*.sdk"
			)
			list(SORT _quackle_agl_sdk_roots COMPARE NATURAL ORDER DESCENDING)
			set(_quackle_agl_frameworks_dirs "")
			foreach(_quackle_agl_sdk_root ${_quackle_agl_sdk_roots})
				list(APPEND _quackle_agl_frameworks_dirs "${_quackle_agl_sdk_root}/System/Library/Frameworks")
			endforeach()
			list(APPEND _quackle_agl_frameworks_dirs "/System/Library/Frameworks")
			foreach(_quackle_agl_frameworks_dir ${_quackle_agl_frameworks_dirs})
				if(EXISTS "${_quackle_agl_frameworks_dir}/AGL.framework")
					set(QUACKLE_AGL_FRAMEWORK_DIR "${_quackle_agl_frameworks_dir}" CACHE PATH "Directory containing an AGL.framework usable for linking Qt6 on Apple")
					break()
				endif()
			endforeach()
		endif()
		if(QUACKLE_AGL_FRAMEWORK_DIR)
			target_link_options(Quackle PRIVATE "-F${QUACKLE_AGL_FRAMEWORK_DIR}")
		endif()
	endif()
else()
	target_link_libraries(Quackle Qt5::Core Qt5::Widgets)
endif()


if(APPLE)
	target_link_libraries(Quackle "-framework CoreFoundation")
	if (BUNDLE)
		set_target_properties(Quackle
				PROPERTIES
				RESOURCE ${QUACKLE_RESOURCES}
				MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Quackle.plist"
		)
	endif()
endif()
