cmake_minimum_required (VERSION 3.16)

project (Quackle VERSION 1.1.0)

message("-- Configuring Quackle")

include("${CMAKE_CURRENT_SOURCE_DIR}/../Settings.cmake")

option(BUNDLE "Build a macOS .app bundle populated with data resources (use the macdeploy target to add Qt frameworks)" OFF)
set(QT_VERSION "6" CACHE STRING "Version of Qt (anything past the first digit is ignored)")


add_subdirectory(.. libquackle)
add_subdirectory(../quackleio quackleio)
add_subdirectory(../data data)
add_subdirectory(../test test)
add_subdirectory(../encodeleaves encodeleaves)
add_subdirectory(../makegaddag makegaddag)
add_subdirectory(../makeminidawg makeminidawg)
add_subdirectory(../gaddagize gaddagize)

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

target_precompile_headers(Quackle PRIVATE <QtWidgets>)

target_link_libraries(Quackle quackleio quackle)
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

	# On macOS the double-arrow glyphs used to indicate secondary board
	# arrows render poorly in the system fonts, so prefer the single-arrow
	# forms. This matches the historical qmake (quacker.pro) build.
	target_compile_definitions(Quackle PRIVATE FORCE_SECONDARY_ARROW_GLYPHS=1)

	if (BUNDLE)
		set_target_properties(Quackle
				PROPERTIES
				RESOURCE ${QUACKLE_RESOURCES}
				MACOSX_BUNDLE_INFO_PLIST "${CMAKE_CURRENT_SOURCE_DIR}/Quackle.plist"
		)

		# Populate the bundle with the data/ tree (alphabets, lexica,
		# strategy, themes). At runtime Settings looks these up via
		# CFBundleCopyResourceURL under Contents/Resources/data (see
		# quacker/settings.cpp). The DATA_* lists come from the data/
		# subdirectory added above; copy each file into the bundle while
		# preserving its path relative to data/ (strategy/ in particular
		# has subdirectories).
		set(_quackle_data_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/../data")
		set(_quackle_bundle_data_dir "$<TARGET_BUNDLE_CONTENT_DIR:Quackle>/Resources/data")

		set(_quackle_data_reldirs "")
		foreach(_quackle_data_file
				${DATA_ALPHABETS} ${DATA_LEXICA} ${DATA_STRATEGY} ${DATA_THEMES})
			file(RELATIVE_PATH _quackle_data_rel "${_quackle_data_src_dir}" "${_quackle_data_file}")
			get_filename_component(_quackle_data_reldir "${_quackle_data_rel}" DIRECTORY)
			string(MAKE_C_IDENTIFIER "${_quackle_data_reldir}" _quackle_data_key)
			if(NOT _quackle_data_reldir IN_LIST _quackle_data_reldirs)
				list(APPEND _quackle_data_reldirs "${_quackle_data_reldir}")
				set(_quackle_data_group_${_quackle_data_key} "")
			endif()
			list(APPEND _quackle_data_group_${_quackle_data_key} "${_quackle_data_file}")
		endforeach()

		foreach(_quackle_data_reldir ${_quackle_data_reldirs})
			string(MAKE_C_IDENTIFIER "${_quackle_data_reldir}" _quackle_data_key)
			add_custom_command(TARGET Quackle POST_BUILD
				COMMAND "${CMAKE_COMMAND}" -E make_directory
					"${_quackle_bundle_data_dir}/${_quackle_data_reldir}"
				COMMAND "${CMAKE_COMMAND}" -E copy_if_different
					${_quackle_data_group_${_quackle_data_key}}
					"${_quackle_bundle_data_dir}/${_quackle_data_reldir}"
				VERBATIM
			)
		endforeach()

		# `macdeploy` bundles the Qt frameworks and plugins into
		# Quackle.app so it runs on a machine without Qt installed and can
		# be signed and packaged into a .dmg. It is a separate target
		# (not run on every build) because it rewrites the bundle in place
		# and is only needed for distribution. Building it first (re)builds
		# Quackle, so the data copy above has already run.
		#
		# Qt6's package sets MACDEPLOYQT_EXECUTABLE; fall back to searching
		# the Qt bin dir for Qt5.
		if(NOT MACDEPLOYQT_EXECUTABLE)
			if("${QT_VERSION}" MATCHES "^6")
				get_target_property(_quackle_qmake_exe Qt6::qmake IMPORTED_LOCATION)
			else()
				get_target_property(_quackle_qmake_exe Qt5::qmake IMPORTED_LOCATION)
			endif()
			get_filename_component(_quackle_qt_bin_dir "${_quackle_qmake_exe}" DIRECTORY)
			find_program(MACDEPLOYQT_EXECUTABLE macdeployqt HINTS "${_quackle_qt_bin_dir}")
		endif()

		if(MACDEPLOYQT_EXECUTABLE)
			# Optional code-signing identity (a certificate name or SHA-1).
			# When set, macdeployqt signs the app (with a hardened runtime,
			# required for notarization). Leave empty for an unsigned local
			# build.
			set(QUACKLE_CODESIGN_IDENTITY "" CACHE STRING
				"Code-signing identity for the macdeploy target (empty = don't sign)")
			set(_quackle_macdeploy_args "")
			if(QUACKLE_CODESIGN_IDENTITY)
				list(APPEND _quackle_macdeploy_args
					"-codesign=${QUACKLE_CODESIGN_IDENTITY}"
					"-hardened-runtime")
			endif()

			add_custom_target(macdeploy
				COMMAND "${MACDEPLOYQT_EXECUTABLE}"
					"$<TARGET_BUNDLE_DIR:Quackle>"
					${_quackle_macdeploy_args}
				DEPENDS Quackle
				COMMENT "Bundling Qt frameworks into Quackle.app with macdeployqt"
				VERBATIM
			)
		else()
			message(WARNING "macdeployqt not found; the `macdeploy` target is unavailable "
				"and Quackle.app will not run without Qt installed")
		endif()
	endif()
endif()

if(WIN32)
	if("${QT_VERSION}" MATCHES "^6")
		get_target_property(_quackle_qmake_exe Qt6::qmake IMPORTED_LOCATION)
	else()
		get_target_property(_quackle_qmake_exe Qt5::qmake IMPORTED_LOCATION)
	endif()
	get_filename_component(_quackle_qt_bin_dir "${_quackle_qmake_exe}" DIRECTORY)
	find_program(QUACKLE_WINDEPLOYQT windeployqt HINTS "${_quackle_qt_bin_dir}")
	if(QUACKLE_WINDEPLOYQT)
		add_custom_command(TARGET Quackle POST_BUILD
			COMMAND "${QUACKLE_WINDEPLOYQT}"
				$<$<CONFIG:Debug>:--debug>
				$<$<NOT:$<CONFIG:Debug>>:--release>
				--no-compiler-runtime
				--no-translations
				--no-opengl-sw
				--no-system-d3d-compiler
				--no-system-dxc-compiler
				--no-network
				--no-svg
				--no-svgwidgets
				--skip-plugin-types generic,iconengines,networkinformation,tls
				--exclude-plugins qico,qjpeg,qsvg,qsvgicon
				"$<TARGET_FILE:Quackle>"
			COMMENT "Deploying Qt DLLs and plugins alongside Quackle.exe"
		)
	else()
		message(WARNING "windeployqt not found; Quackle.exe will not run outside the Qt build environment without manually copying Qt DLLs")
	endif()
endif()
