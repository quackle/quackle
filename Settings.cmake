include_guard()

message("-- Including settings")
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Commented out so that automated builds can work
# but we'll need to set this and set up matching Qt libs
# for building a final app bundle
#
# Note that Qt6 uses std::filesystem which requires minimum of 10.15
# Qt5 can be set lower (was working fine with 10.10)
#
# set(CMAKE_OSX_DEPLOYMENT_TARGET 10.15)
