cmake_minimum_required (VERSION 3.16)

project (gaddagize)

message("-- Configuring gaddagize")

# gaddagize.pro: standalone console utility, no libquackle/Qt dependency.
add_executable(gaddagize
	gaddagize.cpp
)
