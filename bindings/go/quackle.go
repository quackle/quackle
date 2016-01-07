package quackle

// #cgo CXXFLAGS: -I../..
// #cgo pkg-config: QtCore
// #cgo LDFLAGS: -L${SRCDIR} -lquackle -lquackleio -lQtCore
import "C"
