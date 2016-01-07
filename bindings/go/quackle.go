package quackle

// #cgo CXXFLAGS: -I../..
// #cgo pkg-config: QtCore
// #cgo LDFLAGS: -L. -lquackle -lquackleio -lQtCore
import "C"
