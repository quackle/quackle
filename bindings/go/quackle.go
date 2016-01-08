package quackle

// #cgo CXXFLAGS: -I../..
// #cgo pkg-config: QtCore
// #cgo LDFLAGS: -L${SRCDIR}/../../lib/release -L${SRCDIR}/../../quackleio/lib/release -lquackle -lquackleio -lQtCore
import "C"
