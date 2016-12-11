#
# Makefile
# @author Evgeny Ukhanov <mrlsd@ya.ru>
#

.PHONY: run, build
default: run

setup:
	@stack setup

run:
	@stack build
	@stack exec main-exe

build:
	@stack build

