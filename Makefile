##
## EPITECH PROJECT, 2021
## B-FUN-400-LIL-4-1-wolfram-dorian.berten
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/wolfram-exe ./wolfram

clean:
	stack clean

fclean: clean
	rm -f wolfram

re: fclean all