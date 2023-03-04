##
## EPITECH PROJECT, 2022
## B-FUN-400-PAR-4-1-wolfram-sacha.lliso
## File description:
## Makefile
##

STACK_PATH  =   stack path --local-install-root

B_PATH 	:=	$(shell $(STACK_PATH))

NAME 			= 	glados

all:
	stack build
	cp $(B_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

test: all
	./test.bash

.PHONY: all clean fclean re

