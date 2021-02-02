##
## EPITECH PROJECT, 2020
## tmp_evalexpr
## File description:
## Makefile
##


CC	=	ghc

NAME	=	hal

all:	$(NAME)

$(NAME):
	stack setup
	stack build
	stack install --local-bin-path ./
	stack clean --full

clean:
	stack clean --full

fclean:	clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re