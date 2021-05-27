##
## Makefile for B-FUN-300-STG-3-1-funPoolDay2-auguste.thomann in /home/auguste/delivery/tek2/Func_pool/B-FUN-300-STG-3-1-funPoolDay2-auguste.thomann
##
## Made by 
## Login   <>
##
## Started on  Wed Jan 27 10:58:51 2021 
## Last update Thu Jan 27 11:15:07 2021 
##


##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

NAME    :=	doop

CC	:=	ghc
C	:=	.hs

_FILES  :=	DoOp

_FILES_	:=	$(addsuffix $(C), $(_FILES))

CFLAGS	+=	-dynamic

COLOR   :=      \033[01;38;5;31m

WHITE   :=      \033[0;0m

CLEAR   :=      \033[2K

all:    $(NAME)

$(NAME):	$(_FILES_)
	@$(CC) $(CFLAGS) $(_FILES_)
	@echo -e "$(CLEAR)$(NAME) : $(COLOR)OK$(WHITE)"
	@echo -e "\033[01;38;5;154m========================== Compiling : Done\033[0;0m"

re:     all

.PHONY: $(NAME) re 