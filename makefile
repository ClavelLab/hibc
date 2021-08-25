MAKE=make
NAME=hiBC
DATE=$(shell /bin/date "+%Y-%m-%d")

# https://github.com/inukshuk/sqleton
vis:
	sqleton --layout neato -e -o $(NAME).png $(NAME).db
	sqleton --layout neato -e -o $(NAME).svg $(NAME).db
	open $(NAME).png
