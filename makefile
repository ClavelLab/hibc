MAKE=make
NAME=hiBC
DATE=$(shell /bin/date "+%Y-%m-%d")
FOLDER = visualization

# https://github.com/inukshuk/sqleton
vis:
	sqleton --layout neato -e -o $(FOLDER)/$(DATE)-$(NAME).png $(NAME).db
	open $(FOLDER)/$(DATE)-Database-$(NAME).png
