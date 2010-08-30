LIBS = $(shell find . -maxdepth 1 -type d | sed "s/^\.\///" | grep -v "\.")

all clean doc:
	for d in $(LIBS); do $(MAKE) -C $$d $@; done

.PHONY: all
