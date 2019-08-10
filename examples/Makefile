EXAMPLES = $(shell find . -maxdepth 1 -type d | sed "s/^\.\///" | grep -v "\.")

all test clean doc:
	for d in $(EXAMPLES); do $(MAKE) -C $$d $@ || exit 1; done

.PHONY: all
