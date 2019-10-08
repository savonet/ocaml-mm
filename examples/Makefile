EXAMPLES = $(shell find . -maxdepth 1 -type d | sed "s/^\.\///" | grep -v "\.")

all test clean doc:
	for d in $(EXAMPLES); do $(MAKE) -C $$d $@ || exit 1; done

# We are only testing here things not requiring soundcard or graphical output
test-base:
	for d in test; do $(MAKE) -C $$d test || exit 1; done

.PHONY: all test
