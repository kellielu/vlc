.PHONY: install
install:
	cd compiler && $(MAKE) all && $(MAKE) install

.PHONY: uninstall
uninstall:
	cd compiler && $(MAKE) uninstall

.PHONY:test
test:
	./test.sh

clean:
	cd compiler && $(MAKE) clean
	rm -f tests/*.cu tests/*.log
	
