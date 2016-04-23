.PHONY: install
install:
	cd compiler && $(MAKE) install

.PHONY: uninstall
uninstall:
	cd compiler && $(MAKE) uninstall

.PHONY:test
test:
	dos2unix test.sh
	./test.sh

clean:
	cd compiler && $(MAKE) clean
	rm tests/test.log
	
