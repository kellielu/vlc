OBJS =  ast.cmo sast.cmo exceptions.ml semant.cmo utils.cmo codegen_ptx.cmo codegen_c.cmo parser.cmo scanner.cmo processor.cmo vlc.cmo 

.PHONY:all
all: clean vlc

# Make for installing
.PHONY:install
install: vlc /usr/local/bin/vlc_std/vlc
	cp vlc /usr/local/bin

# Make for uninstalling
.PHONY: uninstall
uninstall: clean
	rm -rf /usr/local/bin/vlc_std
	rm -rf /usr/local/bin/vlc

/usr/local/bin/vlc_std:
	mkdir /usr/local/bin/vlc_std

/usr/local/bin/vlc_std/vlc: /usr/local/bin/vlc_std
	cp vlc /usr/local/bin/vlc_std/

# Make for testing and cleaning
.PHONY:test
test:
	../test.sh

.PHONY:clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi *.output vlc


# Make for compiler 
vlc: $(OBJS)
	ocamlc -o vlc $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.cmo: %.ml
	ocamlc -w A -c $<

%.cmi: %.mlix
	ocamlc -w A -c $<

ast.cmo:
ast.cmx:
sast.cmo: ast.cmx
sast.cmx: ast.cmx
exceptions.cmo:
exceptions.cmx:
semant.cmo: ast.cmo exceptions.cmo
semant.cmx: ast.cmx exceptions.cmx
utils.cmo: ast.cmo parser.cmi semant.cmo sast.cmo
utils.cmx: ast.cmx parser.cmx semant.cmx sast.cmx 
codegen_ptx.cmo: parser.cmi utils.cmo ast.cmo semant.cmo
codegen_ptx.cmx: parser.cmx utils.cmx ast.cmx semant.cmx
codegen_c.cmo: parser.cmi utils.cmo ast.cmo semant.cmo codegen_ptx.cmo
codegen_c.cmx: parser.cmx utils.cmx ast.cmx semant.cmx codegen_ptx.cmx
processor.cmo: scanner.cmo parser.cmi
processor.cmx: scanner.cmx parser.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
parser.cmi: ast.cmo
vlc.cmo: parser.cmi codegen_ptx.cmo codegen_c.cmo utils.cmo processor.cmo ast.cmo semant.cmo
vlc.cmx: parser.cmx codegen_ptx.cmx codegen_c.cmx utils.cmx processor.cmx ast.cmx semant.cmx

# Shortcut makes
# Make for hello_host
.PHONY:hha
hha: vlc
	./vlc -a ../sample-generated-programs/hello_host.vlc 
.PHONY:hht
hht: vlc
	./vlc -t ../sample-generated-programs/hello_host.vlc 

.PHONY:hhc
hhc: vlc
	./vlc -c ../sample-generated-programs/hello_host.vlc | tee ../output/generated.cu

# Make for tests
.PHONY: tfd
tfd: vlc
	./vlc -c ../tests/test-function_decl.vlc	

# Make for hello_kernel
.PHONY:hka
hka: vlc
	./vlc -a ../sample-generated-programs/hello_kernel.vlc 
.PHONY:hkt
hkt: vlc
	./vlc -t ../sample-generated-programs/hello_kernel.vlc 

.PHONY:hkc
hkc: vlc
	./vlc -c ../sample-generated-programs/hello_kernel.vlc | tee ../output/generated.cu

# Make for hello_ptx
.PHONY:hpa
hpa: vlc
	./vlc -a ../sample-generated-programs/hello_ptx_test.vlc 
.PHONY:hpt
hpt: vlc
	./vlc -t ../sample-generated-programs/hello_ptx_test.vlc 

.PHONY:hpc
hpc: vlc
	./vlc -c ../sample-generated-programs/hello_ptx_test.vlc | tee ../output/generated.cu

.PHONY: vecadd
vecadd:
	nvcc -lcuda -o ../sample-generated-programs/hello_ptx ../sample-generated-programs/hello_ptx_test.cu 


