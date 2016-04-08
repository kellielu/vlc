OBJS =  ast.cmo semant.cmo environment.cmo utils.cmo codegen.cmo parser.cmo scanner.cmo processor.cmo vlc.cmo 

# codegen.cmo
.PHONY:all
all: clean vlc

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
environment.cmo: ast.cmo semant.cmo
environment.cmx: ast.cmx semant.cmx
vlc.cmo: parser.cmi codegen.cmo utils.cmo processor.cmo environment.cmo ast.cmo semant.cmo
vlc.cmx: parser.cmx codegen.cmx utils.cmx processor.cmx environment.cmx ast.cmx semant.cmx
semant.cmo: ast.cmo
semant.cmx: ast.cmx
codegen.cmo: parser.cmi environment.cmo utils.cmo ast.cmo semant.cmo
codegen.cmx: parser.cmx environment.cmx utils.cmx ast.cmx semant.cmx
processor.cmo: scanner.cmo parser.cmi
processor.cmx: scanner.cmx parser.cmx
utils.cmo: ast.cmo parser.cmi semant.cmo
utils.cmx: ast.cmx parser.cmx semant.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
parser.cmi: ast.cmo


.PHONY:test
test:
	dos2unix ../test.sh
	../test.sh

.PHONY:clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi *.output vlc
	rm -f ../output/*


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
