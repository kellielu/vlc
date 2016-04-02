OBJS =  ast.cmo environment.cmo codegen.cmo parser.cmo scanner.cmo vlc.cmo
# processor.cmo
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
environment.cmo: ast.cmo
environment.cmx: ast.cmx
vlc.cmo: parser.cmi codegen.cmo environment.cmo ast.cmo
vlc.cmx: parser.cmx codegen.cmx environment.cmx ast.cmx
codgen.cmo: parser.cmi environment.cmo ast.cmo
codegen.cmx: parser.cmx environment.cmx ast.cmx
# processor.cmo: scanner.cmo parser.cmi
# processor.cmx: scanner.cmx parser.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
parser.cmi: ast.cmo


.PHONY:test
test: generator
	cd ../test && ./runtests.sh

.PHONY:clean
clean:
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi *.output vlc