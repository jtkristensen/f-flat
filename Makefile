.PHONY: all clean

src  = ./src/
bin  = ./bin/
lib  = ./lib/
lex  = mono $(lib)fslex.exe
yacc = mono $(lib)fsyacc.exe
pp   = $(lib)FSharp.PowerPack.dll
core = $(lib)FSharp.Core.dll
fsc  = fsharpc --nologo

absyn    = $(bin)AbSyn.dll
lexer    = $(bin)Lexer.dll
parser   = $(bin)Parser.dll
verifier = $(bin)Transformations.dll
types    = $(bin)Types.dll
symtab   = $(bin)SymTab.dll
midi     = $(bin)MIDI.dll
compose  = $(bin)Compose.dll
compiler = $(bin)Fflat.exe

dependencies = -r $(absyn) -r $(lexer) -r $(parser) -r $(verifier) -r $(types) -r $(symtab) -r $(midi) -r $(compose) -r $(pp)

all: AbSyn.dll Lexer.dll Parser.dll Transformations.dll SymTab.dll Types.dll MIDI.dll Compose.dll

AbSyn.dll:
	$(fsc) -a $(src)AbSyn.fs -o $(bin)AbSyn.dll

Lexer.dll: Parser.dll
	$(lex) $(src)Lexer.fsl -o $(src)/Lexer.fs
	$(fsc) -a $(src)Lexer.fs -r $(parser) -r $(pp) -o $(lexer)

Parser.dll: AbSyn.dll
	$(yacc) -v --module Parser $(src)Parser.fsp -o $(src)Parser.fs
	$(fsc) -a $(src)Parser.fs -r $(absyn) -r $(pp) -o $(parser)

Transformations.dll: SymTab.dll Types.dll AbSyn.dll
	$(fsc) -a $(src)Transformations.fs -r $(symtab) -r $(absyn) -r $(types) -o $(verifier)

Types.dll: SymTab.dll
	$(fsc) -a $(src)Types.fsi $(src)Types.fs -r $(symtab) -o $(types)

SymTab.dll:
	$(fsc) -a $(src)SymTab.fs -o $(symtab)

MIDI.dll: Types.dll AbSyn.dll Compose.dll
	$(fsc) -a $(src)MIDI.fs -r $(absyn) -r $(types) -r $(symtab) -r $(compose) -r $(verifier) -o $(midi)

Compose.dll: Types.dll Transformations.dll
	$(fsc) -a $(src)Compose.fs -r $(symtab) -r $(absyn) -r $(verifier) -r $(types) -o $(compose)

Fflat.exe : all
	cp ./lib/run_script/fflat ./bin/
	$(fsc) $(src)Fflat.fsx -o $(compiler)

clean:
	rm -f ./tests/*.mid ./tests/*.py
	rm -f $(src)Lexer.fs $(src)Parser.fs
	rm -f $(src)Parser.fsyacc.output $(src)Parser.fsi
	rm -f $(bin)*.dll $(bin)*.exe $(bin)fflat

test:
	$(MAKE) clean
	$(MAKE) Fflat.exe
	reset
	cd ./bin && ./tests.sh
