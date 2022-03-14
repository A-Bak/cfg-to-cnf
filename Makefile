#Compiler and Linker
CC          = ghc

#Directories - Source, Objects, Binary
SRCDIR      := Converter
BUILDDIR    := build
TARGETDIR   := bin

#Flags and includes
CFLAGS      := -XScopedTypeVariables
PROFILE     := -prof-fprof-auto -XScopedTypeVariables

MAIN 		:= Main
DATA 		:= Data
CONVERT 	:= ConvertCNF
PARSE 		:= Parse
RULES 		:= SimpleRules

TARGET 		:= cfg-to-cnf.exe

#Resources
CFGDIR		:= example_grammars

CFG1		:= cfg_1.txt
CFG2		:= cfg_2.txt
CFG3		:= cfg_3.txt
CFG1		:= cfg_4.txt
CFGEMPTY	:= cfg_empty.txt


##########################################################################

all: clean subdirs build run

build: subdirs $(BUILDDIR)/$(MAIN).o

run: build
	./$(TARGETDIR)/$(TARGET) -i $(CFGDIR)/$(CFG1)

##########################################################################
	
#Main.hs
$(BUILDDIR)/$(MAIN).o: $(SRCDIR)/$(MAIN).hs $(BUILDDIR)/$(DATA).o $(BUILDDIR)/$(CONVERT).o $(BUILDDIR)/$(PARSE).o $(BUILDDIR)/$(RULES).o
		$(CC) $(CFLAGS) $< -o $(TARGETDIR)/$(TARGET) -odir $(BUILDDIR)

#Data.hs
$(BUILDDIR)/$(DATA).o: $(SRCDIR)/$(DATA).hs
		$(CC) $(CFLAGS) -c $< -odir $(BUILDDIR)

#ConvertCNF.hs
$(BUILDDIR)/$(CONVERT).o: $(SRCDIR)/$(CONVERT).hs $(BUILDDIR)/$(DATA).o
		$(CC) $(CFLAGS) -c $< -odir $(BUILDDIR)

#Parse.hs
$(BUILDDIR)/$(PARSE).o: $(SRCDIR)/$(PARSE).hs $(BUILDDIR)/$(DATA).o
		$(CC) $(CFLAGS) -c $< -odir $(BUILDDIR)

#SimpleRules.hs	
$(BUILDDIR)/$(RULES).o: $(SRCDIR)/$(RULES).hs $(BUILDDIR)/$(DATA).o
		$(CC) $(CFLAGS) -c $< -odir $(BUILDDIR)

##########################################################################

.PHONY: clean

subdirs:
	-@mkdir $(TARGETDIR) $(BUILDDIR) || : 

clean:
	-@del /q /s $(TARGETDIR)\*
	-@del /q /s $(BUILDDIR)\*
	-@del /q /s $(SRCDIR)\*.hi
	-@del /q /s $(SRCDIR)\*.o


	