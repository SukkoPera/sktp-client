WIC64LIB = ./wic64-library
include $(WIC64LIB)/Makefile.inc
INCLUDES = -I$(WIC64LIB)
WIC64_SOURCES = $(WIC64LIB)/wic64.asm $(WIC64LIB)/wic64.h
MYFLAGS = -f cbm -l $(@:prg=lst) -Dwic64_include_load_and_run=1

.PHONY: all clean

OPTS = -v --color -f cbm -l $(@:prg=lst)

all: sktp2wic.prg sktp2wic4.prg

sktp2wic.prg: sktp2wic.asm $(WIC64_SOURCES)
	$(ASM) $(ASMFLAGS) $(MYFLAGS) $(INCLUDES) -DPLUS4=0 -o $@ sktp2wic.asm

sktp2wic4.prg: sktp2wic.asm $(WIC64_SOURCES)
	$(ASM) $(ASMFLAGS) $(MYFLAGS) $(INCLUDES) -DPLUS4=1 -o $@ sktp2wic.asm

clean:
	rm -f *.prg *.lst
