WIC64_LIBPATH = ./wic64-library
include $(WIC64_LIBPATH)/Makefile.inc
INCLUDES = -I$(WIC64_LIBPATH)
WIC64_SOURCES = $(WIC64_LIBPATH)/wic64.asm $(WIC64_LIBPATH)/wic64.h
WIC64_FLAGS = $(ASMFLAGS) -Dwic64_include_load_and_run=1
MYFLAGS = -f cbm -l $(@:prg=lst) -DDEBUG_DOWNLOAD=0
X64SC_OPTS = -autostart-warp
XPLUS4_OPTS = -autostart-warp

.PHONY: all clean

OPTS = -v --color -f cbm -l $(@:prg=lst)

all: sktp2wic.prg sktp2wic4.prg

sktp2wic.prg: sktp2wic.asm $(WIC64_SOURCES)
	$(ASM) $(WIC64_FLAGS) $(MYFLAGS) $(INCLUDES) -DPLUS4=0 -o $@ $<

sktp2wic4.prg: sktp2wic.asm $(WIC64_SOURCES)
	$(ASM) $(WIC64_FLAGS) $(MYFLAGS) $(INCLUDES) -DPLUS4=1 -o $@ $<

clean:
	rm -f *.prg *.lst

test64: sktp2wic.prg
	x64sc -userportdevice 23 $(X64SC_OPTS) $^

test+4: sktp2wic4.prg
	xplus4 $(XPLUS4_OPTS) $<
