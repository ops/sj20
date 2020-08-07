#
# Makefile for VIC-20 SJ20 library
#

SJ20_NTSC ?= 0
ifeq ($(SJ20_NTSC),1)
  ASFLAGS += -DSJ20_NTSC
  MODEL := ntsc
endif
MODEL ?= pal

SJ20_IO ?= 1
ifeq ($(SJ20_IO),1)
  ASFLAGS += -DSJ20_IO
endif

SJ20_EXT_MESSAGES ?=1
ifeq ($(SJ20_EXT_MESSAGES),1)
  ASFLAGS += -DSJ20_EXT_MESSAGES
endif

SJ20_BASIC_EXTENSIONS ?=1
ifeq ($(SJ20_BASIC_EXTENSIONS),1)
  ASFLAGS += -DSJ20_BASIC_EXTENSIONS
endif

LIBRARY_BASE = sj20
LIBRARY_SUFFIX = lib
LIBRARY := $(LIBRARY_BASE)-$(MODEL).$(LIBRARY_SUFFIX)

AR := ar65
AS := ca65

SJ20_SAVE ?= 1
ifeq ($(SJ20_SAVE),1)
  ASFLAGS += -DSJ20_SAVE
endif


# Additional assembler flags and options.
ASFLAGS += -t vic20 -g

# Set OBJECTS
LIB_OBJECTS := sj20.o

$(LIBRARY): $(LIB_OBJECTS)
	$(AR) $(ARFLAGS) $@ $(LIB_OBJECTS)

clean:
	$(RM) $(LIB_OBJECTS)
	$(RM) $(LIBRARY)
	$(RM) *~
