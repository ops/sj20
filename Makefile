#
# Makefile for VIC-20 SJ20
#

PROGRAM_BASE = sj20
PROGRAM_SUFFIX = prg
PROGRAM := $(PROGRAM_BASE).$(PROGRAM_SUFFIX)

LIBRARY_BASE = sj20
LIBRARY_SUFFIX = lib
LIBRARY := $(LIBRARY_BASE).$(LIBRARY_SUFFIX)

CONFIG = sj20.cfg

IMAGE := sj20.d64

AR := ar65
AS := ca65
LD := ld65

#START_ADDR ?= 45056
START_ADDR ?= 8192

SJ20_SAVE ?= 1
ifeq ($(SJ20_SAVE),1)
  ASFLAGS += -DSJ20_SAVE
endif

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

# Additional assembler flags and options.
ASFLAGS += -t vic20 -g

# Additional linker flags and options.
LDFLAGS = -C $(CONFIG)

# Set OBJECTS
OBJECTS := main.o
LIB_OBJECTS := sj20.o

all: $(LIBRARY) $(PROGRAM)
.PHONY: all image clean

$(PROGRAM): $(CONFIG) $(OBJECTS) $(LIBRARY)
	$(LD) $(LDFLAGS) -o $@ -S $(START_ADDR) $(OBJECTS) $(LIBRARY)

$(LIBRARY): $(LIB_OBJECTS)
	$(AR) $(ARFLAGS) $@ $(LIB_OBJECTS)

image: $(PROGRAM)
	c1541 -format sj20,os d64 $(IMAGE)
	c1541 $(IMAGE) -write $(PROGRAM)
	c1541 $(IMAGE) -write 8k

clean:
	$(RM) $(OBJECTS) $(LIB_OBJECTS)
	$(RM) $(PROGRAM) $(LIBRARY)
	$(RM) $(IMAGE)
	$(RM) *~
