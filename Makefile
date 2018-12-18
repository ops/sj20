#
# Makefile for VIC-20 SJ20
#

PROGRAM_BASE = sj20
SUFFIX = prg
PROGRAM := $(PROGRAM_BASE).$(SUFFIX)
CONFIG = sj20.cfg

AS := ca65
LD := ld65

START_ADDR ?= 45056

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

# Additional assembler flags and options.
ASFLAGS += -t vic20

# Additional linker flags and options.
LDFLAGS = -C $(CONFIG)

# Set OBJECTS
OBJECTS := sj20.o

$(PROGRAM): $(CONFIG) $(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ -S $(START_ADDR) $(OBJECTS)

clean:
	$(RM) $(OBJECTS) $(PROGRAM) *~
