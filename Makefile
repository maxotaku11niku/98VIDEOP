# THIS MAKEFILE DOESN'T WORK YET!!

# Directories
TARGET	:= bin
BUILD	:= obj
BINO	:= objb
PROCESS	:= objp
SOURCES	:= src
# Find source files
export ASMFILES		:= $(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.asm)))
export OFILES	:= $(ASMFILES:.asm=.o)
# Targets
all: $(PROCESS) $(BUILD) $(BINO) midprocess

$(BUILD):
	[ -d $(BUILD) ] || mkdir -p $(BUILD)
	make -f $(CURDIR)/Makefile

$(BINO):
	[ -d $(BINO) ] || mkdir -p $(BINO)
	make -f $(CURDIR)/Makefile

$(PROCESS):
	[ -d $(PROCESS) ] || mkdir -p $(PROCESS)
	make -f $(CURDIR)/Makefile

$(TARGET):
	[ -d $(TARGET) ] || mkdir -p $(TARGET)
	make -f $(CURDIR)/Makefile

midprocess	: $(BUILD)/$(OFILES)
	ld -b elf32-i386 -o $(PROCESS)/98videop.o $^
	
$(BINO)/$(OFILES)	: $(BUILD)/$(OFILES)
	objcopy -O elf32-i386 $< $@

$(BUILD)/$(OFILES)	: $(SOURCES)/$(ASMFILES)
	as $< -o $@ -march=i386 -mtune=i386 --32