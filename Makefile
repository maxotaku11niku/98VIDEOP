# Directories
TARGET	:= bin
BUILD	:= obj
PROCESS	:= objp
SOURCES	:= src
# Find source files
export ASMFILES		:= $(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.asm)))
export OFILES	:= $(ASMFILES:.asm=.o)
# Targets
all: $(PROCESS) $(BUILD) $(TARGET)/98VIDEOP.COM

$(BUILD):
	[ -d $(BUILD) ] || mkdir -p $(BUILD)
	make -f $(CURDIR)/Makefile

$(PROCESS):
	[ -d $(PROCESS) ] || mkdir -p $(PROCESS)
	make -f $(CURDIR)/Makefile

$(TARGET):
	[ -d $(TARGET) ] || mkdir -p $(TARGET)
	make -f $(CURDIR)/Makefile

	
$(TARGET)/98VIDEOP.COM	: $(PROCESS)/98videop.o
	objcopy -O binary $< $@

$(PROCESS)/98videop.o	: $(BUILD)/$(OFILES)
	ld -T doscom.lds -m i386pe -o $(PROCESS)/98videop.o $^

$(BUILD)/$(OFILES)	: $(SOURCES)/$(ASMFILES)
	as $< -o $@ -march=i386 -mtune=i286 --32