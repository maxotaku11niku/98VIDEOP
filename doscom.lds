/* Simple GNU ld linker script to build a DOS .COM executable */

MEMORY
{
	segment (wxa) : ORIGIN = 0, LENGTH = 64K
}

SECTIONS
{
    . = 0x100;
	.text ALIGN(0x10) : { *(.text) } >segment
	.data ALIGN(0x10) : { *(.data) *(.rdata) } >segment
	.bss (NOLOAD) :  ALIGN(0x10) { *(.bss) } >segment
	/DISCARD/ : { *(.rdata$zzz) *(.reloc) } >segment
}