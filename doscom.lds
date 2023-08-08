/* Simple GNU ld linker script to build a DOS .COM executable */

SECTIONS
{
    . = 0x100;
	.text : { *(.text) }
    .data : { *(.data) }
    .bss : { *(.bss) }
}