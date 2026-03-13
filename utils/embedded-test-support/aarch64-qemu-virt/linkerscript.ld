MEMORY
{
   ram : ORIGIN = 0x40000000, LENGTH = 128K
}

SECTIONS
{
   .text     : { *(.start*) ; *(.text*) } > ram
   .rodata   : { *(.rodata*) } > ram
   .data     : { *(.data*) } > ram
   .bss      : { *(.bss*) } > ram
   /DISCARD/ : { *(.swift_modhash*) *(.comment*) *(.note*) }
}
