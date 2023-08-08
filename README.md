# 98VIDEOP.COM
**Video for MS-DOS on PC-98**

# About the program
**98VIDEOP** is a basic video player that works in real mode in MS-DOS on PC-98. The file format supports temporal compression, up to 16 colours fixed palette and full 16-bit PCM compressed to 8-bit ADPCM sound of up to 44100 Hz sample rate. It is very specifically tuned to the hardware of a typical early-90s PC-98 (for instance, it always assumes 640x400 resolution) and thus this program *won't run on IBM-compatible PCs.* It is merely meant as a demostration not as a practical program for everyday use, obviously. If you somehow thought it *was* supposed to have practical value, you should check out other programs.

I have so far only tested this program in the Neko Project II emulator, so I would appreciate a test on real hardware.

Example videos can be found [here](https://drive.google.com/drive/folders/1yOV2QJiZee277CnVz2NsthKSnRO7FKu2)

# Minimum requirements
In brief: the PC-9801VX

CPU: Intel 80286, 12 MHz 80386 recommended for a 'reasonable' video, but lower quality videos don't require quite as much speed

RAM: 256 KB

GPU: μPD7220 with 16-colour capability

Sound: Buzzer with PIT, PC-9801-86 recommended for higher quality sound

Storage: Program itself is only a kilobyte or so, may need lots of megabytes for the actual videos though

# Building
I build the source using the GNU assembler. If you're at all interested in building this from source, well, there's only one source file :). The makefile and linker script are all you need, really, so just invoke `make` in the directory and you should be alright.