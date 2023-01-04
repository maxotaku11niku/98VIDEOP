# 98VIDEOP.COM
**Video for MS-DOS on PC-98**

# About the program
**98VIDEOP** is a basic video player that works in real mode in MS-DOS on PC-98. The file format supports temporal compression, up to 16 colours fixed palette and full 16-bit PCM compressed to 8-bit ADPCM sound of up to 44100 Hz sample rate. It is very specifically tuned to the hardware of a typical early-90s PC-98 (for instance, it always assumes 640x400 resolution) and thus this program *won't run on IBM-compatible PCs.* It is merely meant as a demostration not as a practical program for everyday use, obviously. If you somehow thought it *was* supposed to have practical value, you should check out other programs.

I have so far only tested this program in the Neko Project II emulator, so I would appreciate a test on real hardware. I have a demonstration video ready, but it is not present in a release because of potential intellectual property issues. I should have an example video of some kind ready soon.

# Building
I build the source using the GNU assembler and a cursed and totally unportable batch file. If you're at all interested in building this from source, well, there's only one source file :). Remember that this is intended to be a DOS .COM executable, so you should have a `.org 0x100` directive or similar to specify the origin point. Double check the actual file you get doesn't have 256 empty bytes at the beginning; it seems that modern assemblers can't handle such old stuff.