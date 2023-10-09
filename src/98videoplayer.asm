;#98VIDEOP.COM
;#Video player for PC-98
;#Maxim Hoxha 2022-2023

;#Permission is hereby granted, free of charge, to any person
;#obtaining a copy of this software and associated documentation
;#files (the "Software"), to deal in the Software without
;#restriction, including without limitation the rights to use,
;#copy, modify, merge, publish, distribute, sublicense, and/or sell
;#copies of the Software, and to permit persons to whom the
;#Software is furnished to do so, subject to the following
;#conditions:

;#The above copyright notice and this permission notice shall be
;#included in all copies or substantial portions of the Software.

;#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;#EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;#OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;#NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;#HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;#WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;#FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;#OTHER DEALINGS IN THE SOFTWARE.

.arch i286
.intel_syntax noprefix
.code16 ;#This is intended to run in real mode

.text
PROCEDURE_main: ;#int main(char[] argv)
	mov ax, cs
	mov baseseg, ax ;#Get loaded segment
	add ax, 0x1FFF
	shr ax, 12
	shl ax, 12 ;#Ensure 64 KB alignment since disk transfers use DMA
	mov filebufferseg, ax ;#Set file buffer segment
	mov ax, 0x3D00
	;#DOS-specific: null-terminate command stub in PSP
	xor bx, bx
	mov bl, [0x80]
	movb [bx+0x81], 0x00
	mov dx, 0x82	;#The PSP command stub starts from 81h, but it includes the space trailing the executable name, so we have to skip over that
	int 0x21		;#DOS API: Open file (filename given from command for reading)
	jc main_file_error
	mov filehandle, ax
	mov ah, 0x0D
	int 0x18		;#PC98 CRT BIOS: Turn off text display
	mov ah, 0x40
	int 0x18		;#PC98 CRT BIOS: Turn on graphics display
	mov ah, 0x42
	mov ch, 0xC0
	int 0x18		;#PC98 CRT BIOS: Set display options (640x400, display page 0, colour mode)
	mov al, 0x01
	out 0x6A, al	;#PC98 GDC I/O: Write mode register 2 (set to 16-colour mode)
	call PROCEDURE_clean_screen ;#Preemptive screen clear just in case
	call PROCEDURE_install_vsync_vector
	mov bx, filehandle
	call PROCEDURE_video_read ;#We're not saving registers (apart from ip) to the stack because we don't give a fuck about what these functions do to them. Not very robust, but it isn't hard to add a few push instructions anyway
	mov bx, filehandle
	mov ah, 0x3E
	int 0x21		;#DOS API: Close file (the video file we opened earlier)
	cmp cx, 0xDEAD
	je main_wrongformat
	call PROCEDURE_clean_screen
	mov al, 0x00	;#Just clear palette colour 0 to make the screen readable after quitting
	out 0xA8, al	;#PC98 GDC I/O: Set palette index
	out 0xAA, al	;#PC98 GDC I/O: Set palette colour green level
	out 0xAC, al	;#PC98 GDC I/O: Set palette colour red level
	out 0xAE, al	;#PC98 GDC I/O: Set palette colour blue level
	mov al, 0x00
	call PROCEDURE_restore_vsync_vector
	jmp main_endprog
main_file_error:
	mov ah, 0x09
	lea dx, file_openerror_message
	int 0x21		;#DOS API: Display string (file_openerror_message)
	mov al, 0x01
	jmp main_endprog
main_wrongformat:
	call PROCEDURE_restore_vsync_vector
	mov al, 0x02
main_endprog:
	mov ah, 0x0C
	int 0x18		;#PC98 CRT BIOS: Turn on text display
	mov ah, 0x41
	int 0x18		;#PC98 CRT BIOS: Turn off graphics display
	mov ah, 0x4C
	int 0x21		;#DOS API: Terminate with return code (0x00 -> success, 0x01 -> could not open file, 0x02 -> file in wrong format)
;#END OF PROCEDURE_main


;#Clears the screen very fast
PROCEDURE_clean_screen: ;#void clean_screen(void)
	mov dx, 4
	lea bx, planeseg
clean_screen_loop:
	mov ax, [bx]
	mov es, ax	;#segments? oh crap
	mov di, 0
	mov ax, 0
	mov cx, 16000
	cld
	rep stosw
	add bx, 2
	dec dx
	jnz clean_screen_loop
	ret
;#END OF PROCEDURE_clean_screen

;#Installs our interrupt vector for VSYNC
PROCEDURE_install_vsync_vector: ;#void install_vsync_vector(void)
	cli
	mov ax, 0x350A
	int 0x21		;#DOS API: Get interrupt vector (0x0A [hardware: VSYNC]) -> es:bx
	mov old_vsync_vector_segment, es
	mov old_vsync_vector_offset, bx
	mov dx, offset PROCEDURE_vsync_interrupt
	mov ax, 0x250A
	int 0x21		;#DOS API: Set interrupt vector (0x0A [hardware: VSYNC] to our vsync function)
	out 0x64, al	;#PC-98 GDC I/O: CRT interrupt reset
	in al, 0x02		;#PC-98 interrupt controller I/O: read mask
	mov old_interrupt_mask, al
	and al, 0xFB	;#Add VSYNC interrupts
	out 0x02, al	;#PC-98 interrupt controller I/O: set mask
	sti
	ret

;#Restores the old interrupt vector for VSYNC
PROCEDURE_restore_vsync_vector: ;#void restore_vsync_vector(void)
	cli
	mov dx, old_vsync_vector_offset
	mov ds, old_vsync_vector_segment
	mov ax, 0x250A
	int 0x21		;#DOS API: Set interrupt vector (0x0A [hardware: VSYNC])
	push cs
	pop ds
	mov al, old_interrupt_mask ;#Restore previous interrupt mask
	out 0x02, al	;#PC-98 interrupt controller I/O: set mask
	sti
	ret

;#Installs our interrupt vector for the timer
PROCEDURE_install_timer_vector: ;#void install_timer_vector(void)
	cli
	mov ax, 0x3508
	int 0x21		;#DOS API: Get interrupt vector (0x08 [hardware: Timer]) -> es:bx
	mov old_timer_vector_segment, es
	mov old_timer_vector_offset, bx
	mov dx, offset PROCEDURE_buzzer_interrupt_8bit
	mov ax, 0x2508
	int 0x21		;#DOS API: Set interrupt vector (0x08 [hardware: Timer] to our timer function)
	in al, 0x02		;#PC-98 interrupt controller I/O: read mask
	and al, 0xFE	;#Add Timer interrupts
	out 0x02, al	;#PC-98 interrupt controller I/O: set mask
	mov al, 0x06
	out 0x37, al	;#PC-98 system port I/O: turn on buzzer
	sti
	ret
	
;#Restores the old interrupt vector for the timer
PROCEDURE_restore_timer_vector: ;#void restore_timer_vector(void)
	cli
	mov dx, old_timer_vector_offset
	mov ds, old_timer_vector_segment
	mov ax, 0x2508
	int 0x21		;#DOS API: Set interrupt vector (0x08 [hardware: Timer])
	push cs
	pop ds
	in al, 0x02		;#PC-98 interrupt controller I/O: read mask
	or al, 0x01		;#Turn off Timer interrupts
	out 0x02, al	;#PC-98 interrupt controller I/O: set mask
	mov al, 0x07
	out 0x37, al	;#PC-98 system port I/O: turn off buzzer
	mov al, 0xFF
	out 0x71, al
	out 0x71, al	;#turn off timer
	sti
	ret

;#Reads in the video file
;#handle [bx]: File handle returned by DOS
PROCEDURE_video_read: ;#void video_read(uint16 handle [in bx])
	mov ax, 0xC000
	mov cx, ax
	mov filebuffercurwritepos, ax
	xor dx, dx
	lea di, magic_number
	mov ds, filebufferseg
	mov ah, 0x3F
	int 0x21		;#DOS API: Read from file (0xC000 bytes into filebuffer)
	xor si, si
	xor bx, bx
video_read_magicnum_loop: ;#Check that the file signature is ok
	mov al, [si]
	mov ah, cs:[di]
	test ah, ah
	jz video_read_end_magicnum
	xor ah, al
	jnz video_read_wrongformat
	inc si
	inc di
	jmp video_read_magicnum_loop
video_read_wrongformat:
	push cs
	pop ds
	mov ah, 0x09
	lea dx, file_formaterror_message
	int 0x21		;#DOS Main API, ah = 09h: Display string, dx = &file_formaterror_message: Pointer to string
	mov cx, 0xDEAD
	ret	

video_read_end_magicnum: ;#Set various options
	lodsb
	mov ah, 0
	inc ax
	mov cs:frameskip, ax
	mov cs:framesleft, ax
	lodsw
	mov cs:numframes_lo, ax
	lodsw
	mov cs:numframes_hi, ax
	lodsw
	mov bx, ax
	test bx, 0x0010
	jz video_read_fullres
	push ax
	mov ah, 0x42
	mov ch, 0x80
	int 0x18		;#PC98 CRT BIOS: Set display options (640x200, display page 0, colour mode)
	pop ax
video_read_fullres:
	and bx, 0x0020
	shr bx, 5
	mov cs:is_stereo, bl
	and ax, 0x0007
	mov cs:audiospec, ax
	xor ax, ax
	mov cl, 16
video_read_palette_form: ;#Read colour table into hardware palette
	mov ch, 16
	sub ch, cl
	mov al, ch
	out 0xA8, al	;#PC98 GDC I/O: Set palette index
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAC, al	;#PC98 GDC I/O: Set palette colour red level
	mov al, ah
	and al, 0x0F
	out 0xAA, al	;#PC98 GDC I/O: Set palette colour green level
	inc si
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAE, al	;#PC98 GDC I/O: Set palette colour blue level
	dec cl
	inc ch
	mov al, ch
	out 0xA8, al	;#PC98 GDC I/O: Set palette index
	mov al, ah
	and al, 0x0F
	out 0xAC, al	;#PC98 GDC I/O: Set palette colour red level
	inc si
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAA, al	;#PC98 GDC I/O: Set palette colour green level
	mov al, ah
	and al, 0x0F
	out 0xAE, al	;#PC98 GDC I/O: Set palette colour blue level
	inc si
	dec cl
	jnz video_read_palette_form
	
	push cs
	pop ds
	mov ax, si
	add ax, 0x0002
	mov filebuffercurpos, ax
	
	;#Build APDCM acceleration table
	lea di, accelerationtable_adpcm
	push cs
	pop es
	mov ax, di
	add ax, 0x0900
	mov samplebufferptr, ax ;#set the pointer to the sample buffer above the acceleration table
	xor al, al
	xor cx, cx
	mov cl, 0x68
	rep stosb
	inc al
	mov cl, 0x30
	rep stosb
	dec al
	mov cl, 0x68
	rep stosb ;#build table for shift = 0
	
	mov ah, 0x07
video_read_adpcmtable_formloop: ;#build table for shift 1 to 7
	mov cl, 0x19
	rep stosb
	inc al
	mov cl, 0x4F
	rep stosb
	inc al
	mov cl, 0x30
	rep stosb
	dec al
	mov cl, 0x4F
	rep stosb
	dec al
	mov cl, 0x19
	rep stosb
	inc al
	dec ah
	jnz video_read_adpcmtable_formloop
	
	mov cl, 0x19
	rep stosb
	inc al
	mov cl, 0xCE
	rep stosb
	dec al
	mov cl, 0x19
	rep stosb ;#build table for shift = 8
	
	
	
	;#Check the available sound hardware
	mov dx, 0xA460
	in al, dx
	and al, 0xE0
	cmp al, 0x40 ;#86 at I/O port 018x
	je video_read_has86
	cmp al, 0x50 ;#86 at I/O port 028x
	je video_read_has86
	cmp al, 0x60 ;#WSS-compatible
	je video_read_haswss
	cmp al, 0x70 ;#WSS-compatible
	je video_read_haswss
	cmp al, 0x80 ;#WSS-compatible
	je video_read_haswss
	jmp video_read_nopcm ;#Only has basic internal buzzer
	
video_read_has86:
	;#Prepare audio here for 86 soundboard (turns out there is no ADPCM support)
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	and al, 0x7F ;#stop FIFO
	out dx, al

	out 0x5F, al
	and al, 0xBF ;#set to CPU->FIFO mode
	out dx, al

	out 0x5F, al
	or al, 0x08 ;#reset the FIFO
	out dx, al

	out 0x5F, al
	and al, 0xF7 ;#clear it again to turn FIFO on
	out dx, al

	out 0x5F, al
	and al, 0xDF ;#turn off FIFO interrupts
	out dx, al

	out 0x5F, al
	and al, 0xEF ;#clear interrupt flag
	out dx, al
	
	mov bx, audiospec
	out 0x5F, al
	and al, 0xF8 ;#set sample rate
	or al, bl
	out dx, al

	mov dx, 0xA46A ;#set to the appropriate mode, output to both channels
	mov al, 0xB2
	or al, bh
	out dx, al
	
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	or al, 0x20 ;#turn on FIFO interrupts
	out dx, al
	
	mov dx, 0xA46A ;#set irq size to 1024 bytes
	mov al, 0x08
	out dx, al
	
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	or al, 0x80 ;#start playing PCM
	out dx, al
	
	mov dx, 0xA466
	mov al, 0xA0 ;#set volume to maximum
	out dx, al
	
	movw fifo_io_addr, 0xA46C
	movb using_sound, 0x01
	jmp video_read_startplay
	
video_read_haswss:
	;#Prepare audio here for a WSS-compatible soundboard
	mov ax, filebufferseg
	add ax, 0x1000
	mov samplebufferseg, ax
	shr ax, 12
	mov samplebuffer_dmabank, ax
	xor ax, ax
	mov samplebufferptr, ax
	xor di, di
	mov ax, samplebufferseg
	mov es, ax
	mov cx, 0x4000
	xor ax, ax
	rep stosw
	
	mov ax, 0x0400
	mov currentreadsample, ax
	
	mov dx, 0x0C24
	mov al, 0x80
	out dx, al ;#turn on sound
	
	mov dx, 0x0F40
	mov al, 0x0B
	out dx, al ;#set to use expansion INT 0 and DMA channel 3
	
	mov dx, 0x0F44
	mov al, 0x6C
	out dx, al
	inc dx
	mov al, 0xC0
	out dx, al ;#set to MODE2
	
	mov dx, 0x0F44
video_read_haswss_mcepoll1:
	in al, dx
	cmp al, 0x80
	je video_read_haswss_mcepoll1
	
	mov al, 0x70
	out dx, al
	inc dx
	mov al, 0x10
	out dx, al ;#allow changing sample format and make sure samples are held if missed
	
	mov dx, 0x0F44
video_read_haswss_mcepoll2:
	in al, dx
	cmp al, 0x80
	je video_read_haswss_mcepoll2

	mov al, 0x68
	out dx, al
	inc dx
	mov al, 0x40
	mov bx, audiospec
	mov cl, [sampleratespec_to_wssdiv + bx]
	mov ch, is_stereo
	shl ch, 4
	or al, cl
	or al, ch
	out dx, al ;#set sample format appropriately
	
	mov dx, 0x0F44
video_read_haswss_mcepoll3:
	in al, dx
	cmp al, 0x80
	je video_read_haswss_mcepoll3
	
	mov al, 0x26
	out dx, al
	inc dx
	mov al, 0x00
	out dx, al ;#set left DAC to 0 dB
	
	mov dx, 0x0F44
	mov al, 0x27
	out dx, al
	inc dx
	mov al, 0x00
	out dx, al ;#set right DAC to 0 dB
	
	mov al, 0x1B
	out 0x17, al ;#set channel 3 to use demand mode, with autoinitialisation read transfer
	out 0x19, al ;#reset pointer and counter
	mov al, samplebuffer_dmabank
	out 0x25, al ;#set DMA bank for channel 3
	xor ax, ax
	out 0x0D, al
	out 0x0D, al ;#set DMA offset for channel 3 (forced to 0)
	mov ax, 0x8000
	out 0x0F, al
	xchg al, ah
	out 0x0F, al ;#set DMA count for channel 3 to a long length
	mov al, 0x03
	out 0x15, al ;#unmask channel 3
	mov ax, cs
	mov ds, ax
	
	mov dx, 0x0F44
	mov al, 0x2A
	out dx, al
	inc dx
	mov al, 0x02
	out dx, al ;#enable interrupts
	
	mov dx, 0x0F44
	mov al, 0x69
	out dx, al
	inc dx
	mov al, 0x05
	out dx, al ;#set to single-channel DMA mode and turn playback on
	
	movw fifo_io_addr, 0x0F47
	movb using_sound, 0x02
	jmp video_read_startplay

video_read_nopcm:
	;#Prepare audio here if there is no proper PCM soundboard available
	;#Clear sample buffer
	mov di, samplebufferptr
	push cs
	pop es
	mov cx, 0x2000
	mov ax, current_sample_midpoint
	rep stosw
	
	mov al, 0x36
	out 0x77, al ;#set PIT mode for channel 0 (interrupt timer, set to rate generator mode)
	
	in al, 0x42 ;#check the bus frequency
	and ax, 0x0020 ;#isolate bus frequency bit

	;#0 -> 2.4576 MHz
	;#1 -> 1.9968 MHz
	mov bx, offset shiftdownvalues
	mov dx, audiospec
	shr ax, 2
	add bx, dx
	add bx, ax
	mov cl, [bx]
	mov current_buzzer_shiftdown, cl
	mov bx, 0x8000
	shr bx, cl
	mov current_sample_midpoint, bx
	mov bx, offset sampleratespec_to_buzzfreq
	shl dx, 1
	shl ax, 1
	add bx, dx
	add bx, ax
	mov cx, [bx]
	mov al, cl
	out 0x71, al ;#set timer counter low byte
	mov al, ch
	out 0x71, al ;#set timer counter high byte
	
	mov al, 0x70
	out 0x77, al ;#set PIT mode for channel 1 (buzzer, set to interrupt stop mode)
	mov dx, 0x3FDB
	xor al, al
	out dx, al
	out dx, al
	mov al, 0x50
	out 0x77, al ;#set PIT to only need 8 bits for each sample
	
	call PROCEDURE_install_timer_vector
	
	movb using_sound, 0x00
	
video_read_startplay:
	mov cx, numframes_lo
	mov dx, numframes_hi
	
video_read_frameloop:
	call PROCEDURE_frameloop
video_read_vsync_wait:
	in al, 0x41		;#PC-98 keyboard I/O: read keyboard data
	cmp al, 0x80
	je video_read_end_of_video ;#If Esc is pressed, stop playback
	hlt
	mov bx, framesleft
	cmp bx, 0
	jg video_read_vsync_wait
	mov bx, frameskip
	mov framesleft, bx
	dec cx
	jnz video_read_frameloop
	test dx, dx
	jz video_read_end_of_video
	dec dx
	jne video_read_frameloop
	
video_read_end_of_video:
	mov al, using_sound
	test al, al
	jnz video_read_end_pcm
	call PROCEDURE_restore_timer_vector
	jmp video_read_endfunc
video_read_end_pcm:
	cmp al, 0x01
	je video_read_endfunc
	;# Turn off WSS-compatible soundboard and DMA channel 3
	
	out 0x19, al ;#reset pointer and counter
	mov al, 0x07
	out 0x15, al ;#mask channel 3
	
	mov dx, 0x0F44
	mov al, 0x2A
	out dx, al
	inc dx
	mov al, 0x00
	out dx, al ;#disable interrupts
	
	mov dx, 0x0F44
	mov al, 0x69
	out dx, al
	inc dx
	mov al, 0x00
	out dx, al ;#turn playback off
	
	mov dx, 0x0C24
	mov al, 0x00
	out dx, al ;#turn off sound
	
video_read_endfunc:
	mov cx, 0xF00D
	ret

;#Interrupt routine triggered by VSYNC
PROCEDURE_vsync_interrupt: ;#void vsync_interrupt(void)
	push ax
	
	mov ax, cs:framesleft
	dec ax
	mov cs:framesleft, ax
	
	out 0x64, al	;#PC-98 GDC I/O: CRT interrupt reset
	mov al, 0x20
	out 0x00, al	;#PC-98 interrupt controller: signal end of interrupt
	pop ax
	iret

;#Interrupt routine to set the buzzer duty level for the current sample. Not used if the 86 soundboard is present.
PROCEDURE_buzzer_interrupt_8bit: ;#void buzzer_interrupt_16bit(void)
	push dx
	push si
	push ax
	
	mov dx, 0x3FDB
	mov si, cs:currentreadsample
	mov al, cs:[si]
	out dx, al
	addw cs:currentreadsample, 1
	
	mov al, 0x20
	out 0x00, al	;#PC-98 interrupt controller: signal end of interrupt
	
	pop ax
	pop si
	pop dx
	iret

;#Attempt to read a sector-aligned chunk from file, skipping if we don't need to read any more data
PROCEDURE_tryreadsection: ;#void tryreadsection(uint16 length [in ax])
	push dx
	push cx
	push bx
	push ds
	mov dx, cs:filebuffercurwritepos
	push dx
	mov cx, cs:filebuffercurpos
	sub dx, cx
	mov bx, dx
	sub ax, bx ;#bx has the length as yet unread
	jbe tryreadsection_end_noread ;#if requested length is less than or equal to the length as yet unread, don't bother
	add ax, 0x07FF
	and ax, 0xF800
	mov bx, cs:filehandle
	pop dx
	push dx
	add dx, ax
	jc tryreadsection_wraparound ;#if requested length would result in overflowing our buffer, account for this, since DOS might try to write beyond the buffer segment
	pop dx
tryreadsection_backfromwrap:
	mov cx, ax
	mov ds, cs:filebufferseg
	mov ah, 0x3F
	int 0x21		;#DOS API: Read from file
	add dx, cx
	jmp tryreadsection_end
tryreadsection_wraparound:
	pop dx
	mov cx, dx
	not cx
	inc cx
	sub ax, cx
	mov ds, cs:filebufferseg
	push ax
	mov ah, 0x3F
	int 0x21		;#DOS API: Read from file
	pop ax
	xor dx, dx
	test ax, ax
	jz tryreadsection_end
	jmp tryreadsection_backfromwrap
tryreadsection_end_noread:
	pop dx
tryreadsection_end:
	mov cs:filebuffercurwritepos, dx
	pop ds
	pop bx
	pop cx
	pop dx
	ret
	
;#Loop for each frame
PROCEDURE_frameloop: ;#void frameloop(void)
	push cx ;#push our loop counters, doesn't take too long
	push dx
	
	mov ds, filebufferseg
	lodsw ;#ax has audio length
	mov cx, ax
	push cx
	mov cs:filebuffercurpos, si
	mov bl, cs:is_stereo
	test bl, bl
	jz notstereo
	shl ax, 1
notstereo:
	shl ax, 1
	add ax, 4
	call PROCEDURE_tryreadsection
	
	mov al, cs:using_sound
	mov si, cs:filebuffercurpos
	test al, al
	jz frameloop_buzaudio
	cmp al, 0x01
	je frameloop_86audio
	jmp frameloop_wssaudio
	
	;#Decode ADPCM for buzzer PCM
frameloop_buzaudio:
	pop bp
	push bp
	mov di, cs:samplebufferptr
	push cs
	pop es
	mov cs:currentreadsample, di
	mov cx, cs:adpcmshiftval
frameloop_buzaudio_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov ch, cs:current_buzzer_shiftdown
	xchg ch, cl
	sar ax, cl
	xchg ch, cl
	xor ch, ch
	add ax, cs:current_sample_midpoint
	stosb ;#store sample in buffer
	mov cl, cs:[bx+accelerationtable_adpcm]
	pop ax
	xchg al, ah
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov ch, cs:current_buzzer_shiftdown
	xchg ch, cl
	sar ax, cl
	xchg ch, cl
	xor ch, ch
	add ax, cs:current_sample_midpoint
	stosb ;#store sample in buffer
	mov cl, cs:[bx+accelerationtable_adpcm]
	dec bp
	jnz frameloop_buzaudio_pushloop
	push cx
	mov cx, 0x0010
	rep stosb ;#Add padding to reduce clipping(?)
	pop cx
	pop ax
	mov bl, cs:is_stereo
	test bl, bl
	jz no_skip_stereodata
	shl ax, 1
	add si, ax ;#Needed to skip over unused stereo difference data (buzzer audio is forced mono)
no_skip_stereodata:
	jmp frameloop_videodata_process
	
frameloop_86audio:
	;#Decode ADPCM for the 86 soundboard
	test bl, bl
	jnz frameloop_86audio_stereo
	pop di
	mov dx, cs:fifo_io_addr
	mov cx, cs:adpcmshiftval
frameloop_86audio_mono_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	xchg al, ah ;#output sample to both channels
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	mov cl, cs:[bx+accelerationtable_adpcm]
	pop ax
	xchg al, ah
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	xchg al, ah ;#output sample to both channels
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	mov cl, cs:[bx+accelerationtable_adpcm]
	dec di
	jnz frameloop_86audio_mono_pushloop
	jmp frameloop_videodata_process
	
frameloop_86audio_stereo:
	pop bp
	mov dx, cs:fifo_io_addr
	mov cl, cs:adpcmshiftval
	mov ch, cs:adpcmshiftval_diff
	mov bx, bp
	shl bx, 1
frameloop_86audio_stereo_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov al, [bx+si-2]
	xor ah, ah
	xchg cl, ch
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample_diff
	mov cs:lastsample_diff, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov di, ax
	mov ax, cs:lastsample
	push bx
	mov bx, ax
	add ax, di
	xchg al, ah ;#output left sample
	out dx, al
	xchg al, ah
	out dx, al
	sub bx, di
	mov ax, bx
	xchg al, ah ;#output right sample
	out dx, al
	xchg al, ah
	out dx, al
	pop bx
	xchg cl, ch
	pop ax
	xchg al, ah
	xor ah, ah
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov al, [bx+si-1]
	xor ah, ah
	xchg cl, ch
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample_diff
	mov cs:lastsample_diff, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov di, ax
	mov ax, cs:lastsample
	push bx
	mov bx, ax
	add ax, di
	xchg al, ah ;#output left sample
	out dx, al
	xchg al, ah
	out dx, al
	sub bx, di
	mov ax, bx
	xchg al, ah ;#output right sample
	out dx, al
	xchg al, ah
	out dx, al
	pop bx
	xchg cl, ch
	dec bp
	jnz frameloop_86audio_stereo_pushloop
	mov cs:adpcmshiftval_diff, ch
	xor ch, ch
	add si, bx
	jmp frameloop_videodata_process

frameloop_wssaudio:
	;#Decode ADPCM for WSS-compatible audio
	pop bp
	mov di, cs:currentreadsample
	mov ax, cs:samplebufferseg
	mov es, ax
	mov dx, 0x0F44
	mov al, 0x2F
	out dx, al
	inc dx
	mov cx, bp
	shl cx, 1
	dec cx
	mov al, cl
	out dx, al
	dec dx
	mov al, 0x2E
	out dx, al
	inc dx
	mov al, ch
	out dx, al ;#set DMA length (in number of samples)
	test bl, bl
	jnz frameloop_wssaudio_stereo
	mov cx, cs:adpcmshiftval
	inc dx
	out dx, al ;#reenable DMA
frameloop_wssaudio_mono_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	stosw
	mov cl, cs:[bx+accelerationtable_adpcm]
	pop ax
	xchg al, ah
	xor ah, ah
	mov bx, cx
	shl bx, 8
	add bx, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	stosw
	mov cl, cs:[bx+accelerationtable_adpcm]
	dec bp
	jnz frameloop_wssaudio_mono_pushloop
	jmp frameloop_wssaudio_cleanup
	
frameloop_wssaudio_stereo:	
	mov cl, cs:adpcmshiftval
	mov ch, cs:adpcmshiftval_diff
	mov bx, bp
	shl bx, 1
	inc dx
	out dx, al ;#reenable DMA
frameloop_wssaudio_stereo_pushloop:
	lodsw
	push ax
	xor ah, ah
	push di
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov al, [bx+si-2]
	xor ah, ah
	xchg cl, ch
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample_diff
	mov cs:lastsample_diff, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	pop di
	mov dx, ax
	mov ax, cs:lastsample
	push bx
	mov bx, ax
	add ax, dx
	stosw ;#output left sample
	sub bx, dx
	mov ax, bx
	stosw ;#output right sample
	pop bx
	xchg cl, ch
	pop ax
	xchg al, ah
	xor ah, ah
	push di
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample
	mov cs:lastsample, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	mov al, [bx+si-1]
	xor ah, ah
	xchg cl, ch
	mov di, cx
	and di, 0x00FF
	shl di, 8
	add di, ax
	cbw
	shl ax, cl
	add ax, cs:lastsample_diff
	mov cs:lastsample_diff, ax
	mov cl, cs:[di+accelerationtable_adpcm]
	pop di
	mov dx, ax
	mov ax, cs:lastsample
	push bx
	mov bx, ax
	add ax, dx
	stosw ;#output left sample
	sub bx, dx
	mov ax, bx
	stosw ;#output right sample
	pop bx
	xchg cl, ch
	dec bp
	jnz frameloop_wssaudio_stereo_pushloop
	mov cs:adpcmshiftval_diff, ch
	xor ch, ch
	add si, bx

frameloop_wssaudio_cleanup:
	cmp di, 0x8000
	jb frameloop_wssaudio_cleanup_nooverflow
	push ds
	push si
	push cx
	mov ax, cs:samplebufferseg
	mov ds, ax
	mov bx, 0x8000
	mov si, bx
	mov cx, di
	xor di, di
	sub cx, bx
	shr cx, 1
	rep movsw
	pop cx
	pop si
	pop ds
frameloop_wssaudio_cleanup_nooverflow:
	mov cs:currentreadsample, di
	
frameloop_videodata_process:
	mov cs:adpcmshiftval, cx
	;#Get data from file
	lodsw ;#ax has planes to do
	mov cs:doplanes, ax
	mov bx, 0
frameloop_planereadloop:
	mov dx, cs:[bx+planetests]
	mov ax, cs:doplanes
	test ax, dx
	jnz frameloop_noplaneskip
	jmp frameloop_planeend ;#Prevent generating a conditional near jump, which is unsupported below 80386
frameloop_noplaneskip:
	lodsw ;#ax has plane length
	mov cs:[bx+videolen], ax
	mov cs:filebuffercurpos, si
	inc ax
	shl ax, 1
	call PROCEDURE_tryreadsection
	mov si, cs:filebuffercurpos
	mov ax, cs:[planeseg+bx]
	mov es, ax
	
	;#backwards-compatibility-breaking proposal
	;#lodsw ;#ax has number of 1-length deltas
	;#test ax, ax
	;#jz frameloop_fillstart ;#If length is zero, skip to doing fills
	;#mov dx, ax
	;#add dx, 0x007F
	;#shr dx, 7
	;#and ax, 0x007F
	;#jz frameloop_onedeltaloop ;#ax & 0xF == 0 => ax is a multiple of 128
	;#shl ax, 2
	;#mov di, 4*128
	;#sub di, ax
	;#add di, offset frameloop_onedeltaloop
	;#jmp di
	
frameloop_onedeltaloop: ;#partially unrolled for SPEED!
	;#.rept 128
	;#lodsw ;#ax has offset
	;#mov di, ax
	;#movsw
	;#.endr
	;#dec dx
	;#jnz frameloop_onedeltaloop
	
	;#Write data fills
frameloop_fillstart:
	lodsw ;#ax has number of fills
	test ax, ax
	jz frameloop_copystart ;#If length is zero, skip to doing copies
	mov dx, ax
	add dx, 0x007F
	shr dx, 7
	and ax, 0x007F
	jz frameloop_fillloop ;#ax & 0x7F == 0 => ax is a multiple of 128
	mov di, ax
	shl di, 3
	add ax, di ;#ax *= 9 effectively, each fill iteration is 9 bytes worth of instructions
	mov di, 9*128
	sub di, ax
	add di, offset frameloop_fillloop
	jmp di

frameloop_fillloop: ;#partially unrolled for SPEED!
	.rept 128
	lodsw ;#ax has offset
	mov di, ax
	lodsw ;#ax has length
	mov cx, ax
	lodsw ;#ax has word to copy
	rep stosw
	.endr
	dec dx
	jnz frameloop_fillloop
	
	;#Write data copies
frameloop_copystart:
	lodsw ;#ax has number of copies
	test ax, ax
	jz frameloop_planeend ;#If length is zero, skip to the end of the plane
	mov dx, ax
	add dx, 0x007F
	shr dx, 7
	and ax, 0x007F
	jz frameloop_copyloop ;#ax & 0x7F == 0 => ax is a multiple of 128
	shl ax, 3 ;#ax *= 8 effectively, each copy iteration is 8 bytes worth of instructions
	mov di, 8*128
	sub di, ax
	add di, offset frameloop_copyloop
	jmp di
	
frameloop_copyloop: ;#partially unrolled for SPEED!
	.rept 128
	lodsw ;#ax has offset
	mov di, ax
	lodsw ;#ax has length
	mov cx, ax
	rep movsw
	.endr
	dec dx
	jnz frameloop_copyloop
	
frameloop_planeend:
	mov cs:filebuffercurpos, si
	add bx, 2
	cmp bx, 8
	jnb frameloop_stopplaneread
	jmp frameloop_planereadloop ;#Prevent generating a conditional near jump, which is unsupported below 80386
frameloop_stopplaneread:
	push cs
	pop ds
	
	pop dx
	pop cx
	ret
	
.data
	file_openerror_message:		.ascii	"Error, could not open file.$"
	file_formaterror_message:	.ascii	"Error, file is not a .98v file.$"
	magic_number:				.ascii	"98V\0"
	planeseg:					.word	0xA800, 0xB000, 0xB800, 0xE000
	planetests:					.word	0x0001, 0x0002, 0x0004, 0x0008
							  ;#Hz     44100.0 33075.0 22050.0 16537.5 11025.0  8268.8  5520.0  4130.0
	sampleratespec_to_wssdiv:	.byte	  0x0B,   0x0D,   0x07,   0x02,   0x03,   0x00,   0x01,   0x01
	sampleratespec_to_buzzfreq:	.word	0x0038, 0x004A, 0x006F, 0x0095, 0x00DF, 0x0129, 0x01BD, 0x0253 ;#2.4576 MHz bus
								.word	0x002D, 0x003C, 0x005B, 0x0079, 0x00B5, 0x00F1, 0x016A, 0x01E3 ;#1.9968 MHz bus
	shiftdownvalues:			.byte	  0x0A,   0x0A,   0x09,   0x09,   0x08,   0x08,   0x08,   0x08 ;#2.4576 MHz bus
								.byte	  0x0B,   0x0A,   0x0A,   0x09,   0x09,   0x08,   0x08,   0x08 ;#1.9968 MHz bus
	old_vsync_vector_offset:	.word	0x0000
	old_vsync_vector_segment:	.word	0x0000
	old_timer_vector_offset:	.word	0x0000
	old_timer_vector_segment:	.word	0x0000
	fifo_io_addr:				.word	0x0000
	using_sound:				.byte	0x00
	is_stereo:					.byte	0x00
	current_buzzer_shiftdown:	.byte	0x00
	old_interrupt_mask:			.byte	0x00
	current_sample_midpoint:	.word	0x0000
	baseseg:					.word	0x0000
	filebufferseg:				.word	0x0000
	filebuffercurpos:			.word	0x0000
	filebuffercurwritepos:		.word	0x0000
	audiolen:					.word	0x0000
	currentreadsample:			.word	0x0000
	videolen:					.word	0x0000, 0x0000, 0x0000, 0x0000
	doplanes:					.word	0x0000, 0x0000
	frameskip:					.word	0x0000
	framesleft:					.word	0x0000
	audiospec:					.word	0x0000
	lastsample:					.word	0x0000
	lastsample_diff:			.word	0x0000
	adpcmshiftval:				.word	0x0000
	adpcmshiftval_diff:			.word	0x0000
	numframes_lo:				.word	0x0000
	numframes_hi:				.word	0x0000 ;#32-bit to support very long videos
	filehandle:					.word	0x0000
	samplebufferseg:			.word	0x0000
	samplebufferptr:			.word	0x0000
	samplebuffer_dmabank:		.word	0x0000
	accelerationtable_adpcm:	.byte	0x00