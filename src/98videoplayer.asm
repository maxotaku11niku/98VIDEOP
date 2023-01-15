#98VIDEOP.COM
#Video player for PC-98
#Maxim Hoxha 2022

#Permission is hereby granted, free of charge, to any person
#obtaining a copy of this software and associated documentation
#files (the "Software"), to deal in the Software without
#restriction, including without limitation the rights to use,
#copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the
#Software is furnished to do so, subject to the following
#conditions:

#The above copyright notice and this permission notice shall be
#included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#OTHER DEALINGS IN THE SOFTWARE.

.arch i486
.intel_syntax noprefix
.code16 #This is intended to run in real mode
.org 0x100 #This is intended to be a .com file, so all absolute addresses must start from 0x0100

PROCEDURE_main: #int main(char[] argv)
	mov ax, cs
	mov baseseg, ax #Get loaded segment
	add ax, 0x1000
	mov samplebufferseg, ax #Set sample buffer segment
	mov ax, 0x3D00
	#DOS-specific: null-terminate command stub in PSP
	xor bx, bx
	mov bl, [0x80]
	movb [bx+0x81], 0x00
	mov dx, 0x82	#The PSP command stub starts from 81h, but it includes the space trailing the executable name, so we have to skip over that
	int 0x21		#DOS API: Open file (filename given from command for reading)
	jc main_file_error
	mov filehandle, ax
	mov ah, 0x0D
	int 0x18		#PC98 CRT BIOS: Turn off text display
	mov ah, 0x40
	int 0x18		#PC98 CRT BIOS: Turn on graphics display
	mov ah, 0x42
	mov ch, 0xC0
	int 0x18		#PC98 CRT BIOS: Set display options (640x400, display page 0, colour mode)
	mov al, 0x01
	out 0x6A, al	#PC98 GDC I/O: Write mode register 2 (set to 16-colour mode)
	call PROCEDURE_clean_screen #Preemptive screen clear just in case
	call PROCEDURE_install_vsync_vector
	mov bx, filehandle
	call PROCEDURE_video_read #We're not saving registers (apart from ip) to the stack because we don't give a fuck about what these functions do to them. Not very robust, but it isn't hard to add a few push instructions anyway
	mov bx, filehandle
	mov ah, 0x3E
	int 0x21		#DOS API: Close file (the video file we opened earlier)
	cmp cx, 0xDEAD
	je main_wrongformat
	call PROCEDURE_clean_screen
	mov al, 0x00	#Just clear palette colour 0 to make the screen readable after quitting
	out 0xA8, al	#PC98 GDC I/O: Set palette index
	out 0xAA, al	#PC98 GDC I/O: Set palette colour green level
	out 0xAC, al	#PC98 GDC I/O: Set palette colour red level
	out 0xAE, al	#PC98 GDC I/O: Set palette colour blue level
	mov al, 0x00
	call PROCEDURE_restore_vsync_vector
	jmp main_endprog
main_file_error:
	mov ah, 0x09
	lea dx, file_openerror_message
	int 0x21		#DOS API: Display string (file_openerror_message)
	mov al, 0x01
	jmp main_endprog
main_wrongformat:
	call PROCEDURE_restore_vector
	mov al, 0x02
main_endprog:
	mov ah, 0x0C
	int 0x18		#PC98 CRT BIOS: Turn on text display
	mov ah, 0x41
	int 0x18		#PC98 CRT BIOS: Turn off graphics display
	mov ah, 0x4C
	int 0x21		#DOS API: Terminate with return code (0x00 -> success, 0x01 -> could not open file, 0x02 -> file in wrong format)
#END OF PROCEDURE_main


#Clears the screen very fast
PROCEDURE_clean_screen: #void clean_screen(void)
	mov dx, 4
	lea bx, planeseg
clean_screen_loop:
	mov ax, [bx]
	mov es, ax	#segments? oh crap
	mov di, 0
	mov ax, 0
	mov cx, 16000
	cld
	rep stosw
	add bx, 2
	dec dx
	jnz clean_screen_loop
	ret
#END OF PROCEDURE_clean_screen

#Installs our interrupt vector for VSYNC
PROCEDURE_install_vsync_vector: #void install_vsync_vector(void)
	cli
	mov ax, 0x350A
	int 0x21		#DOS API: Get interrupt vector (0x0A [hardware: VSYNC]) -> es:bx
	mov old_vsync_vector_segment, es
	mov old_vsync_vector_offset, bx
	mov dx, new_vsync_vector_offset
	mov ax, 0x250A
	int 0x21		#DOS API: Set interrupt vector (0x0A [hardware: VSYNC] to our vsync function)
	out 0x64, al	#PC-98 GDC I/O: CRT interrupt reset
	in al, 0x02		#PC-98 interrupt controller I/O: read mask
	and al, 0xFB	#Add VSYNC interrupts
	out 0x02, al	#PC-98 interrupt controller I/O: set mask
	sti
	ret

#Restores the old interrupt vector for VSYNC
PROCEDURE_restore_vsync_vector: #void restore_vsync_vector(void)
	cli
	mov dx, old_vsync_vector_offset
	mov ds, old_vsync_vector_segment
	mov ax, 0x250A
	int 0x21		#DOS API: Set interrupt vector (0x0A [hardware: VSYNC])
	push cs
	pop ds
	in al, 0x02		#PC-98 interrupt controller I/O: read mask
	or al, 0x04		#Turn off VSYNC interrupts
	out 0x02, al	#PC-98 interrupt controller I/O: set mask
	sti
	ret

#Installs our interrupt vector for the timer
PROCEDURE_install_timer_vector: #void install_timer_vector(void)
	cli
	mov ax, 0x3508
	int 0x21		#DOS API: Get interrupt vector (0x08 [hardware: Timer]) -> es:bx
	mov old_timer_vector_segment, es
	mov old_timer_vector_offset, bx
	mov dx, new_timer_vector_offset
	mov ax, 0x2508
	int 0x21		#DOS API: Set interrupt vector (0x08 [hardware: Timer] to our timer function)
	in al, 0x02		#PC-98 interrupt controller I/O: read mask
	and al, 0xFE	#Add Timer interrupts
	out 0x02, al	#PC-98 interrupt controller I/O: set mask
	mov al, 0x06
	out 0x37, al	#PC-98 system port I/O: turn on buzzer
	sti
	ret
	
#Restores the old interrupt vector for the timer
PROCEDURE_restore_timer_vector: #void restore_timer_vector(void)
	cli
	mov dx, old_timer_vector_offset
	mov ds, old_timer_vector_segment
	mov ax, 0x2508
	int 0x21		#DOS API: Set interrupt vector (0x08 [hardware: Timer])
	push cs
	pop ds
	in al, 0x02		#PC-98 interrupt controller I/O: read mask
	or al, 0x01		#Turn off Timer interrupts
	out 0x02, al	#PC-98 interrupt controller I/O: set mask
	mov al, 0x07
	out 0x37, al	#PC-98 system port I/O: turn off buzzer
	mov al, 0xFF
	out 0x71, al
	out 0x71, al	#turn off timer
	sti
	ret

#Reads in the video file
#handle [bx]: File handle returned by DOS
PROCEDURE_video_read: #void video_read(uint16 handle [in bx])
	mov cx, 0x24
	lea dx, filebuffer
	mov ah, 0x3F
	int 0x21		#DOS API: Read from file (0x24 bytes into filebuffer)
	mov si, dx
	lea di, magic_number
	mov bx, 0
video_read_magicnum_loop: #Check that the file signature is ok
	mov al, [si]
	mov ah, [di]
	test ah, ah
	jz video_read_end_magicnum
	xor ah, al
	jnz video_read_wrongformat
	inc si
	inc di
	jmp video_read_magicnum_loop
video_read_end_magicnum: #Set various options
	lodsb
	mov ah, 0
	inc ax
	mov frameskip, ax
	mov framesleft, ax
	lodsw
	mov numframes_lo, ax
	lodsw
	mov numframes_hi, ax
	lodsw
	mov audiospec, ax
	xor ax, ax
	mov cl, 16
video_read_palette_form: #Read colour table into hardware palette
	mov ch, 16
	sub ch, cl
	mov al, ch
	out 0xA8, al	#PC98 GDC I/O: Set palette index
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAC, al	#PC98 GDC I/O: Set palette colour red level
	mov al, ah
	and al, 0x0F
	out 0xAA, al	#PC98 GDC I/O: Set palette colour green level
	inc si
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAE, al	#PC98 GDC I/O: Set palette colour blue level
	dec cl
	inc ch
	mov al, ch
	out 0xA8, al	#PC98 GDC I/O: Set palette index
	mov al, ah
	and al, 0x0F
	out 0xAC, al	#PC98 GDC I/O: Set palette colour red level
	inc si
	mov ah, [si]
	mov al, ah
	and al, 0xF0
	shr al, 4
	out 0xAA, al	#PC98 GDC I/O: Set palette colour green level
	mov al, ah
	and al, 0x0F
	out 0xAE, al	#PC98 GDC I/O: Set palette colour blue level
	inc si
	dec cl
	jnz video_read_palette_form
	
	#Check for 86 soundboard
	mov dx, 0xA460
	in al, dx
	and al, 0xE0
	cmp al, 0x40
	jne video_read_no86
	
	#Prepare audio here for 86 soundboard (turns out there is no ADPCM support)
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	and al, 0x7F #stop FIFO
	out dx, al

	out 0x5F, al
	and al, 0xBF #set to CPU->FIFO mode
	out dx, al

	out 0x5F, al
	or al, 0x08 #reset the FIFO
	out dx, al

	out 0x5F, al
	and al, 0xF7 #clear it again to turn FIFO on
	out dx, al

	out 0x5F, al
	and al, 0xDF #turn off FIFO interrupts
	out dx, al

	out 0x5F, al
	and al, 0xEF #clear interrupt flag
	out dx, al
	
	mov bx, audiospec
	out 0x5F, al
	and al, 0xF8 #set sample rate
	or al, bl
	out dx, al

	mov dx, 0xA46A #set to the appropriate mode, output to both channels
	mov al, 0xB2
	or al, bh
	out dx, al
	
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	or al, 0x20 #turn on FIFO interrupts
	out dx, al
	
	mov dx, 0xA46A #set irq size to 1024 bytes
	mov al, 0x08
	out dx, al
	
	mov dx, 0xA468
	in al, dx
	out 0x5F, al
	or al, 0x80 #start playing PCM
	out dx, al
	
	mov dx, 0xA466
	mov al, 0xA0 #set volume to maximum
	out dx, al
	mov al, 0x01
	mov using_86, al
	jmp video_read_startplay
	
video_read_no86:
	#Prepare audio here if there is no 86 soundboard available
	#Clear sample buffer
	xor di, di
	mov ax, samplebufferseg
	mov es, ax
	mov cx, 0xFFFF
	mov ax, current_sample_midpoint1
	rep stosw
	push cs
	pop es
	
	mov al, 0x36
	out 0x77, al #set PIT mode for channel 0 (interrupt timer, set to rate generator mode)
	
	in al, 0x42 #check the bus frequency
	and ax, 0x0020 #isolate bus frequency bit

	#0 -> 2.4576 MHz
	#1 -> 1.9968 MHz
	lea bx, shiftdownvalues
	mov dx, audiospec
	shr ax, 2
	add bx, dx
	add bx, ax
	mov cl, [bx]
	mov current_buzzer_shiftdown, cl
	lea bx, sampleratespec_to_buzzfreq
	shl dx, 1
	shl ax, 1
	add bx, dx
	add bx, ax
	mov cx, [bx]
	mov al, cl
	out 0x71, al #set timer counter low byte
	mov al, ch
	out 0x71, al #set timer counter high byte
	mov ax, cx
	and ax, 0x0001
	shr cx, 1
	mov current_sample_midpoint1, cx
	add cx, ax
	mov current_sample_midpoint2, cx
	
	mov al, 0x70
	out 0x77, al #set PIT mode for channel 1 (buzzer, set to interrupt stop mode)
	
	call PROCEDURE_install_timer_vector
	
	mov al, 0x00
	mov using_86, al
	
video_read_startplay:
	mov cx, numframes_lo
	mov dx, numframes_hi
	
video_read_frameloop:
	call PROCEDURE_frameloop
video_read_vsync_wait:
	mov bx, framesleft
	cmp bx, 0
	jg video_read_vsync_wait
	mov bx, frameskip
	mov framesleft, bx
	dec cx
	jnz video_read_frameloop
	test dx, dx
	jz video_read_endfunc
	dec dx
	jne video_read_frameloop
	
	mov ax, using_86
	cmp ax, 0x01
	je video_read_endfunc
	call PROCEDURE_restore_timer_vector
	jmp video_read_endfunc
video_read_wrongformat:
	mov ah, 0x09
	lea dx, file_formaterror_message
	int 0x21		#DOS Main API, ah = 09h: Display string, dx = &file_formaterror_message: Pointer to string
	mov cx, 0xDEAD
	ret
video_read_endfunc:
	mov cx, 0xF00D
	ret

#Interrupt routine triggered by VSYNC
PROCEDURE_vsync_interrupt: #void vsync_interrupt(void)
	push ax
	
	mov ax, cs:framesleft
	dec ax
	mov cs:framesleft, ax
	
	out 0x64, al	#PC-98 GDC I/O: CRT interrupt reset
	mov al, 0x20
	out 0x00, al	#PC-98 interrupt controller: signal end of interrupt
	pop ax
	iret

#Interrupt routine to set the buzzer duty level for the current sample. Not used if the 86 soundboard is present.
PROCEDURE_buzzer_interrupt: #void buzzer_interrupt(void)
	push dx
	push si
	push ax
	push ds
	
	mov dx, 0x3FDB
	mov si, cs:currentreadsample
	mov ax, cs:samplebufferseg
	mov ds, ax
	lodsw
	out dx, al
	xchg al, ah
	out dx, al
	mov cs:currentreadsample, si
	
	mov al, 0x20
	out 0x00, al	#PC-98 interrupt controller: signal end of interrupt
	pop ds
	pop ax
	pop si
	pop dx
	iret
	
#Loop for each frame
PROCEDURE_frameloop: #void frameloop(void)
	push cx #push our loop counters, doesn't take too long
	push dx
	
	lodsw #ax has audio length
	mov cx, ax
	push cx
	shl cx, 1
	add cx, 4
	lea dx, filebuffer
	mov bx, filehandle
	mov ah, 0x3F
	int 0x21		#DOS API: Read from file (length of audio + 4 bytes into filebuffer)
	
	mov al, using_86
	cmp al, 0x01
	je frameloop_86audio
	
	#Decode ADPCM for buzzer PCM
	lea si, filebuffer
	pop bp
	xor di, di
	mov ax, samplebufferseg
	mov es, ax
	mov dx, 0xA46C
	mov cx, adpcmshiftval
frameloop_buzaudio_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov bx, ax
	test bl, 0x80
	jz frameloop_buzaudio_noneg1
	not bl
	not ah
frameloop_buzaudio_noneg1:
	shl ax, cl
	add ax, lastsample
	mov lastsample, ax
	mov ch, current_buzzer_shiftdown
	xchg ch, cl
	sar ax, cl
	xchg ch, cl
	xor ch, ch
	add ax, current_sample_midpoint1
	stosw #store sample in buffer
	cmp bl, 0x18
	ja frameloop_buzaudio_nodrop1
	dec cx
	jns frameloop_buzaudio_noshift1
	xor cx, cx
	jmp frameloop_buzaudio_noshift1
frameloop_buzaudio_nodrop1:
	cmp bl, 0x68
	jb frameloop_buzaudio_noshift1
	inc cx
	test cl, 0x08
	jz frameloop_buzaudio_noshift1
	mov cl, 0x08
frameloop_buzaudio_noshift1:
	pop ax
	xor al, al
	xchg al, ah
	mov bx, ax
	test bl, 0x80
	jz frameloop_buzaudio_noneg2
	not bl
	not ah
frameloop_buzaudio_noneg2:
	shl ax, cl
	add ax, lastsample
	mov lastsample, ax
	mov ch, current_buzzer_shiftdown
	xchg ch, cl
	sar ax, cl
	xchg ch, cl
	xor ch, ch
	add ax, current_sample_midpoint2
	stosw #store sample in buffer
	cmp bl, 0x18
	ja frameloop_buzaudio_nodrop2
	dec cx
	jns frameloop_buzaudio_noshift2
	xor cx, cx
	jmp frameloop_buzaudio_noshift2
frameloop_buzaudio_nodrop2:
	cmp bl, 0x68
	jb frameloop_buzaudio_noshift2
	inc cx
	test cl, 0x08
	jz frameloop_buzaudio_noshift2
	mov cl, 0x08
frameloop_buzaudio_noshift2:
	dec bp
	jnz frameloop_buzaudio_pushloop
	xor ax, ax
	mov currentreadsample, ax
	jmp frameloop_videodata_process
	
frameloop_86audio:
	#Decode ADPCM for 86 PCM
	lea si, filebuffer
	pop di
	mov dx, 0xA46C
	mov cx, adpcmshiftval
frameloop_86audio_pushloop:
	lodsw
	push ax
	xor ah, ah
	mov bx, ax
	test bl, 0x80
	jz frameloop_86audio_noneg1
	not bl
	not ah
frameloop_86audio_noneg1:
	shl ax, cl
	add ax, lastsample
	mov lastsample, ax
	xchg al, ah #output sample to both channels
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	cmp bl, 0x18
	ja frameloop_86audio_nodrop1
	dec cx
	jns frameloop_86audio_noshift1
	xor cx, cx
	jmp frameloop_86audio_noshift1
frameloop_86audio_nodrop1:
	cmp bl, 0x68
	jb frameloop_86audio_noshift1
	inc cx
	test cl, 0x08
	jz frameloop_86audio_noshift1
	mov cl, 0x08
frameloop_86audio_noshift1:
	pop ax
	xor al, al
	xchg al, ah
	mov bx, ax
	test bl, 0x80
	jz frameloop_86audio_noneg2
	not bl
	not ah
frameloop_86audio_noneg2:
	shl ax, cl
	add ax, lastsample
	mov lastsample, ax
	xchg al, ah #output sample to both channels
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	xchg al, ah
	out dx, al
	cmp bl, 0x18
	ja frameloop_86audio_nodrop2
	dec cx
	jns frameloop_86audio_noshift2
	xor cx, cx
	jmp frameloop_86audio_noshift2
frameloop_86audio_nodrop2:
	cmp bl, 0x68
	jb frameloop_86audio_noshift2
	inc cx
	test cl, 0x08
	jz frameloop_86audio_noshift2
	mov cl, 0x08
frameloop_86audio_noshift2:
	dec di
	jnz frameloop_86audio_pushloop
	
frameloop_videodata_process:
	mov adpcmshiftval, cx
	#Get data from file
	lodsw #ax has planes to do
	mov doplanes, ax
	mov bx, 0
frameloop_planereadloop:
	mov dx, [bx+planetests]
	mov ax, doplanes
	test ax, dx
	jz frameloop_planeend
	lodsw #ax has plane length
	mov [bx+videolen], ax
	mov cx, ax
	inc cx
	shl cx, 1
	lea dx, filebuffer
	push bx
	mov bx, filehandle
	mov ah, 0x3F
	int 0x21		#DOS API: Read from file (length of plane + 2 bytes into filebuffer)
	pop bx
	#We'll want to return  to our read point later
	
	#Write data to planes
	lea si, filebuffer
	mov dx, [videolen+bx]
	test dx, dx
	jz frameloop_planeend #If length is zero, skip to the end of the plane
	mov ax, [planeseg+bx]
	mov es, ax
frameloop_deltaloop:
	lodsw #ax has offset
	mov cx, ax
	and cx, 0x7FFF
	mov di, cx
	test ax, 0x8000
	jz frameloop_copydelta
	lodsw #ax has length
	mov cx, ax
	lodsw #ax has word to copy
	rep stosw
	sub dx, 3
	ja frameloop_deltaloop
	jmp frameloop_planeend
frameloop_copydelta:
	lodsw #ax has length
	mov cx, ax
	push cx
	rep movsw #copy time
	pop cx
	add cx, 2
	sub dx, cx
	ja frameloop_deltaloop
	
frameloop_planeend:
	push cs #Reset ds after this plane
	pop ds
	add bx, 2
	cmp bx, 8
	jb frameloop_planereadloop
	
	pop dx
	pop cx
	ret
	
	file_openerror_message:		.ascii	"Error, could not open file.$"
	file_formaterror_message:	.ascii	"Error, file is not a .98v file.$"
	magic_number:				.ascii	"98V\0"
	planeseg:					.word	0xA800, 0xB000, 0xB800, 0xE000
	planetests:					.word	0x0001, 0x0002, 0x0004, 0x0008
								#Hz     44100.0 33075.0 22050.0 16537.5 11025.0  8268.8  5520.0  4130.0
	sampleratespec_to_buzzfreq:	.word	0x0038, 0x004A, 0x006F, 0x0095, 0x00DF, 0x0129, 0x01BD, 0x0253 #2.4576 MHz bus
								.word	0x002D, 0x003C, 0x005B, 0x0079, 0x00B5, 0x00F1, 0x016A, 0x01E3 #1.9968 MHz bus
	shiftdownvalues:			.byte	  0x0A,   0x0A,   0x09,   0x09,   0x08,   0x08,   0x07,   0x07 #2.4576 MHz bus
								.byte	  0x0B,   0x0A,   0x0A,   0x09,   0x09,   0x08,   0x08,   0x07 #1.9968 MHz bus
	old_vsync_vector_offset:	.word	0x0000
	old_vsync_vector_segment:	.word	0x0000
	old_timer_vector_offset:	.word	0x0000
	old_timer_vector_segment:	.word	0x0000
	new_vsync_vector_offset:	.dc.w	PROCEDURE_vsync_interrupt
	new_timer_vector_offset:	.dc.w	PROCEDURE_buzzer_interrupt
	using_86:					.byte	0x00
	current_buzzer_shiftdown:	.byte	0x00
	current_sample_midpoint1:	.word	0x0000
	current_sample_midpoint2:	.word	0x0000
	temp_pwmval:				.word	0x0000
	baseseg:					.word	0x0000
	samplebufferseg:			.word	0x0000
	audiolen:					.word	0x0000
	currentsamplestart:			.word	0x0000
	currentreadsample:			.word	0x0000
	videolen:					.word	0x0000, 0x0000, 0x0000, 0x0000
	doplanes:					.word	0x0000, 0x0000
	frameskip:					.word	0x0000
	framesleft:					.word	0x0000
	audiospec:					.word	0x0000
	lastsample:					.word	0x0000
	adpcmshiftval:				.word	0x0000
	numframes_lo:				.word	0x0000
	numframes_hi:				.word	0x0000 #32-bit to support very long videos
	filehandle:					.word	0x0000
	filebuffer:					.word	0x0000