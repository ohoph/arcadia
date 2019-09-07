; ##################################################################
; #            A R C A D I A		B O A T L O A D E R 		   #
; ##################################################################
;
; Fork of The Mike Operating System Bootloader 
; small changes made for arcadia by @ophelia
; Copyright (C) 2006 - 2014 MikeOS Developers -- see doc/LICENSE.TXT
;
; Based on a free boot loader by E Dehling. It scans the FAT12
; floppy for KERNEL.BIN (kernel), loads it and executes it.
; This must grow no larger than 512 bytes (one sector), with the final
; two bytes being the boot signature (AA55h). Note that in FAT12,
; a cluster is the same as a sector: 512 bytes.
; ##################################################################

	BITS 16

	jmp short bootloader_start	; jumping past the disk description table
	nop							; padding out before disk description


; ##################################################################
;
; Disk description table, to make it a valid floppy
; Note: some of these values are hard-coded in the source!
; Values are those used by IBM for 1.44 MB, 3.5" diskette

OEMLabel		db "ARCABOOT"	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 1		; Reserved sectors for boot record
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
					; (224 * 32 = 7168 = 14 sectors to read)
LogicalSectors		dw 2880		; Number of logical sectors
MediumByte		db 0F0h		; Medium descriptor byte
SectorsPerFat		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides			dw 2		; Number of sides/heads
HiddenSectors		dd 0		; Number of hidden sectors
LargeSectors		dd 0		; Number of LBA sectors
DriveNo			dw 0		; Drive No: 0
Signature		db 41		; Drive signature: 41 for floppy
VolumeID		dd 00000000h	; Volume ID: any number
VolumeLabel		db "ARCADIA    "; Volume Label: any 11 chars
FileSystem		db "FAT12   "	; File system type: don't change!


; ##################################################################
; main part of the bootloader

bootloader_start:
	mov ax, 07C0h				; setting up a 4K stack space above the buffer
	add ax, 544					; 8k buffer = 512 paragraphs + 32 paragraphs (loader)
	cli							; disabling interrupts while changing the stack
	mov ss, ax
	mov sp, 4096
	sti							; restoring interrupts because we are done with altering the stack

	mov ax, 07C0h				; set data segment to where we're loaded
	mov ds, ax
	cmp dl, 0					; if bios fails to set value in dl correctly
	je no_change
	mov [bootdev], dl			; saving boot device number stored in dl to memory space of variable
	mov ah, 8					; getting the drive parameters
	int 13h
	jc fatal_disk_error
	and cx, 3Fh					; maximum sector number
	mov [SectorsPerTrack], cx	; sector numbers start at 1
	movzx dx, dh				; maximum head number
	add dx, 1					; head numbers start at 0 - add 1 for total
	mov [Sides], dx

no_change:
	mov eax, 0					; needed for older bios as mentioned before


; we need to load the root directory from the disk. the technical details:
; start of root = ReservedForBoot + NumberOfFats * SectorsPerFat = logical 19
; number of root = RootDirEntries * 32 bytes/entry / 512 bytes/sector = 14
; start of user data = (start of root) + (number of root) = logical 33

floppy_ok:						; ready to read the first block of data
	mov ax, 19					; as calculated above, root dir starts at logical sector 19
	call l2hts

	mov si, buffer				; set ES:BX to point to our buffer (see end of code)
	mov bx, ds
	mov es, bx
	mov bx, si

	mov ah, 2					; params needed for int 13h; reading floppy sectors
	mov al, 14					; read 14 floppy disk sectors

	pusha						; prepare to enter the loop


read_root_dir:
	popa						; popping general purpose registers in case they are altered by int 13h
	pusha 						; pushing general purpose registers in case they were altered by int 13h

	stc							; sets carry flag; a few BIOSes do not set carry flag properly on error
	int 13h						; read sectors using BIOS

	jnc search_dir				; if read went OK, skip ahead
	call reset_floppy			; otherwise, reset floppy controller and try again
	jnc read_root_dir			; floppy reset OK?

	jmp reboot					; if not, fatal double error


search_dir:
	popa

	mov ax, ds					; root dir is now in [buffer]
	mov es, ax					; set DI to this info
	mov di, buffer

	mov cx, word [RootDirEntries]; searching all (224) entries
	mov ax, 0					; searching at offset 0


next_root_entry:
	xchg cx, dx					; We use CX (counter register) in the inner loop

	mov si, kern_filename		; start searching for kernel filename to load it 
	mov cx, 11
	rep cmpsb
	je found_file_to_load		; Pointer DI will be at offset 11

	add ax, 32					; bump searched entries by 1 (32 bytes per entry)

	mov di, buffer				; point to next entry
	add di, ax

	xchg dx, cx					; get the original CX back
	loop next_root_entry

	mov si, file_not_found		; leave and print error message if there is no kernel found
	call print_string
	jmp reboot


found_file_to_load:				; fetching cluster and loading FAT into RAM
	mov ax, word [es:di+0Fh]	; offset 11 + 15 = 26, contains 1st cluster
	mov word [cluster], ax

	mov ax, 1					; sector 1 = first sector of first FAT
	call l2hts

	mov di, buffer				; ES:BX points to our buffer
	mov bx, di

	mov ah, 2					; int 13h params: read (FAT) sectors
	mov al, 9					; All 9 sectors of 1st FAT

	pusha						; preparing to enter loop


read_fat:
	popa						; in case registers are altered by int 13h
	pusha

	stc 						; set carry flag
	int 13h						; reads sectors using the BIOS

	jnc read_fat_ok				; if read went OK, skip ahead
	call reset_floppy			; otherwise, reset floppy controller and try again
	jnc read_fat				; floppy reset OK?

fatal_disk_error:
	mov si, disk_error			; if not, print error message and reboot
	call print_string
	jmp reboot					; fatal double error


read_fat_ok:
	popa

	mov ax, 2000h				; segment where we'll load the kernel (free memory)
	mov es, ax
	mov bx, 0

	mov ah, 2					; int 13h floppy read params
	mov al, 1

	push ax						; save in case we (or int calls) lose it


; Now we must load the FAT from the disk. here's how we find out where it starts:
; FAT cluster 0 = media descriptor = 0F0h
; FAT cluster 1 = filler cluster = 0FFh
; Cluster start = ((cluster number) - 2) * SectorsPerCluster + (start of user)
;               = (cluster number) + 31

load_file_sector:
	mov ax, word [cluster]		; convert sector to logical
	add ax, 31

	call l2hts					; sets appropriate params for int 13h

	mov ax, 2000h				; sets buffer past what we've already read
	mov es, ax
	mov bx, word [pointer]

	pop ax						; saving general purpose registers in case we (or int calls) lose it
	push ax

	stc
	int 13h

	jnc calculate_next_cluster	; if there's no error, jumps to next function
	call reset_floppy			; otherwise, reset floppy and retry
	jmp load_file_sector


; In the FAT, cluster values are stored in 12 bits, so we have to
; do a bit of maths to work out whether we're dealing with a byte
; and 4 bits of the next byte -- or the last 4 bits of one byte
; and then the subsequent byte!

calculate_next_cluster:
	mov ax, [cluster]
	mov dx, 0
	mov bx, 3
	mul bx
	mov bx, 2
	div bx						; DX = [cluster] mod 2
	mov si, buffer
	add si, ax					; AX = word in FAT for the 12 bit entry
	mov ax, word [ds:si]

	or dx, dx					; If DX = 0 [cluster] is even; if DX = 1 then it's odd

	jz even						; If [cluster] is even, drop last 4 bits of word
								; with next cluster; if odd, drop first 4 bits

odd:
	shr ax, 4					; shift out first 4 bits (they belong to another entry)
	jmp short next_cluster_cont


even:
	and ax, 0FFFh				; masking out final 4 bits


next_cluster_cont:
	mov word [cluster], ax		; storing cluster

	cmp ax, 0FF8h				; FF8h = end of file marker in FAT12
	jae end

	add word [pointer], 512		; increasing buffer pointer 1 sector length
	jmp load_file_sector


end:							; we did find the file to load
	pop ax						; cleaning up the stack (AX was pushed earlier)
	mov dl, byte [bootdev]		; providing the kernel with boot device info

	jmp 2000h:0000h				; jumping to the entry point of the loaded kernel (yay)


; ##################################################################
; BOOTLOADER SUBROUTINES

reboot:
	mov ax, 0
	int 16h						; waiting for any keystroke
	mov ax, 0
	int 19h						; rebooting the system


print_string:					; output string in SI to screen
	pusha

	mov ah, 0Eh					; int 10h teletype function

.repeat:
	lodsb						; get char from string
	cmp al, 0
	je .done					; if char is zero, end of string
	int 10h						; else print it
	jmp short .repeat

.done:
	popa
	ret


reset_floppy:					; IN: [bootdev] = boot device; OUT: carry set on error
	push ax
	push dx
	mov ax, 0
	mov dl, byte [bootdev]
	stc
	int 13h
	pop dx
	pop ax
	ret


l2hts:							; calculating head, track and sector settings for int 13h
								; IN: logical sector in AX, OUT: correct registers for int 13h
	push bx
	push ax

	mov bx, ax					; save logical sector

	mov dx, 0					; first the sector
	div word [SectorsPerTrack]
	add dl, 01h					; physical sectors start at 1
	mov cl, dl					; Sectors belong in CL for int 13h
	mov ax, bx

	mov dx, 0					; now calculating the head
	div word [SectorsPerTrack]
	mov dx, 0
	div word [Sides]
	mov dh, dl					; head/side
	mov ch, al					; track

	pop ax
	pop bx

	mov dl, byte [bootdev]		; srtting correct device

	ret


; ##################################################################
; STRINGS AND VARIABLES

	kern_filename	db "KERNEL  BIN"	;  kernel filename

	disk_error	db "Floppy Error. Press any key...", 0
	file_not_found	db "KERNEL.BIN not found!", 0

	bootdev		db 0 	; Boot device number
	cluster		dw 0 	; Cluster of the file we want to load
	pointer		dw 0 	; Pointer into Buffer, for loading kernel



; ##################################################################
; END OF BOOT SECTOR AND BUFFER START

	times 510-($-$$) db 0		; pad remainder of boot sector with zeros to make it fit as multiple of 512
	dw 0AA55h					; boot signature 


buffer:							; disk buffer begins at 8k after this, stack starts



