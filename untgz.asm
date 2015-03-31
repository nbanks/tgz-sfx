; Copyright (C) Lux Software Scriptorium Inc.  All rights reserved.
; Written by Nathan Banks

; This is a gnu-zip decompressor.  This compressor uses the deflate alogrithm,
; which is a very nice implementation of an LZ77 (Lempel-Ziv 1977) sliding
; window/huffman cross.  This program uses a unique table implementation to
; decompress the huffman codes, and the sliding window is actually very
; simple (copy from the previous outgoing data stream).
;    The huffman trees should be 8k in size, which gives more than enough
; space for any real tree.  The lowest 200h bytes of the tree are used to
; assign the lengths to each of the characters. (ie if the char=A=65, the bit
; length will be stored in offset position 65.)  The first step, after the
; tree is made, is to get the next 9 bits off the input stream.  Val (the
; value) will probably take less than 9 bits, but nine bits is a good
; start, and it's the maximum amount guarenteed by obtaining one word.  Get
; the word addressed at the offset (Val SHL 1+200h).  If this word is 200h or
; under, it represents a valid character and then advance the number of bits
; indicated by the table.  If it's over 200h, it represents an offset to a
; mini-tree.  In this case, let NewVal=the next 4 bits (the size of the
; mini-tree, 16 Words).  Add Val to NewVal to index the new offset and
; repeat.
;    Although this takes up a little more space than a traditional tree (any
; 8-bit value takes up 2 table spaces, 7-bit takes 4 etc), it makes it easy
; to decompress relitively large amounts of data at the same time.  It also
; isn't too large, and can still support some tree-sturctured searching.
; This is the best compromize I can think of, and it works really well.

%include "exebin.mac"

  EXE_begin
  EXE_stack 0x800	  ; use the 0x800 default

 section .text ; These will be written together, sort of.
  JMP Begin

 section .text ; A procedure that halts after displaying a message.
  Message DB 'Error decompressing file!',7,13,10,'$'
 ; The message is in CS just in case the procedure is called while DS is ???.
GiveError:
  Mov AH,09h
  Push CS
  Mov DX,Message
  Pop DS ; Now DS is set up properly (to CS)
  Int 21h
  Mov AX,4C00h ; Terminate program.
  Int 21h

 section .bss

 align 2
  Input  ResW 1
  Output ResW 1   ; These are the DOS file handlers.
  Header:
    Header_ID                ResW 1 ; $8B1F
    Header_CompressionMethod ResB 1
    Header_Flags             ResB 1
    Header_MTime             ResD 1 ; Unix time stamp.
    Header_ExtraFlags        ResB 1
    Header_OS                ResB 1
  SizeOfHeader EQU 10

  FText    EQU 1 ; These are the flag options
  FHCRC    EQU 2
  FExtra   EQU 4
  FName    EQU 8
  FComment EQU 10h

  FullBufSize EQU 1000h ; A 4K buffer is a good size
   ; (But there are two extra bytes that are allocated as an overflow).
 align 2
  OutBufSeg ResW 1 ; The output/lookback buffer
  BufOffset ResW 1 ; The position in the Output/Lookback buffer

  InBufSeg  ResW 1 ; InBufSeg^ is the stream buffer.
  BytePos   ResW 1 ; Offset  within the input stream.
  BufSize   ResW 1 ; The current size of the buffer that's loaded.
  Temp      ResW 1 ; Used for very temporary data
  BitPos    ResB 1 ; The second position for the input stream.

; *** MEMORY STUFF ***
 section .text
AllocMem: ; Function AllocMem(AmountNeeded:Word):Word; Assembler;
 ; AmountNeeded is stored in BX.
  Mov AH,48h
  Int 21h
  JNC .End
  JMP GiveError
.End:
  ret

; *** BASIC FILE IO SECTION ***

 section .text
Reset: ; Procedure Reset(Name:PChar); Assembler;
 ; DS:DX -> Name, a Null terminated string.  Returns the handle in AX.
  Mov AX,3D02h ; Open file Read/Write who knows about sharing...
  Xor CX,CX ; Just in case this actually does something (server mask)
  Int 21h
  JNC .NoError
  JMP GiveError
.NoError:
  Ret

ReWrite: ; Function ReWrite(Name:PChar):Word; Assembler;
 ; DS:DX -> Name, a Null terminated string.
  Mov AH,3Ch ; Create/Truncate file.
  Xor CX,CX ; No file attributes set.
  Int 21h
  JNC .NoError
  JMP GiveError
.NoError:
  Ret

Close: ; Procedure Close(Handle:Word); Assembler;
 ; BX = Handle is Input or Output...
  Mov AH,3Eh
  Int 21h
  JNC .NoError
  JMP GiveError
.NoError:
  Ret

SeekForward: ; Procedure SeekForward(Amount:Word); Assembler;
 ; Presumes the input file.
 ; DX = Amount
  Mov AX,4201h ; Seek, from the current file position.
  Mov BX,[Input]
  Xor CX,CX ; Just clear this, must be forward then.
  Int 21h
  Ret

BlockRead: ; Function BlockRead(Point:Pointer; Amount:Word):Word; Assembler;
 ; This returns the amount that was actually read. Reads from input.
 ; DS:DX -> Buffer, CX = Amount, BX = the file handle
  Mov AH,3Fh ; Read file.
  Int 21h
  JNC .NoError
  JMP GiveError
.NoError:
  Ret

BlockWrite: ; Procedure BlockWrite(Point:Pointer; Amount:Word); Assembler;
 ; This calls GiveError if the Amount<>AmountWritten. Writes to Output.
 ; DS:DX -> Buffer, CX = Amount, BX = the file handle
  Mov AH,40h    ; Write file.
  Push CX       ; In case it's destroyed.
  Int 21h
  Pop CX        ; Doesn't kill the flags.
  JNC .NoError
  CMP AX,CX     ; Compare it to the origional amount.
  JE .NoError   ; If the amount written is less than the wanted amount, oopz.
  JMP GiveError
.NoError:
  Ret

; *** UNTAR SECTION ***

 section .data

 align 2
  SizeLeft DD 0  ; The space left to write in this file.
  OutputFileOpen DB 0 ; True if an output file is open.
  Months DB 31,28,31,30,31,30,31,31,30,31,30,31 ; Used by settime.
  SHLVals DB 0,5,11,0,9,5

 section .bss

 alignb 2
  Time ResD 1 ; The origional time variable (UNIX sex since 1970.).
  s ResW 1    ; Used by SetTime
  m ResW 1
  h ResW 1
  d ResW 1
  y ResW 1
  Mo ResW 1
  TarPos ResW 1  ; Used by UnTar.
  Amount ResW 1  ; The total value that's used by UnTar

 Section .text

; Procedure SetTime; Assembler;
;  Sets Output's file time to Time, where Time is the # of sex since 1970.

 MiniDiver:
   Mov AX,DI
   Xor DX,DX
   Div CX
   Mov DI,AX

   Mov AX,SI
   Div CX      ; CX is still 60, DX is the wanted high Word.
   Mov [BX],DX ; Store the seconds.
   Mov SI,AX
   Inc BX
   Inc BX      ; Get it ready for the next position.
   ret

 SHLStep:      ; Used to create the DOS time variable.
   LodSB
   Mov CL,AL
   SHL Word [BX],CL
   Or DX,Word [BX]
   Inc BX
   Inc BX
   ret

SetTime: ; The real start for SetTime.
  Mov DI,[Time+2]  ; High Word
  Mov SI,[Time]    ; Low Word
  Mov BX,s
  Mov CX,60
  Call MiniDiver ; s
  Call MiniDiver ; m
  Mov CX,24
  Call MiniDiver ; h
  Mov CX,365
  Call MiniDiver ; d
  Mov [BX],SI    ; y

  Mov AX,SI
  Mov CL,4
  Inc AX      ; Adjust for 1970
  IDiv CL     ; Div 4
  Mov Byte [Months+1],28 ; Default to non-leepyear.
  CMP AH,3
  JNE .NotLeep
  Mov Byte [Months+1],29 ; This is a leepyear.
.NotLeep:
  Xor AH,AH   ; Get rid of the upper bit now.
  Dec AX      ; We want one less than this value.
  Sub [d],AX    ; Which makes D one greater.

  Mov SI,Months
  Xor CX,CX
  Mov BX,[d]
  Xor AH,AH
.LoopStart:
  Mov [d],BX
  LodSB       ; AX=amount for this month.
  Inc CX      ; Guarenteed one.
  Sub BX,AX
  JA .LoopStart ; Until BX<AX
  Mov [Mo],CX

  Sub Word [y],10 ; DOS starts at 1980
  Mov SI,SHLVals  ; Source A
  Mov BX,s        ; Source B
  Xor DX,DX       ; Dest
  Call SHLStep
  Call SHLStep
  Call SHLStep
  Push DX
  Xor DX,DX
  Call SHLStep
  Call SHLStep
  Call SHLStep
  Pop CX       ; CX=Time, DX=Date
  Mov BX,[Output]
  Mov AX,5701h ; Set the date and time.
  Int 21h
  ret

; Procedure Untar(Amount:Word); Assembler;
; Takes the tar from OutBufSeg:0 and restores the files.  Presumes 512-byte
; breaks (normal).  Amount is stored in CX
 OctConvert: ; DI is the dest, SI is the offset within the header.
   Mov Word [DI],0   ; This converts the octal number to a Long.
   Mov Word [DI+2],0
   Add SI,[TarPos]
  ; Mov ES,OutBufSeg ; Should be set from before.
   Mov DX,11
   Mov CL,3
 .OctLoop:
   SHL Word [DI+2],CL ; Upper bits.
   Mov AX,[DI]
   ROL AX,CL
   Mov BX,AX
   And AX,7          ; We want only the lower three bits.
   Or Word [DI+2],AX
   And BX,~7         ; Lose the lower three bits.
   Mov Word [DI],BX

   Mov AL,[ES:SI]
   Inc SI
   Sub AL,'0'
   Or Byte [DI],AL
   Dec DX
   JNZ .OctLoop
   ret

UnTar:
  Mov [Amount],CX
  Mov Word [TarPos],0
.BigStart:
  CMP Byte [OutputFileOpen],1
  JNE .ItsAHeader

.ItsNotAHeader:   ; BlockWrite stuff to an already openned file.
  Mov CX,[Amount]
  Sub CX,[TarPos] ; Use this value throughout NotLastBlock.
  CMP Word [SizeLeft+2],0
  JNE .NotLastBlock
  CMP [SizeLeft],CX
  JBE .LastBlock
.NotLastBlock:    ; If SizeLeft>Amount-TarPos then...
  Sub [SizeLeft],CX
  JNC .NoHiDec
  Dec Word [SizeLeft+2]
.NoHiDec:
  Push DS ; CX is already set from before.
  Mov DX,[TarPos]
  Mov BX,[Output]
  Add [TarPos],CX  ; We need to do this after setting DX...
  Mov DS,[OutBufSeg]
  Call BlockWrite  ; BlockWrite(@Mem[OutBufSeg:TarPos],Amount-TarPos)
  Pop DS
  JMP .NextLoop
.LastBlock:        ; Else...
  Push DS
  Mov CX,[SizeLeft] ; This is only the lower word, but SizeLeft<=8000h.
  Mov BX,[Output]
  Mov DX,[TarPos]
  Mov DS,[OutBufSeg]
  Call BlockWrite
  Pop DS
  Call SetTime
  Mov BX,[Output]  ; It's not going to do anything more with this file.
  Call Close
  Mov Byte [OutputFileOpen],0 ; The output is now closed.
  Mov CX,[TarPos]
  Add CX,[SizeLeft]
  Add CX,1FFh
  Mov Word [SizeLeft],0 ; Upper word is definately 0 already.
  And CX,0FE00h
  Mov [TarPos],CX
  JMP .NextLoop

.ItsAHeader: ; It wasn't writing stuff.
  Mov ES,[OutBufSeg]
  Mov DI,[TarPos]
  CMP Byte [ES:DI],0
  JE .ForgetAnything ; Jump past if there's no file name.

  Mov CX,100 ; The length of the file name.
.StartSlashing:
  Mov AL,[ES:DI] ; Change to DOS directory slashes.
  CMP AL,'/'
  JNE .NoChange
  Mov Byte [ES:DI],'\'
.NoChange:
  Inc DI
  Loop .StartSlashing

  Mov SI,[TarPos]  ; Check TarHeader^.LinkFlag.
  CMP Byte [ES:SI+156],'5' ; If it's '5', it's a directory.
  JNE .NotDir
  Mov AH,39h
  Push DS
  Mov DX,[TarPos]  ; DS:DX -> directory name.
  Mov DS,[OutBufSeg]
  Int 21h
  Pop DS           ; Any error is probably cuz the directory was there b4.
  JMP .WasDir

.NotDir:           ; It's some sort of file.
  Mov DI,SizeLeft
  Mov SI,124       ; TarHeader^.Size
  Call OctConvert
  Mov DI,Time
  Mov SI,136       ; TarHeader^.mTime
  Call OctConvert

  Push DS
  Mov DX,[TarPos]  ; The header Starts with the file name.
  Mov DS,[OutBufSeg]
  Call ReWrite     ; Now we're rewriting.
  Pop DS
  Mov [Output],AX  ; Now that DS is restored.
  Mov Byte [OutputFileOpen],1 ; Now the outputFile is open.
.WasDir:
.ForgetAnything:
  Add Word [TarPos],512 ; Get it ready for the next header/start of info.
.NextLoop:
  Mov SI,[TarPos]
  CMP SI,[Amount]
  JAE .End
  JMP .BigStart
.End:
  ret

; *** FILE INTERFACE SECTION ***

section .text
GetOverHeader: ; Procedure GetOverHeader; Assembler;
  Mov DX,Header
  Mov CX,SizeOfHeader
  Mov BX,[Input]
  Call BlockRead  ; BlockRead(@Header,SizeOf(Header))
  Test Byte [Header_Flags],FExtra
  JZ .NoExtra ; If Flags and FExtra<>0 then
  Mov DX,Temp
  Mov CX,2
  Mov BX,[Input]
  Call BlockRead ; BlockRead(@Size,2)
  Mov DX,[Temp]
  Call SeekForward ; SeekForward(Size)
.NoExtra:
  Test Byte [Header_Flags],FName ; If Flags and FName<>0 then
  JZ .NoName
.NameLoop:
  Mov DX,Temp ; The temp position.
  Mov CX,1    ; Only one byte at a time.
  Mov BX,[Input]
  Call BlockRead ; BlockRead(@Temp,2);
  CMP Byte [Temp],0
  JNE .CommentLoop ; Go until it gets a zero.
.NoName:
  Test Byte [Header_Flags],FComment
  JZ .NoComment
.CommentLoop:
  Mov DX,Temp ; The temp position.
  Mov CX,1    ; Only one byte at a time.
  Mov BX,[Input]
  Call BlockRead ; BlockRead(@Temp,2);
  CMP Byte [Temp],0
  JNE .CommentLoop ; Go until it gets a zero.
.NoComment:
  Test Byte [Header_Flags],FHCRC
  JZ .NoCRC
  Mov DX,Temp ; Just read into the Temp, and ignore.
  Mov CX,2
  Mov BX,[Input]
  Call BlockRead ; BlockRead(@Temp,2)
.NoCRC:
  ret

FixBitsAfterInc: ; Procedure FixBitsAfterInc; Assembler;
 ; This procedure ensures the stream contains enough new data.
.Start:
  CMP Byte [BitPos],8
  JB .EndTime
  Inc Word [BytePos]
  Mov AX,[BytePos]
  CMP AX,FullBufSize
  JB .BufferNotFull
  Mov ES,[InBufSeg]
  Mov AX,[ES:FullBufSize]
  Mov [ES:0],AX

 ; Read More...
  Mov DX,2       ; Start at position 2
  Mov CX,FullBufSize
  Mov BX,[Input]
  Push DS        ; Redo the segment last :)
  Mov DS,[InBufSeg]
  Call BlockRead ; BlockRead(@Mem[InBufSeg:DX],FullBufSize)
  Pop DS
  Mov [BufSize],AX ; Store BufSize now...

  Mov Word [BytePos],0
.BufferNotFull:
  Dec AX    ; Decrement it twice, but it might be negative now.
  Dec AX
  CMP AX,[BufSize] ; If BytePos>BufSize+2 then it's past the EOF.
  JLE .NotOverflow
  JMP GiveError ; It's read past the end of the file.
.NotOverflow:
  Sub Byte [BitPos],8  ; Get rid of 8 more bits (A byte)
  JMP .Start ; Loop until the breakout at the beginning.
.EndTime:
  ret

GetNextBits: ; Function GetNextBits(Number:Byte):Word; Assembler;
 ; Number should be stored in CL.
 ; This procedure returns Number bits from the stream, updating everything.

  Push CX     ; We want the number later.
  Mov ES,[InBufSeg]
  Mov DI,[BytePos]
  Mov CL,[BitPos]
  Mov BH,CL   ; In case it's needed later, for Sub CL,BitPos
  Mov AX,[ES:DI]
  SHR AX,CL
  Mov DX,1
  Pop CX      ; Get the number back, who cares about CH.
  SHL DX,CL
  Add [BitPos],CL
  Dec DX      ; 1 SHL Number-1
  CMP DX,01FFh
  JBE .Fine
  Mov CL,10h
  Sub CL,BH   ; BH is the old bitpos value.
  Xor BH,BH   ; Only start reading the extra byte if you have to.
  Mov BL,[ES:DI+2]
  SHL BX,CL   ; BX=ES:[DI+2] SHL (16-BitPos)
  Or AX,BX
.Fine:
  And AX,DX
  Push AX
  Call FixBitsAfterInc
  Pop AX
  ret

; *** DECOMPRESSION SECTION ***
 section .bss
  SizeBase EQU 9 ; This should be 9 or less cuz a word has 9+ valid bits
         ; It's the number of bits that can be collected without linking
 Alignb 2 ; These buffers are segments allocated in the main program.
  TreeTreeSeg ResW 1 ; The tree used to create the trees.
  MainTreeSeg ResW 1 ; The primary tree used for the huffman codes.
  DistTreeSeg ResW 1 ; The tree used to store LZ77 lengths.
  TreeOffset  ResW 1 ; A temporary value used while creating the trees.
  DefaultTrees ResB 1 ; Storage used by InitTrees for the parameter

 section .data       ; This is also only used for this section, but oh well.
 BitTreeDisorder DB 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15
TempName: DB "Temp.dat",0
TempFile: DW 0

 section .text
FindChar: ; Function FindChar(TreeSeg:Word):Word; Assembler;
 ; TreeSeg is stored in AX.
 ; This procedure looks up the next character in the stream based on the
 ; huffman table at TreeSeg^.  It updates the positions appropriately.

  Mov [Temp],AX ; Just anywhere.  Now TreeSeg is in Temp.
  Mov ES,[InBufSeg]
  Mov SI,[BytePos]
  Mov CL,[BitPos]
  Mov AX,[ES:SI]
  SHR AX,CL     ; BX is the current stream (Next 9 bits).

  Mov ES,[Temp] ; Switch ES although it may go back to the other again.
  Mov DI,AX
  And DI,(1 << SizeBase)-1
  SHL DI,1
  Add DI,200h
  Mov DI,[ES:DI]  ; DI=Current Step=
    ; MemW[TreeSeg:$200+(CurStream and (1 SHL SizeBase-1)) SHL 1]

  CMP DI,200h  ; Odds are 1/300 that DI>=200h (It usually jumps).
  JB .ThatsTheLastStep
  ; Only get the last character if there's more than nine bits.
  Mov ES,[InBufSeg]
  Inc SI      ; SI was BytePos before, now make it BytePos+2
  Mov CL,10h
  Inc SI
  Mov BL,[ES:SI]
  Sub CL,[BitPos] ; Now CL>8 because BitPos<8 at any time.
  SHL BX,CL       ; Because of this, BH will now be cleared.
  Or AX,BX
  Mov CL,SizeBase
  SHR AX,CL
 ; CurStream:=
 ;     (CurStream+Mem[InBufSeg:BytePos+2] SHL (16-BitPos)) SHR SizeBase
  Mov ES,[Temp] ; TreeSeg, Back to this one again.
  Mov CL,4      ; For the next SHR's.
.LoopStart:
  Mov BX,AX
  And BX,15
  SHL BX,1
  Mov DI,[ES:DI+BX]
  ; CurStep:=MemW[TreeSeg:CurStep+(CurStream and 15) SHL 1]
  SHR AX,CL  ; CL=4
  CMP DI,200h
  JAE .LoopStart ; Do it again if it's too high.
.ThatsTheLastStep:

  Mov AL,[ES:DI] ; Get the number of bits for that value.
  CMP AL,0
  JNE .Fine
  JMP GiveError  ; This is an invalid node of the tree.
.Fine:
  Add [BitPos],AL  ; Move it forward the appropriate number of bits.
  Push DI        ; Save the Current Step
  Call FixBitsAfterInc
  Pop AX         ; Return the Current Step because it's now the character.
  ret

 section .bss
 alignb 2
 ; These variables are only used by SetUpTrees under InitTrees.
  BLCounts  ResW 16 ; Total bit-levels.
  NextCodes ResW 16
  NumMain ResW 1
  NumDist ResW 1
  NumTree ResW 1
  Pos     ResW 1
 ; These ones would normally be passed as variables.
  Segger  ResW 1
  MaxChar ResW 1

 section .text
; Procedure InitTrees(DefaultTrees:Boolean); Assembler;
 ; DefaultTrees is in AL.  Set to 1 if true, 0 if false.
 ; The actual label comes later.
 AddCode: ; Procedure AddCode(Code,Character,Segger,Bits:Word); Assembler;
   ;                           BX   AX        ES     CX
   ; Mov BX,[Code] ; We need the reverse of the code. (ie 1110110 -> 0110111)
   ; Mov CX,[Bits]
   Push CX     ; We'll need this value again.
   Xor DX,DX
 .RevStart:
   SHL DX,1
   SHR BX,1
   JNC .NoBit
   Or DX,1
 .NoBit:
   Loop .RevStart

   ; If Bits>SizeBase then
   ;   PlaceCode(ReverseCode,Character,Segger,$200,Bits,SizeBase);
  ; This part Places the Character in the appropriate spots, and if it
  ; takes too many bits, it sets up all the appropriate links.
   ; Mov DX,[ReverseCode]
   ; Mov AX,[Character]
   ; Mov ES,[Segger]
   Pop CX       ; Get the bits back.
   Mov BX,200h  ; The offset to the area it's working on.
   Mov CH,CL    ; We want the total number of bits in the upper byte.
   Mov CL,SizeBase
 .Start:
   CMP CH,CL
   JB .Normal   ; The number of bits is less than the maximum
   JE .OnlyOne  ; It's equal, so only store one value.
  ; This slot is shared with other characters (The code is more than max).

   Mov DI,1
   SHL DI,CL ; DI=1 SHL MaxBits
   Dec DI
   And DI,DX
   Sub CH,CL ; Now the number of bits is decremented by the Current base.
   SHL DI,1
   SHR DX,CL ; Get the first bits off DX
   Add DI,BX ; DI=(ReverseCode and (1 SHL MaxBits-1)) SHL 1+OfSer
   Mov BX,[ES:DI]     ; BX is now the new offset.
   Mov CL,4  ; Next time it will always take up four bits.
   CMP BX,0  ; Check to see if the allocation is done.
   JNE .AlreadyDone     ; Otherwise allocate enough space for four bits.
   Mov BX,[TreeOffset]  ; Now BX points to the new offset.
   Mov [ES:DI],BX       ; Store this new offset in this area.
   Add Word [TreeOffset],20h ; Don't increment BX, only this offset.
 .AlreadyDone:
   ; Now that this is settled (The old pos points to a new one)
   ; update the pointer that takes care of the remaining bits.
   JMP .Start ; Loop through again until Bits<=MaxBits (CH<=CL)
 .OnlyOne:
  ; MemW[Segger:OfSer+ReverseCode SHL 1]:=Character
   Mov DI,DX
   SHL DI,1
   Mov [ES:DI+BX],AX ; Just store the one value and continue.
   JMP .EndSpot  ; Jump past the normal one.
 .Normal:        ; This is last because it's the most common (one jump)
  ; For Pos:=0 to 1 SHL (MaxBits-Bits)-1 do
  ;   MemW[Segger:OfSer+(Pos SHL Bits+ReverseCode) SHL 1]:=Character
   Sub CL,CH
   Mov SI,1  ; Just a temporary variable for now.
   SHL SI,CL
   Dec SI    ; Now SI=1 SHL (MaxBits-Bits)-1
   Mov CL,CH ; Now CL=Bits
 .FinalLoop:
   Mov DI,SI
   SHL DI,CL ; SHL by bits.
   Or DI,DX  ; DI=DI or ReverseCode
   SHL DI,1  ; This is because they are word values, not byte ones.
   Mov [ES:DI+BX],AX ; Store the character in here.
   Dec SI
   JGE .FinalLoop ; While SI>=0 (Signed)

 .EndSpot:
   ret

 SetUpTree: ; Procedure SetUpTree(Segger,MaxChar:Word); Assembler;
  ; Segger and MaxChar should be set up using the variables in .bss
  ; Tree Offset must be set up before calling this.  Max char is the maximum
  ; character that will be used.  The first offsets must contain the number
  ; of bits used for each character.
  MaxBits EQU 15 ; This may be calculated, but who cares?

  ; FillChar(BLCounts,SizeOf(BLCounts),0)
   Push DS
   Mov DI,BLCounts
   Mov CX,16
   Pop ES
   Xor AX,AX
 Rep StoSW ; Now BLCounts is entirely clear.

 ; For Pos:=0 to MaxChar do
 ;   Inc(BLCounts[Mem[Segger:Pos]]);
   Mov ES,[Segger]
   Mov CX,[MaxChar]
   Xor BX,BX
   Inc CX    ; Max char doesn't include 0.
   Xor SI,SI
 BLCountStart:
   Mov BL,[ES:SI]
   SHL BX,1 ; Change the offset to words.
   Inc SI
   Inc Word [BLCounts+BX]
   Loop BLCountStart

  ; Temp:=0;
  ; For Pos:=1 to MaxBits do
  ; Begin
  ;   Temp:=(Temp+BLCounts[Pos-1]) SHL 1;
  ;   NextCodes[Pos]:=Temp;
  ; End;
   Mov CX,MaxBits
   Mov SI,BLCounts
   Xor DX,DX  ; Temp
 .StartNextCode:
   LodSW
   Add DX,AX
   SHL DX,1 ; DX=(DX+AX) SHL 1 it works.
   Mov [SI+32],DX
  ; Mov [SI+(Offset NextCodes-Offset BLCounts+2-2)],DX
  ; This will account for the difference between the arrays and the inc.
   Loop .StartNextCode

   Mov Word [TreeOffset],200h+(2 << SizeBase)
  ; Past the bit levels, & the roots.

 ; For Pos:=0 to MaxChar do
 ; Begin
 ;   Temp:=Mem[Segger:Pos];
 ;   If Temp<>0 then
 ;   Begin
 ;     AddCode(NextCodes[Temp],Pos,Segger,Temp);
 ;     Inc(NextCodes[Temp]);
 ;   End;
 ; End;
   Mov CX,[MaxChar]
   Xor SI,SI
   Inc CX
 .MainStart:
   Xor BX,BX
   Mov BL,[ES:SI]
   Or BX,BX
   JZ .Skip
   Push CX   ; We'll need these later.
   Push SI
   Mov CX,BX ; Bits=Temp
   SHL BX,1  ; Convert to a word offset.
   Inc Word [BX+NextCodes] ; Inc(NextCodes[Temp])
   Mov BX,[BX+NextCodes]   ; Retreive the value.
   Dec BX    ; Restore the origional value, not the incremented one.
  ; Code=NextCodes[Temp]

   Mov AX,SI ; Character=Pos
   Mov ES,[Segger] ; Pass on the segment.
   Call AddCode

   Pop SI
   Pop CX
 .Skip:
   Inc SI
   Loop .MainStart
   ret

 ReadBitLevels: ; Procedure ReadBitLevels(Segger,MaxChar:Word); Assembler;
  ; Segger and MaxChar are passed using the variables in BSS.
  ; Segger is the segment to which everything should be written.  Max char is
  ; the number of characters to write.  The segment should already be
  ; initialized to zero for error detection to occur.
   Xor DI,DI ; The destination pointer. (ES:[DI])
 .Start:
   Push DI       ; We wanna keep this value.
   Mov AX,[TreeTreeSeg] ; The parameter for FindChar.
   Call FindChar ; Returns the next character off the stream in AX.
   Pop DI        ; Get this guy back.
   Mov ES,[Segger] ; Needed for all of them.
   CMP AX,16     ; Now figure out what to do.
   JE .Code16    ; Repeat the previous bit length
   CMP AX,17
   JE .Code17    ; Repeat 0
   JA .Code18    ; Repeat a large number of 0's
   StoSB         ; Just store the character otherwise.
 .GoBack:
   CMP DI,[MaxChar] ; More efficient then using a register in this case.
   JBE .Start
   ret           ; You get off early here.

 .Code18:        ; Repeat a large number of 0's
   Xor AL,AL
   Mov BX,7
   Mov CX,11
   JMP .GetBits
 .Code17:        ; Repeat 0
   Xor AL,AL
   Mov BX,3
   JMP .Add3
 .Code16:        ; Repeat the previous bit length
   Mov AL,[ES:DI-1] ; The previous bit length.
   Mov BX,2      ; Two bits.
 .Add3:
   Mov CX,3
 .GetBits:   ; AL=Value, BX=Bits to get, CX=Count to add.
   Push AX   ; Restore this one just before it's needed.
   Push DI   ; We're going to need this value.
   Push CX
   Mov CL,BL ; The parameter for GetBits.
   Call GetNextBits
   Pop CX
   Pop DI
   Add CX,AX ; Add the amount specified.
   Mov ES,[Segger] ; Restore this again.
   Pop AX    ; It's needed.  Restore it here, so the return is OK.
 Rep StoSB
   JMP .GoBack ; The ret is elsewhere, we don't need one here.

InitTrees:
  Mov [DefaultTrees],AL ; Use this later.
 ; FillChar(Mem[MainTreeSeg:0],$2000,0); {Initialize it to this.}
  Mov ES,[MainTreeSeg]
  Xor DI,DI
  Mov CX,1000h
  Xor AX,AX
Rep StoSW

  CMP Byte [DefaultTrees],1 ; True
  JE .UseDefault
  Mov CX,5   ; Get Five bits.
  Call GetNextBits ; With the value of 5
  Inc AH     ; Add 256 to AX...
  Mov [NumMain],AX ; NumMain:=GetNextBits(5)+256;
  Mov CX,5
  Call GetNextBits ; With a value of 5
  Mov [NumDist],AX ; NumDist:=GetNextBits(5);
  Mov CX,4
  Call GetNextBits ; With a value of 4
  Add AX,3
  Mov [NumTree],AX ; NumTree:=GetNextBits(4)+3;

  Mov ES,[TreeTreeSeg] ; FillChar(Mem[TreeTreeSeg:0],$800,0);
  Xor DI,DI            ; Initialize this one.
  Xor AX,AX
  Mov CX,400h
Rep StoSW

 ; For Pos:=0 to NumTree do {The NumTree is done in a different order.}
 ;   Mem[TreeTreeSeg:BitTreeDisorder[Pos]]:=GetNextBits(3);

  Mov CX,[NumTree]
  Inc CX ; One more to make up for 0..NumTree
  Mov SI,BitTreeDisorder ; The starting position.
.TreeLoop:
  Push CX
  Push SI
  Mov CX,3 ; The parameter for GetNextBits
  Call GetNextBits ; The result is returned in AX.
  Mov ES,[TreeTreeSeg] ; We'll need this for later.
  Xor BX,BX ; We don't want the high byte messing things up.
  Pop SI
  Mov BL,[SI] ; Now BX may be used as an offset.
  Pop CX
  Inc SI
  Mov [ES:BX],AL ; Now the value is placed.
  Loop .TreeLoop
  Mov AX,[TreeTreeSeg]
  Mov [Segger],AX
  Mov Word [MaxChar],18
  Call SetUpTree ; SetUpTree(TreeTreeSeg,18);
  Mov AX,[MainTreeSeg]
  Mov BX,[NumMain]
  Mov [Segger],AX
  Mov [MaxChar],BX
  Call ReadBitLevels ; ReadBitLevels(MainTreeSeg,NumMain);
  JMP .EndDefault
.UseDefault:

 ; Mov ES,[MainTreeSeg] {Should already be set from before.}
  Xor DI,DI
  Mov AL,8
  Mov CX,144
Rep StoSB ; FillChar(Mem[MainTreeSeg:0],144,8);
  Mov AL,9
  Mov CX,112
Rep StoSB ; FillChar(Mem[MainTreeSeg:144],112,9);
  Mov AL,7
  Mov CX,24
Rep StoSB ; FillChar(Mem[MainTreeSeg:256],24,7);
  Mov AL,8
  Mov CX,8
Rep StoSB ; FillChar(Mem[MainTreeSeg:280],8,8);
  Mov Word [NumMain],287
.EndDefault:

  Mov AX,[MainTreeSeg]
  Mov BX,[NumMain]
  Mov [Segger],AX
  Mov [MaxChar],BX
  Call SetUpTree ; SetUpTree(MainTreeSeg,NumMain);

 ; FillChar(Mem[DistTreeSeg:0],$800,0); {Initialize this one too.}
  Mov ES,[DistTreeSeg]
  Xor DI,DI
  Mov CX,400h
  Xor AX,AX
Rep StoSW

  CMP Byte [DefaultTrees],1 ; True
  JE .StillDefault
  Mov AX,[DistTreeSeg]
  Mov BX,[NumDist]
  Mov [Segger],AX
  Mov [MaxChar],BX
  Call ReadBitLevels ; ReadBitLevels(DistTreeSeg,NumDist)
  JMP .OtherEnd
.StillDefault:
  Xor DI,DI
  Mov CX,31
  Mov AL,5
  Mov [NumDist],CX   ; NumDist:=31
Rep StoSB ; FillChar(Mem[DistTreeSeg:0],31,5);

.OtherEnd:
  Mov AX,[DistTreeSeg]
  Mov BX,[NumMain]
  Mov [Segger],AX
  Mov [MaxChar],BX
  Call SetUpTree ; SetUpTree(DistTreeSeg,NumDist);
  ret

; Procedure ActuallyDecompress; Assembler;
 section .bss ; Only used by ActuallyDecompress.
  alignb 2
  Length ResW 1
  LastOne ResB 1 ; Boolean
 section .data ; Only used by ActuallyDecompress.
   LengthStart DB 0,1,2,3,4,5,6,7,8,10,12,14,
               DB 16,20,24,28,32,40,48,56,64,80,96,112,128,160,192,224,255
     ; Each of the LengthStarts are three less than the real value.
   DistStart DW 1,2,3,4,5,7,9,13,17,25,33,49,
             DW 65,97,129,193,257,385,513,769,1025,1537,
             DW 2049,3073,4097,6145,8193,12289,16385,24577

 FixOutBuf: ; Procedure FixOutBuf; Assembler;
   CMP Word [BufOffset],8000h ; Only do stuff if the buffer is full.
   JB .ForgetIt

   Mov CX,8000h   ; The buf is this big.
   Call UnTar     ; Now convert it to the files.

   Sub Word [BufOffset],8000h
   Mov CX,[BufOffset] ; The segments are already set.
   Push DS            ; We need this again.
   Mov DS,[OutBufSeg]
   Xor DI,DI      ; Move back to the beginning.
   Mov SI,8000h   ; Start here
 Rep MovSB        ; Move BufOffset bytes.  Now it's restored properly.
   Mov AH,2       ; Write a character
   Mov DL,'.'
   Int 21h        ; Gives a '.' through DOS to signal progress.
   Pop DS
 .ForgetIt:
   ret

ActuallyDecompress: ; The real start of the primary procedure.
.HugeLoop:
  Mov CL,1
  Call GetNextBits ; LastOne:=GetNextBits(1)=1;
  Mov [LastOne],AL
  Mov CL,2
  Call GetNextBits ; CurChar:=GetNextBits(2);
 ; AX=CurChar...
  CMP AX,3
  JNE .Not3
  JMP GiveError ; AX=3: GiveError;{Undefined.}
.Not3:
  CMP AX,0
  JNE .Is1or2
  JMP .Not1or2
.Is1or2:
    ; 1,2: Compressed block with Dynamic (1) or Fixed (2) huffman codes.}
 ; Mov AX,[CurChar]
  And AX,1 ; Now it's True if one, false if two
  Call InitTrees ; InitTrees(CurChar=1);
.StartMainLoop:
  Mov AX,[MainTreeSeg]
  Call FindChar  ; the segment is stored in AX...
  CMP AX,256
  JA .DoStringCopy
  JNE .DoNormalChar
  JMP .EndMainLoop
.DoNormalChar:   ; Under 256, just do this.
 ; Mov AX,CurChar
  Mov ES,[OutBufSeg] ; Just store the current value as a byte.
  Mov BX,[BufOffset]
  Inc Word [BufOffset]
  Mov [ES:BX],AL
  Call FixOutBuf
  JMP .StartMainLoop

.DoStringCopy:
; Length:=LengthStart[CurChar-257]+3+
;   GetNextBits(LengthBits[CurChar-257]);
 ; Mov AX,[CurChar]
  Sub AX,257   ; Now we have the proper offset...
  Push AX      ; We'll want the origional value later.
  SHR AX,1
  SHR AX,1
  JZ .NoProb   ; Don't decrement it to wrap around to 0.
  Dec AX
  CMP AX,6
  JNE .NoProb  ; Skip the zeroing unless AX=6
  Xor AX,AX    ; Length is 258, don't read extra bits.
.NoProb:
  Mov CL,AL    ; AX is now the same as LengthBits[CurChar]
  Call GetNextBits
  Pop BX
  Mov BL,[BX+LengthStart]
  Add AX,BX    ; The upper bits are 0 cuz CurChar-257<256
  Add AX,3     ; Three extra ones for good luck.
  Push AX      ; This length will be restored in CX later.

 ; CopyPos:=FindChar(DistTreeSeg);
  Mov AX,[DistTreeSeg]
  Call FindChar  ; Now CopyPos is in AX...
 ; CopyPos:=(BufOffset-
 ;   (DistStart[CopyPos]+GetNextBits(DistBits[CopyPos]))) and $7FFF;
  Push AX      ; We'll need this value again...
  SHR AX,1     ; Calculate the number of bits for this value.
  JZ .NoDec    ; We don't want it to wrap around.
  Dec AX
.NoDec:
  Mov CL,AL
  Call GetNextBits ; Now get that many bits.
  Pop BX       ; Now the origional CopyPos is in BX
  Mov SI,[BufOffset] ; SI will have the final value.
  SHL BX,1     ; Array of words
  Add AX,[BX+DistStart] ; We've added DistStart[CopyPos]
  Sub SI,AX
  And SI,7FFFh ; Keep it in the first 32K for now...
 ; Now SI is copyPos, length is next on the stack.

  Pop CX       ; The length is now restored.
 ; This copies CX bytes for CopyPos (SI) to BufOffset (DI)
 ; It can't start after $8000, but it may overflow into this region.
  Mov ES,[OutBufSeg] ; Temporarily use ES:[DI]
  Mov DI,[BufOffset]
.CopyStart:
  Mov AL,[ES:SI]
  Inc SI
  StoSB
  CMP SI,8000h ; If the copy pos overflowed...
  JB .NextRun
  CMP DI,8000h ; And the BufOffset hasn't...
  JAE .NextRun
  Sub SI,8000h
 ; Note the two conditions, if the CopyPos is over the limit but
 ; the other one is too, it's writing correctly into the overflow
 ; area, otherwise it should wrap back to the beginning..
.NextRun:
  Loop .CopyStart
  Mov [BufOffset],DI ; Get it back here again.
  Call FixOutBuf
  JMP .StartMainLoop
.EndMainLoop:
  JMP .LoopBack

.Not1or2:
 ; 0: An uncompressed block.
 ; Note that the uncompressed block may later be referred to as part of
 ; the last 32k just as any other block may be.  This is why I just use
 ; the normal routines (ie. GetNextBits) to deal with this reletively
 ; rare block type.  This should result in slower, but smaller code.
  Mov AL,[BitPos] ; If BitPos<>0 then Pos:=GetNextBits(8-BitPos);
  Or AL,AL
  JZ .NoGetRemaining
  Mov CX,8
  Sub CL,AL
  Call GetNextBits
.NoGetRemaining:

  Mov CX,16        ; Length:=GetNextBits(16);
  Call GetNextBits ; The length given in bytes.
  Mov [Length],AX
  Mov CX,16        ; Pos:=GetNextBits(16);
  Call GetNextBits ; Should be the one's complement of Length
  Not AX
  CMP AX,[Length]
  JE .ValidLength
  JMP GiveError    ; If Length<>Word(Not Pos) then GiveError;
.ValidLength:
 ; This is cheap, but it's smaller than other methods.
  Mov CX,[Length]
.CheapStart:
  Push CX   ; Mem[OutBufSeg:BufOffset]:=GetNextBits(8);
  Mov CL,8
  Call GetNextBits
  Mov ES,[OutBufSeg]
  Mov BX,[BufOffset]
  Inc Word [BufOffset]
  Mov [ES:BX],AL   ; The bit value.
  Call FixOutBuf   ; Ensure it hasn't overflowed or anything.
  Pop CX
  Loop .CheapStart
.LoopBack:
  CMP Byte [LastOne],1 ; True
  JE .Ender
  JMP .HugeLoop
.Ender:

  Mov CX,[BufOffset] ; Get that last section of output.
  Call UnTar   ; Extract this too.
  ret

; *** MAIN PROGRAM ***

section .data
  InName DB  'FileName.EXE',0 ; Only allow this name for now.
section .text
Begin:
  CLD  ; Standard for the entire program.
 ; Get the appropriate memory.
  Mov BX,(FullBufSize+11h) >> 4
  Call AllocMem      ; $11 is the right amount,
  Mov [InBufSeg],AX  ; 2 extra bytes plus $F for the SHR 4 lost bits.

  Mov BX,811h ; 32K lookback buffer, plus some space.
  Call AllocMem
  Mov [OutBufSeg],AX

  Mov BX,200h          ; This is more than enough for any tree (8k).
  Call AllocMem        ; Plus more than enough space (512 bytes)
  Mov [MainTreeSeg],AX ; for the bit lengths of each of the characters.

  Mov BX,80h    ; This is more than enough for any distance tree (2.5k)
  Call AllocMem ; Plus 512 bytes for the bit lengths.
  Mov [DistTreeSeg],AX

  Mov BX,80h    ; This is a small area of memory that's used for
  Call AllocMem ; the Huffman codes that create the huffman codes.
  Mov [TreeTreeSeg],AX
 ; Set up the files.

  Push DS          ; This section
  Mov AX,3000h     ; Get version number.
  Int 21h
  Mov DX,InName    ; The default name of itself.
  CMP AL,3
  JB .SkipSelfFile ; To early a DOS version for a true test.

  Mov ES,[CS:2Ch]  ; This points to the segment of environment space.
  Mov DS,[CS:2Ch]  ; They're both set here.
  Xor DI,DI        ; Start at the beginning.
  Mov CX,0FFFFh    ; Tons of space.
  Xor AL,AL        ; Search for nuls.
.Looper:
  RepNE ScaSB      ; Find one Nul.
  CMP Byte [ES:DI],0 ; Another Null right after?
  JNE .Looper      ; Nope, get past this string now.
  Add DI,3         ; Start of the ASCII-Z string of where the file is.
  Mov DX,DI
.SkipSelfFile:
  Call Reset       ; Opens the file from the appropriate directory.
  Pop DS
  Mov [Input],AX ; Only use this file handle...

  Mov DX,EXE_allocsize+header_end-header_start
  Call SeekForward   ; The file size of the compiled code.

  Call GetOverHeader ; Get info such as the name, and skip to the good stuff.

 ; Init the buffer and position pointers.
  Push DS
  Mov CX,FullBufSize+2 ; 2 more for Overflow
  Xor DX,DX
  Mov BX,[Input]
  Mov DS,[InBufSeg]
  Call BlockRead ; BlockRead(@Mem[InBufSeg:0],FullBufSize+2);
  Pop DS
  Mov [BufSize],AX
  Mov Byte [BitPos],0
  Mov Word [BytePos],0
  Mov Word [BufOffset],0

  Call ActuallyDecompress

  Mov BX,[Input]
  Call Close
  Mov AX,4C00h ; Terminate program.
  Int 21h
  EXE_end
