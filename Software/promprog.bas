'--------------------------------
' System settings
'--------------------------------
$regfile = "m16def.dat"
$crystal = 8000000
$hwstack = 40
$swstack = 32
$framesize = 32
'$sim

$baud = 9600

Config Lcdpin = Pin , Rs = Portd.3 , E = Portd.2 , Db4 = Portb.4 , Db5 = Portb.3 , Db6 = Portb.2 , Db7 = Portb.1
Config Lcd = 16 * 2

Config Base = 0
Baud = 9600

'--------------------------------
' Input pin's aliases
'--------------------------------
Sw2 Alias Pina.7
Sw3 Alias Pina.6
Sw5 Alias Pina.0
Sw6 Alias Pinb.0
Sw4 Alias Pina.1
Sw1 Alias Pinc.7
Burn Alias Portd.4
Ben Alias Porta.5
Cs Alias Portc.1
A8 Alias Portd.5
Sb0 Alias Porta.2
Sb1 Alias Porta.3
Sb2 Alias Porta.4
Datclk Alias Portc.5
Datload Alias Portc.6
Dat Alias Pinc.4
Addrclk Alias Portc.0
Addren Alias Portd.7
Addrdat Alias Portd.6


'--------------------------------
' Port pin direction settings
'--------------------------------
Ddra = &B00111100
Ddrb = &B10111110
Ddrc = &B01101111
Ddrd = &B11111110

'--------------------------------
' Constants
'--------------------------------
Const Prom74188 = 31
Const Prom74188b = 8
Const Prom74287 = 255
Const Prom74287b = 4
Const Prom74571 = 511
Const Prom74571b = 4

'--------------------------------
' Public variables
'--------------------------------
Dim Content(512) As Byte
Dim Seltype As Byte
Dim Seladdr As Word
Dim Selwide As Byte
Dim Curaddr As Word
Dim Curdata As Byte
Dim Str4 As String * 4
Dim Str8 As String * 8
Dim Selstr As String * 16
Dim I As Byte
Dim Curpos As Byte , Curs As Byte
Dim Bitpos As Byte
Dim Goout As Byte
Dim Goroot As Byte
Dim Errcount As Byte
Dim Okprog As Byte
Dim Intelhex As String * 41

'--------------------------------
' Subroutines and function
'--------------------------------
Declare Sub Flushdata                                       ' clears memory with chip content
Declare Sub Showrecord(byval Torow As Byte)                 ' shows data at current address
Declare Sub Setaddr                                         ' send address to shift register
Declare Sub Exporthex                                       ' send HEX file to PC
Declare Sub Readhex                                         ' read file from PC in HEX format
Declare Sub Readchip                                        ' read data from chip from current address
Declare Sub Setbit(byval Cislo As Byte)                     ' set programmed bit
Declare Function Checkbit(byval I As Byte) As Byte          ' check if bit is programmed or not

'--------------------------------
' Beginning of program
'--------------------------------
Initlcd

'--------------------------------
' Set up pins
'--------------------------------
Reset Burn
Reset Sb0
Reset Sb1
Reset Sb2
Set Ben
Reset Addrdat
Reset Addren
Reset Addrclk
Set Datload
Reset Datclk
Set Cs

'--------------------------------
' Custom LCD characters
'--------------------------------
Deflcdchar 0 , 1 , 3 , 7 , 15 , 7 , 3 , 1 , 32              ' left
Deflcdchar 1 , 16 , 24 , 28 , 30 , 28 , 24 , 16 , 32        ' right
Deflcdchar 2 , 32 , 32 , 31 , 14 , 4 , 32 , 32 , 32         ' up
Deflcdchar 3 , 32 , 32 , 4 , 14 , 31 , 32 , 32 , 32         ' down
Deflcdchar 4 , 1 , 1 , 1 , 5 , 9 , 31 , 8 , 4               ' enter



Enable Interrupts

'--------------------------------
' Set up clear memory
'--------------------------------
Call Flushdata

'--------------------------------
' Welcome message
'--------------------------------
Cls
Cursor Off Noblink
Locate 1 , 1
Lcd "PROM Programmer"
Locate 2 , 7
Lcd "v1.1"
Wait 1
Cls

Do
   '--------------------------------
   ' Chip selection menu
   '--------------------------------
   Goroot = 0
   Locate 1 , 1
   Lcd "Select   1=74188"
   Locate 2 , 1
   Lcd "2=74287  3=74571"
   Do
      If Sw1 = 0 Then
         Waitms 50
         Bitwait Sw1 , Set
         Seltype = 1
         Seladdr = Prom74188
         Selwide = Prom74188b
         Selstr = "74188 Selected"
         Exit Do
      End If
      If Sw2 = 0 Then
         Waitms 50
         Bitwait Sw2 , Set
         Seltype = 2
         Seladdr = Prom74287
         Selwide = Prom74287b
         Selstr = "74S287 Selected"
         Exit Do
      End If
      If Sw3 = 0 Then
         Waitms 50
         Bitwait Sw3 , Set
         Seltype = 3
         Seladdr = Prom74571
         Selwide = Prom74571b
         Selstr = "74S571 Selected"
         Exit Do
      End If
   Loop
   '--------------------------------
   ' Chip selected,
   ' editing, loading file
   ' and programming
   '--------------------------------
   Do
      Cursor Off Noblink
      Cls
      Locate 1 , 1
      Lcd Selstr
      Locate 2 , 1
      Lcd "EDT PC RD PRG EX"
      Do
      '--------------------------------
      ' Edit memory content
      '--------------------------------
      If Sw1 = 0 Then
         Waitms 50
         Bitwait Sw1 , Set
         Cls
         Cursor Off Blink
         Curaddr = 0
         Call Showrecord(1)
         Locate 2 , 1
         Lcd Chr(2) ; "  " ; Chr(3) ; "  " ; Chr(0) ; "  " ; Chr(1) ; "  " ; Chr(4) ; " EX"
         Locate 1 , 5
         Goout = 0
         Curpos = 0                                         ' cursor position for address browsing
         Do
            If Sw1 = 0 Then
               Waitms 50
               Bitwait Sw1 , Set                            'previous address
               If Curaddr <> 0 Then
                  Decr Curaddr
                  Call Showrecord(1)
                  Curpos = 0
                  Locate 1 , 5
               End If
            End If
            If Sw2 = 0 Then                                 'next address
               Waitms 50
               Bitwait Sw2 , Set
               If Curaddr <> Seladdr Then
                  Incr Curaddr
               End If
               Call Showrecord(1)
               Curpos = 0
               Locate 1 , 5
            End If
            If Sw3 = 0 Then                                 'previous bit
               Waitms 50
               Bitwait Sw3 , Set
               If Curpos = 0 Then
                  Curpos = Selwide
               Else
                  Curpos = Curpos - 1
               End If
               Curs = 5 + Curpos
               Locate 1 , Curs
            End If
            If Sw4 = 0 Then                                 'next bit
               Waitms 50
               Bitwait Sw4 , Set
               If Curpos = Selwide Then
                  Curpos = 0
               Else
                  Curpos = Curpos + 1
               End If
               Curs = 5 + Curpos
               Locate 1 , Curs
            End If
            If Sw5 = 0 Then                                 ' toggle bit
               Waitms 50
               Bitwait Sw5 , Set
               If Curpos = 0 Then
                  Goout = 1
                  Exit Do
               Else
                  Bitpos = Selwide - Curpos
                  Toggle Content(curaddr).bitpos
                  Call Showrecord(1)
                  Curs = 5 + Curpos
                  Locate 1 , Curs
               End If
            End If
            If Sw6 = 0 Then                                 ' exit editing
               Waitms 50
               Bitwait Sw6 , Set
               Goout = 1
               Exit Do
            End If
         Loop
         If Goout = 1 Then
            Exit Do
         End If
      End If
      '--------------------------------
      ' Load/Save memory content
      ' from computer
      '--------------------------------
      If Sw2 = 0 Then
         Waitms 50
         Bitwait Sw2 , Set
         Do
            Goout = 0
            Locate 2 , 1
            Lcd "Load Save       "
            If Sw1 = 0 Then                                 ' load data from PC
               Waitms 50
               Bitwait Sw1 , Set
               Locate 2 , 1
               Lcd "Waiting for data"
               Call Readhex                                 ' read HEX file from UART
               Goout = 1
            End If
            If Sw2 = 0 Then                                 'save to PC
               Waitms 50
               Bitwait Sw2 , Set
               Locate 2 , 1
               Lcd "Sending to PC   "
               Call Exporthex
               Wait 1
               Locate 2 , 1
               Lcd "HEX file sent EX"
               Bitwait Sw6 , Reset
               Waitms 50
               Bitwait Sw6 , Set
               Goout = 1
            End If
            If Goout = 1 Then
               Exit Do
            End If
         Loop
         If Goout = 1 Then
            Exit Do
         End If
      End If
      '--------------------------------
      ' Load memory content
      ' from chip in socket
      '--------------------------------
      If Sw3 = 0 Then
         Goout = 0
         Goroot = 0
         Waitms 50
         Bitwait Sw3 , Set
         Cls
         Locate 1 , 1
         Lcd Selstr
         For Curaddr = 0 To Seladdr
            Call Setaddr
            Call Readchip
            Call Showrecord(2)
            If Seltype = 1 Then
               Waitms 100
            End If
         Next Curaddr
         Wait 1
         Cls
         Locate 1 , 1
         Lcd Selstr
         Locate 2 , 1
         Lcd "Reading done  EX"
         If Sw6 = 0 Then
            Waitms 50
            Bitwait Sw6 , Set
         End If
      End If
      '--------------------------------
      ' Start programming
      '--------------------------------
      If Sw4 = 0 Then
         Goout = 0
         Goroot = 0
         Waitms 50
         Bitwait Sw4 , Set
         Locate 2 , 1
         Lcd "PRG chip? EN/EX "
         Do
            If Sw5 = 0 Then                                 ' programing confirmed
               Waitms 50
               Bitwait Sw5 , Set
               '--------------------------------
               ' Starting programming procedure
               '--------------------------------
               Errcount = 0
               For Curaddr = 0 To Seladdr
                  Call Showrecord(2)                        ' show current record
                  Call Setaddr                              ' set up address on chip
                  For I = 0 To 7
                  If Content(curaddr).i = 1 Then            ' bit needs to be programmed
                     Call Setbit(i)                         ' select bit for programing
                     Reset Cs                               ' select chip
                     Waitus 10                              ' wait 10 microseconds
                     Set Burn                               ' enable programming voltage
                     Reset Ben                              ' ground programmed bit
                     Waitus 200                             ' wait 200us - stabilizing
                     Set Cs                                 ' deselect chip
                     Waitms 1                               ' wait default programing time - 1ms
                     Reset Cs                               ' select chip again
                     Waitus 200                             ' wait 200us - stabilizing
                     Set Ben                                ' deselect programmed bit
                     Reset Burn                             ' disable high voltage
                     Waitus 10                              ' wait another 10us
                     Waitms 4                               ' recovery time 4x programing time
                     Okprog = Checkbit(i)
                     If Okprog = 0 Then                     ' bit was not programmed
                        Reset Cs                            ' select chip
                        Waitus 10                           ' wait 10 microseconds
                        Set Burn                            ' enable programming voltage
                        Reset Ben                           ' ground programmed bit
                        Waitus 200                          ' wait 200us - stabilizing
                        Set Cs                              ' deselect chip
                        Waitms 4                            ' wait default extended time - 4ms
                        Reset Cs                            ' select chip again
                        Waitus 200                          ' wait 200us - stabilizing
                        Set Ben                             ' deselect programmed bit
                        Reset Burn                          ' disable high voltage
                        Waitus 10                           ' wait another 10us
                        Waitms 16                           ' recovery time 4x programing time
                        Okprog = Checkbit(i)
                        If Okprog = 0 Then                  ' bit was not programmed again, last try with maximum pulse width
                           Reset Cs                         ' select chip
                           Waitus 10                        ' wait 10 microseconds
                           Set Burn                         ' enable programming voltage
                           Reset Ben                        ' ground programmed bit
                           Waitus 200                       ' wait 200us - stabilizing
                           Set Cs                           ' deselect chip
                           Waitms 20                        ' wait default programing time - 1ms
                           Reset Cs                         ' select chip again
                           Waitus 200                       ' wait 200us - stabilizing
                           Set Ben                          ' deselect programmed bit
                           Reset Burn                       ' disable high voltage
                           Waitus 10                        ' wait another 10us
                           Waitms 80                        ' recovery time 4x programing time
                           Okprog = Checkbit(i)
                           If Okprog = 0 Then               ' programming of bit failed, increasing error counter and continuing
                              Incr Errcount
                           End If
                        End If
                     End If
                  End If
                  Next I
               Next Curaddr
               '--------------------------------
               ' Programming done
               '--------------------------------
               Wait 1
               Locate 2 , 1
               Lcd "Done, " ; Errcount ; " ERR   "
               Locate 2 , 15
               Lcd "EX"
               Bitwait Sw6 , Reset
               Waitms 50
               Bitwait Sw6 , Set
               Goroot = 1
               Goout = 1
               Exit Do
            End If
            If Sw6 = 0 Then                                 ' exit from programing routine
               Waitms 50
               Bitwait Sw6 , Set
               Goout = 1
            End If
            If Goout = 1 Then
               Exit Do
            End If
         Loop
      End If
      If Goroot = 1 Then
         Exit Do
      End If
      '--------------------------------
      ' Exit routine
      '--------------------------------
      If Sw6 = 0 Then
         Waitms 50
         Bitwait Sw6 , Set
         Cls
         Goroot = 1
         Exit Do
      End If
      '--------------------------------
      ' Edit recodrs using
      ' PS/2 keyboard
      '--------------------------------
      Loop
      If Goroot = 1 Then
         Exit Do
      End If
   Loop
Loop

'--------------------------------
' Subroutine to prepare
' clean memory content
'--------------------------------
Sub Flushdata
   Dim Addr As Word
   For Addr = 0 To 512
      Content(addr) = 0
   Next Addr
End Sub

'--------------------------------
' Subroutine to show memory
' content at current address
'--------------------------------
Sub Showrecord(torow As Byte)
   Curdata = Content(curaddr)
   Str4 = Str(curaddr)
   Str4 = Format(str4 , "0000")
   Locate Torow , 1
   Lcd Str4 ; "="
   Str8 = Bin(curdata)
   Str8 = Right(str8 , Selwide)
   Lcd Str8
End Sub

'--------------------------------
' Subroutine to send address
' to shift register
'--------------------------------
Sub Setaddr
   Local I As Byte
   If Seltype = 3 Then
      If Curaddr.8 = 1 Then
         Set A8
      Else
         Reset A8
      End If
   End If
   For I = 0 To 7
      If Curaddr.i = 1 Then
         Set Addrdat
      Else
         Reset Addrdat
      End If
      Set Addrclk
      Reset Addrclk
   Next I
   Set Addren
   Reset Addren
End Sub

'--------------------------------
' Subroutine to read data from
' chip from current address
'--------------------------------
Sub Readchip
   Local I As Byte
   Reset Cs                                                 ' Enable reading from chip
   Reset Datload
   Set Datload                                              ' lock data into shift register
   For I = 7 To 0 Step -1                                   ' shift one bit
      Content(curaddr).i = Dat                              ' store readed bit
      Set Datclk
      Reset Datclk
   Next I
   Set Cs                                                   ' disable chip
End Sub


'--------------------------------
' Subroutine for setting one
' data bit to low
'--------------------------------
Sub Setbit(byval Cislo As Byte)                             ' cislo must be 0-7
   Sb0 = Cislo.0
   Sb1 = Cislo.1
   Sb2 = Cislo.2
End Sub

Function Checkbit(byval I As Byte) As Byte
   Local J As Byte , K As Byte , Rdat As Byte
   Reset Cs                                                 ' Enable reading from chip
   Reset Datload
   Set Datload                                              ' lock data into shift register
   For J = 7 To 0 Step -1
      Rdat.j = Dat                                          ' store readed bit
      Set Datclk
      Reset Datclk                                          ' shift one bit
   Next J
   Set Cs
   Checkbit = Rdat.i
End Function


'--------------------------------
' Subroutine to send stored
' chip data to PC in HEX format
'--------------------------------
Sub Exporthex
   Local I As Byte , Numbytes As Word , Blocks As Byte
   Local K As Byte , Rdata As Byte , Str2 As String * 2
   Local Chksum As Byte , Pom As Byte
   Numbytes = Seladdr + 1
   Numbytes = Numbytes \ 16
   Blocks = Numbytes
   Curaddr = 0
    For I = 1 To Blocks
      Intelhex = ":10"                                      ' 10H to send 16 bytes
      Chksum = &H10
      Str4 = Hex(curaddr)
      Str2 = Mid(str4 , 1 , 2)
      Chksum = Chksum + Hexval(str2)
      Str2 = Mid(str4 , 3 , 2)
      Chksum = Chksum + Hexval(str2)
      Intelhex = Intelhex + Str4                            ' add address offset
      Intelhex = Intelhex + "00"                            ' record type data
      For K = 1 To 16
         Rdata = Content(curaddr)                           ' read data from address
         Str2 = Hex(rdata)
         Chksum = Chksum + Rdata
         Intelhex = Intelhex + Str2
         Incr Curaddr
      Next K
      Chksum = Not Chksum
      Chksum = Chksum + 1
      Str2 = Hex(chksum)
      Print Intelhex ; Str2                                 ' send data record to UART
   Next I
   Print ":00000001FF"                                      ' send EOF to UART
End Sub


'--------------------------------
' Subroutine to read stored
' chip data to PC in HEX format
'--------------------------------
Sub Readhex
   Local Inpchar As Byte , I As Byte
   Local Reclength As Byte
   Local Offset As Word , Offs As Word
   Local Rectyp As Byte
   Local Chksum As Byte
   Local Pom As Byte
   Local Txt2 As String * 2
   Local Txt4 As String * 4

   Do
      Do
         Inpchar = Waitkey()
      Loop Until Inpchar = &H3A                             ' waiting for start marker
      Inpchar = Waitkey()                                   ' reading first character of record length
      Txt2 = Chr(inpchar)
      Inpchar = Waitkey()                                   ' reading a secoond character of record length
      Txt2 = Txt2 + Chr(inpchar)
      Reclength = Hexval(txt2)                              ' calculate record length
      Chksum = Reclength
      Txt4 = ""
      Inpchar = Waitkey()                                   ' reading 4 characters of address offset
      Txt2 = Chr(inpchar)
      Inpchar = Waitkey()
      Txt2 = Txt2 + Chr(inpchar)
      Pom = Hexval(txt2)
      Chksum = Chksum + Pom
      Txt4 = Txt2
      Inpchar = Waitkey()
      Txt2 = Chr(inpchar)
      Inpchar = Waitkey()
      Txt2 = Txt2 + Chr(inpchar)
      Pom = Hexval(txt2)
      Chksum = Chksum + Pom
      Txt4 = Txt4 + Txt2
      Offset = Hexval(txt4)
      Inpchar = Waitkey()                                   ' reading first character of record type
      Txt2 = Chr(inpchar)
      Inpchar = Waitkey()                                   ' reading a secoond character of record type
      Txt2 = Txt2 + Chr(inpchar)
      Rectyp = Hexval(txt2)                                 ' calculate record type
      Chksum = Chksum + Rectyp
      If Rectyp = 0 Then                                    ' check record type - 0 is data, 1 i EOF
         For I = 1 To Reclength                             ' read reclength bytes as data
            Inpchar = Waitkey()                             ' reading first character of data
            Txt2 = Chr(inpchar)
            Inpchar = Waitkey()                             ' reading a secoond character of data
            Txt2 = Txt2 + Chr(inpchar)
            Pom = Hexval(txt2)                              ' calculate data byte
            Chksum = Chksum + Pom
            Offs = Offset - 1
            Offs = Offs + I
            Content(offs) = Pom                             ' store data
         Next I
         Inpchar = Waitkey()                                ' reading first character of checksum
         Txt2 = Chr(inpchar)
         Inpchar = Waitkey()                                ' reading a secoond character of checksum
         Txt2 = Txt2 + Chr(inpchar)
         Pom = Hexval(txt2)
         Chksum = Not Chksum
         Chksum = Chksum + 1
         If Pom <> Chksum Then
            Locate 2 , 1
            Lcd "Checksum err  EX"
            Bitwait Sw6 , Reset
            Waitms 50
            Bitwait Sw6 , Set
            Exit Sub
         End If
      Else
         Locate 2 , 1
         Lcd "File loaded   EX"
            Bitwait Sw6 , Reset
            Waitms 50
            Bitwait Sw6 , Set
         Exit Sub
      End If
   Loop
End Sub
