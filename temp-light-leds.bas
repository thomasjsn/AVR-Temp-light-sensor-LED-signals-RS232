'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_TEMP_LIGHT_LEDs_v.1.0
'  date: 23/10/2011
'  prot: 2.10
'  sn# : 65
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 38400
Config Portb = Output
Config Portd.2 = Output
Config Portd.3 = Output
Config Portd.4 = Output
Config Portd.5 = Input
Config Portd.6 = Input
Config Portd.7 = Input
Config Watchdog = 128

$version 1 , 0 , 9

'in
'PC0: Temp A
'PC1: Light A
'PC2: In 1 A
'PC3: In 2 A
'PC4: In 3 A
'PC5: In 4 A

'out
'PB0: Buzzer
'PB1: Green PWM
'PB2: Yellow PWM
'PB3: Red PWM
'PD3: Lifesignal
'PD4: Link activity

'serial
'PD0: Rx
'PD1: Tx

Dim Send As String * 30 , Stored_id As Eram Byte
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 9 , Com_value As Word
Dim Com_com As String * 1 , Com_nr As String * 1
Dim Led As Byte , Com_stat As String * 4 , Status As Byte
Dim Value As Word , Values As String * 4 , Id As Byte , Ids As String * 2

Dim Crc As Byte
Dim Verinfo As String * 20

Config Timer1 = Pwm , Pwm = 8 , Prescale = 1 , Compare A Pwm = Clear Up , Compare B Pwm = Clear Up
Config Timer2 = Pwm , Prescale = 1 , Compare Pwm = Clear Up
Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Min_id = 32
Const Max_id = 125
Const Pwm_max = 255
Const Out_max = 1
Const Stat_max = 7

Led_life Alias Portd.3
Led_act Alias Portd.4

If Stored_id >= Min_id And Stored_id <= Max_id Then Id = Stored_id Else Id = Min_id

Ids = Hex(id)                                               'module id number
Const Status_serial = "65"                                  'serial number
Const Status_name = "OSID"                                  'unit name
Const Status_verboot = "1.0.0"                              'status version bootloader
Const Status_verprot = "2.1.1"                              'status version protocol
Const Status_dio = "0001"                                   'digital inputs, outputs
Const Status_ai = "060A"                                    'analog inputs, bits
Const Status_ao = "0308"                                    'analog outputs, bits

Start Watchdog                                              'startup parameters
Set Status.0
If Id = Min_id Then Set Status.1

Main:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then                               'check if serial received
   Serialchar = Inkey()
   If Serialchar = Id Or Serialchar = 126 Then              'look for address or broadcast
      Led = 203
      Goto Set_value
      End If
   End If

If Led > 0 Then Decr Led                                    'activity LED timer
If Led = 200 Then Led_act = 1
If Led = 0 Then Led_act = 0

If Status = 0 Then                                          'life led & statusbyte set
   Led_life = 1
   Else
   Led_life = 0
   Led_act = 1
   End If

Reset Watchdog
Waitus 50
Goto Main
End

Set_value:                                                  'serial receive
Input Comminput Noecho                                      'read serialport

Com_com = Mid(comminput , 2 , 1)                            'command check
Com_nr = Mid(comminput , 4 , 1)                             'output nr check
Com_stat = Mid(comminput , 6 , 4)                           'output full check
Com_value = Hexval(com_stat)

If Com_com = "o" Then                                       'output
Select Case Com_nr

Case "0"                                                    'set digital output status
   If Com_stat <> "" Then
      If Com_value > Out_max Then Com_value = Out_max       'max binary value
      If Com_value.0 = 1 Then Portb.0 = 1 Else Portb.0 = 0  'digital output 1
      End If

   Value = 0                                                'get digital output status
   If Portb.0 = 1 Then Set Value.0                          'digital output 1
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",o,0:" + Values
   Gosub Serialsend
   'Goto Main

Case "1"                                                    'analog out 1
   If Com_stat <> "" Then
      If Com_value > Pwm_max Then Com_value = Pwm_max       'max binary value
      Pwm1a = Com_value
      End If
   Values = Hex(pwm1a)
   'Values = Format(values , "0000")
   Send = Ids + ",o,1:" + Values
   Gosub Serialsend

Case "2"                                                    'analog out 2
   If Com_stat <> "" Then
      If Com_value > Pwm_max Then Com_value = Pwm_max       'max binary value
      Pwm1b = Com_value
      End If
   Values = Hex(pwm1b)
   'Values = Format(values , "0000")
   Send = Ids + ",o,2:" + Values
   Gosub Serialsend

Case "3"                                                    'analog out 3
   If Com_stat <> "" Then
      If Com_value > Pwm_max Then Com_value = Pwm_max       'max binary value
      Ocr2 = Com_value
      End If
   Values = Hex(ocr2)
   'Values = Format(values , "0000")
   Send = Ids + ",o,3:" + Values
   Gosub Serialsend

End Select
Goto Main
End If

If Com_com = "i" Then                                       'input
Select Case Com_nr

Case "0"                                                    'get digital input status
   Value = 0
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,0:" + Values
   Gosub Serialsend
   'Goto Main

Case "1"                                                    'analog input 1
   Value = Getadc(0)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,1:" + Values
   Gosub Serialsend
   'Goto Main

Case "2"                                                    'analog input 2
   Value = Getadc(1)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,2:" + Values
   Gosub Serialsend
   'Goto Main

Case "3"                                                    'analog input 3
   Value = Getadc(2)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,3:" + Values
   Gosub Serialsend
   'Goto Main

Case "4"                                                    'analog input 4
   Value = Getadc(3)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,4:" + Values
   Gosub Serialsend
   'Goto Main

Case "5"                                                    'analog input 5
   Value = Getadc(4)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,5:" + Values
   Gosub Serialsend
   'Goto Main

Case "6"                                                    'analog input 6
   Value = Getadc(5)
   Values = Hex(value)
   'Values = Format(values , "0000")
   Send = Ids + ",i,6:" + Values
   Gosub Serialsend
   'Goto Main

End Select
Goto Main
End If

If Com_com = "s" Then                                       'status
Select Case Com_nr

Case "0"                                                    'status byte
   If Com_stat <> "" Then
      If Com_value > Stat_max Then Com_value = Stat_max     'max binary value
      If Com_value.0 = 1 Then Reset Status.0                'bootflag
      If Com_value.1 = 1 Then Reset Status.1                'default address
      If Com_value.2 = 1 Then Toggle Status.2               'manual fail
      End If
   Values = Hex(status)
   'Values = Format(values , "0000")
   Send = Ids + ",s,0:" + Values
   Gosub Serialsend

Case "1"                                                    'serial number
   Send = Ids + ",s,1:" + Status_serial
   Gosub Serialsend
Case "2"                                                    'unit name
   Send = Ids + ",s,2:" + Status_name
   Gosub Serialsend
Case "3"                                                    'firmware version
   Verinfo = Version(2)
   Send = Ids + ",s,3:" + Verinfo
   Gosub Serialsend
Case "4"                                                    'compiled date
   Verinfo = Version()
   Send = Ids + ",s,4:" + Verinfo
   Gosub Serialsend
Case "5"                                                    'bootloader version
   Send = Ids + ",s,5:" + Status_verboot
   Gosub Serialsend
Case "6"                                                    'protocol version
   Send = Ids + ",s,6:" + Status_verprot
   Gosub Serialsend
Case "7"                                                    'digital I/Os
   Send = Ids + ",s,7:" + Status_dio
   Gosub Serialsend
Case "8"                                                    'analog inputs & bits
   Send = Ids + ",s,8:" + Status_ai
   Gosub Serialsend
Case "9"                                                    'analog outputs & bits
   Send = Ids + ",s,9:" + Status_ao
   Gosub Serialsend

End Select
Goto Main
End If

If Com_com = "u" Then                                       'setup
Select Case Com_nr

Case "0"                                                    'reboot
   Send = Ids + ",u,0:0001"
   Gosub Serialsend
   Wait 1

Case "1"                                                    'address
   If Com_value >= Min_id And Com_value <= Max_id Then      'store address
      Stored_id = Com_value
      Id = Stored_id
      End If
   Send = Ids + ",u,1:00" + Hex(id)
   Gosub Serialsend
   If Ids <> Hex(id) Then Wait 1                            'reboot if address change

End Select
Goto Main
End If

Goto Main
End

Serialsend:
   Crc = Checksum(send)
   Print Send + "#" + Str(Crc)
   Return
End
