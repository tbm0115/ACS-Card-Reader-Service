Public Class ACSCardReader
  Dim Protocol, hContext, hCard, ReaderCount As Integer
  Dim RdrState As SCARD_READERSTATE
  Dim sReaderList As String
  Dim sReaderGroup As String
  Dim errString As String
  Dim SendLen, RecvLen As Integer
  Dim ReaderLen, ATRLen As Integer
  Dim dwState, dwActProtocol, retCode As Long
  Dim ATRVal(256) As Byte
  Dim connActive As Boolean = False
  Dim SendBuff(262), RecvBuff(262) As Byte
  Public pioSendRequest, pioRecvRequest As SCARD_IO_REQUEST
  Dim goodBlocks() As Long = {&H0,
&H1,
&H2,
&H3,
&H4,
&H5,
&H6,
&H7,
&H8,
&H9,
&H10,
&H11,
&H12,
&H13,
&H14,
&H15,
&H16,
&H17,
&H18,
&H19,
&H20,
&H21,
&H22,
&H23,
&H24,
&H25,
&H26,
&H27,
&H28,
&H29,
&H30,
&H31,
&H32,
&H33,
&H34,
&H35,
&H36,
&H37,
&H38,
&H39,
&H40,
&H41,
&H42,
&H43,
&H44,
&H45,
&H46,
&H47,
&H48,
&H49,
&H50,
&H51,
&H52,
&H53,
&H54,
&H55,
&H56,
&H57,
&H58,
&H59,
&H60,
&H61,
&H62,
&H63
}

  Private _evtLog As EventLog
  Private _lstReaders As New List(Of String)
  Private _selReader As String

  Private _init, _atr, _auth, _read,_verbose As Boolean

  Public ReadOnly Property IsInitialized As Boolean
    Get
      If Not _init Then
        _init = Initialize()
      End If
      Return _init
    End Get
  End Property
  Public ReadOnly Property IsConnected As Boolean
    Get
      If Not connActive Then
        connActive = Connect()
      End If
      Return Connect()
    End Get
  End Property
  Public ReadOnly Property IsATRConnected As Boolean
    Get
      If Not _atr Then
        _atr = GetATR()
      End If
      Return _atr
    End Get
  End Property
  Public ReadOnly Property IsAuthorized As Boolean
    Get
      If Not _auth Then
        _auth = LoadAuthorization()
      End If
      Return _auth
    End Get
  End Property
  Public ReadOnly Property IsAllClear As Boolean
    Get
      If Not Me.IsInitialized Then Return False
      If Not Me.IsConnected Then Return False
      If Not Me.IsATRConnected Then Return False
      If Not Me.IsAuthorized Then Return False
      Return True
    End Get
  End Property
  Public ReadOnly Property CardIsRead As Boolean
    Get
      Return _read
    End Get
  End Property
  Public ReadOnly Property GetUID As String
    Get
      If SendCommand({&HFF, &HCA, &H0, &H0, &H0}, &H16) Then
        Dim tmpStr As String = ""
        Dim out As String()
        If RecvLen > 2 Then '' Check if enough data is available
          '' Trim output
          For i As Integer = 0 To RecvLen - 3
            tmpStr += Hex(RecvBuff(i))
          Next i
          Return tmpStr
        Else
          Call displayOut(1, retCode, "Not enough data")
        End If
      Else
        Call displayOut(1, retCode, "GetUID could not send command!")
      End If
      Return ""
    End Get
  End Property
  Public Property Verbose As Boolean
    Get
      Return _verbose
    End Get
    Set(value As Boolean)
      _verbose = value
    End Set
  End Property

  Public Sub New(ByVal ReferenceEventLog As EventLog)
    _evtLog = ReferenceEventLog

    _init = Initialize()
    connActive = Connect()
    _atr = GetATR()
    _auth = LoadAuthorization()
  End Sub

  Private Sub displayOut(ByVal errType As Integer, ByVal retVal As Integer, ByVal PrintText As String)
    If Not _verbose Then Exit Sub
    Select Case errType
      Case 0 '' Default
      Case 1 '' Error
        If Not retVal = 0 Then
          PrintText += vbLf & ModWinsCard.GetScardErrMsg(retVal)
        End If
        '' Log errors
        _evtLog.WriteEntry(PrintText, EventLogEntryType.Error)
      Case 2 '' Transmit
        PrintText = "<" + PrintText
        _evtLog.WriteEntry(PrintText, EventLogEntryType.Information)
      Case 3 '' Receive
        PrintText = ">" + PrintText
        '' Log Receiving values
        _evtLog.WriteEntry(PrintText, EventLogEntryType.Information)
      Case 4 '' Success
    End Select
  End Sub
  Private Sub ClearBuffers()
    Dim indx As Long
    For indx = 0 To 262
      RecvBuff(indx) = &H0
      SendBuff(indx) = &H0
    Next indx
  End Sub

  Private Function Initialize() As Boolean
    Static failMsgSent As Boolean = False
    Dim ReaderCount As Integer
    Dim ctr As Integer

    For ctr = 0 To 255
      sReaderList = sReaderList + vbNullChar
    Next

    ReaderCount = 255

    ' 1. Establish context and obtain hContext handle
    retCode = ModWinsCard.SCardEstablishContext(ModWinsCard.SCARD_SCOPE_USER, 0, 0, hContext)

    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then
      'ResetReader() '' Revoke All Clear
      Call displayOut(1, retCode, "")
      '' Display error message only once in Event Log.
      If Not failMsgSent Then
        failMsgSent = True
        _evtLog.WriteEntry("Initialize couldn't establish context", EventLogEntryType.Error)
      End If
      _init = False
      Return False
    Else
      failMsgSent = False
    End If

    ' 2. List PC/SC card readers installed in the system
    retCode = ModWinsCard.SCardListReaders(hContext, "", sReaderList, ReaderCount)

    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then
      If Not failMsgSent Then
        failMsgSent = True
        _evtLog.WriteEntry("Initialize couldn't find readers", EventLogEntryType.Error)
      End If
      _init = False
      Return False
    Else
      failMsgSent = False
    End If

    ' Load Available Readers
    Call LoadListToControl(sReaderList)
    If _lstReaders.Count > 0 Then
      _selReader = _lstReaders.Item(0)
    End If

    _init = True
    Return True
  End Function
  Private Sub LoadListToControl(ByVal ReaderList As String)
    Dim sTemp As String
    Dim indx As Integer

    indx = 1
    sTemp = ""
    _lstReaders.Clear()

    While (Mid(ReaderList, indx, 1) <> vbNullChar)

      While (Mid(ReaderList, indx, 1) <> vbNullChar)
        sTemp = sTemp + Mid(ReaderList, indx, 1)
        indx = indx + 1
      End While

      indx = indx + 1

      _lstReaders.Add(sTemp)

      sTemp = ""
    End While
  End Sub

  Public Function CheckCard() As Boolean
    Static succMsgSent As Boolean = False
    Static lastState As Long
    Dim refState As Long = ModWinsCard.SCARD_STATE_PRESENT
    retCode = ModWinsCard.SCardState(hCard, refState, dwActProtocol, ATRVal(0), ATRLen)

    If Not succMsgSent And Not lastState = refState Then
      succMsgSent = True
      lastState = refState
      _evtLog.WriteEntry("Card State: " & ModWinsCard.GetScardStateMsg(dwState), EventLogEntryType.Information)
    End If

  End Function

  Private Function Connect() As Boolean
    Static msgSent As Boolean = False
    '' Connect to selected reader using hContext handle and obtain hCard handle
    If connActive Then
      retCode = ModWinsCard.SCardDisconnect(hCard, ModWinsCard.SCARD_UNPOWER_CARD)
    End If

    '' Shared Connection
    retCode = ModWinsCard.SCardConnect(hContext, _selReader, ModWinsCard.SCARD_SHARE_SHARED, ModWinsCard.SCARD_PROTOCOL_T0 Or ModWinsCard.SCARD_PROTOCOL_T1, hCard, Protocol)

    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then
      If Not msgSent Then
        msgSent = True
        _evtLog.WriteEntry("Connection couldn't connect to " & _selReader, EventLogEntryType.Error)
      End If
      connActive = False
      Return False
    Else
      msgSent = False
    End If

    connActive = True
    Return True
  End Function

  Private Function GetATR() As Boolean
    If Not connActive Then displayOut(1, 0, "ATR cannot continue without begin connected.") : _atr = False : Return False

    Static failMsgSent As Boolean = False
    Static succMsgSent As Boolean = False
    Dim tmpWord As Long
    Dim tmpStr As String
    Dim indx As Integer
    tmpWord = 32
    ATRLen = tmpWord


    retCode = ModWinsCard.SCardStatus(hCard, _selReader, ReaderLen, dwState, dwActProtocol, ATRVal(0), ATRLen)
    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then
      succMsgSent = False
      If Not failMsgSent Then
        failMsgSent = True
        _evtLog.WriteEntry("ATR couldn't connect", EventLogEntryType.Error)
      End If
      Return False
    Else
      failMsgSent = False
      If Not succMsgSent Then
        succMsgSent = True
        _evtLog.WriteEntry("Card State: " & ModWinsCard.GetScardStateMsg(dwState), EventLogEntryType.Information)
      End If

      tmpStr = "Card Available..." & vbLf
      tmpStr += "ATR Length : " + ATRLen.ToString
      'Call displayOut(3, 0, tmpStr)
      tmpStr = "ATR Value : "
      Dim val As String
      For indx = 0 To ATRLen - 1
        tmpStr += Microsoft.VisualBasic.Right("00" & Hex(ATRVal(indx)), 2) + " "
        val += Convert.ToChar(ATRVal(indx))
      Next indx
      tmpStr += vbLf & vbTab & val
      'Call displayOut(3, 0, tmpStr)
    End If

    tmpStr = "Active Protocol"

    Select Case dwActProtocol
      Case 1
        tmpStr = tmpStr + "T=0"
      Case 2
        tmpStr = tmpStr + "T=1"
      Case Else
        tmpStr = "No protocol is defined."
    End Select

    Call displayOut(3, 0, tmpStr)
    InterpretATR()

    _atr = True
    Return True
  End Function
  Private Sub InterpretATR()
    Dim RIDVal, cardName, sATRStr, lATRStr, tmpVal As String
    Dim indx, indx2 As Integer

    ' 4. Interpret ATR and guess card
    ' 4.1. Mifare cards using ISO 14443 Part 3 Supplemental Document
    If CInt(ATRLen) > 14 Then
      RIDVal = ""
      sATRStr = ""
      lATRStr = ""

      For indx = 7 To 11
        RIDVal = RIDVal & Format(Hex(ATRVal(indx)))
      Next indx

      For indx = 0 To 4
        'shift bit to right
        tmpVal = ATRVal(indx)
        For indx2 = 1 To 4
          tmpVal = tmpVal / 2
        Next indx2

        If ((indx = 1) And (tmpVal = 8)) Then
          lATRStr = lATRStr + "8X"
          sATRStr = sATRStr + "8X"
        Else
          If indx = 4 Then
            lATRStr = lATRStr + Format(Hex(ATRVal(indx)))
          Else
            lATRStr = lATRStr + Format(Hex(ATRVal(indx)))
            sATRStr = sATRStr + Format(Hex(ATRVal(indx)))
          End If
        End If
      Next indx

      If RIDVal = "A00036" Then
        cardName = ""
        Select Case ATRVal(12)
          Case 0 : cardName = "No card information"
          Case 1 : cardName = "ISO 14443 A, Part1 Card Type"
          Case 2 : cardName = "ISO 14443 A, Part2 Card Type"
          Case 3 : cardName = "ISO 14443 A, Part3 Card Type"
          Case 5 : cardName = "ISO 14443 B, Part1 Card Type"
          Case 6 : cardName = "ISO 14443 B, Part2 Card Type"
          Case 7 : cardName = "ISO 14443 B, Part3 Card Type"
          Case 9 : cardName = "ISO 15693, Part1 Card Type"
          Case 10 : cardName = "ISO 15693, Part2 Card Type"
          Case 11 : cardName = "ISO 15693, Part3 Card Type"
          Case 12 : cardName = "ISO 15693, Part4 Card Type"
          Case 13 : cardName = "Contact Card (7816-10) IIC Card Type"
          Case 14 : cardName = "Contact Card (7816-10) Extended IIC Card Type"
          Case 15 : cardName = "0Contact Card (7816-10) 2WBP Card Type"
          Case 16 : cardName = "Contact Card (7816-10) 3WBP Card Type"
        End Select
      End If

      ' Felica and Topaz Cards
      If ATRVal(12) = &H3 Then
        If ATRVal(13) = &HF0 Then
          Select Case ATRVal(14)
            Case &H11 : cardName = cardName + ": FeliCa 212K"
            Case &H12 : cardName = cardName + ": Felica 424K"
            Case &H4 : cardName = cardName + ": Topaz"
          End Select
        End If
      End If

      If ATRVal(12) = &H3 Then
        If ATRVal(13) = &H0 Then
          Select Case ATRVal(14)
            Case &H1 : cardName = cardName + ": Mifare Standard 1K"
            Case &H2 : cardName = cardName + ": Mifare Standard 4K"
            Case &H3 : cardName = cardName + ": Mifare Ultra light"
            Case &H4 : cardName = cardName + ": SLE55R_XXXX"
            Case &H6 : cardName = cardName + ": SR176"
            Case &H7 : cardName = cardName + ": SRI X4K"
            Case &H8 : cardName = cardName + ": AT88RF020"
            Case &H9 : cardName = cardName + ": AT88SC0204CRF"
            Case &HA : cardName = cardName + ": AT88SC0808CRF"
            Case &HB : cardName = cardName + ": AT88SC1616CRF"
            Case &HC : cardName = cardName + ": AT88SC3216CRF"
            Case &HD : cardName = cardName + ": AT88SC6416CRF"
            Case &HE : cardName = cardName + ": SRF55V10P"
            Case &HF : cardName = cardName + ": SRF55V02P"
            Case &H10 : cardName = cardName + ": SRF55V10S"
            Case &H11 : cardName = cardName + ": SRF55V02S"
            Case &H12 : cardName = cardName + ": TAG IT"
            Case &H13 : cardName = cardName + ": LRI512"
            Case &H14 : cardName = cardName + ": ICODESLI"
            Case &H15 : cardName = cardName + ": TEMPSENS"
            Case &H16 : cardName = cardName + ": I.CODE1"
            Case &H17 : cardName = cardName + ": PicoPass 2K"
            Case &H18 : cardName = cardName + ": PicoPass 2KS"
            Case &H19 : cardName = cardName + ": PicoPass 16K"
            Case &H1A : cardName = cardName + ": PicoPass 16KS"
            Case &H1B : cardName = cardName + ": PicoPass 16K(8x2)"
            Case &H1C : cardName = cardName + ": PicoPass 16KS(8x2)"

            Case &H1D : cardName = cardName + ": PicoPass 32KS(16+16)"
            Case &H1E : cardName = cardName + ": PicoPass 32KS(16+8x2)"
            Case &H1F : cardName = cardName + ": PicoPass 32KS(8x2+16)"
            Case &H20 : cardName = cardName + ": PicoPass 32KS(8x2+8x2)"
            Case &H21 : cardName = cardName + ": LRI64"
            Case &H22 : cardName = cardName + ": I.CODE UID"
            Case &H23 : cardName = cardName + ": I.CODE EPC"
            Case &H24 : cardName = cardName + ": LRI12"
            Case &H25 : cardName = cardName + ": LRI128"
            Case &H26 : cardName = cardName + ": Mifare Mini"
          End Select
        Else
          If ATRVal(13) = &HFF Then
            Select Case ATRVal(14)
              Case &H9
                cardName = cardName & ": Mifare Mini"
            End Select
          End If
        End If
        Call displayOut(3, 0, cardName & " is detected.")
      End If
    End If

    '4.2. Mifare DESFire card using ISO 14443 Part 4
    If CInt(ATRLen) = 11 Then
      RIDVal = ""

      For indx = 4 To 9
        RIDVal = RIDVal & Format(Hex(ATRVal(indx)))
      Next indx

      If RIDVal = "6757781280" Then
        Call displayOut(3, 0, "Mifare DESFire is detected.")
      End If
    End If

    '4.3. Other cards using ISO 14443 Part 4
    If CInt(ATRLen) = 17 Then
      RIDVal = ""

      For indx = 4 To 15
        RIDVal = RIDVal & Format(Hex(RecvBuff(indx)), "00")
      Next indx

      If RIDVal = "50122345561253544E3381C3" Then
        Call displayOut(3, 0, "ST19XRC8E is detected.")
      End If
    End If

    '4.4. other cards using ISO 14443 Type A or B
    If lATRStr = "3B8X800150" Then
      Call displayOut(3, 0, "ISO 14443B is detected.")
    Else
      If sATRStr = "3B8X8001" Then
        Call displayOut(3, 0, "ISO 14443A is detected.")
      End If
    End If
  End Sub

  Private Function LoadAuthorization() As Boolean
    If Not _atr Then _auth = False : displayOut(1, 0, "Authorization cannot be attempted without ATR.") : Return False

    Static msgSent As Boolean = False
    '' Setup serial command
    '' Load Authentication Key (Default)
    Dim found As Boolean = False
    Dim arrByte As Byte()
    For i As Integer = 0 To 14 Step 1
      Select Case i
        Case 0
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HFF, &HFF, &HFF, &HFF, &HFF, &HFF}
        Case 1
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HA0, &HB0, &HC0, &HD0, &HE0, &HF0}
        Case 2
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HA1, &HB1, &HC1, &HD1, &HE1, &HF1}
        Case 3
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HA0, &HA0, &HA0, &HA0, &HA0, &HA0}
        Case 4
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HA0, &HA1, &HA2, &HA3, &HA4, &HA5}
        Case 5
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HB0, &HB1, &HB2, &HB3, &HB4, &HB5}
        Case 6
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H4D, &H3A, &H99, &HC3, &H51, &HDD}
        Case 7
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H1A, &H98, &H2C, &H7E, &H45, &H9A}
        Case 8
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H0, &H0, &H0, &H0, &H0, &H0}
        Case 9
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HD3, &HF7, &HD3, &HF7, &HD3, &HF7}
        Case 10
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HAA, &HBB, &HCC, &HDD, &HEE, &HFF}
        Case 11
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H71, &H4C, &H5C, &H88, &H6E, &H97}
        Case 12
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H58, &H7E, &HE5, &HF9, &H35, &HF}
        Case 13
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &HA0, &H47, &H8C, &HC3, &H90, &H91}
        Case 14
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H53, &H3C, &HB6, &HC7, &H23, &HF6}
        Case 15
          arrByte = {&HFF, &H82, &H0, &H0, &H6, &H8F, &HD0, &HA4, &HF2, &H56, &HE9}
      End Select
      If Not IsNothing(arrByte) Then
        '' Set reader authentication key
        SendCommand(arrByte, &H16)
        '' Try Authentication Key on block 0
        ''                  Class  Cmd                 Vers      Block KType Key#
        found = SendCommand({&HFF, &H86, &H0, &H0, &H5, &H0, &H0, &H5, &H60, &H0}, &H16)
        If found Then
          Exit For
        End If
      End If
    Next
    If found Then
      _auth = True
      'Call displayOut(4, 0, "Block 0 Authenticated")
      _evtLog.WriteEntry("Reader Authorized!", EventLogEntryType.Information)
      Return True
    Else
      _auth = False
      'Call displayOut(1, 0, "Failed to Authenticate Block 0")
      If Not msgSent Then
        msgSent = True
        _evtLog.WriteEntry("Authorization failed", EventLogEntryType.Error)
      End If
      Return False
    End If
  End Function

  Private Function SendCommand(ByVal SendBytes As Byte(), ByVal ReceiveLength As Byte) As Boolean
    If Not connActive Then displayOut(1, 0, "Commands cannot be sent without a connection.") : Return False

    Static msgSent As Boolean = False

    Call ClearBuffers()
    Dim cmd As String
    For i As Integer = 0 To SendBytes.Length - 1 Step 1
      cmd += Hex(SendBytes(i)).ToString & " "
      SendBuff(i) = SendBytes(i)
    Next
    SendLen = SendBytes.Length

    'Call displayOut(0, 0, "Setting Command...")

    Dim tmpStr As String

    '' Set receive length
    RecvLen = ReceiveLength

    '' Send Command and get Return Code
    retCode = SendAPDU()

    '' Check if connection was successful
    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then '' Not successful
      If Not msgSent Then
        msgSent = True
        _evtLog.WriteEntry("Send Command couldn't connect!" & vbLf & ModWinsCard.GetScardErrMsg(retCode) & vbLf & "Command: " & cmd, EventLogEntryType.Error)
      End If
      Return False
    Else
      msgSent = False
      '' Get full return value
      If RecvLen > 1 Then
        For i As Integer = RecvLen - 2 To RecvLen - 1
          tmpStr = tmpStr & Hex(RecvBuff(i)) & " "
        Next i

        'Check for response
        If tmpStr.Trim = "63 00" Or tmpStr.Trim = "63 0" Then
          Call displayOut(1, 0, "The Operation Failed. Cmd: '" & cmd & "'")
          Return False
        ElseIf tmpStr.Trim = "6A 81" Then
          Call displayOut(1, 0, "Function not supported. Cmd: '" & cmd & "'")
          Return False
        Else
          Call displayOut(3, 0, "Received: " & tmpStr)
          Return True
        End If
      Else
        Call displayOut(1, retCode, "No data received")
        Return False
      End If
    End If
  End Function
  Private Function SendAPDU() As Long
    Dim indx As Integer
    Dim tmpStr As String

    pioSendRequest.dwProtocol = Protocol
    pioSendRequest.cbPciLength = Len(pioSendRequest)

    tmpStr = ""

    For indx = 0 To SendLen - 1
      tmpStr = tmpStr + Microsoft.VisualBasic.Right("00" & Hex(SendBuff(indx)), 2) + " "
    Next indx

    'Call displayOut(2, 0, "Sending: " & tmpStr)

    retCode = ModWinsCard.SCardTransmit(hCard, pioSendRequest, SendBuff(0), SendLen, pioSendRequest, RecvBuff(0), RecvLen)
    If Not retCode = ModWinsCard.SCARD_S_SUCCESS Then
      'ResetReader() '' Revoke All Clear
      'Call displayOut(1, retCode, "Couldn't Connect")
      Return retCode
    Else
      'Call displayOut(4, 0, "Connected!")
    End If

    tmpStr = ""

    For indx = 0 To RecvLen - 1
      tmpStr = tmpStr & Hex(RecvBuff(indx)) & " "
    Next indx

    Call displayOut(3, 0, tmpStr)
    Return retCode
  End Function

  Public Function ReadValues(Optional ByVal Index As Integer = -1) As String()
    Dim out As String()
    '' Setup serial command
    For j As Integer = 0 To goodBlocks.Length - 1 Step 1
      '' Check if valid return block
      ''                   Cmd           Blk
      If SendCommand({&HFF, &HB0, &H0, goodBlocks(j), &H10}, &H16) Then
        Dim tmpStr As String = ""
        If RecvLen > 2 Then '' Check if enough data is available
          '' Trim output
          For i As Integer = 0 To RecvLen - 3
            tmpStr += Convert.ToChar(RecvBuff(i))
          Next i
          If (Index >= 0 And j = Index) Or Index = -1 Then
            '' Add value to return array
            If Not IsNothing(out) Then
              ReDim Preserve out(out.Length)
            Else
              ReDim out(0)
            End If
            out(out.Length - 1) = tmpStr.Trim
          End If

          '' Write message
          Call displayOut(3, 0, tmpStr)
        Else
          'Call displayOut(1, retCode, "Not enough data")
        End If
      Else
        'Call displayOut(1, retCode, "Error")
      End If
    Next

    _read = True

    Return out
  End Function
End Class
