Imports System.Timers

Public Class ACSCardReaderService
  Public Shared evtLog As System.Diagnostics.EventLog
  Private tmr As Timer
  Private cr As ACSCardReader

  Public Sub New()
    MyBase.New()
    ' This call is required by the designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.
    Me.AutoLog = False

    If Not System.Diagnostics.EventLog.SourceExists("ACSCardWatch") Then
      System.Diagnostics.EventLog.CreateEventSource("ACSCardWatch", "CardWatcherLog")
    End If
    evtLog = New System.Diagnostics.EventLog()
    evtLog.Source = "ACSCardWatch"
    evtLog.Log = "CardWatcherLog"
  End Sub

  Protected Overrides Sub OnStart(ByVal args() As String)
    ' Add code here to start your service. This method should set things
    ' in motion so your service can do its work.
    evtLog.WriteEntry("In OnStart")

    tmr = New Timer(500)
    AddHandler tmr.Elapsed, AddressOf HandleTimer


    '' Setup card reader object
    cr = New ACSCardReader(evtLog)

    'Start timer
    tmr.Start()

  End Sub

  Protected Overrides Sub OnStop()
    ' Add code here to perform any tear-down necessary to stop your service.
    evtLog.WriteEntry("In OnStop")
    tmr.Stop()
    tmr.Dispose()

  End Sub



  Private Sub HandleTimer(ByVal sender As System.Object, ByVal e As System.EventArgs)
    Static cardReadMsg As Boolean = False
    Static couldntReadMsg As Boolean = False

    Dim strRead As String()
    cr.Verbose = False
    If Not IsNothing(cr) Then
      cr.CheckCard()
      If cr.IsAllClear Then
        If Not cr.CardIsRead Then
          cr.Verbose = True
          strRead = cr.ReadValues() '' Read all data
          evtLog.WriteEntry("Found '" & strRead.Count.ToString & "' values. Now attempting to send data through 'ACS' pipe")
          If strRead.Count > 0 Then
            Try
              evtLog.WriteEntry("'" & String.Join("|", strRead) & "' with '" & String.Join("|", strRead).Length.ToString & "' byte(s) available in blocks", EventLogEntryType.Information)
            Catch ex As Exception
              evtLog.WriteEntry("Couldn't send data to 'ACS' pipe due to error:" & vbLf & ex.Message, EventLogEntryType.Error)
            End Try
          Else
            evtLog.WriteEntry("No values were read from card.", EventLogEntryType.Warning)
          End If
        Else
          If Not cardReadMsg Then
            evtLog.WriteEntry("Card has already been read.", EventLogEntryType.Warning)
            cardReadMsg = True
          End If
        End If
        couldntReadMsg = False
      Else
        cardReadMsg = False
        '' Hard reset
        cr = New ACSCardReader(evtLog)
        If Not couldntReadMsg Then
          couldntReadMsg = True
          evtLog.WriteEntry("Couldn't properly connect. Reader or Card not connected/authorized.", EventLogEntryType.Warning)
        End If
      End If
    End If
  End Sub

  Public Enum ServiceCommand
    ReadAll = 128
    CardConnected = 129
    ReadUID = 130
  End Enum

  Protected Overrides Sub OnCustomCommand(ByVal Command As Integer)
    evtLog.WriteEntry("Received command '" & Command.ToString & "'...", EventLogEntryType.Information)
    Dim strRead As String
    Using pipeServer As New System.IO.Pipes.NamedPipeServerStream("ACS", IO.Pipes.PipeDirection.InOut, 1)
      Try
        Try
          pipeServer.WaitForConnection()
          If pipeServer.CanRead Then
            If pipeServer.InBufferSize > 0 Then
              Using sr As New IO.StreamReader(pipeServer)
                sr.ReadToEnd()
                evtLog.WriteEntry("Read remaining buffer...", EventLogEntryType.Warning)
              End Using
            End If
          End If
        Catch ex As Exception
          evtLog.WriteEntry("Couldn't connect to 'ACS' pipe with command '" & Command.ToString & "' due to error:" & vbLf & ex.Message, EventLogEntryType.Error)
        End Try
        Select Case Command
          Case ServiceCommand.ReadAll
            strRead = String.Join("|", cr.ReadValues())
          Case ServiceCommand.CardConnected
            strRead = cr.IsAllClear.ToString
          Case ServiceCommand.ReadUID
            strRead = cr.GetUID
          Case Else
            strRead = ""
        End Select
        If Not String.IsNullOrEmpty(strRead) Then
          Dim response = System.Text.Encoding.ASCII.GetBytes(strRead)
          Try
            '' Output data to pipe
            pipeServer.Write(response, 0, response.Length)
            pipeServer.Flush()
            evtLog.WriteEntry("Sending '" & strRead & "' with '" & response.Length.ToString & "' byte(s) to 'ACS' pipe as a result of the command '" & Command.ToString & "'", EventLogEntryType.Information)
          Catch ex As Exception
            evtLog.WriteEntry("Couldn't send data to 'ACS' pipe with command '" & Command.ToString & "' due to error:" & vbLf & ex.Message, EventLogEntryType.Error)
          End Try
          pipeServer.Disconnect()
        End If
      Catch ex As Exception
        evtLog.WriteEntry("An error occurred while setting up or interacting with 'ACS' pipe with command '" & Command.ToString & "':" & vbLf & ex.Message, EventLogEntryType.Error)
      Finally
        If pipeServer.IsConnected Then
          pipeServer.Disconnect()
        End If
      End Try
    End Using

  End Sub

End Class
