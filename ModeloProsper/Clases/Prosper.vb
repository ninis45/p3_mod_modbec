
Public Class Prosper




    Shared Connected As Boolean
    Shared Server As Object
    Shared AppName As String

    Shared Open As Boolean


    Sub New()

    End Sub
    Public Shared Function Connect() As Object
        Try

            If Connected = False Then
                Server = CreateObject("PX32.OpenServer.1")
                Connected = True
            End If
            Settings.SetBy("open_server", "0")

            Return Server
        Catch ex As Exception
            Throw New Exception("Connect: " + ex.Message)
        End Try

    End Function
    'Deberia ser depreciado, no debe haber ningun DoCmd aca en esta clase
    Public Shared Function Connect(ByVal ProgramPath As String, ByVal ArchivoPVT As String) As Object
        Try
            If Open Then
                Shell(ProgramPath, AppWinStyle.NormalFocus)
            End If


            If Connected = False Then


                Server = CreateObject("PX32.OpenServer.1")
                Connected = True



                DoCmd("PROSPER.OPENFILE=""" & ArchivoPVT & """")

            End If
            Settings.SetBy("open_server", "0")
            Return Server
        Catch ex As Exception


            'Server = Nothing
            'Settings.SetBy("open_server", "1")
            Throw New Exception("Connect: " + ex.Message)
        End Try

    End Function
    Public Shared Function Connect(ByVal ProgramPath As String) As Object
        Try

            Shell(ProgramPath, AppWinStyle.NormalFocus)



            If Connected = False Then


                Server = CreateObject("PX32.OpenServer.1")
                Connected = True

            End If

            Settings.SetBy("open_server", "0")

            Return Server
        Catch ex As Exception
            'Server = Nothing
            ' Settings.SetBy("open_server", "1")
            Throw New Exception("Connect: " + ex.Message)
        End Try

    End Function

    Public Shared Sub Disconnect() 'Public Shared Sub Disconnect()
        If Connected = True Then

            If Open Then
                DoCmd("PROSPER.SHUTDOWN")
            End If


        End If

        If Server IsNot Nothing Then
            System.Runtime.InteropServices.Marshal.ReleaseComObject(Server)
            Server = Nothing
        End If


        Connected = False
        Settings.SetBy("open_server", "1")
    End Sub

    Public Shared Function GetAppName(Strval As String) As String
        ' Try
        ' This utility function extracts the application name from the tag string
        Dim Pos
        Pos = InStr(Strval, ".")
        If Pos < 2 Then
            Throw New Exception(Strval & "Badly formed tag string")
        End If
        GetAppName = Left(Strval, Pos - 1)
        If StrComp(GetAppName, "PROSPER", 1) <> 0 And StrComp(GetAppName, "MBAL", 1) <> 0 And StrComp(GetAppName, "GAP", 1) <> 0 And StrComp(GetAppName, "PVT", 1) <> 0 Then
            Throw New Exception("GetAppName: Unrecognised application name in tag string: " & Strval)
        End If


        Return GetAppName


    End Function

    Public Shared Sub DoCmd(Cmd As String)
        ' Perform a command, then check for errors
        Try
            Dim lErr As Long = 0
            lErr = Server.DoCommand(Cmd)
            If lErr > 0 Then
                If Settings.GetBy("show_messages") = "0" Then
                    Throw New Exception("DoCmd: " & Cmd & "   " & Server.GetErrorDescription(lErr))
                Else

                    MsgBox("DoCmd: " & Cmd & "   " & Server.GetErrorDescription(lErr))
                    ' Server = Nothing
                End If



            End If
        Catch ex As Exception

            Throw New Exception(ex.Message)


        End Try
    End Sub

    Public Shared Sub DoSet(Sv As String, Val As String) 'Public Shared Sub DoSet(Sv As String, Val As String)
        Try
            ' Set a value, then check for errors
            Dim lErr As Long
            lErr = Server.SetValue(Sv, Val)
            AppName = GetAppName(Sv)
            lErr = Server.GetLastError(AppName)
            If lErr > 0 Then

                If Settings.GetBy("show_messages") = "1" Then
                    MsgBox("DoSet: " & Sv & "   " & Val & "   " & Server.GetErrorDescription(lErr))
                    'Server = Nothing
                Else
                    Throw New Exception("DoSet: " & Sv & "   " & Val & "   " & Server.GetErrorDescription(lErr))
                End If


                'End
            End If
        Catch ex As Exception
            'Server = Nothing
            Throw New Exception(ex.Message)
        End Try
    End Sub

    Public Shared Function DoGet(Gv As String) As String 'Public Shared Function DoGet(Gv As String) As String
        Try
            ' Get a value, then check for errors
            Dim lErr As Long
            DoGet = Server.GetValue(Gv)
            AppName = GetAppName(Gv)
            lErr = Server.GetLastError(AppName)
            If lErr > 0 Then
                If Settings.GetBy("show_messages") = "1" Then
                    MsgBox("DoGet: " & Gv & "   " & Server.GetLastErrorMessage(AppName))
                    Server = Nothing
                Else
                    Throw New Exception("DoGet: " & Gv & "   " & Server.GetLastErrorMessage(AppName))
                End If

                'End
            End If
        Catch ex As Exception
            'Server = Nothing
            'If Settings.GetBy("enviroment") = "production" Then
            Throw New Exception(ex.Message)
            'Else
            'MsgBox("DoGet: " & ex.ToString)
            'End If

        End Try
    End Function

    Public Shared Sub DoSlowCmd(Cmd As String) 'Public Shared Sub DoSlowCmd(Cmd As String)
        Try
            ' Perform a command, then wait for the command to exit
            ' Then check for errors
            Dim starttime As Single
            Dim endtime As Single
            Dim CurrentTime As Single
            Dim lErr As Long
            Dim bLoop As Boolean
            Dim steps As Single
            Dim Time As Long

            Time = 0
            steps = 0.001
            AppName = GetAppName(Cmd)
            lErr = Server.DoCommandAsync(Cmd)
            If lErr > 0 Then
                Throw New Exception("DoSlowCmd: " & Cmd & "   " & Server.GetErrorDescription(lErr))
                Disconnect()
                'End
            End If
            While Server.IsBusy(AppName) > 0
                If steps < 2 Then
                    steps = steps * 2
                End If
                starttime = Time ' Timer
                endtime = starttime + steps
                Do
                    CurrentTime = Time 'Timer
                    ' DoEvents() 'to update the excel Form
                    bLoop = True
                    ' Check first for the case where we have gone over midnight
                    ' and the number of seconds will go back to zero
                    If CurrentTime < starttime Then
                        bLoop = False
                        ' Now check for the 2 second pause finishing
                    ElseIf CurrentTime > endtime Then
                        bLoop = False
                    End If
                Loop While bLoop
            End While
            AppName = GetAppName(Cmd)
            lErr = Server.GetLastError(AppName)
            If lErr > 0 Then
                Throw New Exception("DoSlowCmd: " & Cmd & "  " & Server.GetErrorDescription(lErr))
                Disconnect()
                'End
            End If
        Catch ex As Exception
            Throw New Exception("DoSlowCmd: " & ex.ToString)
        End Try
    End Sub

    Public Shared Function DoGAPFunc(Gv As String) As String 'Public Shared Function DoGAPFunc(Gv As String) As String
        Try
            ' Perform a function in GAP, then retrieve return value
            ' Finally, check for errors DoSlowCmd Gv
            Dim lErr As Long
            DoGAPFunc = DoGet("GAP.LASTCMDRET")
            lErr = Server.GetLastError("GAP")
            If lErr > 0 Then
                Throw New Exception("DoGAPFunc: " & Gv & "   " & Server.GetErrorDescription(lErr))
                'End
            End If
        Catch ex As Exception
            Server = Nothing
            Throw New Exception("DoGAPFunc: " & Gv & "   " & ex.ToString)
        End Try
    End Function






End Class