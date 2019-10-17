Imports System
Imports System.IO
Imports System.Text
Imports System.Collections
Imports ModeloProsper.BaseDatos.BD

Public Class Config

    Class Cnf

        Shared directorio As String = ""
        Shared _ArchConfig As String = "simodi.dat"
        Shared result As String = ""
        'Shared OpenFileDialog1 As New OpenFileDialog

        Shared Property Resultado() As String
            Get
                Return result
            End Get
            Set(value As String)
                result = value
            End Set
        End Property

        Shared Property ArchConfig() As String
            Get
                Return _ArchConfig
            End Get
            Set(value As String)
                _ArchConfig = value
            End Set
        End Property

        Shared Sub readFile()
            Dim path As String = My.Computer.FileSystem.CurrentDirectory & "\" & ArchConfig
            Dim bytes = My.Computer.FileSystem.ReadAllBytes(path)
            Resultado = System.Text.Encoding.Unicode.GetString(bytes)
        End Sub

        Shared Sub getFile(tipo As String)
            Dim myStream As Stream = Nothing
            'OpenFileDialog1.Reset()
            'OpenFileDialog1.InitialDirectory = directorio
            'OpenFileDialog1.Filter = tipo
            'OpenFileDialog1.FilterIndex = 0
            'OpenFileDialog1.RestoreDirectory = True
            'If (OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) Then
            '    Try
            '        myStream = OpenFileDialog1.OpenFile
            '        If (myStream IsNot Nothing) Then
            '            Resultado = OpenFileDialog1.FileName
            '            directorio = OpenFileDialog1.FileName.Replace(OpenFileDialog1.SafeFileName, "")
            '        End If
            '    Catch ex As Exception
            '        MsgBox(ex.ToString)
            '    End Try
            'End If
        End Sub

    End Class
End Class