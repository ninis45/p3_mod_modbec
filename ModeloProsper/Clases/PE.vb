Imports ModeloCI
Public Class PE

    Sub New()

    End Sub

    Sub DoClose()

    End Sub
    Shared Function CreateObject(ByVal File As String) As Object
        Try
            Dim bd As New Entities_ModeloCI()


            Return Interaction.CreateObject(File)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function


End Class
