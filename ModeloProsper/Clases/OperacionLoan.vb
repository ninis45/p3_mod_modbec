Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary

Public Class OperacionLoan

    Public Function leerLoan() As Loan
        Dim _loan As New Loan
        If File.Exists("CreaModeloProsper.bin") Then
            Dim myFileStream As Stream = File.OpenRead("CreaModeloProsper.bin")
            Dim deserializer As New BinaryFormatter()
            _loan = CType(deserializer.Deserialize(myFileStream), Loan)
            myFileStream.Close()
        End If
        Return _loan
    End Function

    Public Sub guardarLoan(ByVal _load As Loan)
        Dim myFileStream As Stream = File.Create("CreaModeloProsper.bin")
        Dim serializer As New BinaryFormatter()
        serializer.Serialize(myFileStream, _load)
        myFileStream.Close()
    End Sub

End Class