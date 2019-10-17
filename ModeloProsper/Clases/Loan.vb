Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary

<Serializable()> Public Class Loan

    Private _rutaDB As String = ""
    Public Property RutaBD() As String
        Get
            Return _rutaDB
        End Get
        Set(value As String)
            _rutaDB = value
        End Set
    End Property

    Private _rutaPropesper As String = ""
    Public Property RutaProsper()
        Get
            Return _rutaPropesper
        End Get
        Set(value)
            _rutaPropesper = value
        End Set
    End Property

End Class
