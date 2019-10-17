Public Class StringValue


    Inherits Attribute

    Private ReadOnly _value As String

        Public Sub New(value As String)

            _value = value

        End Sub

        Public ReadOnly Property Value() As String

            Get

                Return _value

            End Get

        End Property


End Class
