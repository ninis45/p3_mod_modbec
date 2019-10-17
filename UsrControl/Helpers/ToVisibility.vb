Imports System.Globalization

Public Class ToVisibility
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.Convert

        'Dim is_integer As Boolean = False
        'Int32.Parse(value, is_integer)

        If value IsNot Nothing AndAlso value = parameter Then


            Return "Visibility"



        End If

        Return "Collapsed"


    End Function

    Public Function ConvertBack(value As Object, targetType As Type, parameter As Object, culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotImplementedException()
    End Function
End Class
