Public Class Debugger
    Sub New()

    End Sub
    Shared Function GetLine(ByVal ex As Exception) As Integer

        Dim st = New StackTrace(ex, True)

        Dim frame = st.GetFrame(0)

        Dim line = frame.GetFileLineNumber()

        Return line
    End Function


End Class
