Public Class ConfigView

    'Dim Context As New ContextViewModel()
    Sub New()

        ' Esta llamada es exigida por el diseñador.
        InitializeComponent()
        ' Me.DataContext = Context

        ' Context.LoadConfig()
        ' Agregue cualquier inicialización después de la llamada a InitializeComponent().

    End Sub
    Private Sub CloseConfig(sender As Object, e As RoutedEventArgs)
        Me.Close()

    End Sub
End Class
