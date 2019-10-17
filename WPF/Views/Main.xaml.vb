Class MainWindow

    Dim Context As New ContextViewModel()
    Sub New()

        ' Esta llamada es exigida por el diseñador.
        InitializeComponent()

        ' Agregue cualquier inicialización después de la llamada a InitializeComponent().
        hstProductividad.Child = New grfProductividad()

        Me.DataContext = Context

        Context.Initialize(hstProductividad, hstCorrelacion, hstVpl, hstGas, hstDiag, hstWc)


    End Sub

    Private Sub ShowConfig(sender As Object, e As RoutedEventArgs)
        Dim ConfigView As New ConfigView()

        ConfigView.DataContext = Context

        'Context.LoadConfig()

        ConfigView.ShowDialog()



    End Sub

    Private Sub ShowPozo(sender As Object, e As RoutedEventArgs)
        Dim PozoView As New PozoView()

        PozoView.DataContext = Context
        PozoView.ShowDialog()
    End Sub
End Class
