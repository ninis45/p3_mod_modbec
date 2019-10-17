Class MainWindow

    Dim Context As ContextViewModel


    Sub New(ByVal IdAgujero As String, ByVal Fecha As String)

        ' Esta llamada es exigida por el diseñador.
        InitializeComponent()

        ' Agregue cualquier inicialización después de la llamada a InitializeComponent().
        hstProductividad.Child = New grfProductividad()

        Context = New ContextViewModel(IdAgujero, Fecha)
        'Context.IdAgujero = IdAgujero
        'Context.FechaPrueba = Fecha

        Me.DataContext = Context

        actualizar(IdAgujero, Fecha)


    End Sub
    Public Sub actualizar(ByVal IdAgujero As String, ByVal Fecha As String)

        If Context Is Nothing Then
            Context = New ContextViewModel(IdAgujero, Fecha)
        Else
            Context.IdAgujero = IdAgujero
            Context.FechaPrueba = Fecha

        End If

        Context.Initialize(hstProductividad, hstCorrelacion, hstVpl, hstGas, hstDiag, hstWc)
    End Sub

    Private Sub ShowConfig(sender As Object, e As RoutedEventArgs)
        Dim ConfigView As New ConfigView()

        ConfigView.DataContext = Context



        ConfigView.ShowDialog()



    End Sub

    Private Sub ShowPozo(sender As Object, e As RoutedEventArgs)
        Dim PozoView As New PozoView()

        PozoView.DataContext = Context
        PozoView.ShowDialog()
    End Sub
End Class
