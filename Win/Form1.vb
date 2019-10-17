Public Class Form1
    Public window As UsrControl.MainWindow
    Sub New()

        ' Esta llamada es exigida por el diseñador.
        InitializeComponent()

        ' Agregue cualquier inicialización después de la llamada a InitializeComponent().
        window = New UsrControl.MainWindow("428123F6-BC47-4E2D-932A-DEB3A3B576C7", "2019-02-11")

        TextBox1.Text = "0D5DC5B6-C338-47B4-B8A0-0DA1DAA63D4E"

        ElementHost1.Child = window


        Dim Lista(3) As Double

        Lista(0) = 1
        Lista(1) = 0
        Lista(2) = 2
        Lista(3) = 1.1

        Array.Sort(Lista)

        Dim l = Lista

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim Servicio As New WCFService.ModeloClient()

        Dim result = Servicio.SensGas(TextBox1.Text, 8.4, 17.5, 0)
        'Dim modelo As ModeloProsper.Modelo = New ModeloProsper.Modelo()
        'modelo.SensGas(TextBox1.Text, 15, 11, 0, 10)
        '' modelo.Execute(TextBox1.Text)
        ''window.actualizar(TextBox1.Text, "2019-02-11")


    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim modelo As ModeloProsper.Modelo = New ModeloProsper.Modelo(TextBox1.Text)

        modelo.Reset(TextBox1.Text, 1)


    End Sub
End Class
