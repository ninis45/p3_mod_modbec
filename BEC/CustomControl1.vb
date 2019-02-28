Public Class CustomControl1
    Inherits Control

    Shared Sub New()
        'Esta llamada a OverrideMetadata indica al sistema que este elemento desea proporcionar un estilo diferente al de su clase base.
        'Este estilo está definido en Themes\Generic.xaml
        DefaultStyleKeyProperty.OverrideMetadata(GetType(CustomControl1), New FrameworkPropertyMetadata(GetType(CustomControl1)))
    End Sub

End Class
