Imports System.IdentityModel.Selectors
Imports System.IdentityModel.Tokens
Imports ModeloCI

Public Class BasicAuthValidate
    Inherits UserNamePasswordValidator

    Public Overrides Sub Validate(ByVal UserName As String, ByVal UserPass As String)
        Dim db As New Entities_ModeloCI()
        'Dim log As New Logging.Log("Auth")
        If UserName = "" Or UserPass = "" Then
            Throw New ArgumentNullException("Usuario o contraseña no puede estar vacio")
        End If

        Dim Estatus = db.SP_LOGIN(UserName, UserPass).SingleOrDefault()

        Select Case Estatus
            Case 0
            Case 1
                Throw New FaultException("Usuario y/o contraseña no válida")
            Case 2
            Case Else
                Throw New FaultException("No existe el estatus del usuario")

        End Select

        ' log.WriteEntry("Usuario autenticado: " + UserName + ", fecha: " + DateTime.Now)


        'Dim estatus = (UserName, UserPass)


        'Throw New SecurityTokenException("Usuario inválido")
        'Return
    End Sub

End Class
