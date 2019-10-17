Imports ModeloCI

Public Class Logger
    Private db As New Entities_ModeloCI()
    Property Configuracion As New CONFIGURACION_ADMINISTRADOR()
    Private Estatus As Integer
    Property Intentos As Integer
    Sub New(ByVal IdModPozo As String, ByVal IdUsuario As String)

        'Remover esta de manera temporal
        If IdUsuario Is Nothing Then
            IdUsuario = "69A2512F-AA2D-4F0E-8045-0430B4093E05"
        End If


        Me.Configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        If Me.Configuracion Is Nothing Then
            Me.Configuracion = New CONFIGURACION_ADMINISTRADOR() With {
                .ESTATUS = 1,
                .MAXREINTENTOS = 1,
                .IDMODPOZO = IdModPozo,
                .IDCONFIGURACION = Guid.NewGuid().ToString().ToUpper(),
                .FECHA_PROGRAMACION = DateTime.Now,
                .FECHA_REGLA = DateTime.Now,
                .IDUSUARIO = IdUsuario
            }

            db.CONFIGURACION_ADMINISTRADOR.Add(Me.Configuracion)
            db.SaveChanges()
        End If


        Intentos = db.EJECUCION_PROCESOS.Where(Function(w) w.IDCONFIGURACION = Configuracion.IDCONFIGURACION And w.IDCONFIGURACION = Configuracion.IDCONFIGURACION).Count()

        'If (Intentos + 1) > Configuracion.MAXREINTENTOS Then

        '    If Configuracion.ESTATUS = 2 Then
        '        SetEstatus(0)

        '    End If

        '    Throw New Exception("Maximo de intentos permitidos:" & Configuracion.MAXREINTENTOS.ToString())
        'End If

        'If Configuracion.ESTATUS = 3 Or Configuracion.ESTATUS = 0 Then
        '    Throw New Exception("Este modelo ya no se puede ejecutar por estar finalizada")
        'End If


    End Sub
    Sub SetLog(ByVal Message As String, ByVal Inicio As DateTime)
        db.EJECUCION_PROCESOS.Add(New EJECUCION_PROCESOS() With {
                   .ERRORS = Message,
                   .FECHA_INICIO = Inicio,
                   .FECHA_FINAL = DateTime.Now,
                   .IDEJECUCION = Guid.NewGuid().ToString().ToUpper(),
                   .IDCONFIGURACION = Configuracion.IDCONFIGURACION
        })

        db.SaveChanges()



    End Sub
    Sub SetEstatus(ByVal Estatus As Integer)
        If Me.Configuracion.ESTATUS = 1 Or Me.Configuracion.ESTATUS = 2 Then
            Me.Configuracion.ESTATUS = Estatus
            db.Entry(Me.Configuracion).State = Entity.EntityState.Modified
            db.SaveChanges()
        End If

    End Sub
End Class
