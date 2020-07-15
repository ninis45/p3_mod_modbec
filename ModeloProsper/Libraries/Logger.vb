Imports ModeloCI

Public Class Logger
    Private db As New Entities_ModeloCI()
    Property Configuracion As New CONFIGURACION_ADMINISTRADOR()
    Private Estatus As Integer
    Property Intentos As Integer
    Property Inicio As DateTime
    Sub New(ByVal IdModPozo As String, Optional ByVal IdUsuario As String = "69A2512F-AA2D-4F0E-8045-0430B4093E05")

        'Remover esta de manera temporal

        Inicio = DateTime.Now

        Me.Configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        If Me.Configuracion Is Nothing Then
            Dim Usuario As USUARIO = db.USUARIO.Find(IdUsuario)


            If Usuario Is Nothing Then
                Throw New Exception("El usuario no existe, favor de verificar datos.")
            End If

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


        Intentos = db.EJECUCION_PROCESOS.Where(Function(w) w.IDCONFIGURACION = Configuracion.IDCONFIGURACION And w.ENDRECORD Is Nothing).Count()






    End Sub

    Sub SetLog(ByVal Estatus As Integer, ByVal Message As String)

        If Intentos < Configuracion.MAXREINTENTOS Then
            ''Throw New Exception("Máximo intentos permitidos: " + Configuracion.MAXREINTENTOS)
            db.EJECUCION_PROCESOS.Add(New EJECUCION_PROCESOS() With {
                   .ESTATUS = Estatus,
                   .ERRORS = Message,
                   .FECHA_INICIO = Inicio,
                   .FECHA_FINAL = DateTime.Now,
                   .IDEJECUCION = Guid.NewGuid().ToString().ToUpper(),
                   .IDCONFIGURACION = Configuracion.IDCONFIGURACION
            })

            db.SaveChanges()

            Intentos += 1
        End If



    End Sub
    Sub SetEstatus(ByVal Estatus As Integer)

        If Intentos + 1 > Me.Configuracion.MAXREINTENTOS Then
            If Me.Configuracion.ESTATUS = 1 Then
                Estatus = -1
                Me.Configuracion.ESTATUS = Estatus
                db.Entry(Me.Configuracion).State = Entity.EntityState.Modified
                db.SaveChanges()
            End If


            Throw New Exception("Numero de intentos permitidos: " + Me.Configuracion.MAXREINTENTOS.ToString())
        End If


        Me.Configuracion.ESTATUS = Estatus
        db.Entry(Me.Configuracion).State = Entity.EntityState.Modified
        db.SaveChanges()


    End Sub
    Sub SetEstatus(ByVal Estatus As Integer, ByVal Message As String)


        SetLog(Estatus, Message)

        'If Me.Configuracion.ESTATUS = 1 Or Me.Configuracion.ESTATUS = 2 Then
        Me.Configuracion.ESTATUS = Estatus
        db.Entry(Me.Configuracion).State = Entity.EntityState.Modified
        db.SaveChanges()
        ' End If

    End Sub
End Class
