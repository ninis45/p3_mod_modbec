Imports ModeloCI
Imports UsrControl.Generales
Public Class AgujeroModel

    Public Property Pozo As String
    Public Property numero As Integer
    Public Property id_agujero As String
    Public Property id_pozo As String
    Public Property id_mod_pozo As String
    Public Property lift_method As Integer

    Public Property IdAgujero As String
    Public Property IdModPozo As String
    Public Property LiftMethod As Integer
    Public Property Estatus As Integer
    Public Property Configuracion As CONFIGURACION_ADMINISTRADOR
    Public Property VWGeneral As VW_EDO_GENERAL


    Public Property Trayectorias As List(Of VW_TRAYECTORIA)
    Public Property Mecanicos As List(Of VW_EDO_MECANICO)
    Public Property FechaPrueba As String
    Public Property CreatedOn As DateTime


    Property DelTemps As Boolean = False
    Property DelTrays As Boolean = False
    Property DelMecas As Boolean = False

    Private db As Entities_ModeloCI



    ' Public db As CIEntities
    Sub New(ByVal db As Entities_ModeloCI, ByVal IdAgujero As String, ByVal FechaPrueba As String)

        Me.db = db
        Me.FechaPrueba = FechaPrueba
        Me.Estatus = 0

        Dim result = db.VW_AGUJEROS.Where(Function(w) w.IDAGUJERO = IdAgujero).SingleOrDefault() '  (From ag In db.AGUJERO Join po In db.POZO On ag.IDPOZO Equals po.IDPOZO Where ag.IDAGUJERO = id_agujero).SingleOrDefault()

        If result Is Nothing Then
            Throw New Exception("No existe el agujero")
        End If

        Dim method = db.CAT_SAP.Find(result.IDCATSAP)
        Me.IdAgujero = result.IDAGUJERO
        Me.Pozo = result.NOMBRE
        Me.Mecanicos = db.VW_EDO_MECANICO.Where(Function(w) w.IDAGUJERO = IdAgujero).ToList()
        Me.Trayectorias = db.VW_TRAYECTORIA.Where(Function(w) w.IDAGUJERO = IdAgujero).OrderBy(Function(o) o.PROFUNDIDADMD).ToList()


        If method IsNot Nothing Then
            Me.LiftMethod = method.PROSPER
        Else
            Throw New Exception("No existe el Lift Method")
            Me.LiftMethod = 0
        End If


        Dim general = (From edo_general In db.VW_EDO_GENERAL Where edo_general.IDAGUJERO = IdAgujero And edo_general.FECHAMODELO = (db.MOD_POZO.Where(Function(w) w.IDAGUJERO = IdAgujero).Max(Function(m) m.FECHAMODELO))).SingleOrDefault() ' db.VW_EDO_GENERAL.Where(Function(w) w.IDAGUJERO = IdAgujero).SingleOrDefault()

        If general IsNot Nothing Then
            ReloadGeneral(general.IDMODPOZO, general.ESTATUS.GetValueOrDefault())
            Me.CreatedOn = general.FECHAMODELO
            VWGeneral = general
        End If


    End Sub
    Sub ReloadGeneral(ByVal IdModPozo As String, ByVal Estatus As Integer)
        Me.IdModPozo = IdModPozo
        Me.Estatus = Estatus
        Me.Configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
    End Sub

    ''' <summary>
    ''' Solo se obtiene datos básicos del Agujero sin tomar en cuenta las del modelo
    ''' </summary>
    Function SaveGeneral(ByVal IdModPozo As String, ByVal ModGeneral As MOD_POZO_GENERAL)
        Try
            Dim Exists = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            If Exists Is Nothing Then


                db.MOD_POZO_GENERAL.Add(ModGeneral)
            Else
                db.Entry(ModGeneral).State = System.Data.Entity.EntityState.Modified
            End If

            db.SaveChanges()
            Return True
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Function



    ''' <summary>
    ''' Genera el arreglo de las trayectorias en un arreglo de dos columnas y regresa los puntos para la temperatura.
    ''' </summary>
    Function SaveTrayectorias(ByVal max_md As Double) As Double(,)
        Dim result(17, 1) As Double

        Try

            Dim mod_trayectorias = db.MOD_POZO_TRAYEC.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()

            If DelTrays Then
                mod_trayectorias.ForEach(Function(e) db.MOD_POZO_TRAYEC.Remove(e))
                db.SaveChanges()
                mod_trayectorias = New List(Of MOD_POZO_TRAYEC)()

            End If


            If mod_trayectorias.Count = 0 Then
                Dim mds(Trayectorias.Count - 1), mvs(Trayectorias.Count - 1) As Double
                Dim tmp_trayectorias As New List(Of VW_TRAYECTORIA)







                If Trayectorias.Count > 0 Then
                    'Indice 0 obligatorio inicialiar PMD a 0
                    result(0, 0) = Trayectorias(0).PROFUNDIDADMD 'dt.Rows(0).Item(0)
                    result(0, 1) = Trayectorias(0).PROFMV 'dt.Rows(0).Item(1)


                    Dim finMD As Double = max_md



                    For i = 0 To Trayectorias.Count - 1
                        mds(i) = Trayectorias(i).PROFUNDIDADMD
                        mvs(i) = Trayectorias(i).PROFMV

                        If Trayectorias(i).PROFUNDIDADMD <= max_md Then
                            tmp_trayectorias.Add(Trayectorias(i))
                        End If

                    Next i

                    Dim finMV As Double = Analisis.InterpolarProfundidadesVertical(mds, mvs, max_md)

                    tmp_trayectorias = tmp_trayectorias.OrderBy(Function(o) o.SEVERIDAD).ToList()


                    Dim indice As Integer = 1 '2
                    Dim j As Integer = tmp_trayectorias.Count - 1
                    Do While (indice < 17)
                        result(indice, 0) = tmp_trayectorias(j).PROFUNDIDADMD 'View.Item(i).Item(0)
                        result(indice, 1) = tmp_trayectorias(j).PROFMV 'View.Item(i).Item(1)
                        indice += 1
                        j -= 1
                    Loop

                    ReDim Preserve result(indice, 1)
                    result(indice, 0) = finMD
                    result(indice, 1) = finMV

                    Analisis.ordenarMatriz(result, 0)
                End If



                For i = 0 To result.GetUpperBound(0)
                    Dim insert_tray As New MOD_POZO_TRAYEC() With {
                        .IDMODPOZOTRAYEC = Guid.NewGuid().ToString().ToUpper(),
                        .IDMODPOZO = IdModPozo,
                        .PROFUNDIDADMD = result(i, 0),
                        .PROFUNDIDADMV = result(i, 1)
                    }


                    db.MOD_POZO_TRAYEC.Add(insert_tray)

                Next i

                db.SaveChanges()
            Else


                'Indice 0 obligatorio inicialiar PMD a 0
                For i = 0 To mod_trayectorias.Count - 1
                    result(i, 0) = mod_trayectorias(i).PROFUNDIDADMD 'dt.Rows(0).Item(0)
                    result(i, 1) = mod_trayectorias(i).PROFUNDIDADMV 'dt.Rows(0).Item(1)
                Next

            End If


            Return result


        Catch ex As Exception
            Return result
        End Try
    End Function
    ''' <summary>
    ''' Guarda estado mecanico
    ''' </summary>
    Function SaveMecanico() As Integer

        Try
            'Eliminamos tuberias
            Dim mecanicos = db.MOD_POZO_TUBERIA.Where(Function(w) w.IDAGUJERO = IdAgujero).ToList()

            If DelMecas AndAlso mecanicos.Count > 0 Then
                mecanicos.ForEach(Function(e) db.MOD_POZO_TUBERIA.Remove(e))
                db.SaveChanges()
                mecanicos = New List(Of MOD_POZO_TUBERIA)()
            End If


            If mecanicos.Count = 0 Then
                Dim mecanico As New Mecanico(db, IdAgujero)

                Dim list_mecanicos = mecanico.GetList()

                If list_mecanicos.Count = 0 Then
                    Throw New Exception("No hay estado mecánico")
                End If

                Dim num As Integer = 0
                For Each tub In list_mecanicos
                    db.MOD_POZO_TUBERIA.Add(New MOD_POZO_TUBERIA() With {
                        .IDMODPOZOTUBERIA = Guid.NewGuid.ToString().ToUpper(),
                        .IDAGUJERO = IdAgujero,
                        .IDTIPOTUBERIA = tub.IDTIPOTUBERIA,
                        .ETIQUETA = tub.ETIQUETA,
                        .CIDIAM = tub.CIDIAM,
                        .CIROUG = tub.CIROUG,
                        .MD = tub.MD,
                        .TIDIAM = tub.TIDIAM,
                        .TODIAM = tub.TODIAM,
                        .TIROUG = tub.TIROUG,
                        .TOROUG = tub.TOROUG
                    })

                    num += 1
                Next
                db.SaveChanges()

                Return list_mecanicos.Count
            Else
                Return mecanicos.Count
            End If


        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Function
    Function SaveModelo() As Boolean
        Try
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function
End Class
