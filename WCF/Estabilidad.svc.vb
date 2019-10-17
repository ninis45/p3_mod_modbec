' NOTA: puede usar el comando "Cambiar nombre" del menú contextual para cambiar el nombre de clase "Estabilidad" en el código, en svc y en el archivo de configuración a la vez.
' NOTA: para iniciar el Cliente de prueba WCF para probar este servicio, seleccione Estabilidad.svc o Estabilidad.svc.vb en el Explorador de soluciones e inicie la depuración.
Imports ModeloCI
Public Class Estabilidad
    Implements IEstabilidad
    Private _recomendacion As String

    Public Sub DoWork() Implements IEstabilidad.DoWork
        Dim db As New Entities_ModeloCI()

        Dim Obj As New ModeloProsper.Estabilidad(db.CONFIGURACION.Find("18158581-9a78-44ab-a5b9-537e1f29fd9d"))

        Dim Rec = Obj.Recomendacion

    End Sub


    'Public Function Pozo(ByVal IdConfiguracion As String) As List(Of ModeloProsper.Clases.EstabilidadDePozos.Parametros_Estabilidad) Implements IEstabilidad.Pozo
    '    Dim db As New Entities_ModeloCI()

    '    Dim ObjEstabilidad As New ModeloProsper.Estabilidad(db.CONFIGURACION.Find(IdConfiguracion))

    '    Return ObjEstabilidad.Pozo()

    '    ''SI TIENE ESTABILIDAD PROGRAMADA
    '    'If mod_pozo.ESTABILIDAD > 0 Then
    '    '    Dim Configuracion = (From config In db.CONFIGURACION Where config.IDMODPOZO = IdModPozo And config.Fecha = (db.CONFIGURACION.Where(Function(w) w.IDMODPOZO = IdModPozo).Max(Function(m) m.Fecha))).SingleOrDefault()

    '    '    If Configuracion IsNot Nothing Then
    '    '        Dim ObjEstabilidad As New Estabilidad(Configuracion)

    '    '        Select Case Configuracion.METODOLOGIA
    '    '            Case 0
    '    '                Dim Resultados = ObjEstabilidad.Mapa()

    '    '                If Resultados.Count > 0 Then
    '    '                    For Each item In Resultados
    '    '                        db.RESULTADOS.Add(New RESULTADOS With {.Ind_Est = item.Ind_Est, .Ptr = item.Ptr, .DiaVal = item.DiaVal, .Dp = item.Dp, .Dpvalve = item.Dpvalve, .Dvalve = item.Dvalve, .GOR = item.GOR, .QI = item.Ql, .Qg = item.Qg, .Pwf_IPR = item.Pwf_IPR, .Twh = item.Twh, .TotalQGas = item.TotalQgas, .Pwf_quicklook = item.Pwf_quicklook, .Pws = item.Pws, .Pti = item.Pti, .Ptri = item.Ptri, .Tvalv = item.Tvalv, .GOR_quicklool = item.GOR_quicklook, .GORFREE = item.GORFREE, .Ptrcalc = item.Ptrcalc, .PI = item.PI, .Qgcrit = item.Qgcrit, .Qcporcent = item.Qcporcent, .HTC = item.HTC, .Pwh = item.Pwh, .Qgi = item.Qgi, .Wc = item.Wc})
    '    '                    Next
    '    '                    db.SaveChanges()
    '    '                End If
    '    '            Case 1
    '    '                Dim Resultados = ObjEstabilidad.Pozo()
    '    '                If Resultados.Count > 0 Then
    '    '                    For Each item In Resultados
    '    '                        db.RESULTADOS.Add(New RESULTADOS With {.Ind_Est = item.Ind_Est, .Ptr = item.Ptr, .DiaVal = item.DiaVal, .Dp = item.Dp, .Dpvalve = item.Dpvalve, .Dvalve = item.Dvalve, .GOR = item.GOR, .QI = item.Ql, .Qg = item.Qg, .Pwf_IPR = item.Pwf_IPR, .Twh = item.Twh, .TotalQGas = item.TotalQgas, .Pwf_quicklook = item.Pwf_quicklook, .Pws = item.Pws, .Pti = item.Pti, .Ptri = item.Ptri, .Tvalv = item.Tvalv, .GOR_quicklool = item.GOR_quicklook, .GORFREE = item.GORFREE, .Ptrcalc = item.Ptrcalc, .PI = item.PI, .Qgcrit = item.Qgcrit, .Qcporcent = item.Qcporcent, .HTC = item.HTC, .Pwh = item.Pwh, .Qgi = item.Qgi, .Wc = item.Wc})
    '    '                    Next
    '    '                    db.SaveChanges()
    '    '                End If
    '    '        End Select

    '    '        Configuracion.RECOMENDACIONES = ObjEstabilidad.Recomendacion
    '    '        db.Entry(Configuracion).State = Entity.EntityState.Modified
    '    '        db.SaveChanges()
    '    '    End If


    '    ' End If
    'End Function

    Public Function Pozo(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of ModeloProsper.Clases.EstabilidadDePozos.Parametros_Estabilidad) Implements IEstabilidad.Pozo




        Dim objpozo As ModeloProsper.Clases.EstabilidadDePozos = New ModeloProsper.Clases.EstabilidadDePozos(Ptr_min, Ptr_max, Ptr_num, Pwh_Min, Pwh_Max, Pwh_num, Qgi_min, Qgi_max, Qgi_num, Dvalve_min, Dvalve_max, Dvalve_num, DiaVal_min, DiaVal_max, DiaVal_num, Wc_min, Wc_max, Wc_num, Cd, Ptr_Act, Pwh_Act, Qgi_Act, Dvalve_Act, DiaValve_Act, Wc_Act, Caso)

        Try

            objpozo.Principal()

            _recomendacion = objpozo.recomendaciones
            Return objpozo.ListaParametrosEstabilidad
        Catch ex As Exception


            Throw New Exception(ex.Message)
        End Try



    End Function


    Public Function Mapa(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of ModeloProsper.Clases.MapaEstabilidad.Parametros_Estabilidad) Implements IEstabilidad.Mapa
        Dim objmapa As ModeloProsper.Clases.MapaEstabilidad = New ModeloProsper.Clases.MapaEstabilidad(Ptr_min, Ptr_max, Ptr_num, Pwh_Min, Pwh_Max, Pwh_num, Qgi_min, Qgi_max, Qgi_num, Dvalve_min, Dvalve_max, Dvalve_num, DiaVal_min, DiaVal_max, DiaVal_num, Wc_min, Wc_max, Wc_num, Cd, Ptr_Act, Pwh_Act, Qgi_Act, Dvalve_Act, DiaValve_Act, Wc_Act, Caso)
        Try
            objmapa.GENERAL()

            _recomendacion = objmapa.recomendaciones
            Return objmapa.ListaParametrosEstabilidad
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function

    Public Function Recomendacion() As String Implements IEstabilidad.Recomendacion

        Return _recomendacion
    End Function
    'Revisar proximamente talvez esta mala podria marcar error por exceder el tiempo de ejecucion
    Public Function Execute(ByVal IdConfiguracion As String) As Boolean Implements IEstabilidad.Execute
        Try
            Dim db As New Entities_ModeloCI()
            Dim ObjEstabilidad As ModeloProsper.Estabilidad
            Dim Config As CONFIGURACION = db.CONFIGURACION.Where(Function(w) w.IDDATOSENTRADAEST = IdConfiguracion).SingleOrDefault()

            If Config Is Nothing Then
                Throw New Exception("La configuracion no existe o fue borrada")
            End If

            If Config.ESTATUS <> 1 Then
                Throw New Exception("No es posible ejecutar: Estado " + Config.ESTATUS)
            End If

            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If

            ObjEstabilidad = New ModeloProsper.Estabilidad(Config)

            If ObjEstabilidad.Execute() Then

            End If




            Return True
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Function
End Class
