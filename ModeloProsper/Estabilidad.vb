Imports ModeloCI
Public Class Estabilidad
    Public Property Recomendacion As String
    Private Caso As Integer
    Private db As New Entities_ModeloCI()
    Private Configuracion As CONFIGURACION
    Public Mapa As List(Of Clases.MapaEstabilidad.Parametros_Estabilidad)
    Public Ql As Double

    Sub New(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal Diam_min As Double, ByVal Diam_max As Double, ByVal Diam_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_ope As Double, ByVal Pwh_ope As Double, ByVal Qgi_ope As Double, ByVal Dvalve_ope As Double, Diam_ope As Double, Wc_ope As Double, ByVal Caso As Integer)
        Configuracion = New CONFIGURACION() With {
            .Ptr_Min = Ptr_min,
            .Ptr_Max = Ptr_max,
            .Ptr_Num = Ptr_num,
            .Pwh_Min = Pwh_Min,
            .Pwh_Max = Pwh_Max,
            .Pwh_Num = Pwh_num,
            .Qgi_Min = Qgi_min,
            .Qgi_Max = Qgi_max,
            .Qgi_Num = Qgi_num,
            .Prof_Valv_Min = Dvalve_min,
            .Prof_Valv_Max = Dvalve_max,
            .Prof_Valv_Num = Dvalve_num,
            .Diam_Orif_Min = Diam_min,
            .Diam_Orif_Max = Diam_max,
            .Diam_Orif_Num = Diam_num,
            .Wc_Min = Wc_min,
            .Wc_Max = Wc_max,
            .Wc_Num = Wc_num,
            .CD = Cd,
            .Ptr_Op = Ptr_ope,
            .Pwh_Op = Pwh_ope,
            .Qgi_Op = Qgi_ope,
            .Prof_Valv_Op = Dvalve_ope,
            .Diam_Orif_Op = Diam_ope,
            .Wc_Op = Wc_ope
        }
        Me.Caso = Caso
    End Sub
    Sub New(ByVal Configuracion As CONFIGURACION)

        If Configuracion Is Nothing Then
            Throw New Exception("No hay datos de configuración")
        End If

        Me.Configuracion = Configuracion

        If Configuracion.Ptr_Num <> 1 Then
            Caso = 0
        End If

        If Configuracion.Pwh_Num <> 1 Then
            Caso = 1
        End If

        If Configuracion.Prof_Valv_Num <> 1 Then
            Caso = 2
        End If

        If Configuracion.Diam_Orif_Num <> 1 Then
            Caso = 3
        End If

        If Configuracion.Wc_Num <> 1 Then
            Caso = 4
        End If
        If Caso = 5 Then
            Throw New System.Exception("Debe de variar el número de puntos")
        End If
    End Sub



    Public Function Execute() As Boolean
        Try
            Dim ObjMapa As Clases.MapaEstabilidad = New Clases.MapaEstabilidad(Configuracion.Ptr_Min, Configuracion.Ptr_Max, Configuracion.Ptr_Num, Configuracion.Pwh_Min, Configuracion.Pwh_Max, Configuracion.Pwh_Num, Configuracion.Qgi_Min, Configuracion.Qgi_Max, Configuracion.Qgi_Num, Configuracion.Prof_Valv_Min, Configuracion.Prof_Valv_Max, Configuracion.Prof_Valv_Num, Configuracion.Diam_Orif_Min, Configuracion.Diam_Orif_Max, Configuracion.Diam_Orif_Num, Configuracion.Wc_Min, Configuracion.Wc_Max, Configuracion.Wc_Num, Configuracion.CD, Configuracion.Ptr_Op, Configuracion.Pwh_Op, Configuracion.Qgi_Op, Configuracion.Prof_Valv_Op, Configuracion.Diam_Orif_Op, Configuracion.Wc_Op, Caso)

            If ObjMapa.GENERAL() Then
                Mapa = ObjMapa.ListaParametrosEstabilidad
                Ql = ObjMapa.Ql
                Recomendacion = ObjMapa.recomendaciones
                Configuracion.ESTATUS = 2
            Else
                Configuracion.ESTATUS = -1
            End If

            Return True
        Catch ex As Exception
            Recomendacion = ex.Message
            Configuracion.ESTATUS = -1
            'Throw New Exception(ex.Message)

            Return False
        End Try
    End Function

    Public Function Save() As Boolean
        Try
            If Configuracion.IDMODPOZO Is Nothing Then
                Throw New Exception("Datos de configuracion incompleta")
            End If

            'BORRAMOS ELEMENTOS PREVIOS EN CASO DE EXISTIR
            '================================================================================================================
            Dim deletes = db.RESULTADOS.Where(Function(w) w.IDDATOSENTRADAEST = Configuracion.IDDATOSENTRADAEST).ToList()



            If deletes.Count > 0 Then
                Task.Run(Sub()
                             deletes.ForEach(Function(e) db.RESULTADOS.Remove(e))
                             db.SaveChanges()
                         End Sub)
            End If

            Configuracion.QL = Ql
            'Configuracion.ESTATUS = IIf(Estatus, 2, -1)
            Configuracion.RECOMENDACIONES = Recomendacion
            db.Entry(Configuracion).State = System.Data.Entity.EntityState.Modified
            db.SaveChanges()

            If Configuracion.ESTATUS = 2 Then
                Task.Run(Sub()
                             Dim ids_estabilidad = db.CAT_ESTABILIDAD_OPC.Where(Function(w) w.Ind_Est IsNot Nothing).ToDictionary(Function(d) d.Ind_Est, Function(d) d.IDCATDESCRIPCION)
                             Using db_n As Entities_ModeloCI = New Entities_ModeloCI()


                                 For Each item In Mapa
                                     Dim insert_ivan As RESULTADOS = New RESULTADOS() With {
                                    .IDRESULTADOS = Guid.NewGuid().ToString().ToUpper(),
                                    .IDCATDESCRIPCION = ids_estabilidad(item.Ind_Est),
                                    .IDDATOSENTRADAEST = Configuracion.IDDATOSENTRADAEST,
                                    .Ind_Est = item.Ind_Est,
                                    .Ptr = item.Ptr,
                                    .DiaVal = item.DiaVal,
                                    .Dp = item.Dp,
                                    .Dpvalve = item.Dpvalve,
                                    .Dvalve = item.Dvalve,
                                    .GOR = item.GOR,
                                    .QI = item.Ql,
                                    .Qg = item.Qg,
                                    .Pwf_IPR = item.Pwf_IPR,
                                    .Twh = item.Twh,
                                    .TotalQGas = item.TotalQgas,
                                    .Pwf_quicklook = item.Pwf_quicklook,
                                    .Pws = item.Pws,
                                    .Pti = item.Pti,
                                    .Ptri = item.Ptri,
                                    .Tvalv = item.Tvalv,
                                    .GOR_quicklool = item.GOR_quicklook,
                                    .GORFREE = item.GORFREE,
                                    .Ptrcalc = item.Ptrcalc,
                                    .PI = item.PI,
                                    .Qgcrit = item.Qgcrit,
                                    .Qcporcent = item.Qcporcent,
                                    .HTC = item.HTC,
                                    .Pwh = item.Pwh,
                                    .Qgi = item.Qgi,
                                    .Wc = item.Wc,
                                    .VARIABLE = item.Variable,
                                    .metodologia = 0
                                }
                                     Dim insert_pob As RESULTADOS = New RESULTADOS() With {
                                    .IDRESULTADOS = Guid.NewGuid().ToString().ToUpper(),
                                    .IDCATDESCRIPCION = ids_estabilidad(item.Ind_Poblano),
                                    .IDDATOSENTRADAEST = Configuracion.IDDATOSENTRADAEST,
                                    .Ind_Est = item.Ind_Poblano,
                                    .Ptr = item.Ptr,
                                    .DiaVal = item.DiaVal,
                                    .Dp = item.Dp,
                                    .Dpvalve = item.Dpvalve,
                                    .Dvalve = item.Dvalve,
                                    .GOR = item.GOR,
                                    .QI = item.Ql,
                                    .Qg = item.Qg,
                                    .Pwf_IPR = item.Pwf_IPR,
                                    .Twh = item.Twh,
                                    .TotalQGas = item.TotalQgas,
                                    .Pwf_quicklook = item.Pwf_quicklook,
                                    .Pws = item.Pws,
                                    .Pti = item.Pti,
                                    .Ptri = item.Ptri,
                                    .Tvalv = item.Tvalv,
                                    .GOR_quicklool = item.GOR_quicklook,
                                    .GORFREE = item.GORFREE,
                                    .Ptrcalc = item.Ptrcalc,
                                    .PI = item.PI,
                                    .Qgcrit = item.Qgcrit,
                                    .Qcporcent = item.Qcporcent,
                                    .HTC = item.HTC,
                                    .Pwh = item.Pwh,
                                    .Qgi = item.Qgi,
                                    .Wc = item.Wc,
                                    .VARIABLE = item.Variable,
                                    .metodologia = 1
                                }

                                     db_n.RESULTADOS.Add(insert_ivan)
                                     db_n.RESULTADOS.Add(insert_pob)


                                 Next



                                 db_n.SaveChanges()
                             End Using

                         End Sub)
            End If


            Return True
        Catch ex As Exception
            Throw New Exception("Guardar: " + ex.Message)
        End Try
    End Function



End Class
