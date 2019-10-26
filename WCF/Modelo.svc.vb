' NOTA: puede usar el comando "Cambiar nombre" del menú contextual para cambiar el nombre de clase "Modelo" en el código, en svc y en el archivo de configuración a la vez.
' NOTA: para iniciar el Cliente de prueba WCF para probar este servicio, seleccione Modelo.svc o Modelo.svc.vb en el Explorador de soluciones e inicie la depuración.

Imports System.Data
Imports ModeloProsper
Imports WCF.Generales
Imports WCF.Helpers
Imports ModeloCI


Public Class Modelo
    Implements IModelo
    Private db As New Entities_ModeloCI()
    Private conexion As BaseDatosServidor
    Private MPrsp As Crea.Modelo
    Private IdLog As String
    'Private IdModPozo As String
    'Private IdAgujero As String
    Private Inicio As DateTime
    Private Fin As DateTime
    Private User As String
    Private Logger As ModeloProsper.Logger



    Public Sub New()

        'conexion = New BaseDatosServidor("SERVER=10.85.35.113\SQLDESA01;User=CamposInteligentes;Password=ciGaitep; Database=CI;")

    End Sub
    Public Sub Reset(ByVal IdModPozo As String, ByVal MaxIntentos As Integer) Implements IModelo.Reset
        Try
            Dim modelo As New ModeloProsper.Modelo(IdModPozo)




            modelo.Reset(IdModPozo, MaxIntentos)


        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Sub
    Public Sub Delete(ByVal IdModPozo As String, ByVal IdUsuario As String) Implements IModelo.Delete
        Try
            Dim mod_configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
            If mod_configuracion Is Nothing Then
                Throw New Exception("El modelo no existe o fue borrado: " + IdModPozo)
            End If

            db.CONFIGURACION_ADMINISTRADOR.Remove(mod_configuracion)
            db.SaveChanges()


        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try


    End Sub
    Public Function Execute(ByVal IdModPozo As String, ByVal User As String) As Boolean Implements IModelo.Execute
        Try
            Dim modelo As New ModeloProsper.Modelo(IdModPozo)
            Dim inicio As DateTime = DateTime.Now

            Logger = New ModeloProsper.Logger(IdModPozo, User)
            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If

            Logger.SetEstatus(2)

            Dim result = modelo.Execute(IdModPozo)

            If result Then
                Logger.SetEstatus(3, "Ejecución correcta(Solicitado)")
            Else
                Logger.SetEstatus(-1, "Hubo un error al realizar la operación")
            End If

            Return result
        Catch ex As Exception
            If Logger.Configuracion.ESTATUS = 2 Then
                Logger.SetEstatus(-1, ex.Message)
            End If

            Throw New Exception(ex.Message)
        End Try




    End Function
    'Private Sub SaveIPR(ByVal IdModPozo As String)
    '    'IPR

    '    Try
    '        Dim Vlp As New VLP_IPR() With {
    '                 .IDVLPIPR = Guid.NewGuid().ToString().ToUpper(),
    '                 .IDMODPOZO = IdModPozo,
    '                 .TITULO1 = MPrsp.NomVLPIPR(0),
    '                 .TITULO2 = MPrsp.NomVLPIPR(1)
    '                             }
    '        db.VLP_IPR.Add(Vlp)
    '        db.SaveChanges()


    '        For i = 0 To 19 ' MPrsp.VLP_PWF.GetUpperBound(0)
    '            db.VLP_IPR_DETALLE.Add(New VLP_IPR_DETALLE() With {
    '                .IDVLPIPRDETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                .IDVLPIPR = Vlp.IDVLPIPR,
    '                .IPR_RTEL = MPrsp.IPR_RTEL(i),
    '                .IPR_PWF = MPrsp.IPR_PWF(i),
    '                .VLP_RTEL = MPrsp.VLPIPR_RTEL(0, i),
    '                .VLP_PWF = MPrsp.VLPIPR_PWF(0, i)
    '            })
    '        Next i
    '        db.SaveChanges()


    '    Catch ex As Exception

    '    End Try
    '    'Dim Xaux(.VLPIPR_RTEL.GetUpperBound(0)), Yaux(.VLPIPR_RTEL.GetUpperBound(0)) As Double

    '    '    Dim vlp = .VLPIPR_PWF

    'End Sub
    'Private Sub SaveCorrelacion(ByVal IdModPozo As String)
    '    Dim cat_correlaciones = db.CAT_CORRELACION.Where(Function(w) w.ENDRECORD Is Nothing).OrderBy(Function(o) o.NUM).ToDictionary(Function(d) d.NUM, Function(d) d.IDCATCORRELACION)


    '    Dim NdatAux As Integer
    '    For i = 0 To MPrsp.NumCorr.Val - 1 '1 to 9
    '        NdatAux = MPrsp.NumDatCorr(MPrsp.NumCor(i)) - 1
    '        Dim Xaux(NdatAux + 1), Yaux(NdatAux + 1) As Double

    '        Dim correlacion As New CORRELACION() With {
    '            .IDCATCORRELACION = cat_correlaciones.Values(MPrsp.NumCor(i)),
    '            .IDCORRELACION = Guid.NewGuid().ToString().ToUpper(),
    '            .IDMODPOZO = IdModPozo
    '        }
    '        db.CORRELACION.Add(correlacion)
    '        db.SaveChanges()


    '        'Guardamos correlaciones generales
    '        For j = 0 To MPrsp.NumDatCorr(MPrsp.NumCor(i))
    '            ' PresWf, ProfDesa
    '            db.CORRELACION_GENERAL.Add(New CORRELACION_GENERAL() With {
    '                .IDCORRELACIONGENERAL = Guid.NewGuid().ToString().ToUpper(),
    '                .IDCORRELACION = correlacion.IDCORRELACION,
    '                .PROFMD = MPrsp.ProfDesa(MPrsp.NumCor(i), j),
    '                .PRES = MPrsp.PresWf(MPrsp.NumCor(i), j)
    '            })

    '            'Dim prof = MPrsp.ProfDesa(MPrsp.NumCor(i), j) ' Ordernar por este rubro
    '            'Dim pres = MPrsp.PresWf(MPrsp.NumCor(i), j)
    '        Next
    '        db.SaveChanges()
    '    Next i

    'End Sub
    'Private Sub SaveVLP(ByVal IdModPozo As String)
    '    Dim NumGraf As Integer = 4
    '    Dim NumDat As Integer = 20


    '    Dim NdatAux As Integer
    '    NdatAux = NumDat - 1
    '    Dim Xaux(NdatAux), Yaux(NdatAux) As Double
    '    Try
    '        Select Case MPrsp.LiftMethod.Val
    '            Case 1

    '                Dim vlp As New VLP_IPR_GASTO_INYECCION() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
    '                        .TITULO = MPrsp.NomVLPIPR(1),
    '                        .IS_VLP = 1
    '                    }
    '                db.VLP_IPR_GASTO_INYECCION.Add(vlp)
    '                db.SaveChanges()


    '                For J = 0 To NdatAux
    '                    'Xaux(J) = .VLPIPR_RTEL(1, J)
    '                    'Yaux(J) = .VLPIPR_PWF(1, J)

    '                    db.VLP_IPR_GASTO_DETALLE.Add(New VLP_IPR_GASTO_DETALLE() With {
    '                                .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                                .IDVLPIPRGASTOINYECCION = vlp.IDVLPIPRGASTOINYECCION,
    '                                .XAUX = MPrsp.VLPIPR_RTEL(1, J),'Revisar
    '                                .YAUX = MPrsp.VLPIPR_PWF(1, J) ' Revisar            
    '                    })

    '                    db.SaveChanges()
    '                Next


    '                'Guardar frecuencias
    '                For i = 0 To 9

    '                    'Insercion VLP
    '                    Dim vlp_inyeccion As New VLP_IPR_GASTO_INYECCION() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
    '                        .TITULO = Format(MPrsp.QgiVec(i), "0.00"),
    '                        .IS_VLP = 0
    '                    }
    '                    db.VLP_IPR_GASTO_INYECCION.Add(vlp_inyeccion)
    '                    db.SaveChanges()


    '                    For j = 0 To NdatAux '19 lugares
    '                        Dim vlp_detalles As New VLP_IPR_GASTO_DETALLE() With {
    '                                .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                                .IDVLPIPRGASTOINYECCION = vlp_inyeccion.IDVLPIPRGASTOINYECCION,
    '                                .XAUX = MPrsp.QliqVLP(i, j),'Revisar
    '                                .YAUX = MPrsp.PwfVLP(i, j) ' Revisar            
    '                        }

    '                        db.VLP_IPR_GASTO_DETALLE.Add(vlp_detalles)
    '                        db.SaveChanges()
    '                    Next j




    '                Next i
    '            Case 2
    '                'Guardar vlp gasto inyeccion
    '                Dim vlp As New VLP_IPR_GASTO_INYECCION() With {
    '                    .IDMODPOZO = IdModPozo,
    '                    .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
    '                    .TITULO = MPrsp.NomVLPIPR(0),
    '                    .IS_VLP = 1
    '                }
    '                db.VLP_IPR_GASTO_INYECCION.Add(vlp)
    '                db.SaveChanges()
    '                For j = 0 To NdatAux
    '                    'Xaux(j) = MPrsp.QliqVLP(1, j)
    '                    'Yaux(j) = MPrsp.PwfVLP(1, j)

    '                    db.VLP_IPR_GASTO_DETALLE.Add(New VLP_IPR_GASTO_DETALLE() With {
    '                                .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                                .IDVLPIPRGASTOINYECCION = vlp.IDVLPIPRGASTOINYECCION,
    '                                .Xaux = MPrsp.QliqVLP(1, j),'Revisar
    '                                .Yaux = MPrsp.PwfVLP(1, j) ' Revisar            
    '                    })

    '                    db.SaveChanges()
    '                Next


    '                'Guardar frecuencias
    '                For i = 0 To 9

    '                    'Insercion VLP
    '                    Dim vlp_inyeccion As New VLP_IPR_GASTO_INYECCION() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
    '                        .TITULO = Format(MPrsp.FrecVec(i), "0.00"),
    '                        .IS_VLP = 0
    '                    }
    '                    db.VLP_IPR_GASTO_INYECCION.Add(vlp_inyeccion)
    '                    db.SaveChanges()


    '                    For j = 0 To NdatAux '19 lugares
    '                        Dim vlp_detalles As New VLP_IPR_GASTO_DETALLE() With {
    '                                .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                                .IDVLPIPRGASTOINYECCION = vlp_inyeccion.IDVLPIPRGASTOINYECCION,
    '                                .XAUX = MPrsp.QliqVLP(i, j),'Revisar
    '                                .YAUX = MPrsp.Pdescarga_BEC(i, j) ' Revisar            
    '                        }

    '                        db.VLP_IPR_GASTO_DETALLE.Add(vlp_detalles)
    '                        db.SaveChanges()
    '                    Next j




    '                Next i
    '                'Guardar vlp gasto inyeccion detalles
    '                'For i = 0 To 9
    '                '    For j = 0 To NdatAux
    '                '        Xaux(j) = MPrsp.QliqVLP(i, j)
    '                '        Yaux(j) = MPrsp.Pdescarga_BEC(i, j)
    '                '    Next
    '                'Next i
    '        End Select
    '    Catch ex As Exception
    '        Throw New Exception(ex.Message)
    '    End Try
    'End Sub
    'Private Sub SaveGAS(ByVal IdModPozo As String)
    '    Try
    '        Select Case MPrsp.LiftMethod.Val
    '            Case 1


    '                Dim gasto As New COMPORTAMIENTO_GAS() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = "Qgi"
    '                }

    '                db.COMPORTAMIENTO_GAS.Add(gasto)
    '                db.SaveChanges()

    '                For i = 0 To MPrsp.QgiVec.Count() - 1
    '                    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
    '                        .IDCOMPORTAMIENTOGAS = gasto.IDCOMPORTAMIENTOGAS,
    '                        .XAUX = MPrsp.QgiVec(i),
    '                        .YAUX = MPrsp.QliqVec(i)
    '                    })
    '                Next i

    '            Case 2

    '                Dim xHz As Double()
    '                Dim yHz As Double()
    '                Dim titulo As String = ""
    '                For j = 0 To 3
    '                    ReDim xHz(99), yHz(99)

    '                    Select Case j
    '                        Case 0 '40 Hz
    '                            titulo = "40 Hz"
    '                        Case 1 '50 Hz
    '                            titulo = "50 Hz"
    '                        Case 2 '60 Hz
    '                            titulo = "60 Hz"
    '                        Case 3 '70 Hz
    '                            titulo = "70 Hz"
    '                    End Select
    '                    Dim gasto As New COMPORTAMIENTO_GAS() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = titulo
    '                    }

    '                    db.COMPORTAMIENTO_GAS.Add(gasto)
    '                    db.SaveChanges()

    '                    For i = 0 To 99
    '                        db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
    '                            .IDCOMPORTAMIENTOGAS = gasto.IDCOMPORTAMIENTOGAS,
    '                            .XAUX = MPrsp.Xcarta_BEC(j)(i),
    '                            .YAUX = MPrsp.Ycarta_BEC(j)(i)
    '                        })
    '                    Next i



    '                Next j

    '                '=========rango minimo==============
    '                Dim rango_min As New COMPORTAMIENTO_GAS() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = "Rango Mínimo"
    '                }
    '                db.COMPORTAMIENTO_GAS.Add(rango_min)
    '                db.SaveChanges()
    '                For i = 0 To MPrsp.Rango_MinX_BEC.Count() - 1
    '                    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
    '                           .IDCOMPORTAMIENTOGAS = rango_min.IDCOMPORTAMIENTOGAS,
    '                           .XAUX = MPrsp.Rango_MinX_BEC(i),
    '                           .YAUX = MPrsp.Rango_MinY_BEC(i)
    '                    })
    '                Next i
    '                db.SaveChanges()
    '                '=========rango maximo==============
    '                Dim rango_max As New COMPORTAMIENTO_GAS() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = "Rango Máximo"
    '                }
    '                db.COMPORTAMIENTO_GAS.Add(rango_max)
    '                db.SaveChanges()

    '                For i = 0 To MPrsp.Rango_MaxX_BEC.Count() - 1
    '                    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
    '                           .IDCOMPORTAMIENTOGAS = rango_max.IDCOMPORTAMIENTOGAS,
    '                           .XAUX = MPrsp.Rango_MaxX_BEC(i),
    '                           .YAUX = MPrsp.Rango_MaxY_BEC(i)
    '                    })

    '                Next i
    '                db.SaveChanges()
    '                '=========Eficiencia==============
    '                Dim eficiencia As New COMPORTAMIENTO_GAS() With {
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = "Mejor Eficiencia"
    '                }
    '                db.COMPORTAMIENTO_GAS.Add(eficiencia)
    '                db.SaveChanges()

    '                For i = 0 To MPrsp.Mejor_EficX_BEC.Count() - 1
    '                    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
    '                           .IDCOMPORTAMIENTOGAS = eficiencia.IDCOMPORTAMIENTOGAS,
    '                           .XAUX = MPrsp.Mejor_EficX_BEC(i),
    '                           .YAUX = MPrsp.Mejor_EficY_BEC(i)
    '                    })
    '                Next i
    '                db.SaveChanges()

    '        End Select

    '    Catch ex As Exception
    '        Throw New Exception(ex.Message)
    '    End Try

    'End Sub
    'Private Sub SaveDiag(ByVal IdModPozo As String)
    '    Dim titulo As String = ""
    '    Try
    '        Select Case MPrsp.LiftMethod.Val
    '            Case 1
    '                'TChart5.Text = "Diagnóstico de BNC"
    '                Dim Xaux(MPrsp.NumDatGrad), Yaux(MPrsp.NumDatGrad) As Double


    '                'Arriba - Abajo
    '                Dim arriba As New DIAGNOSTICOS() With {
    '                    .IDMODPOZO = IdModPozo,
    '                    .TITULO = "Arriba-Abajo"
    '                }
    '                'Abajo - Arriba
    '                Dim abajo As New DIAGNOSTICOS() With {
    '                   .IDMODPOZO = IdModPozo,
    '                       .TITULO = "Abajo-Arriba"
    '                }
    '                'Ptr Medida
    '                Dim medida As New DIAGNOSTICOS() With {
    '                   .IDMODPOZO = IdModPozo,
    '                   .TITULO = "Ptr Medida"
    '                }
    '                'Ptr Teorica
    '                Dim teorica As New DIAGNOSTICOS() With {
    '                   .IDMODPOZO = IdModPozo,
    '                   .TITULO = "Ptr Teoríca"
    '                }
    '                db.DIAGNOSTICOS.Add(arriba)
    '                db.DIAGNOSTICOS.Add(abajo)
    '                db.DIAGNOSTICOS.Add(medida)
    '                db.DIAGNOSTICOS.Add(teorica)
    '                db.SaveChanges()

    '                'ARRIBA - ABAJO -ARRIBA
    '                For j = 0 To MPrsp.NumDatGrad - 1
    '                    'Xaux(J) = MPrsp.PresQL(i, J)
    '                    'Yaux(J) = MPrsp.TVDQL(i, J)

    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                        .IDDIAGNOSTICO = arriba.IDDIAGNOSTICO,
    '                        .XAUX = MPrsp.PresQL(0, j),
    '                        .YAUX = MPrsp.TVDQL(0, j)
    '                    })
    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                      .IDDIAGNOSTICO = abajo.IDDIAGNOSTICO,
    '                      .XAUX = MPrsp.PresQL(1, j),
    '                      .YAUX = MPrsp.TVDQL(1, j)
    '                   })
    '                Next

    '                'MEDIDA - TEORICA
    '                Dim Xaux1(MPrsp.NumDatGG), Yaux1(MPrsp.NumDatGG) As Double
    '                For j = 0 To MPrsp.NumDatGG - 1

    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                        .IDDIAGNOSTICO = medida.IDDIAGNOSTICO,
    '                        .XAUX = MPrsp.PresQL(0, j),
    '                        .YAUX = MPrsp.TVDQL(0, j)
    '                    })

    '                    'Xaux1(I) = MPrsp.PresGG(I)
    '                    'Yaux1(I) = MPrsp.MSDGG(I)
    '                Next

    '                For j = 0 To MPrsp.GGPres.Length - 1
    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                        .IDDIAGNOSTICO = teorica.IDDIAGNOSTICO,
    '                        .XAUX = MPrsp.GGPres(j),
    '                        .YAUX = MPrsp.GGTVD(j)
    '                    })
    '                Next
    '                'Line24.Title = "Ptr Medida"
    '                'Line24.Add(Xaux1, Yaux1)
    '                'Line25.Title = "Ptr Teórica"
    '                'Line25.Add(.GGPres, .GGTVD)

    '            Case 2
    '                'Arriba - Abajo
    '                Dim arriba As New DIAGNOSTICOS() With {
    '                    .IDMODPOZO = IdModPozo,
    '                    .TITULO = "Arriba-Abajo"
    '                }
    '                'Abajo - Arriba
    '                Dim abajo As New DIAGNOSTICOS() With {
    '                   .IDMODPOZO = IdModPozo,
    '                       .TITULO = "Abajo-Arriba"
    '                }
    '                'Temperatura
    '                Dim temp As New DIAGNOSTICOS() With {
    '                    .IDMODPOZO = IdModPozo,
    '                    .TITULO = "Temp [°C]"
    '                }
    '                db.DIAGNOSTICOS.Add(arriba)
    '                db.DIAGNOSTICOS.Add(abajo)
    '                db.DIAGNOSTICOS.Add(temp)
    '                db.SaveChanges()
    '                For J = 0 To MPrsp.NumDatGrad - 1

    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                        .IDDIAGNOSTICO = arriba.IDDIAGNOSTICO,
    '                        .XAUX = MPrsp.PresQL(0, J),
    '                        .YAUX = MPrsp.TVDQL(0, J)
    '                    })

    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                       .IDDIAGNOSTICO = abajo.IDDIAGNOSTICO,
    '                       .XAUX = MPrsp.PresQL(1, J),
    '                       .YAUX = MPrsp.TVDQL(1, J)
    '                    })

    '                    db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
    '                       .IDDIAGNOSTICO = temp.IDDIAGNOSTICO,
    '                       .XAUX = MPrsp.TempQL(0, J),
    '                       .YAUX = MPrsp.TVDQL(0, J)
    '                   })

    '                Next
    '                db.SaveChanges()




    '        End Select

    '    Catch ex As Exception
    '        Throw New Exception(ex.Message)
    '    End Try

    'End Sub
    'Private Sub SaveWC(ByVal IdModPozo As String)
    '    Try
    '        Dim i, j, k As Integer
    '        Dim NumVar1 As Integer = 9
    '        Dim NumVar2 As Integer = 9
    '        Dim NumVar3 As Integer = 0

    '        Dim Xaux(NumVar1), Yaux(NumVar2) As Double

    '        Select Case MPrsp.LiftMethod.Val
    '            Case 1

    '                For i = 0 To NumVar1
    '                    Dim wc As New PRODUCTIVIDAD() With {
    '                        .IDPRODUCTIVIDAD = Guid.NewGuid().ToString().ToUpper(),
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = Math.Round(MPrsp.Qgi_Res(i), 1).ToString
    '                    }
    '                    db.PRODUCTIVIDAD.Add(wc)
    '                    db.SaveChanges()

    '                    For j = 0 To NumVar2
    '                        Xaux(j) = MPrsp.Wc_Res(j)
    '                        For k = 0 To NumVar3
    '                            Yaux(j) = MPrsp.Qliq_Res(i, j, k)
    '                        Next k


    '                    Next j

    '                    For j = 0 To Xaux.Count - 1
    '                        db.PRODUCTIVIDAD_DETALLE.Add(New PRODUCTIVIDAD_DETALLE() With {
    '                            .IDPRODUCTIVIDADDETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                            .IDPRODUCTIVIDAD = wc.IDPRODUCTIVIDAD,
    '                            .WC_RES = Xaux(j),
    '                            .QGI_RES = Yaux(j)
    '                         })
    '                    Next j
    '                    db.SaveChanges()
    '                Next i
    '            Case 2
    '                For i = 0 To NumVar1
    '                    Dim wc As New PRODUCTIVIDAD() With {
    '                        .IDPRODUCTIVIDAD = Guid.NewGuid().ToString().ToUpper(),
    '                        .IDMODPOZO = IdModPozo,
    '                        .TITULO = Math.Round(MPrsp.Frec_Res(i), 1).ToString
    '                    }
    '                    db.PRODUCTIVIDAD.Add(wc)
    '                    db.SaveChanges()

    '                    For j = 0 To NumVar2
    '                        Xaux(j) = MPrsp.Wc_Res(j)
    '                        For k = 0 To NumVar3
    '                            Yaux(j) = MPrsp.Qliq_Res(i, j, k)
    '                        Next k


    '                    Next j

    '                    For j = 0 To Xaux.Count - 1
    '                        db.PRODUCTIVIDAD_DETALLE.Add(New PRODUCTIVIDAD_DETALLE() With {
    '                            .IDPRODUCTIVIDADDETALLE = Guid.NewGuid().ToString().ToUpper(),
    '                            .IDPRODUCTIVIDAD = wc.IDPRODUCTIVIDAD,
    '                            .WC_RES = Xaux(j),
    '                            .QGI_RES = Yaux(j)
    '                         })
    '                    Next j
    '                    db.SaveChanges()
    '                Next i

    '        End Select

    '    Catch ex As Exception
    '        Throw New Exception(ex.Message)
    '    End Try

    'End Sub
    'Private Function LoadGeneral(ByVal general As MOD_POZO_GENERAL, ByVal edo_mecanico As List(Of VW_EDO_MECANICO), ByVal edo_trayectoria As List(Of MOD_POZO_TRAYEC), ByVal edo_temp As List(Of MOD_POZO_TEMP)) As Boolean

    '    Dim mod_bnc = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = general.IDMODPOZO).SingleOrDefault()
    '    Dim mod_bec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = general.IDMODPOZO).SingleOrDefault()




    '    If mod_bnc Is Nothing Then
    '        mod_bnc = New MOD_POZO_BNC()
    '    End If
    '    If mod_bec Is Nothing Then
    '        mod_bec = New MOD_POZO_BEC()
    '    End If

    '    Try
    '        ' Fluido: 0 = Oil and Water
    '        '         1 = Dry and Wet Gas
    '        '         2 = Retrograde Condensate
    '        '
    '        MPrsp.Fluid.Val = general.FLUID

    '        '
    '        ' PVTModel: 0 = Black Oil
    '        '           1 = Equation of State
    '        '
    '        MPrsp.PVTModel.Val = general.PVTMODEL

    '        '
    '        ' Separator: 0 = Single Stage Separator
    '        '            1 = Two Stage Separator
    '        '
    '        MPrsp.Separator.Val = general.SEPARATOR

    '        '
    '        ' Emulsion: 0 = No
    '        '           1 = Emulsion + Pump Viscosity Correction
    '        '
    '        MPrsp.Emulsion.Val = general.EMULSION

    '        '
    '        ' Hydrate: 0 = Disable Warning
    '        '          1 = Enable Warning
    '        '
    '        MPrsp.Hydrate.Val = general.HYDRATE

    '        '
    '        ' WatVis: 0 = Use Default Correlation
    '        '         1 = Use Pressure Corrected Correlation
    '        '
    '        MPrsp.WatVis.Val = general.WATVIS

    '        '
    '        ' VisMod: 0 = Newtonian Fluid
    '        '         1 = Non Newtonian Fluid
    '        '
    '        MPrsp.VisMod.Val = general.VISMOD

    '        '
    '        ' FlowType: 0 = Tubing Flow
    '        '           1 = Annular Flow
    '        '
    '        MPrsp.FlowType.Val = general.FLOWTYPE

    '        '
    '        ' WellType: 0 = Producer
    '        '           1 = Inyector
    '        '           2 = Water Inyector
    '        '
    '        MPrsp.WellType.Val = general.WELLTYPE

    '        '
    '        ' LiftMethod: 0 = None
    '        '             1 = Gas Lift (Continous)
    '        '             2 = Electrical Sumersible Pump
    '        '             3 = Hydraulic Drive Downhole Pump
    '        '             4 = Progressive Cavity Pump
    '        '             5 = Coiled Tubing Gas Lift
    '        '             6 = Diluent Injection
    '        '             7 = Jet Pump
    '        '             8 = MultiPhase Pump
    '        '             9 = Sucker Rod Pump
    '        '             10 = Gas Lift (Intermttent)
    '        '
    '        MPrsp.LiftMethod.Val = general.LIFTMETHOD

    '        '
    '        ' LiftType: 0 = No Friction Loss In Annulus
    '        '           1 = Friction Loss In Annulus
    '        '           2 = Safety Equipment
    '        '
    '        MPrsp.LiftType.Val = general.LIFTYPE

    '        '
    '        ' Predict: 0 = Pressure Only
    '        '          1 = Pressure and Temperature (offshore)
    '        '          2 = Pressure and Temperature (on Land)        '
    '        MPrsp.Predict.Val = general.PREDICT

    '        '
    '        ' TempModel: 0 = Rough Approximation
    '        '            1 = Enthalpy Balance
    '        '            2 = Improved Approximation
    '        MPrsp.TempModel.Val = general.TEMPMODEL

    '        '
    '        ' RangeSystem: 0 = Full System
    '        '              1 = Pipeline Only
    '        MPrsp.RangeSystem.Val = general.RANGESYSTEM

    '        '
    '        ' OutputRes: 0 = Show Calculating Data
    '        '            1 = Hide Calculating Data
    '        MPrsp.OutputRes.Val = general.OUTPUTRES

    '        '
    '        ' Completion: 0 = Cased Hole
    '        '             1 = Open hole
    '        MPrsp.Completion.Val = general.COMPLETION

    '        '
    '        ' GravelPack: 0 = None
    '        '             1 = Gravel Pack
    '        '             2 = Pre Packed Screen
    '        '             3 = Wire Wrapped Screen
    '        '             4 = Slotted Liner
    '        MPrsp.GravelPack.Val = general.GRAVELPACK

    '        '
    '        ' InflowType: 0 = Single Branch
    '        '             1 = MultiLateral Well
    '        MPrsp.InflowType.Val = general.INFLOWTYPE

    '        '
    '        ' GasConing: 0 = No
    '        '            1 = Yes
    '        MPrsp.GasConing.Val = general.GASCONING

    '        MPrsp.Company = general.COMPANY
    '        MPrsp.Field = general.FIELD
    '        MPrsp.Locations = general.LOCATIONS
    '        MPrsp.Well = general.WELL
    '        MPrsp.Platform = general.PLATAFORM
    '        MPrsp.Analyst = general.ANALYST
    '        'MPrsp.DatGenDate = general.DATGENDATE

    '        MPrsp.Comenta = general.COMENTA



    '        '
    '        ' IPR
    '        '
    '        MPrsp.IPRMethod.Val = general.IPRMETHOD

    '        ' Datos
    '        ' Presión de Fondo Estática
    '        MPrsp.PRes.Val = general.PRES ' Kg/cm2
    '        ' Temperatura del Yacimiento
    '        MPrsp.TRes.Val = general.TRES  ' oC
    '        ' Porcentaje de agua en la capa
    '        MPrsp.Wc.Val = general.WC  ' %
    '        ' Relación Gas Aceite tomada de la columna de los datos del yacimiento debido a que se debe filtrar.
    '        MPrsp.TotGor.Val = general.TOTGOR
    '        ' Modelo de reduccion de permeabilidad por compactación
    '        ' 0: No
    '        ' 1: Si
    '        MPrsp.Compact.Val = general.COMPACT
    '        ' Considerar las permeabilidades relativas
    '        ' 0: No
    '        ' 1: Si
    '        MPrsp.IRELK.Val = general.IRElK

    '        ' Datos del modelo de Vogel
    '        '
    '        ' Producción de liquido
    '        MPrsp.QTest.Val = general.QTEST ' bl/dia
    '        ' Presión de fondo fluyendo
    '        MPrsp.Ptest.Val = general.PTEST ' Kg/cm2

    '        ' Datos del modelo de Darcy
    '        '
    '        ' Daño Mecánico / Geometrico
    '        ' 0: Enter Skin by Hand
    '        ' 1: Loke
    '        ' 2: MacLeod
    '        ' 3: Karakas - Tariq
    '        MPrsp.MGSkinMethod.Val = general.MGSKINMETHOD

    '        ' Daño por desviación y penetración parcial
    '        ' 0: Cinco / Martín-Bronz
    '        ' 1: Wong-Cliford
    '        ' 2: Cinco (2) / Martín-Bronz
    '        MPrsp.DPSkinMethod.Val = general.MGSKINMETHOD

    '        ' Permeabilidad del yacimiento
    '        MPrsp.ResPerm.Val = general.RESPERM
    '        ' Espesor del yacimiento
    '        MPrsp.Thickness.Val = general.THICKNESS
    '        ' Area de drene
    '        MPrsp.Drainage.Val = general.DRAINAGE
    '        ' Factor de forma
    '        MPrsp.Dietz.Val = general.DIETZ
    '        ' Radio del pozo
    '        MPrsp.WBR.Val = general.WBR
    '        ' Factor de daño total
    '        MPrsp.Skin.Val = general.SKIN

    '        '


    '        Select Case general.LIFTMETHOD
    '            Case 1
    '                ' DATOS DEL BNC
    '                '
    '                ' Emplear RGIL o Qgi
    '                '      0: Usar RGIL
    '                '      1: Usar Qgi
    '                MPrsp.Entry.Val = mod_bnc.ENTRY
    '                ' Metodo de  calculo del BN.
    '                ' 0: Profundidad de Inyección Fija
    '                ' 1: Profundidad de Inyeccion Optima
    '                ' 2: Especificar la profundidad de las valvulas
    '                MPrsp.Method.Val = mod_bnc.METHOD

    '                ' Densidad del gas de bombeo neumatico
    '                MPrsp.Gravity.Val = mod_bnc.GRAVITY
    '                '  Mol de H2S
    '                MPrsp.H2S.Val = mod_bnc.H2S
    '                '  Mol de CO2
    '                MPrsp.CO2.Val = mod_bnc.CO2
    '                '  Mol de N2
    '                MPrsp.N2.Val = mod_bnc.N2
    '                ' Qgi
    '                MPrsp.GLRiny.Val = mod_bnc.GLRINY

    '                ' Realación gas inyectado liquido m3/m3
    '                MPrsp.GLRate.Val = mod_bnc.GLRATE

    '                ' Profundidad de la valvula de inyeccion
    '                MPrsp.ValveDepth.Val = mod_bnc.VALVEDEPTH
    '                ' Diametro de la válvula de BNC
    '                MPrsp.DiamValBNC.Val = mod_bnc.DIAMVAL

    '                MPrsp.QgiMin.Val = mod_bnc.QGIMIN

    '                MPrsp.QgiMax.Val = mod_bnc.QGIMAX

    '            Case 2
    '                '************************DATOS BEC************************************

    '                ' Frecuencia Mínima
    '                MPrsp.FrecMin.Val = mod_bec.FRECMIN
    '                ' Frecuencia Máxima
    '                MPrsp.FrecMax.Val = mod_bec.FRECMAX
    '                ' Profundidad del BEC
    '                MPrsp.Prof_BEC.Val = mod_bec.PROF_BEC
    '                ' Frecuencia de operación del BEC
    '                MPrsp.Frec_BEC.Val = mod_bec.FREC_BEC
    '                ' Diámetro exterior máximo de BEC
    '                MPrsp.ODMax_BEC.Val = mod_bec.ODMAX_BEC
    '                ' Longitud del Cable
    '                MPrsp.LongCable_BEC.Val = mod_bec.LONGCABLE_BEC
    '                ' Eficiencia del separación de gas del BEC
    '                MPrsp.EfiSepGas_BEC.Val = mod_bec.EFISEPGAS_BEC
    '                ' Etapas de BEC
    '                MPrsp.Etapas_BEC.Val = mod_bec.ETAPAS_BEC
    '                ' Voltaje en superficie del BEC
    '                MPrsp.VoltSup_BEC.Val = mod_bec.VOLTSUP_BEC
    '                ' Factor de desgaste del BEC
    '                MPrsp.Desgaste_BEC.Val = mod_bec.DESGASTE_BEC
    '                ' Modelo de reducción del gas
    '                MPrsp.ReducGas_BEC.Val = mod_bec.REDUCGAS_BEC
    '                ' Número de bomba del catálogo de bombas
    '                MPrsp.Bomba_BEC.Val = Int32.Parse(mod_bec.BOMBA_BEC)
    '                ' Número de motor del catálogo de motres
    '                MPrsp.Motor_BEC.Val = Int32.Parse(mod_bec.MOTOR_BEC)
    '                ' Número de la potencia del motor del catálogo de motores-potencia
    '                MPrsp.PotenciaMotor_BEC.Val = Int32.Parse(mod_bec.POTENCIAMOTOR_BEC)
    '                ' Número de cable del catálogo de cables
    '                MPrsp.Cable_BEC.Val = Int32.Parse(mod_bec.CABLE_BEC)
    '                ' Presión de Succión de la bomba
    '                MPrsp.PreSuc_BEC.Val = mod_bec.PRESUC_BEC
    '                ' Corriente de la bomba BEC
    '                MPrsp.Corriente_BEC.Val = mod_bec.CORRIENTE_BEC
    '                ' Potencia de la bomba BEC
    '                MPrsp.Potencia_BEC.Val = mod_bec.POTENCIA_BEC
    '                ' Presión de Descarga de la bomba
    '                MPrsp.PreDes_BEC.Val = mod_bec.PREDES_BEC
    '        End Select


    '        'DATOS ESTADO MECANICO

    '        For i = 0 To edo_mecanico.Count - 1
    '            ' Etiqueta
    '            MPrsp.Label(i) = edo_mecanico(i).ETIQUETA
    '            ' Tipo de elemento ->
    '            '      0: E.M.R
    '            '      1: Tubing
    '            '      2: SSSV
    '            '      3: Restriction
    '            '      4: Casing
    '            If i > 0 Then MPrsp.DType.Val(i) = edo_mecanico(i).NUMERO.ToString()
    '            ' Profundidad desarrollada
    '            MPrsp.Depth.Val(i) = IIf(edo_mecanico(i).ETIQUETA.Contains("SSSV"), "0", edo_mecanico(i).MD.ToString())
    '            ' Diametro interior de la tuberia de produccion
    '            MPrsp.TID.Val(i) = edo_mecanico(i).TIDIAM.ToString()
    '            ' Rugosidad del interior de la tuberia de produccion
    '            MPrsp.TIR.Val(i) = edo_mecanico(i).TIROUG.ToString()
    '            ' Diametro exterior de la tuberia de produccion
    '            MPrsp.TOD.Val(i) = edo_mecanico(i).TODIAM.ToString()
    '            ' Rugosidad del exterior de la tuberia de produccion
    '            MPrsp.TOR.Val(i) = edo_mecanico(i).TOROUG.ToString()
    '            ' Diametro interior de la tuberia de revestimiento
    '            MPrsp.CID.Val(i) = edo_mecanico(i).CIDIAM.ToString()
    '            ' Rugosidad del interior de la tuberia de revestimiento
    '            MPrsp.CIR.Val(i) = edo_mecanico(i).CIROUG.ToString()

    '            'MsgBox("Elemento No:  " & I.ToString & Chr(13) & _
    '            '       "Etiqueta:     " & MPrsp.Label(I).ToString & Chr(13) & _
    '            '       "Tipo Tub:     " & MPrsp.DType(I).ToString & Chr(13) & _
    '            '       "Profund:      " & MPrsp.Depth(I).ToString & Chr(13) & _
    '            '       "Diam Int TP:  " & MPrsp.TID(I).ToString & Chr(13) & _
    '            '       "Rug Int TP:   " & MPrsp.TIR(I).ToString & Chr(13) & _
    '            '       "Diam Ext TP:  " & MPrsp.TOD(I).ToString & Chr(13) & _
    '            '       "Rug Ext TP:   " & MPrsp.TOR(I).ToString & Chr(13) & _
    '            '       "Diam Int TR:  " & MPrsp.CID(I).ToString & Chr(13) & _
    '            '       "Rug Int TR:   " & MPrsp.CIR(I).ToString & Chr(13))

    '        Next i

    '        'DATOS TRAYECTORIA

    '        ' Registro de Desviaciones
    '        '
    '        ' Trayecto_SQL(0, 0): Nombre del pozo
    '        ' Trayecto_SQL(0, 1): Numero de agujero
    '        ' Trayecto_SQL(0, 2): Fecha del modelo
    '        ' Trayecto_SQL(0, 3): IDTRAYECTORIAPROSPER
    '        ' Trayecto_SQL(0, 4): IDDATOSMODELOPROSPER
    '        '
    '        For I = 0 To edo_trayectoria.Count - 1
    '            MPrsp.RDEnable.Val(I) = 1
    '            ' Profundidad Desarrollada
    '            MPrsp.RDMd.Val(I) = edo_trayectoria(I).PROFUNDIDADMD
    '            ' Profundidad Vertical
    '            MPrsp.RDTvd.Val(I) = edo_trayectoria(I).PROFUNDIDADMV
    '        Next I


    '        ' Coeficiente de Transferencia de Calor Inicial
    '        MPrsp.Htc.Val = general.HTC

    '        ' Presion en la cabeza del pozo
    '        MPrsp.THPres.Val = general.THPD
    '        ' Temperatura en la cabeza del pozo (Fluyendo)
    '        MPrsp.THTemp.Val = general.THTD


    '        'DATOS TEMPERATURA

    '        For I = 0 To edo_temp.Count - 1
    '            ' Profundidad Desarrollada
    '            MPrsp.PTMd.Val(I) = edo_temp(I).PROFUNDIDADMD

    '            ' Temperatura
    '            MPrsp.PTTmp.Val(I) = edo_temp(I).TEMPERATURA
    '        Next I

    '        ' RGA del aforo
    '        MPrsp.RGA_Aforo.Val = general.RGATOTALAFORO
    '        ' Presion en la TR (BNC)
    '        MPrsp.TRPres.Val = general.TRPRES

    '        Return True

    '    Catch ex As Exception
    '        Throw New Exception(ex.Message)

    '        Return False
    '    End Try
    'End Function
    'Private Function GetTrayectorias(ByVal trayectorias As List(Of VW_TRAYECTORIA), ByVal max_md As Double) As Double(,)
    '    Dim result(17, 1) As Double
    '    Dim mds(trayectorias.Count), mvs(trayectorias.Count) As Double
    '    Dim tmp_trayectorias As New List(Of VW_TRAYECTORIA)







    '    If trayectorias.Count > 0 Then
    '        result(0, 0) = trayectorias(0).PROFUNDIDADMD 'dt.Rows(0).Item(0)
    '        result(0, 1) = trayectorias(0).PROFMV 'dt.Rows(0).Item(1)
    '        Dim finMD As Double = max_md
    '        Dim finMV As Double = Analisis.InterpolarProfundidadesVertical(mds, mvs, max_md)


    '        For i = 0 To trayectorias.Count - 1
    '            mds(i) = trayectorias(i).PROFUNDIDADMD
    '            mvs(i) = trayectorias(i).PROFMV

    '            If trayectorias(i).PROFUNDIDADMD <= max_md Then
    '                tmp_trayectorias.Add(trayectorias(i))
    '            End If

    '        Next i


    '        tmp_trayectorias = tmp_trayectorias.OrderBy(Function(o) o.SEVERIDAD).ToList()


    '        Dim indice As Integer = 1 '2
    '        Dim j As Integer = tmp_trayectorias.Count - 1
    '        Do While (indice < 17)
    '            result(indice, 0) = tmp_trayectorias(j).PROFUNDIDADMD 'View.Item(i).Item(0)
    '            result(indice, 1) = tmp_trayectorias(j).PROFMV 'View.Item(i).Item(1)
    '            indice += 1
    '            j -= 1
    '        Loop
    '        ReDim Preserve result(indice, 1)
    '        result(indice, 0) = finMD
    '        result(indice, 1) = finMV

    '        Analisis.ordenarMatriz(result, 0)
    '    End If





    '    Return result
    'End Function




    Private Sub Program() Implements IModelo.Program
        Dim path = "C:\Program Files (x86)\Petroleum Experts\IPM 11\prosper.exe"
        Dim proceso = Shell(path, AppWinStyle.NormalFocus, False, -1)

    End Sub
    Public Function Sensibilidad_BNF(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal QGI_Min As Double, ByVal QGI_Max As Double, ByVal FileD As Byte(), ByVal FileName As String) As List(Of Gas) Implements IModelo.Sensibilidad_BNF
        Try
            Dim Result As New List(Of Gas)

            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If


            Dim modelo As New ModeloProsper.Modelo()

            Dim TmpResult = modelo.Sensibilidad_BN(Qgi, Pwh, WC, QGI_Min, QGI_Max, FileD, FileName)

            If TmpResult.GetUpperBound(0) > 0 Then
                For i = 0 To TmpResult.GetUpperBound(1)
                    Result.Add(New Gas() With {.Xaux = TmpResult(i, 0), .Yaux = TmpResult(i, 1)})
                Next
            End If

            Return Result
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function

    Public Function Sensibilidad_BN(ByVal IdModPozo As String) As Boolean Implements IModelo.Sensibilidad_BN
        Try

            Dim modelo As New ModeloProsper.Modelo(IdModPozo)
            Dim inicio As DateTime = DateTime.Now

            Logger = New ModeloProsper.Logger(IdModPozo, modelo.ModPozo.CONFIGURACION_ADMINISTRADOR.SingleOrDefault().IDUSUARIO)

            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If

            Logger.SetEstatus(2)

            Dim License = My.Settings.LicensePROSPER


            Dim result = modelo.Sensibilidad_BN()

            If result Then
                Logger.SetEstatus(3, "Ejecución correcta(Solicitado)")
            Else
                Logger.SetEstatus(-1, "Hubo un error al realizar la operación")




            End If

            Return result
        Catch ex As Exception
            If Logger.Configuracion.ESTATUS = 2 Then
                Logger.SetEstatus(-1, ex.Message)
            End If

            Throw New Exception(ex.Message)
        End Try

    End Function
    ''Public Function Sensibilidad_BN(ByVal IdModPozo As String, ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double) As List(Of Gas) Implements IModelo.SensGas
    ''    Try
    ''        Dim Prosper = Settings.GetBy("open_server")

    ''        If Prosper = 0 Then
    ''            Throw New Exception("Open Server ocupado, verificar si se encuentra apagado o bloqueado")
    ''        End If
    ''        Dim Modelo As New ModeloProsper.Modelo(IdModPozo)
    ''        Dim ListGas As New List(Of Gas)


    ''        Dim Results = Modelo.Sensibilidad_BN(IdModPozo)

    ''        'For i = 0 To Results.GetUpperBound(1)
    ''        '    ListGas.Add(New Gas() With {.Xaux = Results(0, i), .Yaux = Results(1, i)})
    ''        'Next




    ''        Return ListGas

    ''    Catch ex As Exception
    ''        Throw New Exception(ex.Message)
    ''    End Try

    ''End Function

    Public Function Execute(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal File As Byte) As Boolean

        Return False
    End Function
    Public Sub Scheduled()

    End Sub
    Public Function Monitor(ByRef OpenServer As String) As List(Of String) Implements IModelo.Monitor
        Dim Prosper = Settings.GetBy("open_server")
        Dim Messages As New List(Of String)
        Dim Estatus() As Integer = {0, 1, 2, -1}

        Dim PxServer As Process() = Process.GetProcessesByName("pxserver")



        If OpenServer <> Prosper Then

            If PxServer.Length > 0 Then
                Messages.Add(String.Format("Open Server: {0} | {1} ", PxServer.Length, DateTime.Now.ToString()))
            Else
                Messages.Add(String.Format("Open Server: {0} | {1} ", "0", DateTime.Now.ToString()))
            End If


            Dim Modelos As New Dictionary(Of Integer, Integer) From {
                {1, 0},
                {2, 0}
            }
            Dim ListModelos = db.VW_MOD_POZO.Where(Function(w) Estatus.Contains(w.ESTATUS)).GroupBy(Function(g) g.ESTATUS).Select(Function(s) New With {.Total = s.Count(), .Descripcion = s.Key}).ToList()


            If ListModelos.Count > 0 Then
                For Each Mode In ListModelos
                    Modelos(Mode.Descripcion) = Mode.Total
                Next
            End If



            If Modelos.Count > 0 Then
                Dim Message As String = "En cola " + Modelos(1).ToString() + ", proceso  " + Modelos(2).ToString()
                Messages.Add(Message)
            End If

            OpenServer = Prosper
        End If






        Return Messages
    End Function

    Public Sub ShutDown() Implements IModelo.ShutDown
        ModeloProsper.Modelo.ShutDown()
    End Sub
End Class
