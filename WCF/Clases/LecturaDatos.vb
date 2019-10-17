Imports System.Data
Imports System.Threading
Imports ModeloProsper

Imports WCF.Generales

Public Class LecturaDatos
    Private GastoInyeccion() As Double
    Private tablacatalogo As DataTable
    Private MPrsp As ModeloProsper.Crea.Modelo
    Private Resu_SQL(1, 70) As String
    Private EdoMec_SQL(20, 15) As String
    Private Trayecto_SQL(20, 10) As String
    Private Temperatura_SQL(20, 10) As String
    Private NumDatEdoMec As Integer
    Private NumDatTrayecto As Integer
    Private NumDatTemp As Integer
    Private IDagujero As String
    Private fechaprueba As String
    Private IDMODPOZO As String = ""
    Public Sub New(GastoInyeccion() As Double, IDagujero As String, fechaprueba As String, IDMODPOZO As String)
        Me.GastoInyeccion = GastoInyeccion
        Me.IDagujero = IDagujero
        Me.fechaprueba = fechaprueba
        Me.IDMODPOZO = IDMODPOZO
    End Sub
    Public Function inicio() As String
        Dim resultado As String = ""
        Try
            tablacatalogo = objConsulta.getCatCorrelacion()
            'GuardarDatosGenerales()
            If Lectura_Datos() Then
                MPrsp = New ModeloProsper.Crea.Modelo(NumDatEdoMec, NumDatTrayecto, NumDatTemp)
                agregarDatosProsper(Resu_SQL, EdoMec_SQL, Trayecto_SQL, Temperatura_SQL) ' Se inicializan los datos de la base de datos
                ' Se realiza el modelo de Prosper y no hay error
                If MPrsp.MsgErrDatos.Length = 0 AndAlso MPrsp.ModeloProsper() Then
                    '-------------- Graficas---------------------
                    PreparaGraficaIPR()
                    PreparararGraficaCorrelacion()
                    PrepararVLPIPR_Graf()
                    prepararGasBNC()
                    prepararQuickLook()
                    prepararProductivilidad()
                Else
                    resultado = MPrsp.MsgErrDatos & ".-Error en el procesamiento de datos function ModeloProsper"
                End If
            Else
                resultado = "Datos sin registro"
            End If
        Catch ex As Exception
            resultado = ex.ToString
        End Try
        Return resultado
    End Function
    Private Function Lectura_Datos() As Boolean
        Dim resul As Boolean = False
        Try
            Resu_SQL = Analisis.ConvertirDatatableMatrizString(objConsulta.getResultadoModeloGeneral(IDMODPOZO)) 'ESTE SE VA A SUSTITUIR
            If Resu_SQL.GetUpperBound(0) >= 0 Then
                ' ESTADO MECANICO
                EdoMec_SQL = Analisis.ConvertirDatatableMatrizString(objConsulta.getResultadoMecanico(IDagujero, IDMODPOZO)) 'ESTO FALTA QUE INTEGRE DAVID
                NumDatEdoMec = EdoMec_SQL.GetUpperBound(0) '+ 1
                If NumDatEdoMec > 1 Then
                    Analisis.recortarNombre(EdoMec_SQL, 5, 15)
                    ' TRAYECTORIA
                    Trayecto_SQL = Analisis.seleccionPuntostrayectoria(objConsulta.getResultadoTrayectoria(IDagujero), EdoMec_SQL(EdoMec_SQL.GetUpperBound(0), 7))
                    NumDatTrayecto = Trayecto_SQL.GetUpperBound(0)
                    If NumDatTrayecto > 0 Then
                        objConsulta.guardarTrayectoria(Trayecto_SQL, IDMODPOZO) 'AQUI SE DEBERIA DE GUARDAR EN LA BASE DE DATOS LA TRAYECTORIA
                        ' TEMPERATURA
                        Dim tb As DataTable = objConsulta.getDatosFormacion(IDagujero, fechaprueba)
                        'Dim tb As DataTable = obBD.getDatosTemperatura(IDAGUJERO, fechaprueba)
                        If tb.Rows.Count > 0 Then
                            Dim b = Resu_SQL(Resu_SQL.GetUpperBound(0), 66) 'TEMPERATURA EN LA PRIMERA CABEZA
                            Dim a = ((tb.Rows(0).Item(1) - b) / (tb.Rows(0).Item(3) - Trayecto_SQL(0, 1)))
                            Temperatura_SQL = Analisis.obtenerTemperatura(Trayecto_SQL, a, b)
                            NumDatTemp = Temperatura_SQL.GetUpperBound(0)
                            objConsulta.guardarTemperatura(Temperatura_SQL, IDMODPOZO)
                            resul = True
                        End If
                    End If
                End If
            End If
        Catch ex As Exception
            Throw New Exception(ex.ToString)
        End Try
        Return resul
    End Function
    Private Sub agregarDatosProsper(ByVal Resu_SQL(,) As String, ByVal EdoMec_SQL(,) As String, ByVal Trayecto_SQL(,) As String, ByVal Temperatura_SQL(,) As String)
        Try
            ' Para el control de la ayuda, el diagnóstico y el rastreo de la Clase ModeloProsper
            MPrsp.ProgramPath = DireccionProper
            MPrsp.ArchivoPVT = DireccionPVT
            MPrsp.NomCorr = Analisis.getArreglo(tablacatalogo, 1, 0)
            inicializarDatosGenerales(Resu_SQL)
            'DATOS DE TRAYECTORIA
            InicializarDatosTrayectoria(Trayecto_SQL)
            ' MPrsp.RDEnable.Val = Trayecto_SQL
            'ESTADO MECANICO
            inicializarDatosMecanicos(EdoMec_SQL)
            'Array.Sort()
            'TEMPERATURA
            inicializarDatosTemperatura(Temperatura_SQL)
        Catch ex As Exception
            Throw New Exception(ex.ToString)
        End Try
    End Sub
    Private Sub PreparaGraficaIPR()
        With MPrsp
            Dim NdatAux As Integer = 19
            Dim Xaux(NdatAux), Yaux(NdatAux) As Double
            For J = 0 To NdatAux
                Xaux(J) = .VLPIPR_RTEL(0, J)
                Yaux(J) = .VLPIPR_PWF(0, J)
            Next
            'graficarIPR(._QTest, ._Ptest, .IPR_RTEL, .IPR_PWF, Xaux, Yaux, .NomVLPIPR)
            objConsulta.actualizarGrafica1(IDMODPOZO, .NomVLPIPR(0), .NomVLPIPR(1), .IPR_RTEL, .IPR_PWF, Xaux, Yaux)
        End With
    End Sub
    Private Sub PreparararGraficaCorrelacion()
        With MPrsp
            ReDim Preserve .PresWf(.PresWf.GetUpperBound(0), .NumDatCorr(0))
            ReDim Preserve .ProfDesa(.ProfDesa.GetUpperBound(0), .NumDatCorr(0))
            Dim PresWf(.PresWf.GetUpperBound(0), .PresWf.GetUpperBound(1)) As Double '= .PresWf
            Dim ProfDesa(.ProfDesa.GetUpperBound(0), .ProfDesa.GetUpperBound(1)) As Double '= .ProfDesa
            'Pendiente ordenar
            ordenarCorrelacion(.NumCor, .PresWf, .ProfDesa, PresWf, ProfDesa)
            objConsulta.actualizarGrafica2(IDMODPOZO, Analisis.getArreglo(tablacatalogo, 0, 0), PresWf, ProfDesa)
            'graficarCorrelacion(._Ptest, ._NivMedDisp, PresWf, ProfDesa, Analisis.getArreglo(tablacatalogo, 1, 0))
        End With
    End Sub
    Private Sub ordenarCorrelacion(ByVal orden() As Integer, ByRef PresWf_old(,) As Double, ByRef ProfDesa_old(,) As Double, ByRef PresWf(,) As Double, ByRef ProfDesa(,) As Double)
        Dim dt As New DataTable
        dt.Columns.Add()
        dt.Columns.Add()
        For i = 0 To orden.Length - 1
            For j = 0 To PresWf.GetUpperBound(1)
                PresWf(i, j) = PresWf_old(orden(i), j)
                ProfDesa(i, j) = ProfDesa_old(orden(i), j)
            Next
            Dim row = tablacatalogo.Rows(orden(i))
            dt.Rows.Add({row.Item(0), row.Item(1)})
        Next
        tablacatalogo = dt
    End Sub
    Private Sub PrepararVLPIPR_Graf()
        Dim NumDat As Integer = 20
        With MPrsp
            Dim NdatAux As Integer
            NdatAux = NumDat - 1
            Dim Xaux(NdatAux), Yaux(NdatAux) As Double
            For J = 0 To NdatAux
                Xaux(J) = .VLPIPR_RTEL(1, J)
                Yaux(J) = .VLPIPR_PWF(1, J)
            Next
            objConsulta.actualizarGrafica3(IDMODPOZO, Xaux, Yaux, .QliqVLP, .PwfVLP, GastoInyeccion)
            'graficaVLPIPR(._QTest, ._Ptest, Xaux, Yaux, .QliqVLP, .PwfVLP, .NomVLPIPR(1), ._GastoInyeccion)
        End With
    End Sub
    Private Sub prepararGasBNC()
        With MPrsp
            'Dim Qgi_Res() As Double = Analisis.getArregloCol(.CurvaRendi(), 0, 1)
            'Dim Qliq_Res() As Double = Analisis.getArregloCol(.CurvaRendi(), 1, 1)
            'objConsulta.actualizarGrafica4(IDMODPOZO, Qgi_Res, Qliq_Res)

        End With
    End Sub
    Private Sub prepararQuickLook()
        With MPrsp
            ReDim Preserve .PresQL(1, .NumDatGrad)
            ReDim Preserve .MSDQL(1, .NumDatGrad)
            ReDim Preserve .PresGG(.NumDatGG)
            ReDim Preserve .MSDGG(.NumDatGG)
            Dim PRESION1(.PresQL.GetUpperBound(1)), PROFMD1(.MSDQL.GetUpperBound(1)), PRESION2(.PresQL.GetUpperBound(1)), PROFMD2(.MSDQL.GetUpperBound(1)) As Double
            For I = 0 To .PresQL.GetUpperBound(1)
                PRESION1(I) = .PresQL(0, I)
                PROFMD1(I) = .MSDQL(0, I)
                PRESION2(I) = .PresQL(1, I)
                PROFMD2(I) = .MSDQL(1, I)
            Next
            Dim dt As DataTable = objConsulta.getCAT_MOD_POZO_QUICK()
            'objConsulta.actualizarGrafica5(IDMODPOZO, PRESION1, PROFMD1, PRESION2, PROFMD2, .PresGG, .MSDGG, .GGPres, .GGMSD, dt)
            'graficaQuickLook(PRESION1, PROFMD1, PRESION2, PROFMD2, .PresGG, .MSDGG, .GGPres, .GGMSD, dt)
        End With
    End Sub
    Private Sub prepararProductivilidad()
        With MPrsp
            'Dim Xaux(.Wc_Res.GetUpperBound(0)) As Double
            Dim Yaux(.Qliq_Res.GetUpperBound(1)) As Double
            Dim Qliq_Res(.Qliq_Res.GetUpperBound(0), .Qliq_Res.GetUpperBound(1)) As Double
            'TChart6.Series.Clear()
            For I = 0 To .Qliq_Res.GetUpperBound(0)
                For J = 0 To .Qliq_Res.GetUpperBound(1)
                    'Xaux(J) = .Wc_Res(J)
                    Yaux(J) = .Qliq_Res(I, J, 0)
                    Qliq_Res(I, J) = .Qliq_Res(I, J, 0) 'ESTE ES SOLO PARA ELIMINAR UNA DIMENSION QUE ESTA DE MAS
                Next
                'TChart6.Series.Add(getLinea())
                'valoresLinea(TChart6, I, .Wc_Res, Yaux, Math.Round(.Qgi_Res(I), 1).ToString)
            Next
            objConsulta.actualizarGrafica6(IDMODPOZO, .Wc_Res, Qliq_Res, .Qgi_Res)
        End With
    End Sub
    Private Sub inicializarDatosTemperatura(temperatura_SQL(,) As String)
        'MOD_POZO_TEMP
        For I = 0 To NumDatTemp
            ' Profundidad Desarrollada
            MPrsp.PTMd.Val(I) = temperatura_SQL(I, 0)
            ' Temperatura
            MPrsp.PTTmp.Val(I) = temperatura_SQL(I, 1)
        Next I
    End Sub

    Private Sub inicializarDatosMecanicos(edoMec_SQL(,) As String)
        ' Datos del Estado Mecánico
        '
        ' DoSet "PROSPER.SIN.EQP.Down.DATA.COUNT", DatosEdoMec   ' <---- Variable de salida

        ' EdoMec_SQL(0, 0): Nombre del pozo
        ' EdoMec_SQL(0, 1): Numero de agujero
        ' EdoMec_SQL(0, 2): Fecha del modelo
        ' EdoMec_SQL(0, 3): IDTUBERIAS
        ' EdoMec_SQL(0, 4): IDDATOSMODELOPROSPER
        'MOD_POZO_TUBERIA

        For I = 0 To NumDatEdoMec ' - 1
            ' Etiqueta
            MPrsp.Label(I) = edoMec_SQL(I, 5)
            ' Tipo de elemento ->
            '      0: E.M.R
            '      1: Tubing
            '      2: SSSV
            '      3: Restriction
            '      4: Casing
            If I > 0 Then MPrsp.DType.Val(I) = edoMec_SQL(I, 6)
            ' Profundidad desarrollada
            MPrsp.Depth.Val(I) = edoMec_SQL(I, 7)
            ' Diametro interior de la tuberia de produccion
            MPrsp.TID.Val(I) = edoMec_SQL(I, 8)
            ' Rugosidad del interior de la tuberia de produccion
            MPrsp.TIR.Val(I) = edoMec_SQL(I, 9)
            ' Diametro exterior de la tuberia de produccion
            MPrsp.TOD.Val(I) = edoMec_SQL(I, 10)
            ' Rugosidad del exterior de la tuberia de produccion
            MPrsp.TOR.Val(I) = edoMec_SQL(I, 11)
            ' Diametro interior de la tuberia de revestimiento
            MPrsp.CID.Val(I) = edoMec_SQL(I, 12)
            ' Rugosidad del interior de la tuberia de revestimiento
            MPrsp.CIR.Val(I) = edoMec_SQL(I, 13)

            'MsgBox("Elemento No:  " & I.ToString & Chr(13) & _
            '       "Etiqueta:     " & MPrsp.Label(I).ToString & Chr(13) & _
            '       "Tipo Tub:     " & MPrsp.DType(I).ToString & Chr(13) & _
            '       "Profund:      " & MPrsp.Depth(I).ToString & Chr(13) & _
            '       "Diam Int TP:  " & MPrsp.TID(I).ToString & Chr(13) & _
            '       "Rug Int TP:   " & MPrsp.TIR(I).ToString & Chr(13) & _
            '       "Diam Ext TP:  " & MPrsp.TOD(I).ToString & Chr(13) & _
            '       "Rug Ext TP:   " & MPrsp.TOR(I).ToString & Chr(13) & _
            '       "Diam Int TR:  " & MPrsp.CID(I).ToString & Chr(13) & _
            '       "Rug Int TR:   " & MPrsp.CIR(I).ToString & Chr(13))

        Next I


        '
    End Sub
    ' Registro de Desviaciones
    '
    ' Trayecto_SQL(0, 0): Nombre del pozo
    ' Trayecto_SQL(0, 1): Numero de agujero
    ' Trayecto_SQL(0, 2): Fecha del modelo
    ' Trayecto_SQL(0, 3): IDTRAYECTORIAPROSPER
    ' Trayecto_SQL(0, 4): IDDATOSMODELOPROSPER
    ' Trayecto_SQL(0, 5): PROFUNDIDAD DESARROLLADA
    ' Trayecto_SQL(0, 6): PROFUNDIDAD VERTICAL
    Private Sub InicializarDatosTrayectoria(trayecto_SQL(,) As String)
        ' AQUI VAN LOS DATOS DE LA TRAYECTORIA
        For I = 0 To NumDatTrayecto
            MPrsp.RDEnable.Val(I) = 1
            ' Profundidad Desarrollada
            'MPrsp.PROFUNDIDADMD.Val(I) = trayecto_SQL(I, 0)
            ' Profundidad Vertical
            'MPrsp.PROFUNDIDADMV.Val(I) = trayecto_SQL(I, 1)
        Next I
    End Sub

    Private Sub inicializarDatosGenerales(resu_SQL(,) As String)
        '
        MPrsp._Fluid = CByte(resu_SQL(0, 5))

        '
        ' PVTModel: 0 = Black Oil
        '           1 = Equation of State
        '
        MPrsp._PVTModel = CByte(resu_SQL(0, 6))

        '
        ' Separator: 0 = Single Stage Separator
        '            1 = Two Stage Separator
        '
        MPrsp._Separator = CByte(resu_SQL(0, 7))

        '
        ' Emulsion: 0 = No
        '           1 = Emulsion + Pump Viscosity Correction
        '
        MPrsp._Emulsion = CByte(resu_SQL(0, 8))

        '
        ' Hydrate: 0 = Disable Warning
        '          1 = Enable Warning
        '
        MPrsp._Hydrate = CByte(resu_SQL(0, 9))

        '
        ' WatVis: 0 = Use Default Correlation
        '         1 = Use Pressure Corrected Correlation
        '
        MPrsp._WatVis = CByte(resu_SQL(0, 10))

        '
        ' VisMod: 0 = Newtonian Fluid
        '         1 = Non Newtonian Fluid
        '
        MPrsp._VisMod = CByte(resu_SQL(0, 11))

        '
        ' FlowType: 0 = Tubing Flow
        '           1 = Annular Flow
        '
        MPrsp._FlowType = CByte(resu_SQL(0, 12))

        '
        ' WellType: 0 = Producer
        '           1 = Inyector
        '           2 = Water Inyector
        '
        MPrsp._WellType = CByte(resu_SQL(0, 13))

        '
        ' LiftMethod: 0 = None
        '             1 = Gas Lift (Continous)
        '             2 = Electrical Sumersible Pump
        '             3 = Hydraulic Drive Downhole Pump
        '             4 = Progressive Cavity Pump
        '             5 = Coiled Tubing Gas Lift
        '             6 = Diluent Injection
        '             7 = Jet Pump
        '             8 = MultiPhase Pump
        '             9 = Sucker Rod Pump
        '             10 = Gas Lift (Intermttent)
        '
        MPrsp._LiftMethod = CByte(resu_SQL(0, 14))

        '
        ' LiftType: 0 = No Friction Loss In Annulus
        '           1 = Friction Loss In Annulus
        '           2 = Safety Equipment
        '
        MPrsp._LiftType = CByte(resu_SQL(0, 15))

        '
        ' Predict: 0 = Pressure Only
        '          1 = Pressure and Temperature (offshore)
        '          2 = Pressure and Temperature (on Land)        '
        MPrsp._Predict = CByte(resu_SQL(0, 16))

        '
        ' TempModel: 0 = Rough Approximation
        '            1 = Enthalpy Balance
        '            2 = Improved Approximation
        MPrsp._TempModel = CByte(resu_SQL(0, 17))

        '
        ' RangeSystem: 0 = Full System
        '              1 = Pipeline Only
        MPrsp._RangeSystem = CByte(resu_SQL(0, 18))

        '
        ' OutputRes: 0 = Show Calculating Data
        '            1 = Hide Calculating Data
        MPrsp._OutputRes = CByte(resu_SQL(0, 19))

        '
        ' Completion: 0 = Cased Hole
        '             1 = Open hole
        MPrsp._Completion = CByte(resu_SQL(0, 20))

        '
        ' GravelPack: 0 = None
        '             1 = Gravel Pack
        '             2 = Pre Packed Screen
        '             3 = Wire Wrapped Screen
        '             4 = Slotted Liner
        MPrsp._GravelPack = CByte(resu_SQL(0, 21))

        '
        ' InflowType: 0 = Single Branch
        '             1 = MultiLateral Well
        MPrsp._InflowType = CByte(resu_SQL(0, 22))

        '
        ' GasConing: 0 = No
        '            1 = Yes
        MPrsp._GasConing = CByte(resu_SQL(0, 23))

        MPrsp.Company = resu_SQL(0, 24)
        MPrsp.Field = resu_SQL(0, 25)
        MPrsp.Locations = resu_SQL(0, 26)
        MPrsp.Well = resu_SQL(0, 27)
        MPrsp.Platform = resu_SQL(0, 28)
        MPrsp.Analyst = resu_SQL(0, 29)
        MPrsp.DatGenDate = resu_SQL(0, 30)

        MPrsp.Comenta = resu_SQL(0, 31)
        '
        ' IPR
        '
        MPrsp._IPRMethod = CByte(resu_SQL(0, 32))
        ' Datos
        ' Presión de Fondo Estática
        MPrsp._PRes = CDbl(resu_SQL(0, 33)) ' Kg/cm2
        ' Temperatura del Yacimiento
        MPrsp._TRes = CDbl(resu_SQL(0, 34))  ' oC
        ' Porcentaje de agua en la capa
        MPrsp._Wc = CDbl(resu_SQL(0, 35))   ' %
        ' Relación Gas Aceite tomada de la columna de los datos del yacimiento debido a que se debe filtrar.
        MPrsp._TotGor = CDbl(resu_SQL(0, 36))
        ' Modelo de reduccion de permeabilidad por compactación
        ' 0: No
        ' 1: Si
        MPrsp._Compact = CByte(resu_SQL(0, 37))
        ' Considerar las permeabilidades relativas
        ' 0: No
        ' 1: Si
        MPrsp._IRELK = CByte(resu_SQL(0, 38))
        ' Datos del modelo de Vogel
        '
        ' Producción de liquido
        MPrsp._QTest = CDbl(resu_SQL(0, 39)) ' bl/dia
        ' Presión de fondo fluyendo
        MPrsp._Ptest = CDbl(resu_SQL(0, 40)) ' Kg/cm2
        ' Datos del modelo de Darcy
        '
        ' Daño Mecánico / Geometrico
        ' 0: Enter Skin by Hand
        ' 1: Loke
        ' 2: MacLeod
        ' 3: Karakas - Tariq
        MPrsp._MGSkinMethod = CDbl(resu_SQL(0, 41))
        ' Daño por desviación y penetración parcial
        ' 0: Cinco / Martín-Bronz
        ' 1: Wong-Cliford
        ' 2: Cinco (2) / Martín-Bronz
        MPrsp._DPSkinMethod = CDbl(resu_SQL(0, 42))
        ' Permeabilidad del yacimiento
        MPrsp._ResPerm = CDbl(resu_SQL(0, 43))
        ' Espesor del yacimiento
        MPrsp._Thickness = CDbl(resu_SQL(0, 44))
        ' Area de drene
        MPrsp._Drainage = CDbl(resu_SQL(0, 45))
        ' Factor de forma
        MPrsp._Dietz = CDbl(resu_SQL(0, 46))
        ' Radio del pozo
        MPrsp._WBR = CDbl(resu_SQL(0, 47))
        ' Factor de daño total
        MPrsp._Skin = CDbl(resu_SQL(0, 48))
        '
        ' Datos del BNC
        '
        ' Emplear RGIL o Qgi
        '      0: Usar RGIL
        '      1: Usar Qgi
        MPrsp._Entry = CByte(resu_SQL(0, 49))
        ' Metodo de  calculo del BN.
        ' 0: Profundidad de Inyección Fija
        ' 1: Profundidad de Inyeccion Optima
        ' 2: Especificar la profundidad de las valvulas
        MPrsp._Method = CByte(resu_SQL(0, 50))
        ' Densidad del gas de bombeo neumatico
        MPrsp._Gravity = CDbl(resu_SQL(0, 51))
        '  Mol de H2S
        MPrsp._H2S = CDbl(resu_SQL(0, 52))
        '  Mol de CO2
        MPrsp._CO2 = CDbl(resu_SQL(0, 53))
        '  Mol de N2
        MPrsp._N2 = CDbl(resu_SQL(0, 54))
        ' Qgi
        MPrsp._GLRiny = CDbl(resu_SQL(0, 55))
        ' Realación gas inyectado liquido m3/m3
        MPrsp._GLRate = CDbl(resu_SQL(0, 56))
        ' Profundidad de la valvula de inyeccion
        MPrsp._ValveDepth = CDbl(resu_SQL(0, 57))
        ' Diametro de la válvula de BNC
        MPrsp._DiamValBNC = CDbl(resu_SQL(0, 58))
        'MPrsp._QgiMin = CDbl(resu_SQL(0, 64))
        'MPrsp._QgiMax = CDbl(resu_SQL(0, 65))
       ' MPrsp.GastosInyeccion = GastoInyeccion Depreciado
        ' Perfil de Temperatura (Gradiente Geotermico) 
        ' Temperatura_SQL(0, 0): Nombre del pozo
        ' Temperatura_SQL(0, 1): Numero de agujero
        ' Temperatura_SQL(0, 2): Fecha del modelo
        ' Temperatura_SQL(0, 3): IDTRAYECTORIAPROSPER
        ' Temperatura_SQL(0, 4): IDDATOSMODELOPROSPER
        ' Coeficiente de Transferencia de Calor Inicial
        MPrsp._Htc = resu_SQL(0, 59)
        ' Presion en la cabeza del pozo
        MPrsp._THPres = resu_SQL(0, 60)
        ' Temperatura en la cabeza del pozo (Fluyendo)
        MPrsp._THTemp = resu_SQL(0, 61)
        ' RGA del aforo
        MPrsp._RGA_Aforo = resu_SQL(0, 62)
        ' Presion en la TR (BNC)
        MPrsp._TRPres = resu_SQL(0, 63)
    End Sub
    Public Function FinalizarProceso(ByVal NOMBRE_PROCESO As String) As String
        Dim resultado As String = ""
        Try
            Thread.Sleep(TimeSpan.FromSeconds(5))
            Dim procesos = (From proc In Process.GetProcesses()
                            Where proc.ProcessName = NOMBRE_PROCESO
                            Select proc).ToList
            For Each p In procesos
                Dim perf As New PerformanceCounter("Process", "% Processor Time", p.ProcessName, True)
                perf.NextValue()
                If Math.Round(perf.NextValue() / Environment.ProcessorCount, 1) = 0.0 Then p.Kill()
            Next
        Catch ex As Exception
            resultado = ex.ToString
        End Try
        Return resultado
    End Function
End Class
