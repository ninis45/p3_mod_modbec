Imports ModeloProsper.Prosper
Imports ModeloProsper.Lagrange
Imports ModeloCI

Public Class UpdateModelo
    Public Property Server As Object
    Public Property ArchivoPVT As String

#Region "Parámetros"
    'Dim Prosper_Id As Object
    'Dim ProgramPath As String = "C:\Program Files (x86)\Petroleum Experts\IPM 11\prosper.EXE"
    Dim Archivo As String = "C:\Users\97029592\Desktop\Modelos de pozos\KU-H\Maloob-422.Out"

    Public Property LiftMethod As Integer
    'Parámetros de lectura
    'Private Property THP As New Double                 'Presión en la cabeza kg/cm2g
    Public Property TRES As Double
    Public Property PRES As Double


    Private Property Wc As New Double                  'Corte de agua %
    Private Property GOR_Tot As New Double             'Relación de producción gas-aceite m3/m3

    'Private Property Ql_estimado As New Double         'Gasto de líquido estimado
    Private Property Pws As New Double                 'Presión del yacimiento kg/cm2g

    '***********BEC*******************************************
    Private Property Frecuencia As New Double          'Frecuencia de operación Hz

    '***********BNC*******************************************
    Private Property Qgi As New Double                     'Gasto de inyección de gas MMpcd
    'Private Property PBN As New Double                     'Presión de inyección en cabeza de TR en kg/cm2

    '***********BEC*******************************************
    Private Property Quick_lookk As New List(Of QuickLook)
    Private Property Para_Corr As New List(Of ParametrosAjusteCorrelacion)
    Private Property Carta_pump As New List(Of Chart_Pump)
    Private Property Curva_Rend As New List(Of CurvaRendimiento)
    Public Property PresTR As New List(Of Presion_TR)
    Private Property BEC_Result As New List(Of Result_BEC)
    Private Property BN_Result As New List(Of Result_BN)


    Public Property QLBottom As Double(,)
    Public Property QLTop As Double(,)
    Public Property GralQuickLook As MOD_POZO_QUICK
    Public Property ResultBEC As Result_BEC

    'RESULTADOS QUICKLOOK (ENTRADA)
    '======================================================================
    Public Property QLiq As Double(,)
    Public Property THPD As Double
    Public Property THTD As Double
    Public Property PTR As Double
    Public Property QGT As Double
    Public Property DIAO As Double
    Public Property VALDEPTH As Double
    Public Property LiqRate As Double
    Public Property IndexTest As Integer

    'Public Property QuickLook As Double(,,)

    Public Property CartaX As Double(,)
    Public Property CartaY As Double(,)

    Public Property RangeX As Double(,)
    Public Property RangeY As Double(,)

    Public Property PumpsPoint As Double(,)
    Public Property FlagQLG As Integer = 0


#End Region
#Region "Propiedades Publicas"
    Public WriteOnly Property Presion_Cabeza As Double
        Set(value As Double)
            'THP = value
        End Set
    End Property
    Public WriteOnly Property Corte_Agua As Double
        Set(value As Double)
            Wc = value
        End Set
    End Property
    Public WriteOnly Property Frecuencia_BEC As Double
        Set(value As Double)
            Frecuencia = value
        End Set
    End Property
    Public WriteOnly Property GOR_Total As Double
        Set(value As Double)
            GOR_Tot = value
        End Set
    End Property

    Public ReadOnly Property Quick_Look As List(Of QuickLook)
        Get
            Return Quick_lookk
        End Get
    End Property
    Public ReadOnly Property Carta_Bomba As List(Of Chart_Pump)
        Get
            Return Carta_pump
        End Get
    End Property
    Public ReadOnly Property Curva_Rendimiento As List(Of CurvaRendimiento)
        Get
            Return Curva_Rend
        End Get
    End Property
    Public ReadOnly Property Presion_EA As List(Of Presion_TR)
        Get
            Return PresTR
        End Get
    End Property
    Public ReadOnly Property Resultado_BEC As List(Of Result_BEC)
        Get
            Return BEC_Result
        End Get
    End Property
    Public ReadOnly Property Resultado_BN As List(Of Result_BN)
        Get
            Return BN_Result
        End Get
    End Property

    'BN
    Public WriteOnly Property Qginy As Double
        Set(value As Double)
            Qgi = value
        End Set
    End Property
    'Public WriteOnly Property Ptr As Double
    '    Set(value As Double)
    '        PBN = value
    '    End Set
    'End Property



#End Region
    Sub New()

    End Sub

    Private Function ActualizaModeloBN() As Boolean
        Try
            Dim xx = (From x In Para_Corr
                      Where x.Parametro1 <> 1 Or x.parametro2 <> 1
                      Order By x.Des_Stand Ascending
                      Select x).ToList

            'GOR_Tot = RGA_PVT()

            GOR_Tot = DoGet("PROSPER.SIN.IPR.Single.totgor")

            If GOR_Tot = 0 Then GOR_Tot = RGA_PVT()

            DoSet("PROSPER.ANL.SYS.Pres", THPD)
            DoSet("PROSPER.ANL.SYS.WC", Wc)
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Tot)
            DoSet("PROSPER.ANL.SYS.TubingLabel", xx(0).Ncorr)
            DoSet("PROSPER.ANL.SYS.RateMethod", 2)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[0]", Qgi)
            DoCmd("PROSPER.ANL.SYS.CALC")

            LiqRate = DoGet("PROSPER.OUT.SYS.Results[0].Sol.LiqRate") 'PROSPER.OUT.SYS.Results[0].Sol.LiqRate
            THTD = DoGet("PROSPER.OUT.SYS.Results[0].Sol.WHTemperature")
            Dim Qgas As Double = DoGet("PROSPER.OUT.SYS.Results[0].Sol.GasRate")

            If THTD = 0 Then
                Qgi = DoGet("PROSPER.ANL.VMT.Data[" + IndexTest.ToString() + "].Irate")
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[0]", Qgi)
                DoCmd("PROSPER.ANL.SYS.CALC")

                LiqRate = DoGet("PROSPER.OUT.SYS.Results[0].Sol.LiqRate")
                THTD = DoGet("PROSPER.OUT.SYS.Results[0].Sol.WHTemperature")
            End If

            If THTD = 0 Then
                Throw New Exception("No se pudo realizar el calculo Qgi: " + Qgi.ToString())
            End If

            'Quicklook
            DoSet("PROSPER.ANL.QLG.Surface[0][0]", THPD)
            DoSet("PROSPER.ANL.QLG.Surface[1][0]", THTD)
            DoSet("PROSPER.ANL.QLG.Surface[2][0]", LiqRate)
            DoSet("PROSPER.ANL.QLG.Surface[3][0]", Wc)
            DoSet("PROSPER.ANL.QLG.Surface[4][0]", Qgas + Qgi)
            DoSet("PROSPER.ANL.QLG.Surface[5][0]", Qgi)
            DoSet("PROSPER.ANL.QLG.Surface[6][0]", PTR)
            DoSet("PROSPER.ANL.QLG.TubingLabel", xx(0).Ncorr)

            'Se realizan los calculos del Quick look
            DoCmd("PROSPER.ANL.QLG.CALC")

            FlagQLG = DoGet("PROSPER.OUT.QLG.DONE")

            Dim ProfValve_MV As Double
            Dim ProfValve_MD As Double = DoGet("PROSPER.ANL.QLG.Gaslift[1]")

            If FlagQLG = 1 Then
                For j = 0 To 1

                    Dim N As Integer = DoGet("PROSPER.OUT.QLG.Results[" & j.ToString & "].COUNT") - 1 'Abajo-arriba

                    For i = 0 To N
                        Dim sentido As String
                        If j = 0 Then sentido = 0 Else sentido = 1
                        Dim parametros As New QuickLook
                        parametros.Sentido = sentido
                        parametros.MD = DoGet("PROSPER.OUT.QLG.Results[" & j.ToString & "].MSD[" & i.ToString & "]")
                        parametros.MV = DoGet("PROSPER.OUT.QLG.Results[" & j.ToString & "].TVD[" & i.ToString & "]")
                        parametros.PRES = DoGet("PROSPER.OUT.QLG.Results[0" & j.ToString & "].Pres[" & i.ToString & "]")
                        parametros.TEMP = DoGet("PROSPER.OUT.QLG.Results[" & j.ToString & "].Temp[" & i.ToString & "]")
                        Quick_lookk.Add(parametros)
                        If (parametros.MD = ProfValve_MD) Then ProfValve_MV = parametros.MV
                    Next i

                    'QuickLook.Add(j, vals)
                Next j
            Else
                Throw New Exception("No hay quicklook para la condicion de operación.")
            End If


            Dim MD_TRgra(1) As Double
            Dim MV_TRgra(1) As Double
            Dim Pres_TRgra(1) As Double
            Dim PresTeo_TRgra(1) As Double

            MD_TRgra(0) = DoGet("PROSPER.OUT.QLG.Results[0].MSD[0]")
            MD_TRgra(1) = ProfValve_MD
            MV_TRgra(0) = DoGet("PROSPER.OUT.QLG.Results[0].TVD[0]")
            MV_TRgra(1) = ProfValve_MV
            Pres_TRgra(0) = DoGet("PROSPER.ANL.QLG.Surface[6][0]")
            Pres_TRgra(1) = DoGet("PROSPER.OUT.QLG.Output[11]")
            PresTeo_TRgra(0) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[0].Pres")
            PresTeo_TRgra(1) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[1].Pres")

            For i = 0 To 1
                Dim Ptr As New Presion_TR
                Ptr.Prof_MV = MV_TRgra(i)
                Ptr.Prof_MD = MD_TRgra(i)
                Ptr.Pres = Pres_TRgra(i)
                Ptr.Pres_Teo = PresTeo_TRgra(i)
                PresTR.Add(Ptr)
            Next i
            '.Gasto_Superficie = Ql_estimado,
            GralQuickLook = New MOD_POZO_QUICK() With {
                .QGI = Qgi,
                .QLIQ = LiqRate,
                .PBF = DoGet("PROSPER.OUT.QLG.Output[13]"),
                .PBE = DoGet("PROSPER.OUT.QLG.Output[12]"),
                .PVAL = DoGet("PROSPER.OUT.QLG.Output[0]"),
                .PCA = DoGet("PROSPER.OUT.QLG.Output[11]"),
                .TVAL = DoGet("PROSPER.OUT.QLG.Output[1]"),
                .GOR = DoGet("PROSPER.OUT.QLG.Output[2]"),
                .GORFREE = DoGet("PROSPER.OUT.QLG.Output[3]"),
                .DPVAL = DoGet("PROSPER.OUT.QLG.Output[4]"),
                .PTRTEO = DoGet("PROSPER.OUT.QLG.Output[5]"),
                .DPYAC = DoGet("PROSPER.OUT.QLG.Output[6]"),
                .IP = DoGet("PROSPER.OUT.QLG.Output[7]"),
                .TFCAL = DoGet("PROSPER.OUT.QLG.Output[8]"),
                .TFPER = DoGet("PROSPER.OUT.QLG.Output[9]")
            }


            'Sensibilidad del Qgi
            Dim inc As Double = Qgi * 1.5 / 19
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            For i = 0 To 19
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" + i.ToString + "]", inc * i)
                'DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", QgiVec(i))
            Next i
            DoCmd("PROSPER.ANL.SYS.CALC")
            Dim CurvaRendimientio(1, 19)

            Dim Results = Integer.Parse(DoGet("PROSPER.OUT.SYS.Results.COUNT"))
            ReDim QLiq(1, 19)

            For i = 0 To Results - 1
                QLiq(0, i) = inc * i
                QLiq(1, i) = DoGet("PROSPER.OUT.SYS.Results[" + i.ToString + "].Sol.LiqRate")
            Next i



            Return True
        Catch ex As Exception
            Throw New Exception("BN: " + ex.Message)
        End Try
    End Function

    Private Function ActualizaModeloBEC() As Boolean
        Try
            Dim Carga_Bombs As New Double         'Carga generada por la bomba al gasto dado m
            Dim Gasto_PyT_bomba As New Double     'Gasto de liquido a condiciones de presión y temperatura de la bomba bpd
            Dim Bomba_Prof As New Double          'Profundidad de la  bomba md
            Dim Num_Etapa As New Double           'Numero de etapas de la bomba
            Dim Fac_desgaste As New Double        'Factor de desgaste de la bomba fracción
            Dim Efi_SepGas As New Double          'Eficiencia de separador de gas frac.

            Dim xx = (From x In Para_Corr
                      Where x.Parametro1 <> 1 Or x.parametro2 <> 1
                      Order By x.Des_Stand Ascending
                      Select x).ToList()

            If xx.Count = 0 Then
                Throw New Exception("No hay correlaciones")
            End If
            If GOR_Tot = 0 Then GOR_Tot = DoGet("PROSPER.SIN.IPR.Single.totgor")

            'Análisis de sensibilidad de variables
            DoSet("PROSPER.ANL.SYS.Pres", PRES)
            DoSet("PROSPER.ANL.SYS.WC", Wc)
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Tot)
            DoSet("PROSPER.ANL.SYS.TubingLabel", xx(0).Ncorr)
            'Selección de la frecuencia
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 35) 'OJO, VERIFICAR SI REALMENTE ES 35
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[148].Vals[0]", Frecuencia)
            DoCmd("PROSPER.ANL.SYS.CALC")
            LiqRate = DoGet("PROSPER.OUT.SYS.Results[0].Sol.LiqRate")
            Pws = DoGet("PROSPER.SIN.IPR.Single.Pres")
            Bomba_Prof = DoGet("PROSPER.SIN.ESP.Depth")
            Efi_SepGas = DoGet("PROSPER.SIN.ESP.Efficiency")
            Num_Etapa = DoGet("PROSPER.SIN.ESP.Stages")
            Fac_desgaste = DoGet("PROSPER.SIN.ESP.Wear")
            Carga_Bombs = DoGet("PROSPER.OUT.SYS.Results[0].Sol.PumpHead")
            Gasto_PyT_bomba = DoGet("PROSPER.OUT.SYS.Results[0].Sol.PumpAvRate")

            'PROSPER.ANL.QLE.Quick[3]

            ReDim CartaX(5, 99)
            ReDim CartaY(5, 99)
            ReDim RangeX(3, 99)
            ReDim RangeY(3, 99)
            For j = 0 To 99
                CartaX(0, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[0][" + j.ToString() + "]")
                CartaX(1, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[1][" + j.ToString() + "]")
                CartaX(2, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[2][" + j.ToString() + "]")
                CartaX(3, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[3][" + j.ToString() + "]")
                CartaX(4, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[4][" + j.ToString() + "]")
                CartaX(5, j) = DoGet("PROSPER.OUT.ESP.PUMPRT[5][" + j.ToString() + "]")

                CartaY(0, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[0][" + j.ToString() + "]") * Num_Etapa
                CartaY(1, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[1][" + j.ToString() + "]") * Num_Etapa
                CartaY(2, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[2][" + j.ToString() + "]") * Num_Etapa
                CartaY(3, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[3][" + j.ToString() + "]") * Num_Etapa
                CartaY(4, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[4][" + j.ToString() + "]") * Num_Etapa
                CartaY(5, j) = DoGet("PROSPER.OUT.ESP.PUMPHD[5][" + j.ToString() + "]") * Num_Etapa
                'Dim curvas As New Chart_Pump
                'curvas.Hz35_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[0][" & j.ToString & "]")
                'curvas.Hz35_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[0][" & j.ToString & "]") * Num_Etapa
                'curvas.Hz40_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[1][" & j.ToString & "]")
                'curvas.Hz40_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[1][" & j.ToString & "]") * Num_Etapa
                'curvas.Hz50_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[2][" & j.ToString & "]")
                'curvas.Hz50_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[2][" & j.ToString & "]") * Num_Etapa
                'curvas.Hz60_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[3][" & j.ToString & "]")
                'curvas.Hz60_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[3][" & j.ToString & "]") * Num_Etapa
                'curvas.Hz70_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[4][" & j.ToString & "]")
                'curvas.Hz70_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[4][" & j.ToString & "]") * Num_Etapa
                'curvas.Hz75_Gasto = DoGet("Prosper.OUT.ESP.PUMPRT[5][" & j.ToString & "]")
                'curvas.Hz75_HEAD = DoGet("PROSPER.OUT.ESP.PUMPHD[5][" & j.ToString & "]") * Num_Etapa

                If (j <= 5) Then
                    RangeX(0, j) = DoGet("PROSPER.OUT.ESP.PMINRT[" + j.ToString() + "]")
                    RangeX(1, j) = DoGet("PROSPER.OUT.ESP.PMAXRT[" + j.ToString() + "]")
                    RangeX(2, j) = DoGet("PROSPER.OUT.ESP.PUMBER[" + j.ToString() + "]")

                    RangeY(0, j) = DoGet("PROSPER.OUT.ESP.PMINHD[" + j.ToString() + "]") * Num_Etapa
                    RangeY(1, j) = DoGet("PROSPER.OUT.ESP.PMAXHD[" + j.ToString() + "]") * Num_Etapa
                    RangeY(2, j) = DoGet("PROSPER.OUT.ESP.PUMBEH[" + j.ToString() + "]") * Num_Etapa
                    '    curvas.Min_GastOper = DoGet("Prosper.OUT.ESP.PMINRT[" & j.ToString & "]")
                    '    curvas.Min_HeadOper = DoGet("Prosper.OUT.ESP.PMINHD[" & j.ToString & "]") * Num_Etapa
                    '    curvas.Max_GastOper = DoGet("Prosper.OUT.ESP.PMAXRT[" & j.ToString & "]")
                    '    curvas.Max_HeadOper = DoGet("Prosper.OUT.ESP.PMAXHD[" & j.ToString & "]") * Num_Etapa
                    '    curvas.Mej_EfecGast = DoGet("Prosper.OUT.ESP.PUMBER[" & j.ToString & "]")
                    '    curvas.Mej_EfecHead = DoGet("Prosper.OUT.ESP.PUMBEH[" & j.ToString & "]") * Num_Etapa
                    'Else
                    '    RangeX(0, j) = -1
                    '    RangeX(1, j) = -1
                    '    RangeX(2, j) = -1

                    '    RangeY(0, j) = -1
                    '    RangeY(1, j) = -1
                    '    RangeY(2, j) = -1
                End If

                'Carta_pump.Add(curvas)
            Next j

            'Quick look
            DoSet("PROSPER.ANL.QLE.TubingLabel", xx(0).Ncorr)
            DoSet("PROSPER.ANL.QLE.Quick[0]", THPD)
            DoSet("PROSPER.ANL.QLE.Quick[1]", LiqRate)
            DoSet("PROSPER.ANL.QLE.Quick[2]", Wc)
            DoSet("PROSPER.ANL.QLE.Quick[3]", GOR_Tot)
            DoSet("PROSPER.ANL.QLE.Quick[4]", Pws)
            DoSet("PROSPER.ANL.QLE.Quick[5]", Bomba_Prof)
            DoSet("PROSPER.ANL.QLE.Quick[6]", Frecuencia)
            DoSet("PROSPER.ANL.QLE.Quick[7]", Bomba_Prof)
            DoSet("PROSPER.ANL.QLE.Quick[8]", Efi_SepGas)
            DoSet("PROSPER.ANL.QLE.Quick[9]", Num_Etapa)
            DoSet("PROSPER.ANL.QLE.Quick[10]", Fac_desgaste)
            'Se realizan los calculos del Quick look
            DoCmd("PROSPER.ANL.QLE.CALC")
            For j = 0 To 1
                Dim N As Integer = DoGet("PROSPER.OUT.QLE.Results[" + j.ToString + "].COUNT") - 1 'Abajo-arriba
                For i = 0 To N
                    Dim sentido As Integer
                    If j = 0 Then sentido = 0 Else sentido = 1
                    Dim parametros As New QuickLook
                    parametros.Sentido = sentido
                    parametros.MD = DoGet("PROSPER.OUT.QLE.Results[" & j.ToString & "].MSD[" & i.ToString & "]")
                    parametros.MV = DoGet("PROSPER.OUT.QLE.Results[" & j.ToString & "].TVD[" & i.ToString & "]")
                    parametros.PRES = DoGet("PROSPER.OUT.QLE.Results[" & j.ToString & "].Pres[" & i.ToString & "]")
                    parametros.TEMP = DoGet("PROSPER.OUT.QLE.Results[" & j.ToString & "].Temp[" & i.ToString & "]")
                    Quick_lookk.Add(parametros)
                Next i
            Next j
            Dim indBomba As Integer = DoGet("PROSPER.SIN.ESP.DESPUMP")
            'GralQuickLook = New MOD_POZO_QUICK() With {
            '.Bomba = DoGet("PROSPER.ESP.PUMPNAME(" & indBomba.ToString & ")"),
            '.Potencia_Requerida_Bomba = DoGet("PROSPER.OUT.QLE.Output[0]"),
            '.Voltaje_Requerido = DoGet("PROSPER.OUT.QLE.Output[3]"),
            '.Presion_Succion = DoGet("PROSPER.OUT.QLE.Output[4]"),
            '.Presion_Descarga = DoGet("PROSPER.OUT.QLE.Output[5]"),
            '.Gasto_Superficie = LiqRate,
            '.Gasto_Bomba = Gasto_PyT_bomba,
            '.Gasto_Fondo = DoGet("PROSPER.OUT.QLE.Output[6]"),
            '.GasLibre_Bomba = DoGet("PROSPER.OUT.QLE.Output[7]"),
            '.Carga_Bomba = Carga_Bombs,
            '.Torque_Flecha = DoGet("PROSPER.OUT.QLE.Output[9]"),
            '.Delta_Temperatura = DoGet("PROSPER.OUT.QLE.Output[10]")
            '}
            ''result = True
            'BEC_Result.Add(resul)
            GralQuickLook = New MOD_POZO_QUICK() With {
                .BC_PPR = DoGet("PROSPER.OUT.QLE.Output[0]"),
                .BC_SVOLT = DoGet("PROSPER.OUT.QLE.Output[3]"),
                .BC_PINPRES = DoGet("PROSPER.OUT.QLE.Output[4]"),
                .BC_PDPRES = DoGet("PROSPER.OUT.QLE.Output[5]"),
                .QLIQ = LiqRate,
                .BC_DRATE = DoGet("PROSPER.OUT.QLE.Output[6]"),
                .BC_GASFREE = DoGet("PROSPER.OUT.QLE.Output[7]"),
                .BC_TOSH = DoGet("PROSPER.OUT.QLE.Output[9]"),
                .BC_DACRP = DoGet("PROSPER.OUT.QLE.Output[10]"),
                .BC_PHEAD = DoGet("PROSPER.OUT.SYS.Results[0].Sol.PumpHead"),
                .BC_PRATE = DoGet("PROSPER.OUT.SYS.Results[0].Sol.PumpAvRate")
            }

            Return True
        Catch ex As Exception
            Throw New Exception("BEC: " + ex.Message)
        End Try
    End Function

    Function Execute() As Boolean
        Try
            Me.Server = Connect()
            DoCmd("PROSPER.OPENFILE=""" & ArchivoPVT & """")
            DoCmd("PROSPER.SETUNITSYS(""Pemex"")")

            'Buscar el test
            Dim TotalTests As Integer = DoGet("PROSPER.ANL.VMT.DATA.COUNT")
            For i = 0 To TotalTests - 1

                Dim Enabled As Integer = DoGet("PROSPER.ANL.VMT.Data[" + i.ToString() + "].Enable")

                If Enabled = 0 Then
                    IndexTest = i
                    Exit For
                End If

            Next


            'Asegurar que la correlación ajustada sea la seleccionada
            For i = 0 To 23
                Dim corr As New ParametrosAjusteCorrelacion
                corr.Correlacion = CStr(DoGet("PROSPER.ANL.COR.CORR[" & i.ToString & "].NAME"))
                corr.Ncorr = i
                corr.Parametro1 = DoGet("PROSPER.ANL.COR.CORR[" & i.ToString & "].A[0]")
                corr.parametro2 = DoGet("PROSPER.ANL.COR.CORR[" & i.ToString & "].A[1]")
                corr.Des_Stand = corr.Parametro1 * corr.parametro2
                Para_Corr.Add(corr)
            Next i

            LiftMethod = DoGet("PROSPER.SIN.SUM.LiftMethod")


            Dim Result As Boolean
            Select Case LiftMethod
                Case 1
                    Result = ActualizaModeloBN()
                Case 2
                    Result = ActualizaModeloBEC()
                Case Else
                    Throw New Exception("No existe el sistema artificial")

            End Select
            Disconnect()
            Return Result
        Catch ex As Exception
            Disconnect()
            Throw New Exception("Update Modelo: " + ex.Message)
        End Try
    End Function

    Function RGA_PVT() As Double
        Dim RGA As Double = 0
        Try

            DoSet("PROSPER.PVT.Calc.TempMin", TRES)
            DoSet("PROSPER.PVT.Calc.TempMax", TRES)
            DoSet("PROSPER.PVT.Calc.TempStep", 1)

            DoSet("PROSPER.PVT.Calc.PresMin", PRES)
            DoSet("PROSPER.PVT.Calc.PresMax", PRES)
            DoSet("PROSPER.PVT.Calc.PresStep", 1)

            DoCmd("PROSPER.PVT.CALC")

            RGA = DoGet("PROSPER.PVT.Calc.Results[0].GOR")

        Catch ex As Exception
            RGA = 0
        End Try

        Return RGA

    End Function
    Class QuickLook
        Property Sentido As Integer
        Property MD As New Double
        Property MV As New Double
        Property PRES As New Double
        Property TEMP As New Double
    End Class
    Class ParametrosAjusteCorrelacion
        Property Correlacion As String
        Property Ncorr As New Integer
        Property Parametro1 As New Double
        Property parametro2 As New Double
        Property Des_Stand As New Double
    End Class
    Class CurvaRendimiento
        Property Gas_BN As New Double
        Property Qliq As New Double
    End Class
    Class Presion_TR
        Property Prof_MD As New Double
        Property Prof_MV As New Double
        Property Pres As New Double
        Property Pres_Teo As New Double
    End Class
    Class Chart_Pump
        Property Hz35_Gasto As New Double
        Property Hz35_HEAD As New Double
        Property Hz40_Gasto As New Double
        Property Hz40_HEAD As New Double
        Property Hz50_Gasto As New Double
        Property Hz50_HEAD As New Double
        Property Hz60_Gasto As New Double
        Property Hz60_HEAD As New Double
        Property Hz70_Gasto As New Double
        Property Hz70_HEAD As New Double
        Property Hz75_Gasto As New Double
        Property Hz75_HEAD As New Double
        Property Min_GastOper As New Double
        Property Min_HeadOper As New Double
        Property Max_GastOper As New Double
        Property Max_HeadOper As New Double
        Property Mej_EfecGast As New Double
        Property Mej_EfecHead As New Double
    End Class

    Class Result_BEC
        Property Bomba As String                        'Nombre de la bomba
        Property Potencia_Requerida_Bomba As New Double 'Potencia requerida para la bomba en HP
        Property Voltaje_Requerido As New Double        'Voltaje requerido en Volts
        Property Presion_Succion As New Double          'Presión de succión en kg/cm2
        Property Presion_Descarga As New Double         'Presión de descarga en kg/cm2
        Property Gasto_Superficie As New Double         'Producción en superficie en STBPD
        Property Gasto_Bomba As New Double              'Producción a nivel de la bomba en bpd
        Property Gasto_Fondo As New Double              'Producción a BH en bpd
        Property GasLibre_Bomba As New Double           'Gas libre en la bomba en V/V
        Property Carga_Bomba As New Double              'Carga de la bomba en m
        Property Torque_Flecha As New Double            'Torque en la fleche en N-m
        Property Delta_Temperatura As New Double        'Cambio de temperatura en la bomba en °C
    End Class

    Class Result_BN
        Property Gasto_Superficie As New Double         'Producción en superficie en STBPD
        Property Pwf As New Double                      'Presión de fondo fluyente en kg/cm2
        Property PWS As New Double                      'Presión de fondo cerrado en kg/cm2
        Property Ptp_Valvula As New Double              'Presión en la Tp a nivel de la válvula en kg/cm2
        Property Ptr_Valvula As New Double              'Presión en la Tr a nivel de la válvula en kg/cm2
        Property Temp_Valvula As New Double             'Temperatura a nivel de la válvula en °C
        Property GOR As New Double                      'Relación gas disuelto-aceite en m3/m3
        Property GOR_Libre As New Double                'Relación gas libre-aceite en m3/m3
        Property Dp_Valvula As New Double               'Caida de presión a travez de la válvula kg/cm2
        Property Ptr_teorica As New Double              'Presión calculada en la TR en kg/cm2
        Property Dp_fondo As New Double                 'Caida de presión en el fondo del pozo kg/cm2
        Property IP As New Double                       'Indice de productividad stbpd/psi
        Property Gasto_Critico As New Double            'Gasto de inyección crítico % MMSCFPD
        Property Porcentaje_Qcritico As New Double      'Porcentaje de flujo crítico en la valvula en %
    End Class
End Class



