Imports ModeloProsper.Prosper
Namespace Clases

    ''' <summary>Primer algoritmo de Estabilidad del Dr. Iván</summary>

    Public Class MapaEstabilidad




#Region "Parametros"
        Dim Server As Object
        Dim AppName As String
        Dim Nmapa As Integer
        Public DATTR(,) As Double
        Public IPR_VLP(,) As Double
        Private Prof() As Double
        Private Vol() As Double
        Private Ptr_Incr As Double
        Private Orif_Incr As Double
        Private Pwh_Incr As Double
        Private Qginy_Incr As Double
        Private Dvalvula_Incr As Double
        Private Fw_Incr As Double
        Private MD As Double
        Private TVD As Double
        Private DTR As Double
        Private DTP As Double
        Private datosPVT(29, 3) As Double
        Public recomendaciones As String
        Property ListaParametrosEstabilidad As New List(Of Parametros_Estabilidad)
        Public ArchivoPvt As String

        'Parametros de ajuste de KTZ
        Property Par1 As Double      'Parámetro (multiplicador) de ajuste para la correlación de Kartoatmodjo [adim]
        Property Par2 As Double      'Parámetro (sumando) de ajuste para la correlación de Kartoatmodjo [pie3/bl]
        Property Pexp As Double      'Exponente característico de la correlación de Kartoatmodjo

        Property Drginj As Double    'Densidad relativa del gas de inyección (aire=1)
        Property Qmax As Double      'Gasto máximo de líquido que puede aportar el yacimiento [bpd]
        Property API As Double       'Densidad API del aceite
        Property Drg As Double       'Densidad relativa del gas de la formación (aire=1)

        'Estado Mecanico
        Private Elemento_EM(0) As Integer
        Private MD_EM(0) As Double
        Private TVD_EM(0) As Double
        Private TPID_EM(0) As Double
        Private TPIDrugo_EM(0) As Double
        Private TPOD_EM(0) As Double
        Private TPODrugo_EM(0) As Double
        Private TRID_EM(0) As Double
        Private TRIDrugo_EM(0) As Double
        Private TPVol_EM(0) As Double
        Private EAVol_EM(0) As Double
        Private MD_trayec As Double()
        Private MV_trayec As Double()

        Property Ql As Double

#End Region

#Region "Propiedades"
        Private _Ptr_min As New MaxMinDouble
        Public WriteOnly Property Ptr_min As Double
            Set(ByVal value As Double)
                _Ptr_min.Val = value
            End Set
        End Property

        Private _Ptr_max As New MaxMinDouble
        Public WriteOnly Property Ptr_max As Double
            Set(ByVal value As Double)
                _Ptr_max.Val = value
            End Set
        End Property

        Private _Ptr_num As New MaxMinInteger
        Public WriteOnly Property Ptr_num As Integer
            Set(ByVal value As Integer)
                _Ptr_num.Val = value
            End Set
        End Property


        Private _Pwh_min As New MaxMinDouble
        Public WriteOnly Property Pwh_min As Double
            Set(ByVal value As Double)
                _Pwh_min.Val = value
            End Set
        End Property

        Private _Pwh_max As New MaxMinDouble
        Public WriteOnly Property Pwh_max As Double
            Set(ByVal value As Double)
                _Pwh_max.Val = value
            End Set
        End Property

        Private _Pwh_num As New MaxMinInteger
        Public WriteOnly Property Pwh_num As Integer
            Set(ByVal value As Integer)
                _Pwh_num.Val = value
            End Set
        End Property

        Private _Qginy_Min As New MaxMinDouble
        Public WriteOnly Property Qginy_Min As Double
            Set(ByVal value As Double)
                _Qginy_Min.Val = value
            End Set
        End Property

        Private _Qginy_Max As New MaxMinDouble
        Public WriteOnly Property Qginy_Max As Double
            Set(ByVal value As Double)
                _Qginy_Max.Val = value
            End Set
        End Property

        Private _Qginy_Num As New MaxMinInteger
        Public WriteOnly Property Qginy_Num As Integer
            Set(ByVal value As Integer)
                _Qginy_Num.Val = value
            End Set
        End Property


        Private _Dvalvula_Min As New MaxMinDouble
        Public WriteOnly Property Dvalvula_Min As Double
            Set(ByVal value As Double)
                _Dvalvula_Min.Val = value
            End Set
        End Property

        Private _Dvalvula_Max As New MaxMinDouble
        Public WriteOnly Property Dvalvula_Max As Double
            Set(ByVal value As Double)
                _Dvalvula_Max.Val = value
            End Set
        End Property

        Private _Dvalvula_Num As New MaxMinInteger
        Public WriteOnly Property Dvalvula_Num As Integer
            Set(ByVal value As Integer)
                _Dvalvula_Num.Val = value
            End Set
        End Property

        Private _Orif_Min As New MaxMinDouble
        Public WriteOnly Property Orif_Min As Double
            Set(ByVal value As Double)
                _Orif_Min.Val = value
            End Set
        End Property

        Private _Orif_Max As New MaxMinDouble
        Public WriteOnly Property Orif_Max As Double
            Set(ByVal value As Double)
                _Orif_Max.Val = value
            End Set
        End Property

        Private _Orif_Num As New MaxMinInteger
        Public WriteOnly Property Orif_Num As Integer
            Set(ByVal value As Integer)
                _Orif_Num.Val = value
            End Set
        End Property

        Private _Fw_Min As New MaxMinDouble
        Public WriteOnly Property Fw_Min As Double
            Set(ByVal value As Double)
                _Fw_Min.Val = value
            End Set
        End Property

        Private _Fw_Max As New MaxMinDouble
        Public WriteOnly Property Fw_Max As Double
            Set(ByVal value As Double)
                _Fw_Max.Val = value
            End Set
        End Property

        Private _Fw_Num As New MaxMinInteger
        Public WriteOnly Property Fw_Num As Integer
            Set(ByVal value As Integer)
                _Fw_Num.Val = value
            End Set
        End Property


        Private _Cd As New MaxMinDouble
        Public WriteOnly Property Cd As Double
            Set(ByVal value As Double)
                _Cd.Val = value
            End Set
        End Property
        '***************************************************************************************************************************************
        'Propiedades del árbol de decisión
        Private _Ptr_Act As New MaxMinDouble
        Public WriteOnly Property Ptr_Act() As Double
            Set(ByVal value As Double)
                _Ptr_Act.Val = value
            End Set
        End Property

        Private _Pwh_Act As New MaxMinDouble
        Public WriteOnly Property Pwh_Act() As Double
            Set(ByVal value As Double)
                _Pwh_Act.Val = value
            End Set
        End Property

        Private _Qgi_Act As New MaxMinDouble
        Public WriteOnly Property Qgi_Act() As Double
            Set(ByVal value As Double)
                _Qgi_Act.Val = value
            End Set
        End Property

        Private _Dvalve_Act As New MaxMinDouble
        Public WriteOnly Property Dvalve_Act() As Double
            Set(ByVal value As Double)
                _Dvalve_Act.Val = value
            End Set
        End Property

        Private _DiaValve_Act As New MaxMinDouble
        Public WriteOnly Property DiaValve_Act() As Double
            Set(ByVal value As Double)
                _DiaValve_Act.Val = value
            End Set
        End Property

        Private _Wc_Act As New MaxMinDouble
        Public WriteOnly Property Wc_Act() As Double
            Set(ByVal value As Double)
                _Wc_Act.Val = value
            End Set
        End Property

        Private _Caso As New MaxMinInteger
        Public WriteOnly Property Caso As Double
            Set(ByVal value As Double)
                _Caso.Val = value
            End Set
        End Property
#End Region

        Function GENERAL() As Boolean
            '********************************************************************************
            '* SUBRUTINA PRINCIPAL DEL PROGRAMA DE CÓMPUTO PARA LA GENERACIÓN DE MAPAS      *
            '* DE ESTABILIDAD DE FLUJO EN POZOS CON BOMBEO NEUMÁTICO CONTINUO PRODUCIENDO   *
            '* EN YACIMIENTOS SATURADOS. LOS MAPAS SON GENERADOS CON BASE EN EL CRITERIO DE *
            '* ESTABILIDAD PROPUESTO POR FAIRUZOV Y GUERRERO (2004). EL PROGRAMA OBTIENE    *
            '* DE PROSPER LOS PARÁMETROS DE INTERÉS EN EL SISTEMA DE BN MEDIANTE LAS        *
            '* FUNCIONES PÚBLICAS DE OPEN SERVER.                                           *
            '* VERSIÓN 6.5 (JULIO, 2004): EN EL PROGRAMA ORIGINAL SE INCLUYÓ UNA SUBRUTINA  *
            '* PARA LA GENERACIÓN DE LAS CURVAS CARACTERÍSTICAS DEL SISTEMA DE BN.          *
            '********************************************************************************

            '**********************      IMPORTANTE    ************************************
            '*  EL archivo ".OUT" de PROSPER debe estar abierto; la opción "PRESSURE LOST *
            '*  IN ANNULUS" activa, y el sistema de unidades en "OILFIELD UNITES".        *
            '*  Antes de iniciar las simulaciones debe verificarse que los modelos en     *
            '*  PROSPER sean correctos. El modelo del yacimiento que debe utilizarse es   *
            '*  el de Vogel. En el módulo "DOWNHOLE EQUIPMENT" la descripción del pozo    *
            '*  debe ingresarse en estricto orden descendente, asimismo la información    *
            '*  del giroscopio en el módulo "DEVIATION SURVEY".                           *
            '******************************************************************************

            'Listado de las variables principales en el programa
            Try
                Dim Twf As Double       'Temp. en el fondo del pozo [°C]
                Dim RGAf As Double      'Relación gas de formación - aceite [bl/bl]
                Dim Prate As Double     'Presión de fondo  fluyendo (aforo) [kg/cm2]
                Dim Qrate As Double     'Gasto de aceite (aforo), [bpd]
                Dim Pyac As Double      'Presión del yacimiento [kg/cm2]
                Dim dro As Double       'Densidad relativa del aceite


                'Parametros para optimización
                Dim F1s(_Qginy_Num.Val - 1) As Double
                Dim F2s(_Qginy_Num.Val - 1) As Double
                Dim Ptrteoricas(_Qginy_Num.Val - 1) As Double
                If (_Ptr_num.Val > 1) Then Ptr_Incr = (_Ptr_max.Val - _Ptr_min.Val) / (_Ptr_num.Val - 1)
                If (_Orif_Num.Val > 1) Then Orif_Incr = (_Orif_Max.Val - _Orif_Min.Val) / (_Dvalvula_Num.Val - 1)

                'Lee datos de entrada para generar el mapa de estabilidad
                '**********************************************************************

                'Inicia el enlace con Prosper
                '**********************************************************************
                'Server = CreateObject("PX32.OpenServer.1")
                'AppName = "PROSPER"


                Connect()
                If ArchivoPvt IsNot Nothing Then
                    DoCmd("PROSPER.OPENFILE=""" & ArchivoPvt & """")
                End If

                DoCmd("PROSPER.SETUNITSYS(""Pemex"")")

                Drginj = Val(DoGet("PROSPER.SIN.GLF.GRAVITY"))

                'Lee datos de aforo para construir el modelo de Vogel
                '************************************************************************
                Pyac = Val(DoGet("PROSPER.SIN.IPR.SINGLE.PRES"))
                Prate = Val(DoGet("PROSPER.SIN.IPR.SINGLE.PTEST"))
                Qrate = Val(DoGet("PROSPER.SIN.IPR.SINGLE.QTEST"))
                Twf = Val(DoGet("PROSPER.SIN.IPR.SINGLE.TRES"))

                'Calcula el gasto máximo para el modelo de Vogel
                '************************************************************************
                Qmax = Qrate / (1 - 0.2 * (Prate / Pyac) - 0.8 * (Prate / Pyac) ^ 2)

                'Obtiene de Prosper las propiedades de los fluidos
                '************************************************************************
                RGAf = Val(DoGet("PROSPER.PVT.INPUT.SOLGOR"))
                Drg = Val(DoGet("PROSPER.PVT.INPUT.GRVGAS"))
                API = Val(DoGet("PROSPER.PVT.INPUT.API"))
                dro = 141.5 / (131.5 + API)

                'Determinación de los parametros de estado mecanico
                '************************************************************************
                Estado_Mecanico()

                'Realiza el ajuste de la correlación de Kartoatmodjo
                '************************************************************************
                AJUS_KART(Drg, API, RGAf, Twf, Par1, Par2, Pexp)

                SYSTEM()
                PerfilTP_Quicklook()
                Disconnect()
                Return True
            Catch ex As Exception
                Disconnect()
                Throw New Exception(ex.Message)

            End Try


        End Function

        Sub New(ByVal Ptr_Min As Double, ByVal Ptr_Max As Double, ByVal Ptr_Num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_Num As Integer, ByVal Qginy_Min As Double, ByVal Qginy_Max As Double, ByVal Qginy_Num As Integer, ByVal Dvalvula_Min As Double, ByVal Dvalvula_Max As Double, ByVal Dvalvula_Num As Integer, ByVal Orif_Min As Double, ByVal Orif_Max As Double, ByVal Orif_Num As Double, ByVal Fw_Min As Double, ByVal Fw_Max As Double, ByVal Fw_Num As Double, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer)
            'Ptr_min,                 Ptr_max,                          Ptr_num,                  Pwh_Min,                 Pwh_Max,           ByVal Pwh_num As Integer, ByVal Qgi_min As Double,   ByVal Qgi_max As Double,   ByVal Qgi_num As Integer,   ByVal Dvalve_min As Double,   ByVal Dvalve_max As Double,   ByVal Dvalve_num As Integer,   ByVal DiaVal_min As ,     ByVal DiaVal_max  Double, ByVal DiaVal_num Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act , ByVal Pwh_Act, ByVal Qgi_Act, ByVal Dvalve_Act, DiaValve_Act, Wc_Act, ByVal Caso) ' ByVal Ptr_Act, ByVal Pwh_Act, ByVal Qgi_Act, ByVal Dvalve_Act, DiaValve_Act, Wc_Act, ByVal Caso)
            Me.Ptr_min = Ptr_Min '
            Me.Ptr_max = Ptr_Max
            Me.Ptr_num = Ptr_Num
            Me.Pwh_min = Pwh_Min
            Me.Pwh_max = Pwh_Max
            Me.Pwh_num = Pwh_Num
            Me.Qginy_Min = Qginy_Min
            Me.Qginy_Max = Qginy_Max
            Me.Qginy_Num = Qginy_Num
            Me.Dvalvula_Min = Dvalvula_Min
            Me.Dvalvula_Max = Dvalvula_Max
            Me.Dvalvula_Num = Dvalvula_Num
            Me.Orif_Min = Orif_Min
            Me.Orif_Max = Orif_Max
            Me.Orif_Num = Orif_Num
            Me.Fw_Min = Fw_Min
            Me.Fw_Max = Fw_Max
            Me.Fw_Num = Fw_Num
            Me.Cd = Cd
            Me.Ptr_Act = Ptr_Act
            Me.Pwh_Act = Pwh_Act
            Me.Qgi_Act = Qgi_Act
            Me.Dvalve_Act = Dvalve_Act
            Me.DiaValve_Act = DiaValve_Act
            Me.Wc_Act = Wc_Act
            Me.Caso = Caso
        End Sub

        Public Sub Arbol_Decision(ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer)
            Dim rec As String
            Dim unidades As String
            Dim Ptr_down As Double = _Ptr_min.Val
            Dim Ptr_up As Double = _Ptr_min.Val
            Dim Pwh_down As Double = _Pwh_min.Val
            Dim Pwh_up As Double = _Pwh_min.Val
            Dim Qgi_down As Double = _Qginy_Min.Val
            Dim Qgi_up As Double = _Qginy_Min.Val
            Dim Dvalve_down As Double = _Dvalvula_Min.Val
            Dim Dvalve_up As Double = _Dvalvula_Min.Val
            Dim Diavalve_down As Double = _Orif_Min.Val
            Dim Diavalve_up As Double = _Orif_Min.Val
            Dim Wc_down As Double = _Fw_Min.Val
            Dim Wc_up As Double = _Fw_Min.Val
            Dim i As Integer
            Dim dif As Double = 100
            Dim dif2 As Double = 100

            For i = 0 To _Ptr_num.Val - 1
                Dim ptraux As Double = _Ptr_min.Val + i * Ptr_Incr '_Ptr_incremento.Val
                Format(ptraux, "0.00000")
                If ((Ptr_Act - ptraux) > 0 And (Ptr_Act - ptraux) < dif) Then
                    Ptr_down = ptraux
                    dif = Ptr_Act - ptraux
                End If
                If ((ptraux - Ptr_Act) >= 0 And (ptraux - Ptr_Act) <= dif2) Then
                    Ptr_up = ptraux
                    dif2 = ptraux - Ptr_Act
                End If
            Next i

            dif = 100
            dif2 = 100
            For i = 0 To _Pwh_num.Val - 1
                Dim pwhaux As Double = _Pwh_min.Val + i * Pwh_Incr '_Pwh_incremento.Val
                Format(pwhaux, "0.00000")
                If ((Pwh_Act - pwhaux) >= 0 And (Pwh_Act - pwhaux) <= dif) Then
                    Pwh_down = pwhaux
                    dif = Pwh_Act - pwhaux
                End If
                If ((pwhaux - Pwh_Act) >= 0 And (pwhaux - Pwh_Act) <= dif2) Then
                    Pwh_up = pwhaux
                    dif2 = pwhaux - Pwh_Act
                End If
            Next i
            dif = 100
            dif2 = 100

            For i = 0 To _Qginy_Num.Val - 1
                Dim Qgiaux As Double = _Qginy_Min.Val + i * Qginy_Incr
                Format(Qgiaux, "0.00000")
                If ((Qgi_Act - Qgiaux) >= 0 And (Qgi_Act - Qgiaux) <= dif) Then
                    Qgi_down = Qgiaux
                    dif = Qgi_Act - Qgiaux
                End If
                If ((Qgiaux - Qgi_Act) >= 0 And (Qgiaux - Qgi_Act) <= dif2) Then
                    Qgi_up = Qgiaux
                    dif2 = Qgiaux - Qgi_Act
                End If
            Next i

            dif = 100
            dif2 = 100
            For i = 0 To _Dvalvula_Num.Val - 1
                Dim Dvalveaux As Double = _Dvalvula_Min.Val + i * Dvalvula_Incr
                Format(Dvalveaux, "0.00000")
                If ((Dvalve_Act - Dvalveaux) >= 0 And (Dvalve_Act - Dvalveaux) <= dif) Then
                    Dvalve_down = Dvalveaux
                    dif = Dvalve_Act - Dvalveaux
                End If
                If ((Dvalveaux - Dvalve_Act) >= 0 And (Dvalveaux - Dvalve_Act) <= dif2) Then
                    Dvalve_up = Dvalveaux
                    dif2 = Dvalveaux - Dvalve_Act
                End If
            Next i
            dif = 100
            dif2 = 100
            For i = 0 To _Orif_Num.Val - 1
                Dim Diavalaux As Double = _Orif_Min.Val + i * Orif_Incr
                Format(Diavalaux, "0.00000")
                If ((DiaValve_Act - Diavalaux) >= 0 And (DiaValve_Act - Diavalaux) <= dif) Then
                    Diavalve_down = Diavalaux
                    dif = DiaValve_Act - Diavalaux
                End If
                If ((Diavalaux - DiaValve_Act) >= 0 And (Diavalaux - DiaValve_Act) <= dif2) Then
                    Diavalve_up = Diavalaux
                    dif2 = Diavalaux - DiaValve_Act
                End If
            Next i
            dif = 100
            dif2 = 100
            For i = 0 To _Fw_Num.Val - 1
                Dim Wcaux As Double = _Fw_Min.Val + i * Fw_Incr
                Format(Wcaux, "0.00000")
                If ((Wc_Act - Wcaux) >= 0 And (Wc_Act - Wcaux) <= dif) Then
                    Wc_down = Wcaux
                    dif = Wc_Act - Wcaux
                End If
                If ((Wcaux - Wc_Act) >= 0 And (Wcaux - Wc_Act) <= dif2) Then
                    Wc_up = Wcaux
                    dif2 = Wcaux - Wc_Act
                End If
            Next i

            Dim parametrosDown As List(Of Parametros_Estabilidad)
            Dim parametrosUp As List(Of Parametros_Estabilidad)

            Dim parametrosQgiDown = (From parQgi In ListaParametrosEstabilidad
                                     Where parQgi.Qgi = Qgi_down
                                     Select parQgi).ToList()
            Dim parametrosQgiUp = (From parQgi In ListaParametrosEstabilidad
                                   Where parQgi.Qgi = Qgi_up
                                   Select parQgi).ToList()


            Select Case Caso
                Case 0   'variando Ptr
                    parametrosDown = (From param In ListaParametrosEstabilidad
                                      Where param.Ptr = Ptr_down
                                      Select param).ToList()
                    parametrosUp = (From param In ListaParametrosEstabilidad
                                    Where param.Ptr = Ptr_up
                                    Select param).ToList()
                    rec = "Presión en la Tuberia de Revestimiento"
                    unidades = "kg/cm2"
                Case 1   'variando pwh
                    parametrosDown = (From param In ListaParametrosEstabilidad
                                      Where param.Pwh = Pwh_down
                                      Select param).ToList()
                    parametrosUp = (From param In ListaParametrosEstabilidad
                                    Where param.Pwh = Pwh_up
                                    Select param).ToList()
                    rec = "Presión en la Cabeza del Pozo"
                    unidades = "kg/cm2"
                Case 2   'variando Dvalve
                    parametrosDown = (From param In ListaParametrosEstabilidad
                                      Where param.Dvalve = Dvalve_down
                                      Select param).ToList()
                    parametrosUp = (From param In ListaParametrosEstabilidad
                                    Where param.Dvalve = Dvalve_up
                                    Select param).ToList()
                    rec = "Profundidad de la Válvula de Bombeo Neumático"
                    unidades = "mD"
                Case 3   'variando Diavalve
                    parametrosDown = (From param In ListaParametrosEstabilidad
                                      Where param.DiaVal = Diavalve_down
                                      Select param).ToList()
                    parametrosUp = (From param In ListaParametrosEstabilidad
                                    Where param.DiaVal = Diavalve_up
                                    Select param).ToList()
                    rec = "Diámetro de la Válvula de Bombeo Neumático"
                    unidades = "64avos"
                Case Else   'variando Wc
                    parametrosDown = (From param In ListaParametrosEstabilidad
                                      Where param.Wc = Wc_down
                                      Select param).ToList()
                    parametrosUp = (From param In ListaParametrosEstabilidad
                                    Where param.Wc = Wc_up
                                    Select param).ToList()
                    rec = "Corte de Agua"
                    unidades = "%"
            End Select

            Dim local = (From d1 In parametrosDown
                         Join d2 In parametrosUp On d2.Qgi Equals d1.Qgi
                         Where d2.Qgi = Qgi_down Or d2.Qgi = Qgi_up _
                     Or d1.Qgi = Qgi_down Or d1.Qgi = Qgi_up
                         Select d2, d1).ToList

            Dim cnt As Integer = 0

            For i = 0 To 1
                If (local(i).d1.Ind_Est = 2) Then
                    cnt += 1
                End If
                If (local(i).d2.Ind_Est = 2) Then
                    cnt += 1
                End If
            Next i



            If (cnt = 4) Then
                'Estable
            End If

            'En caso de que no se encuentre en un zona estable

            Dim Nelementos As Integer = parametrosDown.Count  'Toma el numero de elementos que conforman la lista de parametrosDown
            Dim Qgi_rec As Double
            Dim DifQgi As Double = 100

            For i = 0 To Nelementos - 1
                If (parametrosDown(i).Ind_Est = 2 And parametrosUp(i).Ind_Est = 2) Then
                    If (Math.Abs(parametrosDown(i).Qgi - Qgi_Act) < DifQgi) Then
                        Qgi_rec = parametrosDown(i).Qgi
                        DifQgi = Math.Abs(parametrosDown(i).Qgi - Qgi_Act)
                    End If
                End If
            Next i

            Dim Nelem As Integer = parametrosQgiDown.Count 'Toma el numero de elementos que conforma la lista de parametrosQgiDown
            'Dim ind_posición As Integer
            Dim Par_rec As Double
            Dim diferencia As Double = 100

            Select Case Caso
                Case 0   'variando Ptr
                    Par_rec = Ptr_Act
                    For i = 0 To Nelem - 1
                        If (parametrosQgiDown(i).Ind_Est = 2 And parametrosQgiUp(i).Ind_Est = 2) Then
                            If (Math.Abs(parametrosQgiDown(i).Ptr - Ptr_Act) < diferencia) Then
                                Par_rec = parametrosQgiDown(i).Ptr
                                diferencia = Math.Abs(parametrosQgiDown(i).Ptr - Ptr_Act)
                            End If
                        End If
                    Next i
                Case 1   'variando pwh
                    Par_rec = Pwh_Act
                    For i = 0 To Nelem - 1
                        If (parametrosQgiDown(i).Ind_Est = 2 And parametrosQgiUp(i).Ind_Est = 2) Then
                            If (Math.Abs(parametrosQgiDown(i).Pwh - Pwh_Act) < diferencia) Then
                                Par_rec = parametrosQgiDown(i).Pwh
                                diferencia = Math.Abs(parametrosQgiDown(i).Pwh - Pwh_Act)
                            End If
                        End If
                    Next i
                Case 2   'variando Dvalve
                    Par_rec = Dvalve_Act
                    For i = 0 To Nelem - 1
                        If (parametrosQgiDown(i).Ind_Est = 2 And parametrosQgiUp(i).Ind_Est = 2) Then
                            If (Math.Abs(parametrosQgiDown(i).Dvalve - Dvalve_Act) < diferencia) Then
                                Par_rec = parametrosQgiDown(i).Dvalve
                                diferencia = Math.Abs(parametrosQgiDown(i).Dvalve - Dvalve_Act)
                            End If
                        End If
                    Next i
                Case 3   'variando Diavalve
                    Par_rec = DiaValve_Act
                    For i = 0 To Nelem - 1
                        If (parametrosQgiDown(i).Ind_Est = 2 And parametrosQgiUp(i).Ind_Est = 2) Then
                            If (Math.Abs(parametrosQgiDown(i).DiaVal - DiaValve_Act) < diferencia) Then
                                Par_rec = parametrosQgiDown(i).DiaVal
                                diferencia = Math.Abs(parametrosQgiDown(i).DiaVal - DiaValve_Act)
                            End If
                        End If
                    Next i
                Case Else   'variando Wc
                    Par_rec = Wc_Act
                    For i = 0 To Nelem - 1
                        If (parametrosQgiDown(i).Ind_Est = 2 And parametrosQgiUp(i).Ind_Est = 2) Then
                            If (Math.Abs(parametrosQgiDown(i).Wc - Wc_Act) < diferencia) Then
                                Par_rec = parametrosQgiDown(i).Wc
                                diferencia = Math.Abs(parametrosQgiDown(i).Wc - Wc_Act)
                            End If
                        End If
                    Next i
            End Select
            recomendaciones = "Se Recomienda Mantener un Gasto de Inyección de Gas de Bombeo Neumático de " & Qgi_rec.ToString() & " MMSCF, con " & rec & " de " & Par_rec.ToString & " " & unidades & " "
        End Sub

        Private Sub Estado_Mecanico()
            Dim cont As Integer = DoGet("PROSPER.SIN.EQP.DOWN.DATA.COUNT")
            Dim Ndesv As Integer = DoGet("PROSPER.SIN.EQP.DEVN.DATA.COUNT")
            Dim Ncomp As Integer
            ReDim MD_trayec(Ndesv - 1)
            ReDim MV_trayec(Ndesv - 1)

            For i = 0 To Ndesv - 1
                MD_trayec(i) = DoGet("PROSPER.SIN.EQP.Devn.Data[" & CStr(i) & "].Md")
                MV_trayec(i) = DoGet("PROSPER.SIN.EQP.Devn.Data[" & CStr(i) & "].Tvd")
            Next i

            For i = 0 To cont - 1
                Dim componente As Integer = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].Type")
                If componente = 2 Or componente = 3 Or componente = 4 Then Continue For
                ReDim Preserve Elemento_EM(Ncomp)
                ReDim Preserve MD_EM(Ncomp)
                ReDim Preserve TPID_EM(Ncomp)
                ReDim Preserve TPIDrugo_EM(Ncomp)
                ReDim Preserve TPOD_EM(Ncomp)
                ReDim Preserve TPODrugo_EM(Ncomp)
                ReDim Preserve TRID_EM(Ncomp)
                ReDim Preserve TRIDrugo_EM(Ncomp)
                ReDim Preserve TPVol_EM(Ncomp)
                ReDim Preserve EAVol_EM(Ncomp)
                ReDim Preserve TVD_EM(Ncomp)
                Elemento_EM(Ncomp) = componente
                MD_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].Depth")
                TPID_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].TID")
                TPIDrugo_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].TIR")
                TPOD_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].TOD")
                TPODrugo_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].TOR")
                TRID_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].CID")
                TRIDrugo_EM(Ncomp) = DoGet("PROSPER.SIN.EQP.Down.Data[" & CStr(i) & "].CIR")
                Ncomp += 1
            Next i

            For i = 1 To Ncomp - 1
                Dim deltaMd As Double = (MD_EM(i) - MD_EM(i - 1))
                Dim voltp As Double = 0.00064516 * Math.PI * deltaMd * TPID_EM(i) ^ 2 / 4
                Dim VolEA As Double = 0.00064516 * Math.PI * deltaMd * (TRID_EM(i) ^ 2 - TPOD_EM(i) ^ 2) / 4
                TPVol_EM(i) = voltp + TPVol_EM(i - 1)
                EAVol_EM(i) = VolEA + EAVol_EM(i - 1)
                TVD_EM(i) = Vector(MD_trayec, MV_trayec, MD_EM(i))
            Next

        End Sub

        Class Parametros_Estabilidad
            Property Variable As String
            Property Ptr As Double
            Property Pwh As Double
            Property Qgi As Double
            Property Dvalve As Double
            Property DiaVal As Double
            Property Wc As Double
            Property GOR As Double
            Property Ql As Double
            Property Qg As Double
            Property Pwf_IPR As Double
            Property Twh As Double
            Property TotalQgas As Double
            Property Pwf_quicklook As Double
            Property Twf As Double
            Property Pws As Double
            Property Pti As Double
            Property Ptri As Double
            Property Tvalv As Double
            Property GOR_quicklook As Double
            Property GORFREE As Double
            Property Dpvalve As Double
            Property Ptrcalc As Double
            Property Dp As Double
            Property PI As Double
            Property Qgcrit As Double
            Property Qcporcent As Double
            Property HTC As Double
            Property Ind_Est As Integer       '
            Property Ind_Poblano As Integer   '
            Property Bgfi As Double
            Property Bgfreei As Double
            Property Bginyi As Double
            Property Boi As Double
            Property LiqVsp As Double
            Property GasVsp As Double
            Property IDtp As Double
            Property DensidaMezclai As Double
            Property DenGasBN As Double
            Property F1 As Double
            Property F2 As Double
            Property F1_Asheim As Double
            Property F2_Asheim As Double
        End Class


        Private Function getLista(ByVal arreglo() As Double) As List(Of Double)

            Dim result As New List(Of Double)

            For Each elem In arreglo
                result.Add(elem)
            Next

            Return result
        End Function

        Sub AJUS_KART(ByRef drg As Double, ByRef API As Double, ByRef RGAf As Double, ByRef Tyac As Double, ByRef A As Double, ByRef B As Double, ByRef C As Double)

            'Subrutina que calcula los parámetros de ajuste de la correlación
            'de Kartoatmodjo para la relación de solubilidad. Se utiliza regresión
            'no lineal. La correlación se ajusta a los datos generados en Prosper
            'para la correlación PVT activa.
            '*********************************************************************
            Dim Rsk(30) As Double
            Dim Rsm(30) As Double
            Dim Jac(30, 2) As Double
            Dim Fjac(30) As Double
            Dim Xsol(30) As Double
            Dim JT(2, 30)
            Dim JTJ(2, 2) As Double
            Dim TF(2) As Double
            Dim PHI(30) As Double
            Dim PRESPVT(30) As Double

            Dim Pbur As Double
            Dim NELEM As Integer
            Dim Knucleo As Double

            'Calcula la presión de burbujeo
            '********************************************************
            DoSet("PROSPER.PVT.CALC.MODE", 1)
            DoSet("PROSPER.PVT.CALC.PRESUSER[0]", 70)
            DoSet("PROSPER.PVT.CALC.TEMPUSER[0]", Tyac)
            DoCmd("PROSPER.PVT.CALC")
            Pbur = Val(DoGet("PROSPER.PVT.CALC.RESULTS[0].BUBPNT"))

            DoSet("PROSPER.PVT.CALC.MODE", 0)
            DoSet("PROSPER.PVT.CALC.TEMPSTEP", 1)
            DoSet("PROSPER.PVT.CALC.TEMPMIN", Tyac)
            DoSet("PROSPER.PVT.CALC.TEMPMAX", Tyac)
            DoSet("PROSPER.PVT.CALC.PRESSTEP", 30)
            DoSet("PROSPER.PVT.CALC.PRESMIN", 7)
            DoSet("PROSPER.PVT.CALC.PRESMAX", Pbur)
            DoCmd("PROSPER.PVT.CALC")
            Dim suma As Double = 0
            For i = 1 To 30
                Rsm(i) = Val(DoGet("PROSPER.PVT.CALC.RESULTS[" & CStr(i - 1) & "].GOR"))
                PRESPVT(i) = Val(DoGet("PROSPER.PVT.CALC.RESULTS[" & CStr(i - 1) & "].PRES"))
            Next

            'Elimina datos repetidos en términos de GOR
            '**************************************************************
            Dim cont = 0
            For i = 30 To 2 Step -1
                If Rsm(i) = Rsm(i - 1) Then
                    cont = cont + 1
                End If
            Next

            NELEM = 30 - cont
            If API <= 30 Then
                Knucleo = (0.05958 * drg ^ 0.7972) * 10 ^ ((13.1405 * API) / ((Tyac) * 9 / 5 + 32 + 460))
                C = 1.0014
            Else
                Knucleo = (0.0315 * drg ^ 0.7587) * 10 ^ ((11.289 * API) / ((Tyac) * 9 / 5 + 32 + 460))
                C = 1.0937
            End If
            For i = 1 To NELEM
                Rsk(i) = ((PRESPVT(i) * 14.2233 + 14.7) ^ C * Knucleo) / 5.615
            Next

            A = 1.0
            B = 1.0

            Dim iter As Integer = 1

            'iterar:
            Do
                For i = 1 To NELEM
                    PHI(i) = A * Rsk(i) + B - Rsm(i)
                    Jac(i, 1) = 2 * PHI(i) * Rsk(i)
                    Jac(i, 2) = 2 * PHI(i)
                    Fjac(i) = -PHI(i) ^ 2
                Next
                For i = 1 To NELEM
                    For j = 1 To 2
                        JT(j, i) = Jac(i, j)
                    Next
                Next
                Dim SUMA1 As Double = 0
                Dim SUMA2 As Double = 0
                Dim suma3 As Double = 0
                For i = 1 To NELEM
                    SUMA1 = SUMA1 + Jac(i, 1) ^ 2
                    SUMA2 = SUMA2 + Jac(i, 2) ^ 2
                    suma3 = suma3 + Jac(i, 1) * Jac(i, 2)
                Next
                JTJ(1, 1) = SUMA1
                JTJ(1, 2) = suma3
                JTJ(2, 1) = suma3
                JTJ(2, 2) = SUMA2

                SUMA1 = 0
                SUMA2 = 0
                For i = 1 To NELEM
                    SUMA1 = SUMA1 + Jac(i, 1) * Fjac(i)
                    SUMA2 = SUMA2 + Jac(i, 2) * Fjac(i)
                Next

                TF(1) = SUMA1
                TF(2) = SUMA2

                Dim DETER = JTJ(1, 1) * JTJ(2, 2) - JTJ(1, 2) * JTJ(2, 1)
                Xsol(1) = TF(1) * JTJ(2, 2) - TF(2) * JTJ(1, 2)
                Xsol(1) = Xsol(1) / DETER
                Xsol(2) = TF(2) * JTJ(1, 1) - TF(1) * JTJ(2, 1)
                Xsol(2) = Xsol(2) / DETER

                If A <> 0 And B <> 0 Then
                    suma = Math.Abs(Xsol(1) / A) + Math.Abs(Xsol(2) / B)
                Else
                    If A = 0 Then suma = suma + Math.Abs(Xsol(2) / B)
                    If B = 0 Then suma = suma + Math.Abs(Xsol(1) / A)
                End If

                If iter < 100 Then
                    If suma > 0.0001 Then
                        A = A + Xsol(1)
                        B = B + Xsol(2)
                        iter = iter + 1
                    Else
                        Exit Do
                    End If
                Else
                    MsgBox("Se presentaron problemas de convergencia" & vbCrLf & "en el módulo de ajuste de la correlación de Kartoamodjo")
                End If
            Loop While (iter < 100)

            For i = 1 To NELEM
                datosPVT(i - 1, 0) = PRESPVT(i)
                datosPVT(i - 1, 1) = Rsm(i)
                datosPVT(i - 1, 2) = Rsk(i)
                datosPVT(i - 1, 3) = A * Rsk(i) + B
            Next i

        End Sub

        Private Sub PerfilTP_Quicklook()
            Dim ncont As Integer = ListaParametrosEstabilidad.Count
            For i = _Qginy_Num.Val To ncont - 1
                Dim F1aux As Double
                Dim F2aux As Double
                Dim F1_Asheim As Double
                Dim F2_Asheim As Double
                Dim Ind_Est As Integer
                Dim Ind_Poblano As Integer
                Dim Pwh As Double = ListaParametrosEstabilidad(i).Pwh
                Dim Twh As Double = ListaParametrosEstabilidad(i).Twh
                Dim Qliq As Double = ListaParametrosEstabilidad(i).Ql
                Dim Wc As Double = ListaParametrosEstabilidad(i).Wc
                Dim TotalGasRate As Double = ListaParametrosEstabilidad(i).TotalQgas + ListaParametrosEstabilidad(i).Qgi
                Dim QgasIny As Double = ListaParametrosEstabilidad(i).Qgi
                Dim Ptr As Double = ListaParametrosEstabilidad(i).Ptr
                Dim DiaOrif As Double = ListaParametrosEstabilidad(i).DiaVal
                Dim Profvalve As Double = ListaParametrosEstabilidad(i).Dvalve

                If (Qliq > 0) Then
                    DoSet("PROSPER.ANL.QLG.Surface[0][0]", Pwh)
                    DoSet("PROSPER.ANL.QLG.Surface[1][0]", Twh)
                    DoSet("PROSPER.ANL.QLG.Surface[2][0]", Qliq)
                    DoSet("PROSPER.ANL.QLG.Surface[3][0]", Wc)
                    DoSet("PROSPER.ANL.QLG.Surface[4][0]", TotalGasRate)
                    DoSet("PROSPER.ANL.QLG.Surface[5][0]", QgasIny)
                    DoSet("PROSPER.ANL.QLG.Surface[6][0]", Ptr)
                    DoSet("PROSPER.ANL.QLG.InjPoint", 0)
                    DoSet("PROSPER.ANL.QLG.Gaslift[0]", DiaOrif)
                    DoSet("PROSPER.ANL.QLG.Gaslift[1]", Profvalve)
                    DoCmd("PROSPER.ANL.QLG.CALC")

                    Dim ind As Integer = 1
                    Do
                        Dim ind_valvula As Double = DoGet("PROSPER.OUT.QLG.Results[1].GasLif[" & ind.ToString & "]")
                        If ind_valvula <> 0 Then Exit Do
                        ind += 1
                    Loop Until ind = 85

                    ListaParametrosEstabilidad(i).Pti = DoGet("PROSPER.OUT.QLG.Output[0]")
                    ListaParametrosEstabilidad(i).Tvalv = DoGet("PROSPER.OUT.QLG.Output[1]")
                    ListaParametrosEstabilidad(i).GOR_quicklook = DoGet("PROSPER.OUT.QLG.Output[2]")
                    ListaParametrosEstabilidad(i).GORFREE = DoGet("PROSPER.OUT.QLG.Output[3]")
                    ListaParametrosEstabilidad(i).Dpvalve = DoGet("PROSPER.OUT.QLG.Output[4]")
                    ListaParametrosEstabilidad(i).Ptrcalc = DoGet("PROSPER.OUT.QLG.Output[5]")
                    ListaParametrosEstabilidad(i).Dp = DoGet("PROSPER.OUT.QLG.Output[6]")
                    ListaParametrosEstabilidad(i).PI = DoGet("PROSPER.OUT.QLG.Output[7]")
                    ListaParametrosEstabilidad(i).Qgcrit = DoGet("PROSPER.OUT.QLG.Output[8]")
                    ListaParametrosEstabilidad(i).Qcporcent = DoGet("PROSPER.OUT.QLG.Output[9]")
                    ListaParametrosEstabilidad(i).HTC = DoGet("PROSPER.OUT.QLG.Output[10]")
                    ListaParametrosEstabilidad(i).Ptri = DoGet("PROSPER.OUT.QLG.Output[11]")
                    ListaParametrosEstabilidad(i).Pws = DoGet("PROSPER.OUT.QLG.Output[12]")
                    ListaParametrosEstabilidad(i).Pwf_quicklook = DoGet("PROSPER.OUT.QLG.Output[13]")
                    ListaParametrosEstabilidad(i).Bgfi = DoGet("PROSPER.OUT.QLG.Results[1].OilFvf[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).Bginyi = DoGet("PROSPER.OUT.QLG.Results[1].GasFf[" & CStr(ind) & "]")
                    ListaParametrosEstabilidad(i).Bgfreei = DoGet("PROSPER.OUT.QLG.Results[1].GasFs[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).Boi = DoGet("PROSPER.OUT.QLG.Results[1].GasFvf[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).LiqVsp = DoGet("PROSPER.OUT.QLG.Results[1].Vlnslp[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).GasVsp = DoGet("PROSPER.OUT.QLG.Results[1].Vgnslp[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).IDtp = DoGet("PROSPER.OUT.QLG.Results[1].DiaInt[" & CStr(ind - 1) & "]")
                    ListaParametrosEstabilidad(i).DensidaMezclai = DoGet("PROSPER.OUT.QLG.Results[1].RhoMix[" & CStr(ind) & "]")
                    ListaParametrosEstabilidad(i).DenGasBN = DoGet("PROSPER.OUT.QLG.Results[1].RhgF[" & CStr(ind) & "]")
                    ListaParametrosEstabilidad(i).Twf = DoGet("PROSPER.OUT.QLG.Results[1].Temp[0]")

                    Dim Pti As Double = ListaParametrosEstabilidad(i).Pti
                    Dim Bgfi As Double = ListaParametrosEstabilidad(i).Bgfi
                    Dim Rp As Double = ListaParametrosEstabilidad(i).GOR_quicklook + ListaParametrosEstabilidad(i).GORFREE
                    Dim Pwf As Double = ListaParametrosEstabilidad(i).Pwf_quicklook
                    Dim Pws As Double = ListaParametrosEstabilidad(i).Pws
                    Dim Bginyi As Double = ListaParametrosEstabilidad(i).Bginyi
                    Dim Dorif As Double = ListaParametrosEstabilidad(i).DiaVal
                    Dim Twf As Double = ListaParametrosEstabilidad(i).Twf
                    Dim Tti As Double = ListaParametrosEstabilidad(i).Tvalv
                    Dim Boi As Double = ListaParametrosEstabilidad(i).Boi
                    Dim GORFREE As Double = ListaParametrosEstabilidad(i).GORFREE
                    Dim Densf As Double = ListaParametrosEstabilidad(i).DensidaMezclai
                    Dim DenBNi As Double = ListaParametrosEstabilidad(i).DenGasBN

                    F1aux = F1(Pti, Bgfi, Rp, Par2, Par1, Pexp, Qmax, Qliq, Pwf, Pws, Drginj, Bginyi, QgasIny, _Cd.Val, Dorif, Twf, Tti, API, Drg, Boi, GORFREE, ListaParametrosEstabilidad(i).PI * 14.2233, F1_Asheim)
                    ListaParametrosEstabilidad(i).F1 = F1aux
                    ListaParametrosEstabilidad(i).F1_Asheim = F1_Asheim

                    If (F1aux < 1 Or F1_Asheim < 1) Then
                        Dim Qfo As Double = (ListaParametrosEstabilidad(i).LiqVsp + ListaParametrosEstabilidad(i).GasVsp) * 0.00064516 * Math.PI * ListaParametrosEstabilidad(i).IDtp ^ 2 / 4
                        Dim VolEA As Double = Vector(MD_EM, EAVol_EM, ListaParametrosEstabilidad(i).Dvalve)
                        Dim VolTp As Double = Vector(MD_EM, TPVol_EM, ListaParametrosEstabilidad(i).Dvalve)
                        If (F1aux < 1) Then
                            F2aux = F2(Pti, VolEA, VolTp, ListaParametrosEstabilidad(i).Dvalve, Densf, DenBNi, QgasIny, Bginyi, Qfo, F1aux)
                            ListaParametrosEstabilidad(i).F2 = F2aux
                        End If
                        If (F1_Asheim < 1) Then
                            F2_Asheim = F2(Pti, VolEA, VolTp, ListaParametrosEstabilidad(i).Dvalve, Densf, DenBNi, QgasIny, Bginyi, Qfo, F1_Asheim)
                            ListaParametrosEstabilidad(i).F2_Asheim = F2_Asheim
                        End If
                    End If
                End If

                If (Qliq > 0) Then
                    If (F1aux >= 1 Or F2aux >= 1) Then
                        Ind_Est = 2                                                                  'Estable
                        If (ListaParametrosEstabilidad(i).Qgcrit < QgasIny) Then Ind_Est = -1        'Gasto de gas de BN mayor al critico
                        If (ListaParametrosEstabilidad(i).Ptrcalc > Ptr) Then Ind_Est = 1            'Presión en red de BN insuficiente
                    Else
                        Ind_Est = 0                                                                  'Inestable
                    End If
                    'Caso del ingeniero Eduardo Poblano
                    If (F1_Asheim >= 1 Or F2_Asheim >= 1) Then
                        Ind_Poblano = 2                                                                  'Estable
                        If (ListaParametrosEstabilidad(i).Qgcrit < QgasIny) Then Ind_Poblano = -1        'Gasto de gas de BN mayor al critico
                        If (ListaParametrosEstabilidad(i).Ptrcalc > Ptr) Then Ind_Poblano = 1            'Presión en red de BN insuficiente
                    Else
                        Ind_Poblano = 0                                                                  'Inestable
                    End If
                Else
                    Ind_Est = -2                                                                     'Inoperable
                    Ind_Poblano = -2
                End If
                ListaParametrosEstabilidad(i).Ind_Est = Ind_Est
                ListaParametrosEstabilidad(i).Ind_Poblano = Ind_Poblano
            Next i

            Dim ndatos As Integer
            Dim variable As String = ""
            For k = 0 To 1
                If k = 0 Then ndatos = _Ptr_num.Val - 1 : variable = "Ptr"
                If k = 1 Then ndatos = _Orif_Num.Val - 1 : variable = "D. Orificio"

                For i = 0 To _Qginy_Num.Val - 1
                    For j = 0 To ndatos
                        Dim obj As New Parametros_Estabilidad
                        Dim F1aux As Double
                        Dim F1_Asheim As Double
                        Dim F2aux As Double
                        Dim F2_Asheim As Double
                        Dim Ind_Est As Integer
                        Dim Ind_Poblano As Integer
                        Dim Pwh As Double = ListaParametrosEstabilidad(i).Pwh
                        Dim Twh As Double = ListaParametrosEstabilidad(i).Twh
                        Dim Qliq As Double = ListaParametrosEstabilidad(i).Ql
                        Dim Wc As Double = ListaParametrosEstabilidad(i).Wc
                        Dim TotalGasRate As Double = ListaParametrosEstabilidad(i).TotalQgas + ListaParametrosEstabilidad(i).Qgi
                        Dim QgasIny As Double = ListaParametrosEstabilidad(i).Qgi
                        Dim Ptr As Double
                        Dim DiaOrif As Double
                        If k = 0 Then Ptr = _Ptr_min.Val + Ptr_Incr * j Else Ptr = ListaParametrosEstabilidad(i).Ptr
                        If k = 1 Then DiaOrif = _Orif_Min.Val + Orif_Incr * j Else DiaOrif = ListaParametrosEstabilidad(i).DiaVal
                        Dim Profvalve As Double = ListaParametrosEstabilidad(i).Dvalve

                        obj.Variable = variable
                        obj.Ptr = Ptr
                        obj.Pwh = Pwh
                        obj.Qgi = QgasIny
                        obj.Dvalve = Profvalve
                        obj.DiaVal = DiaOrif
                        obj.Wc = Wc
                        obj.GOR = ListaParametrosEstabilidad(i).GOR
                        obj.Ql = Qliq
                        obj.Qg = ListaParametrosEstabilidad(i).Qg
                        obj.Pwf_IPR = ListaParametrosEstabilidad(i).Pwf_IPR
                        obj.TotalQgas = ListaParametrosEstabilidad(i).Qg + QgasIny
                        obj.Twh = ListaParametrosEstabilidad(i).Twh

                        If (Qliq > 0) Then
                            DoSet("PROSPER.ANL.QLG.Surface[0][0]", Pwh)
                            DoSet("PROSPER.ANL.QLG.Surface[1][0]", Twh)
                            DoSet("PROSPER.ANL.QLG.Surface[2][0]", Qliq)
                            DoSet("PROSPER.ANL.QLG.Surface[3][0]", Wc)
                            DoSet("PROSPER.ANL.QLG.Surface[4][0]", TotalGasRate)
                            DoSet("PROSPER.ANL.QLG.Surface[5][0]", QgasIny)
                            DoSet("PROSPER.ANL.QLG.Surface[6][0]", Ptr)
                            DoSet("PROSPER.ANL.QLG.InjPoint", 0)
                            DoSet("PROSPER.ANL.QLG.Gaslift[0]", DiaOrif)
                            DoSet("PROSPER.ANL.QLG.Gaslift[1]", Profvalve)
                            DoCmd("PROSPER.ANL.QLG.CALC")

                            Dim ind As Integer = 1
                            Do
                                Dim ind_valvula As Double = DoGet("PROSPER.OUT.QLG.Results[1].GasLif[" & CStr(ind) & "]")
                                If ind_valvula <> 0 Then Exit Do
                                ind += 1
                            Loop Until ind = 85

                            obj.Pwf_quicklook = DoGet("PROSPER.OUT.QLG.Output[13]")
                            obj.Twf = DoGet("PROSPER.OUT.QLG.Results[1].Temp[0]")
                            obj.Pws = DoGet("PROSPER.OUT.QLG.Output[12]")
                            obj.Pti = DoGet("PROSPER.OUT.QLG.Output[0]")
                            obj.Ptri = DoGet("PROSPER.OUT.QLG.Output[11]")
                            obj.Tvalv = DoGet("PROSPER.OUT.QLG.Output[1]")
                            obj.GOR_quicklook = DoGet("PROSPER.OUT.QLG.Output[2]")
                            obj.GORFREE = DoGet("PROSPER.OUT.QLG.Output[3]")
                            obj.Dpvalve = DoGet("PROSPER.OUT.QLG.Output[4]")
                            obj.Ptrcalc = DoGet("PROSPER.OUT.QLG.Output[5]")
                            obj.Dp = DoGet("PROSPER.OUT.QLG.Output[6]")
                            obj.PI = DoGet("PROSPER.OUT.QLG.Output[7]")
                            obj.Qgcrit = DoGet("PROSPER.OUT.QLG.Output[8]")
                            obj.Qcporcent = DoGet("PROSPER.OUT.QLG.Output[9]")
                            obj.HTC = DoGet("PROSPER.OUT.QLG.Output[10]")
                            'obj.Ind_Est
                            obj.Bgfi = DoGet("PROSPER.OUT.QLG.Results[1].OilFvf[" & CStr(ind - 1) & "]")
                            obj.Bginyi = DoGet("PROSPER.OUT.QLG.Results[1].GasFf[" & CStr(ind) & "]")
                            obj.Bgfreei = DoGet("PROSPER.OUT.QLG.Results[1].GasFs[" & CStr(ind - 1) & "]")
                            obj.Boi = DoGet("PROSPER.OUT.QLG.Results[1].GasFvf[" & CStr(ind - 1) & "]")
                            obj.LiqVsp = DoGet("PROSPER.OUT.QLG.Results[1].Vlnslp[" & CStr(ind - 1) & "]")
                            obj.GasVsp = DoGet("PROSPER.OUT.QLG.Results[1].Vgnslp[" & CStr(ind - 1) & "]")
                            obj.IDtp = DoGet("PROSPER.OUT.QLG.Results[1].DiaInt[" & CStr(ind - 1) & "]")
                            obj.DensidaMezclai = DoGet("PROSPER.OUT.QLG.Results[1].RhoMix[" & CStr(ind) & "]")
                            obj.DenGasBN = DoGet("PROSPER.OUT.QLG.Results[1].RhgF[" & CStr(ind) & "]")
                            'obj.F1
                            'obj.F2

                            Dim Pti As Double = obj.Pti
                            Dim Bgfi As Double = obj.Bgfi
                            Dim Rp As Double = obj.GOR_quicklook + obj.GORFREE
                            Dim Pwf As Double = obj.Pwf_quicklook
                            Dim Pws As Double = obj.Pws
                            Dim Bginyi As Double = obj.Bginyi
                            Dim Dorif As Double = obj.DiaVal
                            Dim Twf As Double = obj.Twf
                            Dim Tti As Double = obj.Tvalv
                            Dim Boi As Double = obj.Boi
                            Dim GORFREE As Double = obj.GORFREE
                            Dim Densf As Double = obj.DensidaMezclai
                            Dim DenBNi As Double = obj.DenGasBN
                            F1aux = F1(Pti, Bgfi, Rp, Par2, Par1, Pexp, Qmax, Qliq, Pwf, Pws, Drginj, Bginyi, QgasIny, _Cd.Val, Dorif, Twf, Tti, API, Drg, Boi, GORFREE, obj.PI * 14.2233, F1_Asheim)
                            obj.F1 = F1aux
                            obj.F1_Asheim = F1_Asheim

                            If (F1aux < 1 Or F1_Asheim < 1) Then
                                Dim Qfo As Double = (obj.LiqVsp + obj.GasVsp) * 0.00064516 * Math.PI * obj.IDtp ^ 2 / 4
                                Dim VolEA As Double = Vector(MD_EM, EAVol_EM, obj.Dvalve)
                                Dim VolTp As Double = Vector(MD_EM, TPVol_EM, obj.Dvalve)
                                If (F1aux < 1) Then
                                    F2aux = F2(Pti, VolEA, VolTp, obj.Dvalve, Densf, DenBNi, QgasIny, Bginyi, Qfo, F1aux)
                                    obj.F2 = F2aux
                                End If
                                If (F1_Asheim < 1) Then
                                    F2_Asheim = F2(Pti, VolEA, VolTp, obj.Dvalve, Densf, DenBNi, QgasIny, Bginyi, Qfo, F1_Asheim)
                                    obj.F2_Asheim = F2_Asheim
                                End If
                            End If

                        End If

                        If (Qliq > 0) Then
                            If (F1aux >= 1 Or F2aux >= 1) Then
                                Ind_Est = 2                                                                  'Estable
                                If (obj.Qgcrit < QgasIny) Then Ind_Est = -1        'Gasto de gas de BN mayor al critico
                                If (obj.Ptrcalc > Ptr) Then Ind_Est = 1            'Presión en red de BN insuficiente
                            Else
                                Ind_Est = 0                                                                  'Inestable
                            End If
                            'Caso del Ingeniero Eduardo Poblano
                            If (F1_Asheim >= 1 Or F2_Asheim >= 1) Then
                                Ind_Poblano = 2                                                                  'Estable
                                If (obj.Qgcrit < QgasIny) Then Ind_Poblano = -1        'Gasto de gas de BN mayor al critico
                                If (obj.Ptrcalc > Ptr) Then Ind_Poblano = 1            'Presión en red de BN insuficiente
                            Else
                                Ind_Poblano = 0                                                                  'Inestable
                            End If
                        Else
                            Ind_Est = -2                                                                     'Inoperable
                            Ind_Poblano = -2
                        End If
                        obj.Ind_Est = Ind_Est
                        obj.Ind_Poblano = Ind_Poblano
                        ListaParametrosEstabilidad.Add(obj)
                    Next j
                Next i
            Next k
        End Sub


        Private Sub SYSTEM() '(ByVal Pwh_act As Double, Pwh_min As Double, Pwh_max As Double, Qgi_min As Double, Qgi_max As Double, ProfValvula_act As Double, ProfValvula_min As Double, ProfValvula_max As Double, Wc_act As Double, Wc_min As Double, Wc_max As Double)
            Dim Pyac As Double
            'Parámetros de System
            Dim QL_System As Double
            Dim Qoil_System As Double
            Dim Qgf_System As Double
            Dim PwfIPR_System As Double
            Dim Twh_System As Double

            Dim Qgis(_Qginy_Num.Val - 1) As Double
            Dim Ptr(_Ptr_num.Val - 1) As Double
            Dim Pwh(_Pwh_num.Val - 1) As Double
            Dim Profvalvula(_Dvalvula_Num.Val - 1) As Double
            Dim Wc(_Fw_Num.Val - 1) As Double
            Dim gor As Double = DoGet("PROSPER.SIN.IPR.Single.totgor")

            If (_Pwh_num.Val > 1) Then Pwh_Incr = (_Pwh_max.Val - _Pwh_min.Val) / (_Pwh_num.Val - 1)
            If (_Qginy_Num.Val > 1) Then Qginy_Incr = (_Qginy_Max.Val - _Qginy_Min.Val) / (_Qginy_Num.Val - 1)
            If (_Dvalvula_Num.Val > 1) Then Dvalvula_Incr = (_Dvalvula_Max.Val - _Dvalvula_Min.Val) / (_Dvalvula_Num.Val - 1)
            If (_Fw_Num.Val > 1) Then Fw_Incr = (_Fw_Max.Val - _Fw_Min.Val) / (_Fw_Num.Val - 1)

            Pyac = Val(DoGet("PROSPER.SIN.IPR.SINGLE.PRES"))
            DoSet("PROSPER.SIN.GLF.ValveDepth", _Dvalve_Act.Val)
            DoSet("PROSPER.SIN.GLF.OrificeDia", _DiaValve_Act.Val)
            DoSet("PROSPER.ANL.SYS.PRES", _Pwh_Act.Val)
            DoSet("PROSPER.ANL.SYS.WC", _Wc_Act.Val)
            DoSet("PROSPER.ANL.SYS.RateMethod", 2)


            'Calcula el Ql a Pwh, Wc, Prof iny y Diametro valvula de operación 07/10/2019
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[0]", _Qgi_Act.Val)
            DoCmd("PROSPER.ANL.SYS.CALC")
            Ql = DoGet("PROSPER.OUT.SYS.Results[0].Sol.LiqRate")



            '*************************Varia Qgi para Ptr y diametro orificio de inyección*******************************
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)


            For i = 0 To _Qginy_Num.Val - 1
                Dim Qgi As Double = _Qginy_Min.Val + Qginy_Incr * i
                Qgis(i) = Qgi
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", Qgi)
            Next i
            DoCmd("PROSPER.ANL.SYS.CALC")

            For i = 0 To _Qginy_Num.Val - 1
                QL_System = CDbl(DoGet("PROSPER.OUT.SYS.Results[" & i.ToString & "][0][0].Sol.LiqRate"))
                Qoil_System = DoGet("PROSPER.OUT.SYS.Results[" & CStr(i) & "][0][0].Sol.OilRate")
                Qgf_System = DoGet("PROSPER.OUT.SYS.Results[" & CStr(i) & "][0][0].Sol.GasRate")
                PwfIPR_System = DoGet("PROSPER.OUT.SYS.Results[" & CStr(i) & "][0][0].Sol.BHP")
                Twh_System = DoGet("PROSPER.OUT.SYS.Results[" & CStr(i) & "][0][0].Sol.WHTemperature")
                Dim objparametro As New Parametros_Estabilidad With {
                    .Variable = "Qgi",
                    .Ptr = _Ptr_Act.Val,
                    .Pwh = _Pwh_Act.Val,
                    .Qgi = Qgis(i),
                    .Dvalve = _Dvalve_Act.Val,
                    .DiaVal = _DiaValve_Act.Val,
                    .Wc = _Wc_Act.Val,
                    .GOR = gor,
                    .Ql = QL_System,
                    .Qg = Qgf_System,
                    .Pwf_IPR = PwfIPR_System,
                    .Twh = Twh_System,
                    .TotalQgas = Qgf_System + Qgis(i),
                    .Pwf_quicklook = 0,
                    .Twf = 0,
                    .Pws = Pyac,
                    .Pti = 0,
                    .Ptri = 0,
                    .Tvalv = 0,
                    .GOR_quicklook = 0,
                    .GORFREE = 0,
                    .Dpvalve = 0,
                    .Ptrcalc = 0,
                    .Dp = Pyac - PwfIPR_System,
                    .PI = QL_System / (Pyac - PwfIPR_System),
                    .Qgcrit = 0,
                    .Qcporcent = 0,
                    .HTC = 0,
                    .Ind_Est = 0,
                    .Bgfi = 0,
                    .Bginyi = 0,
                    .Bgfreei = 0,
                    .Boi = 0,
                    .LiqVsp = 0,
                    .GasVsp = 0,
                    .IDtp = 0,
                    .DensidaMezclai = 0,
                    .DenGasBN = 0,
                    .F1 = 0,
                    .F2 = 0
                     }
                ListaParametrosEstabilidad.Add(objparametro)
            Next i

            '*************************Varia Qgi y Pwh**************************************************************************

            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 27)

            For i = 0 To _Qginy_Num.Val - 1
                Dim Qgi As Double = _Qginy_Min.Val + Qginy_Incr * i
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", Qgi)
            Next i
            For i = 0 To _Pwh_num.Val - 1
                Dim Pwhs As Double = _Pwh_min.Val + Pwh_Incr * i
                Pwh(i) = Pwhs
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[145].Vals[" & CStr(i) & "]", Pwhs)
            Next i
            DoCmd("PROSPER.ANL.SYS.CALC")

            For j = 0 To _Qginy_Num.Val - 1
                For i = 0 To _Pwh_num.Val - 1
                    'If i = 9 And j = 9 Then Stop
                    QL_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.LiqRate")
                    Qoil_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.OilRate")
                    Qgf_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.GasRate")
                    PwfIPR_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.BHP")
                    Twh_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.WHTemperature")

                    Dim objparametro As New Parametros_Estabilidad With {
                    .Variable = "Pwh",
                    .Ptr = _Ptr_Act.Val,
                    .Qgi = Qgis(j),
                    .Pwh = Pwh(i),
                    .Dvalve = _Dvalve_Act.Val,
                    .DiaVal = _DiaValve_Act.Val,
                    .Wc = _Wc_Act.Val,
                    .GOR = gor,
                    .Ql = QL_System,
                    .Qg = Qgf_System,
                    .Pwf_IPR = PwfIPR_System,
                    .Twh = Twh_System,
                    .TotalQgas = Qgf_System + Qgis(j),
                    .Pwf_quicklook = 0,
                    .Twf = 0,
                    .Pws = Pyac,
                    .Pti = 0,
                    .Ptri = 0,
                    .Tvalv = 0,
                    .GOR_quicklook = 0,
                    .GORFREE = 0,
                    .Dpvalve = 0,
                    .Ptrcalc = 0,
                    .Dp = Pyac - PwfIPR_System,
                    .PI = QL_System / (Pyac - PwfIPR_System),
                    .Qgcrit = 0,
                    .Qcporcent = 0,
                    .HTC = 0,
                    .Ind_Est = 0,
                    .Bgfi = 0,
                    .Bginyi = 0,
                    .Bgfreei = 0,
                    .Boi = 0,
                    .LiqVsp = 0,
                    .GasVsp = 0,
                    .IDtp = 0,
                    .DensidaMezclai = 0,
                    .DenGasBN = 0,
                    .F1 = 0,
                    .F2 = 0
                     }
                    ListaParametrosEstabilidad.Add(objparametro)
                Next i
            Next j
            '*************************Varia Qgi y Profunidad de la valvula**************************************************************************
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 31)
            For i = 0 To _Qginy_Num.Val - 1
                Dim Qgi As Double = _Qginy_Min.Val + Qginy_Incr * i
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", Qgi)
            Next i
            For i = 0 To _Dvalvula_Num.Val - 1
                Dim Dvalv As Double = _Dvalvula_Min.Val + Dvalvula_Incr * i
                Profvalvula(i) = Dvalv
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[140].Vals[" & CStr(i) & "]", Dvalv)
            Next i
            DoCmd("PROSPER.ANL.SYS.CALC")

            For j = 0 To _Qginy_Num.Val - 1
                For i = 0 To _Dvalvula_Num.Val - 1
                    QL_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.LiqRate")
                    Qoil_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.OilRate")
                    Qgf_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.GasRate")
                    PwfIPR_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.BHP")
                    Twh_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.WHTemperature")

                    Dim objparametro As New Parametros_Estabilidad With {
                    .Variable = "Dvalvula",
                    .Ptr = _Ptr_Act.Val,
                    .Qgi = Qgis(j),
                    .Pwh = _Pwh_Act.Val,
                    .Dvalve = Profvalvula(i),
                    .DiaVal = _DiaValve_Act.Val,
                    .Wc = _Wc_Act.Val,
                    .GOR = gor,
                    .Ql = QL_System,
                    .Qg = Qgf_System,
                    .Pwf_IPR = PwfIPR_System,
                    .Twh = Twh_System,
                    .TotalQgas = Qgf_System + Qgis(j),
                    .Pwf_quicklook = 0,
                    .Twf = 0,
                    .Pws = Pyac,
                    .Pti = 0,
                    .Ptri = 0,
                    .Tvalv = 0,
                    .GOR_quicklook = 0,
                    .GORFREE = 0,
                    .Dpvalve = 0,
                    .Ptrcalc = 0,
                    .Dp = Pyac - PwfIPR_System,
                    .PI = QL_System / (Pyac - PwfIPR_System),
                    .Qgcrit = 0,
                    .Qcporcent = 0,
                    .HTC = 0,
                    .Ind_Est = 0,
                    .Bgfi = 0,
                    .Bginyi = 0,
                    .Bgfreei = 0,
                    .Boi = 0,
                    .LiqVsp = 0,
                    .GasVsp = 0,
                    .IDtp = 0,
                    .DensidaMezclai = 0,
                    .DenGasBN = 0,
                    .F1 = 0,
                    .F2 = 0
                     }
                    ListaParametrosEstabilidad.Add(objparametro)
                Next i
            Next j
            '*************************Varia Qgi y Wc**************************************************************************
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 16)
            For i = 0 To _Qginy_Num.Val - 1
                Dim Qgi As Double = _Qginy_Min.Val + Qginy_Incr * i
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", Qgi)
            Next i
            For i = 0 To _Fw_Num.Val - 1
                Dim Wcs As Double = _Fw_Min.Val + Fw_Incr * i
                Wc(i) = Wcs
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[" & CStr(i) & "]", Wcs)
            Next i

            DoCmd("PROSPER.ANL.SYS.CALC")
            For j = 0 To _Qginy_Num.Val - 1
                For i = 0 To _Fw_Num.Val - 1
                    QL_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.LiqRate")
                    Qoil_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.OilRate")
                    Qgf_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.GasRate")
                    PwfIPR_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.BHP")
                    Twh_System = DoGet("PROSPER.OUT.SYS.Results[" & j.ToString & "][" & i.ToString & "][0].Sol.WHTemperature")
                    Dim objparametro As New Parametros_Estabilidad With {
                    .Variable = "Wc",
                    .Ptr = _Ptr_Act.Val,
                    .Qgi = Qgis(j),
                    .Pwh = _Pwh_Act.Val,
                    .Dvalve = _Dvalve_Act.Val,
                    .DiaVal = _DiaValve_Act.Val,
                    .Wc = Wc(i),
                    .GOR = gor,
                    .Ql = QL_System,
                    .Qg = Qgf_System,
                    .Pwf_IPR = PwfIPR_System,
                    .Twh = Twh_System,
                    .TotalQgas = Qgf_System + Qgis(j),
                    .Pwf_quicklook = 0,
                    .Twf = 0,
                    .Pws = Pyac,
                    .Pti = 0,
                    .Ptri = 0,
                    .Tvalv = 0,
                    .GOR_quicklook = 0,
                    .GORFREE = 0,
                    .Dpvalve = 0,
                    .Ptrcalc = 0,
                    .Dp = Pyac - PwfIPR_System,
                    .PI = QL_System / (Pyac - PwfIPR_System),
                    .Qgcrit = 0,
                    .Qcporcent = 0,
                    .HTC = 0,
                    .Ind_Est = 0,
                    .Bgfi = 0,
                    .Bginyi = 0,
                    .Bgfreei = 0,
                    .Boi = 0,
                    .LiqVsp = 0,
                    .GasVsp = 0,
                    .IDtp = 0,
                    .DensidaMezclai = 0,
                    .DenGasBN = 0,
                    .F1 = 0,
                    .F2 = 0
                    }
                    ListaParametrosEstabilidad.Add(objparametro)
                Next i
            Next j
        End Sub

        Private Function F1(ByVal Pti As Double, ByVal Bgfi As Double, ByVal Rp As Double, ByVal b As Double, ByVal a As Double,
                            ByVal c As Double, ByVal Qomax As Double, ByVal qo As Double, ByVal Pwf As Double, ByVal Pyac As Double,
                            ByVal Drginy As Double, ByVal Bginyi As Double, ByVal Qgi As Double, ByVal Cd As Double,
                            ByVal Dorif As Double, ByVal Twf As Double, ByVal Tti As Double, ByVal API As Double, ByVal Drgf As Double,
                            ByVal Boi As Double, ByVal GORFREE As Double, ByVal J As Double, ByRef F1_Asheim As Double) As Double

            Dim Omega As Double
            Dim Tprom, Pprom As Double
            Dim Kernel, Kprom, Ki As Double
            Dim Bti, Rsti As Double
            Dim Af, Bf, Boprom, Cprom, Zprom As Double
            Dim Ao As Double
            Tprom = ((Twf + Tti) / 2.0) * 9 / 5 + 32
            Pprom = ((Pwf + Pti) / 2.0) * 14.2233

            If API <= 30 Then
                Kernel = (0.05958 * Drgf ^ 0.7972) / 5.615
                Kprom = Kernel * 10 ^ ((13.1405 * API) / (Tprom + 460.0))
                Ki = Kernel * 10 ^ ((13.1405 * API) / (Tti * 9 / 5 + 32 + 460.0))
            Else
                Kernel = (0.0315 * Drgf ^ 0.7587) / 5.615
                Kprom = Kernel * 10 ^ ((11.289 * API) / (Tprom + 460.0))
                Ki = Kernel * 10 ^ ((11.289 * API) / (Tti * 9 / 5 + 32 + 460.0))
            End If

            DoSet("PROSPER.PVT.CALC.MODE", 1)
            DoSet("PROSPER.PVT.Calc.TempUser[0]", (Twf + Tti) / 2)
            DoSet("PROSPER.PVT.Calc.PresUser[0]", (Pwf + Pti) / 2)
            DoCmd("PROSPER.PVT.CALC")
            Boprom = DoGet("PROSPER.PVT.Calc.Results[0].OilFVF")
            Zprom = DoGet("PROSPER.PVT.Calc.Results[0].ZFactor")
            Cprom = 0.0283 * Zprom * (Tprom + 460)
            Af = (Rp - b) * Cprom / Boprom
            Bf = a * Kprom * Cprom / Boprom / c
            Pwf += 1.03323
            Pti += 1.03323
            Pyac += 1.03323
            Rsti = a * Ki * (Pti * 14.2233) ^ c + b
            Bti = Boi + (Rp - Rsti) * Bgfi
            Omega = 0.2 * (Qomax / qo) * (Pwf / Pyac) * ((Pti + Af - Bf * c * Pti ^ c) / (Pwf + Af - Bf * c * Pwf ^ c)) * Math.Sqrt(1 + 80 * (1 - qo / Qomax))
            Omega = (1 / Pti) * ((Bgfi / Bti) * (Rp - b + a * Ki * (c - 1) * Pti ^ c) + Omega)
            Ao = Math.PI * (Dorif / 64) ^ 2 / (144 * 4)

            F1 = (0.0764 * (1000000 / 86400) ^ 2 / (144 * 32.2 * 14.2233)) * Drginy * Bginyi * Qgi ^ 2 * Omega / (Cd * Ao) ^ 2
            F1_Asheim = (0.0764 * (1000000 / 86400) ^ 2 / (144 * 32.2 * 14.2233)) * Drginy * Bginyi * Qgi ^ 2 * (J / qo) / (Cd * Ao) ^ 2

        End Function

        Private Function F2(ByVal Pti As Double, ByVal VolTr As Double, ByVal VolTp As Double, ByVal Deph_vert As Double, ByVal Densf As Double,
                            ByVal Densg As Double, ByVal Qiny As Double, ByVal Bginy As Double, ByVal Qfo As Double, ByVal F1 As Double) As Double

            F2 = (VolTp / VolTr) * ((Pti + 1.033) / ((Densf - Densg) * Deph_vert)) * (1 + Bginy * Qiny * 0.32774074 / Qfo) * (1 / (1 - F1))
        End Function


        Function ARCCOS(Argumento)

            'Esta función obtiene al ángulo cuyo coseno es "argumento"
            '*********************************************************************
            If Argumento = 1 Then
                Return 0
            ElseIf Argumento = -1 Then
                Return 4 * Math.Atan(1)
            Else
                Return Math.Atan(-Argumento / Math.Sqrt(1 - Argumento * Argumento)) + 2 * Math.Atan(1)
            End If

        End Function
        Private Function Vector(ByVal x() As Double, ByVal y() As Double, ByRef arc As Double) As Double
            Dim kk As Integer
            For kk = 0 To UBound(y) - 1
                If (x(0) > x(1)) Then
                    If (arc <= x(kk) And arc >= x(kk + 1)) Then Vector = y(kk) + (arc - x(kk)) * (y(kk) - y(kk + 1)) / (x(kk) - x(kk + 1))
                ElseIf (x(0) < x(1)) Then
                    If (arc >= x(kk) And arc <= x(kk + 1)) Then Vector = y(kk) + (arc - x(kk)) * (y(kk) - y(kk + 1)) / (x(kk) - x(kk + 1))
                End If
            Next kk
            Return Vector
        End Function

    End Class
End Namespace