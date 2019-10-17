Namespace Clases

    ''' <summary>Primer algoritmo de Estabilidad del Dr. Iván</summary>

    Public Class MapaEstabilidad
        Dim Server As Object
        Dim AppName As String
        Dim Nmapa As Integer
        Public DATTR(,) As Double
        Public IPR_VLP(,) As Double

        Public recomendaciones As String

        Private _Ptr_min As New MaxMinDouble
        Public WriteOnly Property Ptr_min() As Double
            Set(ByVal value As Double)
                _Ptr_min.Val = value
            End Set
        End Property

        Private _Ptr_max As New MaxMinDouble
        Public WriteOnly Property Ptr_max() As Double
            Set(ByVal value As Double)
                _Ptr_max.Val = value
            End Set
        End Property

        Private _Ptr_num As New MaxMinInteger
        Public WriteOnly Property Ptr_num() As Integer
            Set(ByVal value As Integer)
                _Ptr_num.Val = value
            End Set
        End Property

        Private _Ptr_Incr As New MaxMinDouble
        Public WriteOnly Property Ptr_Incr() As Double
            Set(ByVal value As Double)
                _Ptr_Incr.Val = value
            End Set
        End Property

        Private _Pwh_min As New MaxMinDouble
        Public WriteOnly Property Pwh_min() As Double
            Set(ByVal value As Double)
                _Pwh_min.Val = value
            End Set
        End Property

        Private _Pwh_max As New MaxMinDouble
        Public WriteOnly Property Pwh_max() As Double
            Set(ByVal value As Double)
                _Pwh_max.Val = value
            End Set
        End Property

        Private _Pwh_num As New MaxMinInteger
        Public WriteOnly Property Pwh_num() As Integer
            Set(ByVal value As Integer)
                _Pwh_num.Val = value
            End Set
        End Property
        'Pwh_Incr
        Private _Pwh_Incr As New MaxMinDouble
        Public WriteOnly Property Pwh_Incr() As Double
            Set(ByVal value As Double)
                _Pwh_Incr.Val = value
            End Set
        End Property

        Private _Qginy_Min As New MaxMinDouble
        Public WriteOnly Property Qginy_Min() As Double
            Set(ByVal value As Double)
                _Qginy_Min.Val = value
            End Set
        End Property

        Private _Qginy_Max As New MaxMinDouble
        Public WriteOnly Property Qginy_Max() As Double
            Set(ByVal value As Double)
                _Qginy_Max.Val = value
            End Set
        End Property

        Private _Qginy_Num As New MaxMinInteger
        Public WriteOnly Property Qginy_Num() As Integer
            Set(ByVal value As Integer)
                _Qginy_Num.Val = value
            End Set
        End Property
        'Qginy_Incr
        Private _Qginy_Incr As New MaxMinDouble
        Public WriteOnly Property Qginy_Incr() As Double
            Set(ByVal value As Double)
                _Qginy_Incr.Val = value
            End Set
        End Property

        Private _Dvalvula_Min As New MaxMinDouble
        Public WriteOnly Property Dvalvula_Min() As Double
            Set(ByVal value As Double)
                _Dvalvula_Min.Val = value
            End Set
        End Property

        Private _Dvalvula_Max As New MaxMinDouble
        Public WriteOnly Property Dvalvula_Max() As Double
            Set(ByVal value As Double)
                _Dvalvula_Max.Val = value
            End Set
        End Property

        Private _Dvalvula_Num As New MaxMinInteger
        Public WriteOnly Property Dvalvula_Num() As Integer
            Set(ByVal value As Integer)
                _Dvalvula_Num.Val = value
            End Set
        End Property
        'Dvalvula_Incr
        Private _Dvalvula_Incr As New MaxMinDouble
        Public WriteOnly Property Dvalvula_Incr() As Double
            Set(ByVal value As Double)
                _Dvalvula_Incr.Val = value
            End Set
        End Property

        Private _Orif_Min As New MaxMinDouble
        Public WriteOnly Property Orif_Min() As Double
            Set(ByVal value As Double)
                _Orif_Min.Val = value
            End Set
        End Property

        Private _Orif_Max As New MaxMinDouble
        Public WriteOnly Property Orif_Max() As Double
            Set(ByVal value As Double)
                _Orif_Max.Val = value
            End Set
        End Property

        Private _Orif_Num As New MaxMinInteger
        Public WriteOnly Property Orif_Num() As Integer
            Set(ByVal value As Integer)
                _Orif_Num.Val = value
            End Set
        End Property
        'Orif_Incr
        Private _Orif_Incr As New MaxMinDouble
        Public WriteOnly Property Orif_Incr() As Double
            Set(ByVal value As Double)
                _Orif_Incr.Val = value
            End Set
        End Property

        Private _Fw_Min As New MaxMinDouble
        Public WriteOnly Property Fw_Min() As Double
            Set(ByVal value As Double)
                _Fw_Min.Val = value
            End Set
        End Property

        Private _Fw_Max As New MaxMinDouble
        Public WriteOnly Property Fw_Max() As Double
            Set(ByVal value As Double)
                _Fw_Max.Val = value
            End Set
        End Property

        Private _Fw_Num As New MaxMinInteger
        Public WriteOnly Property Fw_Num() As Integer
            Set(ByVal value As Integer)
                _Fw_Num.Val = value
            End Set
        End Property

        Private _Fw_Incr As New MaxMinDouble
        Public WriteOnly Property Fw_Incr() As Double
            Set(ByVal value As Double)
                _Fw_Incr.Val = value
            End Set
        End Property


        Private _Cd As New MaxMinDouble
        Public WriteOnly Property Cd() As Double
            Set(ByVal value As Double)
                _Cd.Val = value
            End Set
        End Property

        Private _MD As New MaxMinDouble
        Public WriteOnly Property MD() As Double
            Set(ByVal value As Double)
                _MD.Val = value
            End Set
        End Property

        Private _TVD As New MaxMinDouble
        Public WriteOnly Property TVD() As Double
            Set(ByVal value As Double)
                _TVD.Val = value
            End Set
        End Property

        Private _DTR As New MaxMinDouble
        Public WriteOnly Property DTR() As Double
            Set(ByVal value As Double)
                _DTR.Val = value
            End Set
        End Property

        Private _DTP As New MaxMinDouble
        Public WriteOnly Property DTP() As Double
            Set(ByVal value As Double)
                _DTP.Val = value
            End Set
        End Property

        Private _Qinj As New MaxMinDouble
        Public WriteOnly Property Qinj() As Double
            Set(ByVal value As Double)
                _Qinj.Val = value
            End Set
        End Property

        'salida
        'Private _datosPVT(29, 3) As Double
        'Public ReadOnly Property datosPVT() As List(Of Double)
        '    Get
        '        'Return _Ptr.val
        '        Return _datosPVT(29, 3)
        '    End Get
        'End Property
        Public datosPVT(29, 3) As Double

        Private _PtrArray() As Double
        Public ReadOnly Property PtrArray() As List(Of Double)
            Get
                'Return _Ptr.val
                Return getLista(_PtrArray)
            End Get
        End Property

        Private _PwhArray() As Double
        Public ReadOnly Property PwhArray() As List(Of Double)
            Get
                Return getLista(_PwhArray)
            End Get
        End Property

        Private _QgiArray() As Double
        Public ReadOnly Property QgiArray() As List(Of Double)
            Get
                Return getLista(_QgiArray)
            End Get
        End Property

        Private _DvalveArray() As Double
        Public ReadOnly Property DvalveArray() As List(Of Double)
            Get
                Return getLista(_DvalveArray)
            End Get
        End Property
        'DiaVal
        Private _DiaValArray() As Double
        Public ReadOnly Property DiaValArray() As List(Of Double)
            Get
                Return getLista(_DiaValArray)
            End Get
        End Property
        'Wc
        Private _WcArray() As Double
        Public ReadOnly Property WcArray() As List(Of Double)
            Get
                Return getLista(_WcArray)
            End Get
        End Property
        'GOR
        Private _GORArray() As Double
        Public ReadOnly Property GORArray() As List(Of Double)
            Get
                Return getLista(_GORArray)
            End Get
        End Property
        'Ql
        Private _QlArray() As Double
        Public ReadOnly Property QlArray() As List(Of Double)
            Get
                Return getLista(_QlArray)
            End Get
        End Property
        'Qg
        Private _QgArray() As Double
        Public ReadOnly Property QgArray() As List(Of Double)
            Get
                Return getLista(_QgArray)
            End Get
        End Property
        'Pwf
        Private _PwfArray() As Double
        Public ReadOnly Property PwfArray() As List(Of Double)
            Get
                Return getLista(_PwfArray)
            End Get
        End Property
        'Pws
        Private _PwsArray() As Double
        Public ReadOnly Property PwsArray() As List(Of Double)
            Get
                Return getLista(_PwsArray)
            End Get
        End Property
        'Pti
        Private _PtiArray() As Double
        Public ReadOnly Property PtiArray() As List(Of Double)
            Get
                Return getLista(_PtiArray)
            End Get
        End Property
        'Ptri
        Private _PtriArray() As Double
        Public ReadOnly Property PtriArray() As List(Of Double)
            Get
                Return getLista(_PtriArray)
            End Get
        End Property
        'Tvalv
        Private _TvalvArray() As Double
        Public ReadOnly Property TvalvArray() As List(Of Double)
            Get
                Return getLista(_TvalvArray)
            End Get
        End Property
        'Dpvalve
        Private _DpvalveArray() As Double
        Public ReadOnly Property DpvalveArray() As List(Of Double)
            Get
                Return getLista(_DpvalveArray)
            End Get
        End Property
        'Ptrcal
        Private _PtrcalArray() As Double
        Public ReadOnly Property PtrcalArray() As List(Of Double)
            Get
                Return getLista(_PtrcalArray)
            End Get
        End Property

        'Dp
        Private _DpArray() As Double
        Public ReadOnly Property DpArray() As List(Of Double)
            Get
                Return getLista(_DpArray)
            End Get
        End Property

        'PI
        Private _PIArray() As Double
        Public ReadOnly Property PIArray() As List(Of Double)
            Get
                Return getLista(_PIArray)
            End Get
        End Property

        'Qgcrit
        Private _QgcritArray() As Double
        Public ReadOnly Property QgcritArray() As List(Of Double)
            Get
                Return getLista(_QgcritArray)
            End Get
        End Property

        'HTC
        Private _HTCArray() As Double
        Public ReadOnly Property HTCArray() As List(Of Double)
            Get
                Return getLista(_HTCArray)
            End Get
        End Property

        'Ind_Est.
        Private _Ind_EstArray() As Double
        Public ReadOnly Property Ind_EstArray() As List(Of Double)
            Get
                Return getLista(_Ind_EstArray)
            End Get
        End Property

        Property ListaParametrosEstabilidad As New List(Of Parametros_Estabilidad)

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
        Public WriteOnly Property Caso() As Double
            Set(ByVal value As Double)
                _Caso.Val = value
            End Set
        End Property

        Sub GENERAL()
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
            Dim Pwf As Double       'Presión de fondo fluyendo [psig]
            Dim Pti As Double       'Presión en el pto. de inyección [psig]
            Dim Twh As Double       'Temp. en la cabeza del pozo [°F]
            Dim Twf As Double       'Temp. en el fondo del pozo [°F]
            Dim Tti As Double       'Temp. en la TP a la profundidad de inyección [°F]
            Dim Ql As Double        'Gasto de líquido [bpd]
            Dim Qoil As Double      'Gasto de aceite [bpd] (Ql=Qoil, ya que Fw=0 en todos los casos)
            Dim Qmax As Double      'Gasto máximo de líquido que puede aportar el yacimiento [bpd]
            Dim Qgf As Double       'Gasto del gas de la formación a c.e. [MMpcd]
            'Dim Cd As Double        'Coeficiente de descarga del orificio [0-1]
            Dim LVinj As Double     'Profundidad vertical al punto de inyección [pie]
            Dim RGAf As Double      'Relación gas de formación - aceite [bl/bl]
            Dim Prate As Double     'Presión de fondo  fluyendo (aforo) [psig]
            Dim Qrate As Double     'Gasto de aceite (aforo), [bpd]
            Dim Pyac As Double      'Presión del yacimiento [psig]
            Dim drginj As Double    'Densidad relativa del gas de inyección (aire=1)
            Dim drg As Double       'Densidad relativa del gas de la formación (aire=1)
            Dim dro As Double       'Densidad relativa del aceite
            Dim API As Double       'Densidad API del aceite

            Dim PBN As Double       'Presión en la red de BN [psig]
            'Dim Ptr_Min As Double   'Presión mínima en la tr para generar el mapa de estabilidad [psig]
            'Dim Ptr_Max As Double   'Presión máxima en la tr para generar el mapa de estabilidad [psig]
            'Dim Ptr_Num As Double   'Número de puntos de la presión en la tr para generar el mapa de estabilidad
            'Dim Ptr_Incr As Double  'Incremento de presión en la tr para generar el mapa [psig]

            Dim Pwh As Double       'Presión en la cabeza del pozo [psig]
            'Dim Pwh_Min As Double   'Presión mínima en la cabeza para generar el mapa de estabilidad [psig]
            'Dim Pwh_Max As Double   'Presión máxima en la cabeza para generar el mapa de estabilidad [psig]
            'Dim Pwh_Num As Double   'Número de puntos de la presión en la cabeza para generar el mapa de estabilidad
            'Dim Pwh_Incr As Double  'Incremento de presión en la cabeza para generar el mapa [psig]

            Dim Qginy As Double      'Gasto del gas de inyección a c.e. [MMpcd]
            'Dim Qginy_Min As Double   'Gasto de gas de inyección mínimo [MMSCF]
            'Dim Qginy_Max As Double   'Gasto de gas de inyección máximo [MMSCF]
            'Dim Qginy_Num As Double   'Número de puntos de gasto de gas de inyección
            'Dim Qginy_Incr As Double  'Incremento en el gasto de gas de inyección

            Dim Dvalvula As Double       'Prufundidad desarrollad de la valvula de inyección [ft]
            'Dim Dvalvula_Min As Double   'mínima Prufundidad del la valvula de inyección [ft]
            'Dim Dvalvula_Max As Double   'Máxima Prufundidad del la valvula de inyección [ft]
            'Dim Dvalvula_Num As Double   'Número de puntos de la Prufundidad del la valvula de inyección
            'Dim Dvalvula_Incr As Double  'Incremento en la Prufundidad del la valvula de inyección

            Dim Orif As Double       'Diámetro de orificio [64avo pg]
            'Dim Orif_Min As Double   'mínimo Diámetro de orificio [64avo pg]
            'Dim Orif_Max As Double   'Máximo Diámetro de orificio [64avo pg]
            'Dim Orif_Num As Double   'Número de puntos del Diámetro de orificio
            'Dim Orif_Incr As Double  'Incremento en el Diámetro de orificio [64avo pg]

            Dim Fw As Double       'Corte de agua [%]
            'Dim Fw_Min As Double   'mínimo Corte de agua [%]
            'Dim Fw_Max As Double   'Máximo Corte de agua [%]
            'Dim Fw_Num As Double   'Número de puntos del Corte de agua
            'Dim Fw_Incr As Double  'Incremento en el Corte de agua [%]


            Dim PwfIPR As Double    'Presión de fondo fluyendo [psig] (para comparar Pwf (grad) y Pwf (IPR))
            Dim Boprom As Double    'Factor de volumen del aceite [bl/bl]
            Dim Boti As Double      'Factor de volumen del aceite en el punto de inyección [bls/STB]
            Dim Bgprom As Double    'Factor de volumen del gas de la formación [cf/SCF]
            Dim Bginj As Double     'Factor de volumen del gas de BN en el pto de inyección [cf/SCF]
            Dim Bgfti As Double     'Factor de volumen del gas de la formación en el punto de inyección [cf/SCF]
            Dim zgfti As Double     'Factor de desviación del gas de formación en el punto de inyección [adim]
            Dim zgfprom As Double   'Factor de desviación del gas asociado a las cond. promedio de P y T entre el pto. de inyección y el fondo del pozo [adim]
            Dim zginj As Double     'Factor de desviación del gas de BN en el pto. de inyección [adim]
            Dim DTR As Double       'Diámetro interno de la TR [pg]
            Dim DTP As Double       'Diámetro externo de la TP [pg]
            Dim TVD As Double       'Profundidad vertical [pie]
            Dim MD As Double        'Profundidad desarrollada [pie]
            Dim RUGTR As Double     'Rugosidad de la TR [pg]
            Dim Ptr As Double       'Presión en la TR, [psig]
            Dim Pv As Double        'Presión en la TR a la prof. de la válvula de BN [psig]
            Dim Densf As Double     'Densidad de los fluidos de la formación en el punto de inyección [lbm/pie3]
            Dim Qfo As Double       'Gasto de los fluidos de la formación en el punto de inyección [pie3/s]
            Dim VOLTR As Double     'Volumen del espacio anular [pie3]
            Dim VOLTP As Double     'Volumen de la TP [pie3]
            Dim Nliq As Double      'Prof.desarrollada al nivel de líquido en el espacio anular [pies]
            Dim Qcal As Double      'Gasto calculado del gas de BN de acuerdo al modelo de flujo en el orificio
            Dim Qgc As Double       'Gasto crítico en el orificio de acuerdo al modelo de flujo en el orificio[MMpcd]
            Dim Par1 As Double      'Parámetro (multiplicador) de ajuste para la correlación de Kartoatmodjo [adim]
            Dim Par2 As Double      'Parámetro (sumando) de ajuste para la correlación de Kartoatmodjo [pie3/bl]
            Dim Pexp As Double      'Exponente característico de la correlación de Kartoatmodjo
            Dim Af, Bf, W As Double 'Parámetros en el criterio de estabilidad
            Dim F1, F2 As Double    'Primero y segundo criterios de estabilidad

            Dim Ind_Est As Integer

            ' Variables Doomy
            Dim i, j, ki, l, m, n As Integer ' Varibles de ciclos para los parametros que se varian
            Dim k, mn, cont, Nptos, NDTR As Integer
            Dim iden1, iden2, iden3, poscell As Integer
            Dim iden4, iden5, iden6 As Integer
            Dim cadena As String
            Dim FC As Double

            'Limpia la hoja de cálculo
            '**********************************************************************
            'Sheets("Principal").Range("A21:BM1000").Clear
            'Sheets("Principal").Columns("AN:AN").ColumnWidth = 10.71
            'Sheets("Principal").Cells(18, 41).Value = " "

            'Lee datos de entrada para generar el mapa de estabilidad
            '**********************************************************************

            'Ptr_Min = Val(Sheets("Principal").Cells(9, 3).Value) * 14.223 'Mínima presión en TR
            'Ptr_Max = Val(Sheets("Principal").Cells(10, 3).Value) * 14.223  'Máxima presión en TR
            'Ptr_Num = Val(Sheets("Principal").Cells(11, 3).Value)
            Dim num As Integer
            If (_Ptr_num.Val = 1) Then
                num = 1
            Else
                num = _Ptr_num.Val - 1
            End If
            Ptr_Incr = (_Ptr_max.Val - _Ptr_min.Val) / (num)
            PBN = _Ptr_min.Val  'hay que tener cuidado ya que Ptr se maneja como PBN y Ptr aqui se refiere a la presión en la tr calculada para un volumen de inyección dado

            'Pwh_Min = Val(Sheets("Principal").Cells(9, 5).Value) * 14.223    'Mínima Presión en cabeza
            'Pwh_Max = Val(Sheets("Principal").Cells(10, 5).Value) * 14.223   'Máximo Presión en cabeza
            'Pwh_Num = Val(Sheets("Principal").Cells(11, 5).Value)
            If (_Pwh_num.Val = 1) Then
                num = 1
            Else
                num = _Pwh_num.Val - 1
            End If
            Pwh_Incr = (_Pwh_max.Val - _Pwh_min.Val) / (num)
            Pwh = _Pwh_min.Val

            'Qgi @ c.e. [MMpcd]
            'Qginy_Min = Val(Sheets("Principal").Cells(9, 7).Value)    'Mínimo Gasto de Inyección de gas
            'Qginy_Max = Val(Sheets("Principal").Cells(10, 7).Value)   'Máximo Gasto de Inyección de gas
            'Qginy_Num = Val(Sheets("Principal").Cells(11, 7).Value)
            If (_Qginy_Num.Val = 1) Then
                num = 1
            Else
                num = _Qginy_Num.Val - 1
            End If
            Qginy_Incr = (_Qginy_Max.Val - _Qginy_Min.Val) / (num)
            Qginy = _Qginy_Min.Val

            'Prof. Valvula [md]
            'Dvalvula_Min = Val(Sheets("Principal").Cells(9, 9).Value) * 3.2808 'Mínima profundidad de la valvula
            'Dvalvula_Max = Val(Sheets("Principal").Cells(10, 9).Value) * 3.2808 'Máxima profundidad de la valvula
            'Dvalvula_Num = Val(Sheets("Principal").Cells(11, 9).Value)
            If (_Dvalvula_Num.Val = 1) Then
                num = 1
            Else
                num = _Dvalvula_Num.Val - 1
            End If
            Dvalvula_Incr = (_Dvalvula_Max.Val - _Dvalvula_Min.Val) / (num)
            Dvalvula = _Dvalvula_Min.Val

            'Diám. de orif. [64avo pg]
            'Orif_Min = Val(Sheets("Principal").Cells(9, 11).Value)     'Mínimo diámetro de la valvula
            'Orif_Max = Val(Sheets("Principal").Cells(10, 11).Value)    'Máximo diámetro de la valula
            'Orif_Num = Val(Sheets("Principal").Cells(11, 11).Value)
            If (_Orif_Num.Val = 1) Then
                num = 1
            Else
                num = _Orif_Num.Val - 1
            End If
            Orif_Incr = (_Orif_Max.Val - _Orif_Min.Val) / (num)
            Orif = _Orif_Min.Val

            'Wc [%]
            'Fw_Min = Val(Sheets("Principal").Cells(9, 13).Value)       'Mínimo corte de agua
            'Fw_Max = Val(Sheets("Principal").Cells(10, 13).Value)      'Máximo corte de agua
            'Fw_Num = Val(Sheets("Principal").Cells(11, 13).Value)
            If (_Fw_Num.Val = 1) Then
                num = 1
            Else
                num = _Fw_Num.Val - 1
            End If
            Fw_Incr = (_Fw_Max.Val - _Fw_Min.Val) / (num)
            Fw = _Fw_Min.Val

            'Cd = Val(Sheets("Principal").Cells(9, 14).Value)

            'Validación de los datos básicos de entrada
            '*********************************************************************
            '     If Sheets("Principal").Cells(9, 3).Value = "" Then
            '         MsgBox "Introducir el valor mínimo de Pwh para generar el mapa de estabilidad"
            'Sheets("Principal").Range("C9").Select
            '         GoTo 2000
            '     End If
            '     If Pwh_Incr < 0 Then
            '         MsgBox "Introducir el incremento en Pwh para generar el mapa de estabilidad"
            'Sheets("Principal").Range("C10").Select
            '         GoTo 2000
            '     End If
            '     If Pwh_Num <= 0 Then
            '         MsgBox "El número de puntos en Pwh debe ser mayor o igual a uno"
            'Sheets("Principal").Range("C11").Select
            '         GoTo 2000
            '     End If
            '     If Qginy_Min = 0 Then 'El criterio de estabilidad no aplica a pozos fluyentes
            '         MsgBox "El gasto mínimo del gas de inyección debe ser mayor a cero"
            'Sheets("Principal").Range("F9").Select
            '         GoTo 2000
            '     End If
            '     If Qginy_Num <= 0 Then
            '         MsgBox "El número de puntos en Qgi debe ser mayor o igual a uno"
            'Sheets("Principal").Range("F11").Select
            '         GoTo 2000
            '     End If
            '     If Orif <= 0 Then
            '         MsgBox "El diámetro del orificio debe ser mayor que cero"
            'Sheets("Principal").Range("H9").Select
            '         GoTo 2000
            '     End If
            '     If Cd <= 0 Then
            '         MsgBox "El coeficiente de descarga del orificio debe ser mayor a cero"
            'Sheets("Principal").Range("H11").Select
            '         GoTo 2000
            '     End If
            '     If PBN <= 0 Then
            '         MsgBox "La presión en la red de BN debe ser mayor que cero"
            'Sheets("Principal").Range("O9").Select
            '         GoTo 2000
            '     End If

            'Inicia el enlace con Prosper
            '**********************************************************************
            Server = CreateObject("PX32.OpenServer.1")
            AppName = "PROSPER"
            DoCmd("PROSPER.SETUNITSYS(""OilField"")")

            drginj = Val(DoGet("PROSPER.SIN.GLF.GRAVITY"))

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
            drg = Val(DoGet("PROSPER.PVT.INPUT.GRVGAS"))
            API = Val(DoGet("PROSPER.PVT.INPUT.API"))
            dro = 141.5 / (131.5 + API)

            'Realiza el ajuste de la correlación de Kartoatmodjo
            '************************************************************************
            AJUS_KART(drg, API, RGAf, Twf, Par1, Par2, Pexp)

            'Calcula la relación de volúmenes entre la TP y la TR
            '************************************************************************
            VOLUMENES(Dvalvula * 3.2808, VOLTR, NDTR)  '=====>

            'Borra datos existentes en el módulo
            '"Calculation System (IPR-VLP)" de Prosper
            '************************************************************************
            For cont = 0 To 9
                DoSet("PROSPER.ANL.SYS.SENS.VARS.VAL1[" + CStr(cont) + "]", "")
                DoSet("PROSPER.ANL.SYS.SENS.VARS.VAL2[" + CStr(cont) + "]", "")
                DoSet("PROSPER.ANL.SYS.SENS.VARS.VAL3[" + CStr(cont) + "]", "")
                DoSet("PROSPER.ANL.SYS.RATES[" + CStr(cont) + "]", "")
            Next

            'Asigna Qgi como variable de sensibilidad
            'en el módulo "Calculation System (IPR-VLP)" de Prosper
            '*************************************************************************
            DoSet("PROSPER.ANL.SYS.RATEMETHOD", "1") 'User Selected
            DoSet("PROSPER.ANL.SYS.SENS.VARS.VAR1", "22") 'Selecciona qgi
            DoSet("PROSPER.ANL.SYS.SENS.VARS.VAR2", " ")
            DoSet("PROSPER.ANL.SYS.SENS.VARS.VAR3", " ")

            'Vacía las celdas para los gastos de aceite, si es que ya contienen datos,
            'en el módulo "Calculation System (IPR-VLP)" de Prosper
            '*************************************************************************
            For cont = 0 To 19
                DoSet("PROSPER.ANL.SYS.RATES[" + CStr(cont) + "]", "")
            Next

            'Asigna Qgi como variable de sensibilidad
            'en el módulo "Gradient (traverse)" de Prosper
            '*************************************************************************
            DoSet("PROSPER.ANL.GRD.SENS.VARS.VAR1", "22")
            DoSet("PROSPER.ANL.GRD.SENS.VARS.VAR2", " ")
            DoSet("PROSPER.ANL.GRD.SENS.VARS.VAR3", " ")

            'Vacía las celdas para los gastos de aceite, si es que ya contienen datos,
            'en el módulo "Gradient (traverse)" de Prosper
            '*************************************************************************
            For cont = 0 To 2
                DoSet("PROSPER.ANL.GRD.SENS.VARS.VAL1[" + CStr(cont) + "]", "")
            Next

            k = 0
            'Nptos = Npwh * Nqinj + 20
            poscell = 48
            iden1 = 0 : iden2 = 0 : iden3 = 0
            iden4 = 0 : iden5 = 0 : iden6 = 0

            For i = 0 To _Ptr_num.Val - 1
                PBN = (_Ptr_min.Val + _Ptr_Incr.Val * i) * 14.223

                For j = 0 To _Pwh_num.Val - 1
                    Pwh = (_Pwh_min.Val + _Pwh_Incr.Val * j) * 14.223

                    For ki = 0 To _Dvalvula_Num.Val - 1
                        Dvalvula = (_Dvalvula_Min.Val + _Dvalvula_Incr.Val * ki) * 3.2808
                        DoSet("PROSPER.SIN.GLF.ValveDepth", Str(Dvalvula))
                        If (_Dvalvula_Incr.Val > 0) Then Call VOLUMENES(Dvalvula, VOLTR, NDTR)

                        For l = 0 To _Orif_Num.Val - 1
                            Orif = _Orif_Min.Val + _Orif_Incr.Val * l

                            For m = 0 To _Fw_Num.Val - 1
                                Fw = _Fw_Min.Val + _Fw_Incr.Val * m
                                DoSet("PROSPER.SIN.IPR.Single.Wc", Str(Fw))
                                DoCmd("PROSPER.IPR.CALC")

                                For n = 0 To _Qginy_Num.Val - 1
                                    Qginy = _Qginy_Min.Val + _Qginy_Incr.Val * n

                                    'Encuentra la solución del sistema de producción
                                    '*************************************************************************
                                    SYSTEM_IPR_VLP(Pwh, Fw, Qginy, Ql, Qoil, Qgf, Twh, PwfIPR)

                                    If (Ql = 0) Then

                                        F1 = -2
                                    Else
                                        'Obtiene los parámetros del flujo en la TP requeridos para evaluar F1
                                        '********************************************************************************
                                        PERFIL_PTO(Pwh, Fw, Ql, Qginy, Dvalvula, Pti, Tti, Pwf, Densf, Qfo, LVinj, VOLTP)

                                        'Evalúa el factor F1 del primer criterio de estabilidad
                                        '********************************************************************************
                                        CRITERIO_F1(Pyac, Qmax, Ql, Qginy, drg, drginj, dro, API, RGAf,
                                                Par1, Par2, Pexp, Orif, _Cd.Val, Pwf, Twf, Pti, Tti, Af, Bf,
                                                zgfti, Bgfti, zgfprom, Bgprom, zginj, Bginj, Boti, Boprom, W, F1)

                                        'Si el primer criterio no se cumple, se calcula el factor F2 de Asheim (1988)
                                        'en combinación con el factor F1 del primer criterio
                                        '********************************************************************************
                                        If F1 < 1.0 Then
                                            CRITERIO_F2(1.0, Pti, VOLTR, VOLTP, LVinj, Densf, drginj * 0.0764, Qginy, Bginj, Qfo, F1, F2)
                                        End If
                                        If Qginy > 0 Then ESP_ANULAR(Qginy, drginj, Pti, Tti, Twh, _Cd.Val, Orif / 64.0, DATTR, NDTR, Pv, Ptr, Qcal, Qgc)

                                    End If
                                    'If Qginy > 0 Then ': GoTo 100

                                    'Calcula los parámetros del flujo en el sistema de inyección
                                    '********************************************************************************
                                    '100:

                                    'Imprime solución del sistema IPR y VLP
                                    '********************************************************************************
                                    'Sheets("Principal").Cells(20 + k, 2).Value = Pwh / 14.223
                                    'Sheets("Principal").Cells(20 + k, 3).Value = Qginy
                                    'Sheets("Principal").Cells(20 + k, 4).Value = Ql

                                    '_QlArray(k) = Ql

                                    'Sheets("Principal").Cells(20 + k, 5).Value = Qgf

                                    '_QgArray(k) = Qgf

                                    'Sheets("Principal").Cells(20 + k, 6).Value = Qginy + Qgf
                                    'Sheets("Principal").Cells(20 + k, 7).Value = Qmax
                                    'Sheets("Principal").Cells(20 + k, 8).Value = Pyac

                                    '_PwsArray(k) = Pyac

                                    'Sheets("Principal").Cells(20 + k, 9).Value = Pwf

                                    'PwfArray(k) = Pwf

                                    'Sheets("Principal").Cells(20 + k, 10).Value = Twh
                                    'Sheets("Principal").Cells(20 + k, 11).Value = Twf
                                    'Sheets("Principal").Cells(20 + k, 12).Value = Orif

                                    '_DiaValArray(k) = Orif

                                    'Imprime resultados del gradiente
                                    '********************************************************************************
                                    'Sheets("Principal").Cells(20 + k, 13).Value = Dvalvula / 3.2808

                                    '_DvalveArray(k) = Dvalvula / 3.2808

                                    'Sheets("Principal").Cells(20 + k, 14).Value = LVinj
                                    'Sheets("Principal").Cells(20 + k, 15).Value = Pti

                                    '_PtiArray(k) = Pti

                                    'Sheets("Principal").Cells(20 + k, 16).Value = Tti

                                    '_TvalvArray(k) = Tti

                                    '                        Sheets("Principal").Cells(20 + k, 17).Value = Densf
                                    'Sheets("Principal").Cells(20 + k, 18).Value = Qfo
                                    'Sheets("Principal").Cells(20 + k, 19).Value = drginj * 0.0764 / Bginj
                                    'Sheets("Principal").Cells(20 + k, 20).Value = Qginy * Bginj * 10 ^ 6 / 86400.0
                                    'Sheets("Principal").Cells(20 + k, 21).Value = Boti
                                    'Sheets("Principal").Cells(20 + k, 22).Value = zgfti
                                    'Sheets("Principal").Cells(20 + k, 23).Value = Bgfti
                                    'Sheets("Principal").Cells(20 + k, 24).Value = zginj
                                    'Sheets("Principal").Cells(20 + k, 25).Value = Bginj
                                    'Sheets("Principal").Cells(20 + k, 26).Value = Boprom
                                    'Sheets("Principal").Cells(20 + k, 27).Value = zgfprom
                                    'Sheets("Principal").Cells(20 + k, 28).Value = Bgprom
                                    'Sheets("Principal").Cells(20 + k, 29).Value = Fw

                                    '_WcArray(k) = Fw

                                    'Imprime resultados del modelo del espacio anular
                                    '*************************************************************************
                                    'Sheets("Principal").Cells(20 + k, 30).Value = Qginy
                                    'Sheets("Principal").Cells(20 + k, 31).Value = Pwh
                                    'Sheets("Principal").Cells(20 + k, 32).Value = Pti
                                    'Sheets("Principal").Cells(20 + k, 33).Value = Pv

                                    '_PtriArray(k) = Pv

                                    'Sheets("Principal").Cells(20 + k, 34).Value = Ptr / 14.223

                                    '_PtrcalArray(k) = Ptr / 14.223

                                    'Sheets("Principal").Cells(20 + k, 35).Value = PBN / 14.223

                                    '_PtrArray(k) = PBN / 14.223

                                    'Sheets("Principal").Cells(20 + k, 36).Value = Twh
                                    'Sheets("Principal").Cells(20 + k, 37).Value = Tti
                                    'Sheets("Principal").Cells(20 + k, 38).Value = Qcal
                                    'Sheets("Principal").Cells(20 + k, 39).Value = Qgc

                                    '_QgcritArray(k) = Qgc

                                    'Sheets("Principal").Cells(20 + k, 40).Value = (Pv - Pti) / 14.223
                                    'Sheets("Principal").Cells(20 + k, 42).Value = Qginy

                                    '_QgiArray(k) = Qginy
                                    'Sheets("Principal").Cells(20 + k, 43).Value = Pwh / 14.223

                                    '_PwhArray(k) = Pwh / 14.223

                                    'Sheets("Principal").Cells(20 + k, 44).Value = F1

                                    '_GORArray(k) = Qgf / (Ql * ((100 - Fw) / 100)) 'Nuevo
                                    '_DpvalveArray(k) = Pv - Pti
                                    '_DpArray(k) = Pyac - Pwf
                                    '_PIArray(k) = Ql / (Pyac - Pwf)
                                    '_HTCArray(k) = DoGet("PROSPER.SIN.EQP.Geo.Htc")


                                    If (F1 >= 1 Or F2 >= 1) Then
                                        If (Pv <= Pti) Then
                                            Ind_Est = -2                'Inoperante
                                            If (Qgc < Qginy) Then
                                                Ind_Est = -1            'Gasto de gas mayor al critico
                                            End If
                                        ElseIf (PBN < Ptr) Then
                                            Ind_Est = 1                 'Presión Insuficiente para inyectar el Qgi deseado
                                        Else
                                            Ind_Est = 2                 'Estable
                                        End If
                                    Else
                                        Ind_Est = 0                     'Inestable
                                    End If

                                    '_Ind_EstArray(k) = Ind_Est

                                    k = k + 1

                                    'If Qginy > Qgc Then: Stop: GoTo 500 'Detiene los cálculos en Qgi para dada pwh (se alcanzó flujo crítico en el orificio

                                    'OJO, Hay que revisar

                                    'ObjParametrosEstabilidad.Qg = Qgf
                                    ' ObjParametrosEstabilidad.GOR_quicklook = GOR_quicklook
                                    'ObjParametrosEstabilidad.GORFREE = GORFREE
                                    Dim ObjParametrosEstabilidad As New Parametros_Estabilidad With {
                                        .Ptr = PBN / 14.223,
                                        .Pwh = Pwh / 14.223,
                                        .Qgi = Qginy,
                                        .Dvalve = Dvalvula / 3.2808,
                                        .DiaVal = Orif,
                                        .Wc = Fw,
                                        .GOR = Qgf / (Ql * ((100 - Fw) / 100)),
                                        .Ql = Ql,
                                        .Pwf_IPR = Pwf / 14.223,
                                        .Twh = (5 / 9) * (Twh - 32),
                                        .TotalQgas = Qgf,
                                        .Pwf_quicklook = Pwf / 14.223,
                                        .Pws = Pyac / 14.223,
                                        .Pti = Pti / 14.223,
                                        .Ptri = Pv / 14.223,
                                        .Tvalv = (5 / 9) * (Tti - 32),
                                        .Dpvalve = (Pv - Pti) / 14.223,
                                        .Ptrcalc = Ptr / 14.223,
                                        .Dp = (Pyac - Pwf) / 14.223,
                                        .PI = Ql * 14.223 / (Pyac - Pwf),
                                        .Qgcrit = Qgc,
                                        .Qcporcent = 100 * Qginy / Qgc
                                    }
                                    Dim HTC As Double = DoGet("PROSPER.SIN.EQP.Geo.Htc")
                                    ObjParametrosEstabilidad.HTC = HTC
                                    ObjParametrosEstabilidad.Ind_Est = Ind_Est

                                    ListaParametrosEstabilidad.Add(ObjParametrosEstabilidad)

                                Next n
                            Next m
                        Next l
                    Next ki
                Next j
                '500:
            Next i

            Arbol_Decision(_Ptr_Act.Val, _Pwh_Act.Val, _Qgi_Act.Val, _Dvalve_Act.Val, _DiaValve_Act.Val, _Wc_Act.Val, _Caso.Val)
            'Sheets("Principal").Range("AT13").Select
            'MsgBox("Proceso concluido exitosamente.")
            '2000:
            Server = Nothing
        End Sub

        Sub New(ByVal Ptr_Min As Double, ByVal Ptr_Max As Double, ByVal Ptr_Num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_Num As Integer, ByVal Qginy_Min As Double, ByVal Qginy_Max As Double, ByVal Qginy_Num As Integer, ByVal Dvalvula_Min As Double, ByVal Dvalvula_Max As Double, ByVal Dvalvula_Num As Integer, ByVal Orif_Min As Double, ByVal Orif_Max As Double, ByVal Orif_Num As Double, ByVal Fw_Min As Double, ByVal Fw_Max As Double, ByVal Fw_Num As Double, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal fecha As String)
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

            'Dim Ptr_Act As Double = 69
            'Dim Pwh_Act As Double = 29
            'Dim Qgi_Act As Double = 12.5
            'Dim Dvalve_Act As Double = 1900
            'Dim DiaValve_Act As Double = 48
            'Dim Wc_Act As Double = 0

            'Dim Caso As Integer = 1
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
                Dim ptraux As Double = _Ptr_min.Val + i * _Ptr_Incr.Val '_Ptr_incremento.Val
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
                Dim pwhaux As Double = _Pwh_min.Val + i * _Pwh_Incr.Val '_Pwh_incremento.Val
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
                Dim Qgiaux As Double = _Qginy_Min.Val + i * _Qginy_Incr.Val
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
                Dim Dvalveaux As Double = _Dvalvula_Min.Val + i * _Dvalvula_Incr.Val
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
                Dim Diavalaux As Double = _Orif_Min.Val + i * _Orif_Incr.Val
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
                Dim Wcaux As Double = _Fw_Min.Val + i * _Fw_Incr.Val
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

        Class Parametros_Estabilidad
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
            Property Ind_Est As Integer

        End Class

        Sub DoCmd(Cmd)

            'Subrutina para ejecutar los comandos de Prosper
            '*************************************************************
            Dim lErr As Long
            lErr = Server.Docommand(Cmd)
            If lErr > 0 Then
                MsgBox(Server.GetErrorDescription(lErr))
                Server = Nothing

            End If

        End Sub

        Sub DoSet(Sv, Val)

            'Subrutina para enviar datos de Excel a Prosper
            '************************************************************
            Dim lErr As Long
            lErr = Server.Setvalue(Sv, Val)
            lErr = Server.GetLastError(AppName)
            If lErr > 0 Then
                MsgBox(Server.GetErrorDescription(lErr))
                Server = Nothing

            End If

        End Sub

        Function DoGet(Gv As String) As String

            'Función para enviar datos de Prosper a Excel
            '************************************************************
            Dim lErr As Long
            DoGet = Server.GetValue(Gv)
            lErr = Server.GetLastError(AppName)
            If lErr > 0 Then
                MsgBox(Server.GetErrorDescription(lErr))
                Server = Nothing

            End If

        End Function
        Private Function getLista(ByVal arreglo() As Double) As List(Of Double)

            Dim result As New List(Of Double)

            For Each elem In arreglo
                result.Add(elem)
            Next

            Return result
        End Function

        Sub CRITERIO_F1(ByRef Pyac As Double, ByRef Qmax As Double, ByRef Ql As Double, ByRef Qinj As Double, ByRef drg As Double, ByRef drginj As Double, ByRef dro As Double, ByRef API As Double, ByRef RGAf As Double,
        ByRef Par1 As Double, ByRef Par2 As Double, ByRef Pexp As Double, ByRef Orif As Double, ByRef Cd As Double, ByRef Pwf As Double, ByRef Twf As Double, ByRef Pti As Double, ByRef Tti As Double, ByRef Af As Double, ByRef Bf As Double,
        ByRef zgfti As Double, ByRef Bgfti As Double, ByRef zgfprom As Double, ByRef Bgprom As Double, ByRef zginj As Double, ByRef Bginj As Double, ByRef Boti As Double, ByRef Boprom As Double, ByRef W As Double, ByRef F1 As Double)

            'Calcula el factor F1 del criterio propuesto por Fairuzov y Guerrero (2004)
            '****************************************************************************

            Dim Tprom, Pprom As Double
            Dim Kernel, Kprom, Kti As Double
            Dim Cpti, Cprom As Double
            Dim Bti As Double
            Dim Omega, Aori As Double
            Dim Rsprom, Rsti As Double

            Tprom = (Twf + Tti) / 2.0
            Pprom = (Pwf + Pti) / 2.0

            If API <= 30 Then
                Kernel = (0.05958 * drg ^ 0.7972) / 5.615
                Kprom = Kernel * 10 ^ ((13.1405 * API) / (Tprom + 460.0#))
                Kti = Kernel * 10 ^ ((13.1405 * API) / (Tti + 460.0#))
            Else
                Kernel = (0.0315 * drg ^ 0.7587) / 5.615
                Kprom = Kernel * 10 ^ ((11.289 * API) / (Tprom + 460.0#))
                Kti = Kernel * 10 ^ ((11.289 * API) / (Tti + 460.0#))
            End If

            zgfti = factz(Pti + 14.7, Tti, drg)
            Cpti = 0.0283 * zgfti * (Tti + 460.0#)
            Bgfti = Cpti / (Pti + 14.7)
            Rsti = Par1 * Kti * (Pti + 14.7) ^ Pexp + Par2 / 5.615

            zgfprom = factz(Pprom + 14.7, Tprom, drg)
            Cprom = 0.0283 * zgfprom * (Tprom + 460.0#)
            Bgprom = Cprom / (Pprom + 14.7)
            Rsprom = Par1 * Kprom * (Pprom + 14.7) ^ Pexp + Par2 / 5.615

            zginj = factz(Pti + 14.7, Tti, drginj)
            Cpti = 0.0283 * zginj * (Tti + 460.0#)
            Bginj = Cpti / (Pti + 14.7)

            'Calcula factores de volumen del aceite con PROSPER
            '************************************************************
            DoSet("PROSPER.PVT.CALC.MODE", 1)
            DoSet("PROSPER.PVT.CALC.PRESUSER[0]", Pprom)
            DoSet("PROSPER.PVT.CALC.TEMPUSER[0]", Tprom)
            DoCmd("PROSPER.PVT.CALC")
            Boprom = Val(DoGet("PROSPER.PVT.CALC.RESULTS[0].OILFVF"))

            DoSet("PROSPER.PVT.CALC.MODE", 1)
            DoSet("PROSPER.PVT.CALC.PRESUSER[0]", Pti)
            DoSet("PROSPER.PVT.CALC.TEMPUSER[0]", Tti)
            DoCmd("PROSPER.PVT.CALC")

            Boti = Val(DoGet("PROSPER.PVT.CALC.RESULTS[0].OILFVF"))
            Bti = Boti + (RGAf - Rsti) * Bgfti

            Af = (RGAf - Par2 / 5.615) * Cprom / Boprom
            Bf = Par1 * Kprom * Cprom / Boprom / Pexp

            Omega = Math.Sqrt(1.0 + 80.0 * (1.0 - Ql / Qmax))

            Pwf = ((Pyac) / 8.0#) * (-1.0# + Omega) + 14.7 'presión absoluta
            Pti = Pti + 14.7 'presión absoluta

            W = (0.2 * Pwf / Pyac) * (Qmax / Ql) *
            ((Af + Pti - Bf * Pexp * Pti ^ Pexp) /
            (Af + Pwf - Bf * Pexp * Pwf ^ Pexp)) * Omega
            W = W + (Bgfti / Bti) * (RGAf - Par2 / 5.615 + Par1 * Kti * (Pexp - 1) * Pti ^ Pexp)
            W = W / Pti

            Aori = 3.14159 * (Orif / 64.0#) ^ 2 / 144.0# / 4.0#

            F1 = (Bginj * (Qinj * 10 ^ 6 / 86400.0#) ^ 2 * 0.0764 * drginj * W) / (Cd * Aori) ^ 2 / 144.0# / 32.2

            Pwf = Pwf - 14.7 'presión manométrica
            Pti = Pti - 14.7 'presión manométrica

        End Sub

        Sub CRITERIO_F2(ByRef CTRTP, ByRef Pti, ByRef VOLTR, ByRef VOLTP, ByRef LVinj, ByRef Densf, ByRef Densg, ByRef Qinj, ByRef Bginj, ByRef Qfo, ByRef F1, ByRef F2)

            'Calcula el segundo criterio de estabildad de Asheim (1988)
            'con base en el primer criterio de Fairuzov y Guererro (2004)
            'Se considera que CTRTP= (Pti/Tti/Zti)/(Pci/Tci/Zci)=1
            '****************************************************************************
            F2 = LVinj * (Densf - Densg / Bginj) * (1 - F1)
            F2 = CTRTP * (VOLTP / VOLTR) * 144 * (Pti + 14.7) * (1 + Qinj * Bginj * 10 ^ 6 / Qfo / 86400.0#) / F2

        End Sub

        Sub ESP_ANULAR(ByRef qBN As Double, ByRef drgbn As Double, ByRef Pti As Double, ByRef Tti As Double, ByRef Twh As Double, ByRef Cdes As Double, ByRef Orif As Double,
                ByRef DATTR(,) As Double, ByRef NDTR As Double, ByRef P2EA As Double, ByRef Ptr As Double, ByRef Qcal As Double, ByRef Qgc As Double)

            'Subrutina principal para los modelos del orificio y del espacio anular.
            'Para el modelo del orificio se considera el flujo adiabático a través de
            'un orificio, Beggs(1999). Para el modelo del espacio anular se considera
            'la ecuación de Ikoku (1998).
            '********************************************************************************

            '***************        Modelo del Orificio          ****************************
            'Calcula la presión a la entrada del orificio dados
            'qbn(Mscfd),Psal (psia),Tent(°F),diam(pg),(Presiones absolutas)
            'se asume que la relación de calores específicos del gBN es 1.25
            '********************************************************************************

            Pres1(qBN * 1000.0, Orif, Cdes, Tti, Pti + 14.7, 1.25, drgbn, P2EA, Qcal, Qgc)


            If qBN <= Qgc Then ': GoTo 300

                '***************     Modelo del espacio anular   ********************************
                'Calcula la presión en TR después de la válvula superficial de inyección
                'dados Tmea (°F), qBN(MMscfd), L(ft), Diam(pg). Se considera un gradiente
                'lineal de temperatura en el espacio anular.
                '********************************************************************************

                Dim GraTemp = (Tti - Twh) / DATTR(1, NDTR)
                Dim T1 = Tti
                Dim P1EA As Double = 0
                For i = NDTR To 1 Step -1
                    Dim Tmea = 0.5 * GraTemp * (DATTR(1, i) + DATTR(1, i - 1)) + Twh
                    MD = DATTR(1, i) - DATTR(1, i - 1)
                    TVD = DATTR(2, i) - DATTR(2, i - 1)
                    If _MD.Val <= 0 Or _TVD.Val <= 0 Then Exit For ': GoTo 150
                    DTR = DATTR(3, i)
                    DTP = DATTR(4, i)
                    Dim Rugtub = DATTR(5, i)
                    If i = NDTR Then
                        P1EA = PDVBN(drgbn, P2EA, Tmea, qBN, _MD.Val, _TVD.Val, _DTR.Val, _DTP.Val, Rugtub)
                    Else
                        P1EA = PDVBN(drgbn, P1EA, Tmea, qBN, _MD.Val, _TVD.Val, _DTR.Val, _DTP.Val, Rugtub)
                    End If
                Next
                Ptr = P1EA - 14.7 'Presión abs-man
                P2EA = P2EA - 14.7 'Presión manométrica

            End If

        End Sub

        Sub Pres1(ByRef Gasto As Double, ByRef Dorif As Double, ByRef Cdes As Double, ByRef Temp As Double, ByRef Pres2 As Double, ByRef Kcp As Double, ByRef drgbn As Double, ByRef Pres1 As Double, ByRef Qcal As Double, ByRef Qgc As Double)

            '***************    Modelo del Orificio          ********************************
            'Calcula la presión a la entrada del orificio dados qbn(Mscfd),P2 (psia),
            'T1(°F)y el diámetro del orificio (pg).(Las presiones son absolutas).
            'Se asume que la relación de calores específicos del gBN es 1.25
            '********************************************************************************
            Dim Zbn As Double
            Dim pseudoQgas As Double = 0
            Dim incre As Double = 1
            Pres1 = Pres2

            'Do
            '    Zbn = factz(Pres1, Temp, drgbn)
            '    pseudoQgas = 38.875 * Cdes * Math.PI * Dorif ^ 2 * Pres1 * Math.Sqrt(2 * 32.2 * (Kcp / (Kcp - 1)) * ((Pres2 / Pres1) ^ (2 / Kcp) - (Pres2 / Pres1) ^ ((Kcp + 1) / Kcp))) / (Math.Sqrt(Zbn * drgbn * (Temp + 460)))
            '    If (100 * Math.Abs((Gasto - pseudoQgas) / Gasto) < 0.5) Then Exit Do
            '    pseudoP1 = Pres1
            '    If (pseudoQgas < Gasto) Then
            '        If (incre > 0) Then Pres1 = Pres1 + incre
            '        If (incre < 0) Then Pres1 = Pres1 - 0.5 * incre
            '        incre = (Pres1 - pseudoP1)
            '    Else
            '        Pres1 = Pres1 - 0.5 * Math.Abs(incre)
            '        incre = Pres1 - pseudoP1
            '    End If
            'Loop

            'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            Dim CTA = 976.72 * Cdes * Dorif ^ 2 * Math.Sqrt(Kcp / (Kcp - 1) / (Temp + 460.0#) / drgbn)
            Dim A = 2 / Kcp
            Dim B = (Kcp + 1) / Kcp

            ''Valor inicial supuesto
            ''*************************************************************
            Dim Psup = Pres2 / 0.8
            'iterar:
            Dim niter = 0
            Dim yc, CTE As Double

            Do
                'If (niter = 4) Then Stop
                yc = Pres2 / Psup
                Zbn = factz(Psup, Temp, drgbn)
                CTE = CTA ^ 2 / Zbn
                Dim Omega = Gasto ^ 2 - CTE * Psup ^ 2 * (yc ^ A - yc ^ B)
                Dim Domega = -CTE * Psup * (yc ^ B * (B - 2) - yc ^ A * (A - 2))

                Pres1 = Psup - Omega / Domega

                If niter < 100 Then
                    If Math.Abs(1 - Pres1 / Psup) > 0.001 Then
                        Psup = Pres1
                        niter = niter + 1
                        'GoTo iterar
                    Else
                        Exit Do
                    End If
                Else
                    MsgBox("Se produjo un error en la subrutina del modelo del orificio")
                End If
            Loop While (niter < 100)

            'Para comprobar
            '*******************************************************************
            yc = Pres2 / Pres1
            Qcal = 976.72 * Cdes * Dorif ^ 2 * Math.Sqrt(Kcp / (Kcp - 1)) /
Math.Sqrt(drgbn * (Temp + 460.0#) * Zbn) * Pres1 * Math.Sqrt(yc ^ A - yc ^ B)
            Qcal = Qcal / 1000.0# 'MMpcd

            'Calcula el gasto crítico
            '************************************************************
            yc = (2 / (Kcp + 1)) ^ (Kcp / (Kcp - 1))
            Zbn = factz(Pres1, Temp, drgbn)
            CTE = CTA ^ 2 / Zbn
            Qgc = (CTE * Pres1 ^ 2 * (yc ^ A - yc ^ B)) ^ 0.5 'En Mpcd
            Qgc = Qgc / 1000.0#

        End Sub

        Function PDVBN(ByRef drgbn As Double, ByRef P2 As Double, ByRef Tmed As Double, ByRef Gasto As Double, ByRef MD As Double, ByRef TVD As Double, ByRef dext As Double, ByRef dint As Double, ByRef Rugtub As Double)
            Dim contador As Integer = 0
            'Determina la presión después de la válvula superficial de inyección
            'con la ecuación de Ikoku dados qbn(MMscfd),P2 (psia),Tmed(°F),diam(pg),
            'MD(ft),TVD(ft),Dext(pg), y Dint(pg)
            '************************************************************************

            'Constantes para calcular el gradiente según fórmula de Ikoku
            '*************************************************************************
            Dim s = 0.0375 * drgbn * TVD / (Tmed + 460.0#)
            Dim CDA = 25 * drgbn * Gasto ^ 2 * (Tmed + 460.0#) * MD / s / (dext - dint) ^ 3 / (dext + dint) ^ 2

            'Constante para calcular el número de Reynolds
            '*************************************************************************
            Dim CNRE = 20103.6 * Gasto * drgbn / (dext + dint)

            Dim Psup = P2 * 0.95
            'iterar:
            Dim niter = 0
            Do
                Dim Zmed = factz(0.5 * (P2 + Psup), Tmed, drgbn)

                'Cálculo de la densidad del gas a las cond. de flujo, (lbm/pc)
                '****************************************************************
                Dim DNG = 2.7 * drgbn * 0.5 * (P2 + Psup) / (Zmed * (Tmed + 460))

                'Cálculo de la viscosidad del gas, (cp)
                '****************************************************************
                'Dim Mug = 0

                Dim Mug = VISCG(Tmed, DNG, drgbn)

                'Cálculo del factor de fricción según Jain
                '*****************************************************************
                Dim fg As Double = FJAIN(CNRE / Mug, dext - dint, Rugtub)

                Dim P1 As Double = ((P2 ^ 2 + CDA * Zmed ^ 2 * fg * (Math.Exp(s / Zmed) - 1)) / Math.Exp(s / Zmed)) ^ 0.5

                PDVBN = P1

                If niter < 100 Then
                    If Math.Abs(1 - P1 / Psup) > 0.001 Then
                        Psup = P1
                        niter = niter + 1
                        '    'GoTo iterar
                    Else
                        Exit Do
                    End If
                Else
                    MsgBox("Se produjo un error en la subrutina del modelo de Ikoku")
                    Exit Do
                End If
            Loop While (niter < 100)

        End Function

        Function FJAIN(NRE, D, E) As Double

            'Determina el factor de fricción según ecuación de Jain
            '***********************************************************************
            FJAIN = 1 / (1.14 - 2 * Math.Log(E / D + 21.25 / NRE ^ 0.9) / Math.Log(10.0)) ^ 2

        End Function

        Function VISCG(ByRef T As Double, ByRef Densgas As Double, ByRef denrelgas As Double) As Double

            'Subrutina para el cálculo de la viscosidad del gas
            'Utilizando la correlación de Lee
            '******************************************************************
            Dim X As Double = 3.448 + 986.4 / (T + 460.0#) + 0.2897 * denrelgas
            Dim Y As Double = 2.4 - 0.2 * X
            Dim Klee As Double = (9.379 + 0.5794 * denrelgas) * (T + 460.0#) ^ 1.5 /
        (209.2 + 550.4 * denrelgas + 460.0# + T)
            VISCG = Klee * Math.Exp(X * (Densgas / 62.428) ^ Y) / 10 ^ 4

        End Function

        Function factz(ByRef P As Double, ByRef T As Double, ByRef denrelgas As Double) As Double

            'Subrutina para el cálculo del factor de desviación del gas
            'utilizando la correlación de Standing y Katz
            '*****************************************************************
            Dim Zgcal, Zgsup, ro As Double
            Dim Ppr, Ppc, Tpr, Tpc As Double
            Dim A1, A2, A3, A4, A5, A6, A7, A8 As Double

            'Propiedades Pseudocríticas según Sutton
            '*****************************************************************
            Tpc = 169.2 + 349.5 * denrelgas - 74 * denrelgas ^ 2
            Ppc = 756.8 - 131 * denrelgas - 3.6 * denrelgas ^ 2
            A1 = 0.31506 : A2 = -1.0467 : A3 = -0.5783 : A4 = 0.5353
            A5 = -0.6123 : A6 = -0.10489 : A7 = 0.68157 : A8 = 0.68446

            ' Propiedades pseudoreducidas del gas seco
            '******************************************************************
            Tpr = (T + 460) / Tpc
            Ppr = P / Ppc

            Zgsup = 0.27

            Dim iter = 1
            'iterarz:

            Do
                ro = (0.27 * Ppr) / (Zgsup * Tpr)

                Zgcal = 1 + (A1 + A2 / Tpr + A3 / Tpr ^ 3) * ro +
            (A4 + A5 / Tpr) * ro ^ 2 + A5 * A6 * ro ^ 5 / Tpr +
            (A7 * ro ^ 2 / Tpr ^ 3) * (1 + A8 * ro ^ 2) * Math.Exp(-A8 * ro ^ 2)

                If iter < 100 Then
                    If Math.Abs(Zgcal - Zgsup) > 0.001 Then
                        Zgsup = Zgcal
                        iter = iter + 1
                        'GoTo iterarz
                    Else
                        factz = Zgcal
                        Exit Do
                    End If
                Else
                    MsgBox("Se produjo un error en la subrutina del factor de compresibilidad")
                    Exit Do
                End If
            Loop While (iter < 100)

            Return factz
        End Function


        Sub PERFIL_PTO(ByRef Pwh As Double, ByRef Fw As Double, ByRef Ql As Double, ByRef Qginy As Double, ByRef Dvalvula As Double, ByRef Pti As Double, ByRef Tti As Double, ByRef Pwf As Double, ByRef Densf As Double, ByRef Qfo As Double, ByRef LVinj As Double, ByRef VOLTP As Double)
            'Subrutina para extraer de Prosper los parámetros del flujo en la TP
            '***********************************************************************
            Dim Vsg, Vsl, dpto, Atp As Double
            Dim Vsgi, Vsli, Vmi As Double
            Dim Lup, Ldo, DLi As Double
            Dim SUMVOL As Double
            Dim cadena As String
            Dim cont, iden As Integer
            Dim i, j, k As Integer
            Dim CPto As Double

            DoSet("PROSPER.ANL.GRD.PRES", Str(Pwh))
            DoSet("PROSPER.ANL.GRD.WC", Str(Fw))
            DoSet("PROSPER.ANL.GRD.RATE", Str(Ql))
            DoSet("PROSPER.ANL.GRD.SENS.VARS.VAL1[0]", Str(Qginy))
            DoCmd("PROSPER.ANL.GRD.CALC")

            'Elementos del arreglo:
            '****************************************************************
            cadena = Str(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].NUM]") - 1) 'empieza a contar en cero
            cont = CInt(cadena)
            iden = 0
            SUMVOL = 0

            Pwf = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].PRES[" + cadena + "]"))

            For i = cont To 1 Step -1
                CPto = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i) + "]"))
                If Math.Abs(CPto - Dvalvula) < 0.5 Then
                    If iden = 0 Then
                        Densf = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].AVRHSL[" + Str(i + 1) + "]"))
                        Vsl = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VLNSLP[" + Str(i + 1) + "]"))
                        Vsg = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VGNSLP[" + Str(i + 1) + "]"))
                        dpto = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].DIAINT[" + Str(i + 1) + "]"))
                        Atp = 3.14159 * (dpto / 12.0#) ^ 2 / 4
                        Qfo = (Vsl + Vsg) * Atp
                        Vsl = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VLNSLP[" + Str(i) + "]"))
                        Vsg = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VGNSLP[" + Str(i) + "]"))
                        Vmi = Vsl + Vsg
                        LVinj = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].TVD[" + Str(i) + "]"))
                        Pti = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].PRES[" + Str(i) + "]"))
                        Tti = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].TEMP[" + Str(i) + "]"))
                        Lup = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i - 1) + "]"))
                        Ldo = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i) + "]"))
                        DLi = Ldo - Lup
                        iden = 1
                    Else
                        Vsl = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VLNSLP[" + Str(i + 1) + "]"))
                        Vsg = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VGNSLP[" + Str(i + 1) + "]"))
                        dpto = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].DIAINT[" + Str(i + 1) + "]"))
                        Atp = 3.14159 * (dpto / 12.0#) ^ 2 / 4
                        Vmi = Vsl + Vsg
                        LVinj = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].TVD[" + Str(i) + "]"))
                        Pti = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].PRES[" + Str(i) + "]"))
                        Tti = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].TEMP[" + Str(i) + "]"))
                        Lup = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i - 1) + "]"))
                        Ldo = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i) + "]"))
                        DLi = Ldo - Lup
                    End If
                Else
                    If iden = 1 Then
                        Vsl = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VLNSLP[" + Str(i) + "]"))
                        Vsg = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].VGNSLP[" + Str(i) + "]"))
                        Lup = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i - 1) + "]"))
                        Ldo = Val(DoGet("PROSPER.OUT.GRD.RESULTS[0][0][0].MSD[" + Str(i) + "]"))
                        SUMVOL = SUMVOL + (Vsl + Vsg) * (Ldo - Lup)
                    End If
                End If
            Next i

            VOLTP = 1.2 * (DLi + SUMVOL / Vmi)
            VOLTP = Atp * Dvalvula ^ 2 / VOLTP

        End Sub

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

            'Limpia la hoja de cálculo
            '*******************************************************
            'Sheets("PVT").Range("B17:F100").Clear
            'Sheets("PVT").Cells(8, 3).Value = ""
            'Sheets("PVT").Cells(9, 3).Value = ""
            'Sheets("PVT").Cells(10, 3).Value = ""
            'Sheets("PVT").Cells(11, 3).Value = ""
            'Sheets("PVT").Cells(8, 6).Value = ""
            'Sheets("PVT").Cells(9, 6).Value = ""
            'Sheets("PVT").Cells(10, 6).Value = ""

            'Calcula la presión de burbujeo
            '********************************************************
            DoSet("PROSPER.PVT.CALC.MODE", 1)
            DoSet("PROSPER.PVT.CALC.PRESUSER[0]", 1000.0)
            DoSet("PROSPER.PVT.CALC.TEMPUSER[0]", Tyac)
            DoCmd("PROSPER.PVT.CALC")
            Pbur = Val(DoGet("PROSPER.PVT.CALC.RESULTS[0].BUBPNT"))

            DoSet("PROSPER.PVT.CALC.MODE", 0)
            DoSet("PROSPER.PVT.CALC.TEMPSTEP", 1)
            DoSet("PROSPER.PVT.CALC.TEMPMIN", Tyac)
            DoSet("PROSPER.PVT.CALC.TEMPMAX", Tyac)
            DoSet("PROSPER.PVT.CALC.PRESSTEP", 30)
            DoSet("PROSPER.PVT.CALC.PRESMIN", 100)
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
                Knucleo = (0.05958 * drg ^ 0.7972) * 10 ^ ((13.1405 * API) / (Tyac + 460))
                C = 1.0014
            Else
                Knucleo = (0.0315 * drg ^ 0.7587) * 10 ^ ((11.289 * API) / (Tyac + 460))
                C = 1.0937
            End If

            For i = 1 To NELEM
                Rsk(i) = (PRESPVT(i) + 14.7) ^ C * Knucleo
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
                    'Exit Do
                Else
                    If A = 0 Then suma = suma + Math.Abs(Xsol(2) / B)
                    If B = 0 Then suma = suma + Math.Abs(Xsol(1) / A)
                End If

                If iter < 100 Then
                    If suma > 0.0001 Then
                        A = A + Xsol(1)
                        B = B + Xsol(2)
                        iter = iter + 1
                        'GoTo iterar
                    Else
                        Exit Do
                    End If
                Else
                    MsgBox("Se presentaron problemas de convergencia" & vbCrLf & "en el módulo de ajuste de la correlación de Kartoamodjo")
                End If
            Loop While (iter < 100)


            'Imprime datos de entrada y resultados del ajuste
            '*******************************************************
            'Sheets("PVT").Cells(8, 3).Value = drg
            '        Sheets("PVT").Cells(9, 3).Value = API
            '        Sheets("PVT").Cells(10, 3).Value = RGAf * 5.615
            '        Sheets("PVT").Cells(11, 3).Value = Tyac
            '        Sheets("PVT").Cells(12, 3).Value = Pbur
            '        Sheets("PVT").Cells(8, 6).Value = A
            '        Sheets("PVT").Cells(9, 6).Value = B
            '        Sheets("PVT").Cells(10, 6).Value = C

            For i = 1 To NELEM
                datosPVT(i - 1, 0) = PRESPVT(i)
                datosPVT(i - 1, 1) = Rsm(i)
                datosPVT(i - 1, 2) = Rsk(i)
                datosPVT(i - 1, 3) = A * Rsk(i) + B
            Next i

        End Sub

        Sub VOLUMENES(ByRef Dvalvula As Double, ByRef VOLTR As Double, ByRef NDTR As Double)

            'Subrutina para calcular el volumen de la TR, y para guardar en una
            'matriz información del edo. mecánico del pozo, así como del giroscopio
            '(requeridos para el modelo de flujo en el espacio anular)
            '****************************************************************************

            Dim PTVD(), PMD(), ANGDES() As Double
            Dim DITR(), DETP(), DITP(), RUGTP(), RUGTR(), LDTP(), LMTP() As Double
            Dim NDG, NDEM, VEM As Integer
            Dim VOLTP, ITP As Double
            Dim i, j, k, n As Integer

            '************************* IMPORTANTE:  *************************************
            '   En pantalla principal de Prosper debe estar activa la opción:
            '                    PRESSURE LOST IN ANNULUS
            '****************************************************************************
            'Lectura del estado mecánico del pozo:
            '0, es el árbol de válvulas
            '1, es TP
            '2, es la válvula de tormenta
            '3, es una restricción
            '4, es la TR
            '*****************************************************************************

            NDEM = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA.COUNT"))
            k = 1
            For i = 2 To NDEM
                VEM = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].TYPE"))
                If VEM = 1 Or VEM = 4 Then
                    ReDim Preserve LDTP(k), LMTP(k), DITP(k), DETP(k), DITR(k), RUGTP(k), RUGTR(k)
                    LDTP(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].DEPTH"))
                    DITP(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].TID"))
                    DETP(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].TOD"))
                    DITR(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].CID"))
                    RUGTP(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].TIR"))
                    RUGTR(k) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[" + CStr(i - 1) + "].CIR"))
                    k = k + 1
                End If
            Next

            LDTP(0) = Val(DoGet("PROSPER.SIN.EQP.DOWN.DATA[0].DEPTH")) 'longitud del árbol de válvulas
            'LINJ = Val(DoGet("PROSPER.SIN.GLF.VALVEDEPTH"))
            VOLTR = 0

            'If Nliq < LINJ Then: Nliq = LINJ

            'Calcula el volumen del espacio anular hasta el nivel de líquido
            '*************************************************************************
            For i = 1 To k - 1
                If DITP(i) > 0 Then
                    If LDTP(i) < Dvalvula Then
                        ITP = LDTP(i) - LDTP(i - 1)
                        VOLTR = VOLTR + (DITR(i) ^ 2 - DETP(i) ^ 2) * ITP
                    Else
                        ITP = Dvalvula - LDTP(i - 1)
                        VOLTR = VOLTR + (DITR(i) ^ 2 - DETP(i) ^ 2) * ITP
                        Exit For
                        'GoTo 400
                    End If
                Else
                    Exit For
                    'GoTo 400
                End If
            Next i

            '400:

            VOLTR = VOLTR * Math.PI / 4 / 144

            'Lee número de datos del giroscópico
            '************************************************************************
            NDG = Val(DoGet("PROSPER.SIN.EQP.DEVN.DATA.COUNT"))

            ReDim Preserve PTVD(NDG), PMD(NDG), ANGDES(NDG)
            PTVD(0) = 0
            PMD(0) = 0

            For i = 1 To NDG - 1
                PTVD(i) = Val(DoGet("PROSPER.SIN.EQP.DEVN.DATA[" + CStr(i) + "].TVD"))
                PMD(i) = Val(DoGet("PROSPER.SIN.EQP.DEVN.DATA[" + CStr(i) + "].MD"))
                ANGDES(i) = ARCCOS((PTVD(i) - PTVD(i - 1)) / (PMD(i) - PMD(i - 1)))
            Next

            'Ordena profundidades del Edo. Mecánico y del Giroscopio
            '***********************************************************************
            i = 0
            j = 1
            NDTR = 0

            Do


                '900:
                NDTR = NDTR + 1
                ReDim Preserve DATTR(7, NDTR)
                If LDTP(i) < PMD(j) Then
                    DATTR(1, NDTR) = LDTP(i)
                    i = i + 1
                Else
                    DATTR(1, NDTR) = PMD(j)
                    j = j + 1
                End If

                'If DATTR(1, NDTR) < Dvalvula Then
                '    GoTo 900
                'Else
                '    DATTR(1, NDTR) = Dvalvula 'LINJ
                'End If
                If DATTR(1, NDTR) > Dvalvula Then
                    DATTR(1, NDTR) = Dvalvula 'LINJ
                End If
            Loop While (DATTR(1, NDTR) < Dvalvula)

            'Asigna el diám. int. de TR,el diám. ext. de TP y la rugosidad
            '***********************************************************************
            i = 1
            For j = 1 To NDTR
                If DATTR(1, j) <= LDTP(i) Then
                    DATTR(3, j) = DITR(i)  'Diámetro int.TR [pg]
                    DATTR(4, j) = DETP(i)  'Diámetro ext. TP [pg]
                    DATTR(5, j) = RUGTR(i) 'Rugosidad de TR [pg]
                    DATTR(7, j) = DITP(i)  'Diám.int de TP [pg]
                Else
                    DATTR(3, j) = DITR(i + 1)  'Diámetro int.TR [pg]
                    DATTR(4, j) = DETP(i + 1)  'Diámetro ext. TP [pg]
                    DATTR(5, j) = RUGTR(i + 1) 'Rugosidad de TR [pg]
                    DATTR(7, j) = DITP(i + 1)  'Diám.int. de TP [pg]
                    i = i + 1
                End If
            Next

            'Asigna el diám. int. de TR,el diám. ext. de TP y la rugosidad
            '***********************************************************************
            i = 1
            For j = 1 To NDTR
                If DATTR(1, j) <= LDTP(i) Then
                    DATTR(3, j) = DITR(i)  'Diámetro int.TR [pg]
                    DATTR(4, j) = DETP(i)  'Diámetro ext. TP [pg]
                    DATTR(5, j) = RUGTR(i) 'Rugosidad de TR [pg]
                    DATTR(7, j) = DITP(i)  'Diám.int de TP [pg]
                Else
                    DATTR(3, j) = DITR(i + 1)  'Diámetro int.TR [pg]
                    DATTR(4, j) = DETP(i + 1)  'Diámetro ext. TP [pg]
                    DATTR(5, j) = RUGTR(i + 1) 'Rugosidad de TR [pg]
                    DATTR(7, j) = DITP(i + 1)  'Diám.int. de TP [pg]
                    i = i + 1
                End If
            Next

            'Asigna la longitud vertical verdadera
            '***********************************************************************
            i = 1
            For j = 1 To NDTR
                If DATTR(1, j) <= PMD(i) Then
                    DATTR(2, j) = (DATTR(1, j) - DATTR(1, j - 1)) * Math.Cos(ANGDES(i)) + DATTR(2, j - 1)
                    DATTR(6, j) = ANGDES(i) * 180 / 3.14159
                Else
                    DATTR(2, j) = (DATTR(1, j) - DATTR(1, j - 1)) * Math.Cos(ANGDES(i + 1)) + DATTR(2, j - 1)
                    DATTR(6, j) = ANGDES(i + 1) * 180 / 3.14159
                    i = i + 1
                End If
            Next

        End Sub

        Sub SYSTEM_IPR_VLP(ByRef Pwh As Double, ByRef Fw As Double, ByRef Qginy As Double, ByRef Ql As Double, ByRef Qoil As Double, ByRef Qgf As Double, ByRef Twh As Double, ByRef PwfIPR As Double)
            Dim cont, niter As Integer
            Dim q1, q2, qs, incq As Double
            Dim Ph1, Ph2, Pv1, Pv2, Psh, Psv As Double

            DoSet("PROSPER.ANL.SYS.PRES", Str(Pwh))
            DoSet("PROSPER.ANL.SYS.WC", Str(Fw))
            DoSet("PROSPER.ANL.SYS.SENS.VARS.VAL1[0]", Str(Qginy))

            'Inicia búsqueda de la solución
            '*********************************************************************
            q1 = 100.0#
            q2 = q1 * 1.5

            niter = 0

            '100: 'scc: ya se sustituyo
            'Do
            Do
                'Do
                DoSet("PROSPER.ANL.SYS.RATES[0]", q1)
                DoSet("PROSPER.ANL.SYS.RATES[1]", q2)
                DoCmd("PROSPER.ANL.SYS.CALC")

                Pv1 = Val(DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].IPRPRES[0]")) 'IPR
                Pv2 = Val(DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].IPRPRES[1]")) 'IPR
                Ph1 = Val(DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].VLPPRES[0]")) 'VLP
                Ph2 = Val(DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].VLPPRES[1]")) 'VLP

                If Ph1 > Ph2 Then
                    q1 = q2
                    q2 = q2 * 1.5
                    Continue Do
                End If
                'Loop While (Ph1 > Ph2)
                If Ph1 < Pv1 And Ph2 < Pv2 Then
                    q1 = q2
                    q2 = q1 * 1.5
                    Continue Do
                End If
                'Loop While (Ph1 < Pv1 And Ph2 < Pv2)
                'Encuentra la intersección (lineal) de IPR y VLP
                '**********************************************************************
                qs = (Pv1 - Ph1) * (q2 - q1) / ((Ph2 - Ph1) - (Pv2 - Pv1)) + q1
                If qs <= 0 Then
                    Ql = 0
                    Exit Sub
                End If

                DoSet("PROSPER.ANL.SYS.RATES[0]", qs)
                DoCmd("PROSPER.ANL.SYS.CALC")
                Psv = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].IPRPRES[0]") 'IPR
                Psh = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].VLPPRES[0]") 'VLP

                If Math.Abs(1 - Psh / Psv) > 0.0001 Then
                    If niter < 25 Then
                        If Psh > Psv Then
                            q2 = qs
                        Else
                            q1 = qs
                        End If
                        niter = niter + 1
                        Continue Do
                    Else
                        If Math.Abs(Psh - Psv) < 1.5 Then
                            Exit Do
                        Else
                            Ql = 0 : Exit Sub
                        End If
                    End If
                End If
                Exit Do
            Loop

            incq = 0.5
            Do
                DoSet("PROSPER.ANL.SYS.RATES[0]", qs - incq)
                DoSet("PROSPER.ANL.SYS.RATES[1]", qs + incq)
                DoCmd("PROSPER.ANL.SYS.CALC")
                Ql = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].SOL.LIQRATE")
                incq = incq + 1
            Loop While (Ql = 0)
            Qoil = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].SOL.OILRATE")
            Qgf = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].SOL.GASRATE")
            PwfIPR = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].SOL.BHP")
            Twh = DoGet("PROSPER.OUT.SYS.RESULTS[0][0][0].SOL.WHTEMPERATURE")

        End Sub

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

    End Class
End Namespace