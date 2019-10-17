Imports ModeloProsper.Prosper
Namespace Clases

    Public Class EstabilidadDePozos
        Property ProgramPath As String
        Dim Server As Object
        Dim Nombre As String
        Dim Dvalve As Double        'Profundidad desarrollada de la valvula en md
        Dim VolTP As Double         'Volumen de la TP en ft3
        Dim VolTR As Double         'Volumen de la TR en ft3
        Dim Bgi As Double           'Factor de voluemen de gas de BN en ft3/ft3
        Dim Qfpvalve As Double      'Gasto de fluido de la formación ft3/d
        Dim Dfpvalve As Double      'Densidad de los fluidos a la profundidad de la valvula en lb/ft3
        Dim Dgivalve As Double      'Densidad de BN al punto de inyección en lb/ft3
        Dim Qgi As Double           'Gas de BN en MMSCFD

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

        Private _Ptr_incremento As New MaxMinDouble
        Public WriteOnly Property Ptr_incremento() As Double
            Set(ByVal value As Double)
                _Ptr_incremento.Val = value
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

        Private _Pwh_incremento As New MaxMinDouble
        Public WriteOnly Property Pwh_incremento() As Double
            Set(ByVal value As Double)
                _Pwh_incremento.Val = value
            End Set
        End Property

        Private _Qgi_min As New MaxMinDouble
        Public WriteOnly Property Qgi_min() As Double
            Set(ByVal value As Double)
                _Qgi_min.Val = value
            End Set
        End Property

        Private _Qgi_max As New MaxMinDouble
        Public WriteOnly Property Qgi_max() As Double
            Set(ByVal value As Double)
                _Qgi_max.Val = value
            End Set
        End Property

        Private _Qgi_num As New MaxMinInteger
        Public WriteOnly Property Qgi_num() As Integer
            Set(ByVal value As Integer)
                _Qgi_num.Val = value
            End Set
        End Property

        Private _Qgi_incremento As New MaxMinDouble
        Public WriteOnly Property Qgi_incremento() As Double
            Set(ByVal value As Double)
                _Qgi_incremento.Val = value
            End Set
        End Property

        Private _Dvalve_min As New MaxMinDouble
        Public WriteOnly Property Dvalve_min() As Double
            Set(ByVal value As Double)
                _Dvalve_min.Val = value
            End Set
        End Property

        Private _Dvalve_max As New MaxMinDouble
        Public WriteOnly Property Dvalve_max() As Double
            Set(ByVal value As Double)
                _Dvalve_max.Val = value
            End Set
        End Property

        Private _Dvalve_num As New MaxMinInteger
        Public WriteOnly Property Dvalve_num() As Integer
            Set(ByVal value As Integer)
                _Dvalve_num.Val = value
            End Set
        End Property

        Private _Dvalve_incremento As New MaxMinDouble
        Public WriteOnly Property Dvalve_incremento() As Double
            Set(ByVal value As Double)
                _Dvalve_incremento.Val = value
            End Set
        End Property

        Private _DiaVal_min As New MaxMinDouble
        Public WriteOnly Property DiaVal_min() As Double
            Set(ByVal value As Double)
                _DiaVal_min.Val = value
            End Set
        End Property

        Private _DiaVal_max As New MaxMinDouble
        Public WriteOnly Property DiaVal_max() As Double
            Set(ByVal value As Double)
                _DiaVal_max.Val = value
            End Set
        End Property

        Private _DiaVal_num As New MaxMinInteger
        Public WriteOnly Property DiaVal_num() As Integer
            Set(ByVal value As Integer)
                _DiaVal_num.Val = value
            End Set
        End Property

        Private _DiaVal_incremento As New MaxMinDouble
        Public WriteOnly Property DiaVal_incremento() As Double
            Set(ByVal value As Double)
                _DiaVal_incremento.Val = value
            End Set
        End Property

        Private _Wc_min As New MaxMinDouble
        Public WriteOnly Property Wc_min() As Double
            Set(ByVal value As Double)
                _Wc_min.Val = value
            End Set
        End Property

        Private _Wc_max As New MaxMinDouble
        Public WriteOnly Property Wc_max() As Double
            Set(ByVal value As Double)
                _Wc_max.Val = value
            End Set
        End Property

        Private _Wc_num As New MaxMinInteger
        Public WriteOnly Property Wc_num() As Integer
            Set(ByVal value As Integer)
                _Wc_num.Val = value
            End Set
        End Property

        Private _Wc_incremento As New MaxMinDouble
        Public WriteOnly Property Wc_incremento() As Double
            Set(ByVal value As Double)
                _Wc_incremento.Val = value
            End Set
        End Property

        Private _Cd As New MaxMinDouble
        Public WriteOnly Property Cd() As Double
            Set(ByVal value As Double)
                _Cd.Val = value
            End Set
        End Property


        'salida

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
                'Return _Pwh.Val
                Return getLista(_PwhArray)
            End Get
        End Property

        'Private _Wc() As Double
        'Public ReadOnly Property Wc() As List(Of Double)
        '    Get
        '        'Return _Wc.Val
        '        Return getLista(_Wc)
        '    End Get
        'End Property

        'Private _DiaVal() As Double
        'Public ReadOnly Property DiaVal() As List(Of Double)
        '    Get
        '        'Return _Wc.Val
        '        Return getLista(_DiaVal)
        '    End Get
        'End Property

        'Qgi
        Private _QgiArray() As Double
        Public ReadOnly Property QgiArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_QgiArray)
            End Get
        End Property

        'Dvalve
        Private _DvalveArray() As Double
        Public ReadOnly Property DvalveArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_DvalveArray)
            End Get
        End Property

        'DiaVal
        Private _DiaValArray() As Double
        Public ReadOnly Property DiaValArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_DiaValArray)
            End Get
        End Property

        'Wc
        Private _WcArray() As Double
        Public ReadOnly Property WcArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_WcArray)
            End Get
        End Property

        'GOR
        Private _GORArray() As Double
        Public ReadOnly Property GORArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_GORArray)
            End Get
        End Property

        'Ql
        Private _QlArray() As Double
        Public ReadOnly Property QlArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_QlArray)
            End Get
        End Property

        'Qg
        Private _QgArray() As Double
        Public ReadOnly Property QgArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_QgArray)
            End Get
        End Property

        'Pwf_IPR
        Private _Pwf_IPRArray() As Double
        Public ReadOnly Property Pwf_IPRArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_Pwf_IPRArray)
            End Get
        End Property

        'Twh
        Private _TwhArray() As Double
        Public ReadOnly Property TwhArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_TwhArray)
            End Get
        End Property

        'TotalQgas
        Private _TotalQgasArray() As Double
        Public ReadOnly Property TotalQgasArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_TotalQgasArray)
            End Get
        End Property

        'Pwf_quicklook
        Private _Pwf_quicklookArray() As Double
        Public ReadOnly Property Pwf_quicklookArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_Pwf_quicklookArray)
            End Get
        End Property

        'Pws
        Private _PwsArray() As Double
        Public ReadOnly Property PwsArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_PwsArray)
            End Get
        End Property

        'Pti
        Private _PtiArray() As Double
        Public ReadOnly Property PtiArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_PtiArray)
            End Get
        End Property

        'Ptri
        Private _PtriArray() As Double
        Public ReadOnly Property PtriArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_PtriArray)
            End Get
        End Property

        'Tvalv
        Private _TvalvArray() As Double
        Public ReadOnly Property TvalvArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_TvalvArray)
            End Get
        End Property

        'GOR_quicklook
        Private _GOR_quicklookArray() As Double
        Public ReadOnly Property GOR_quicklookArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_GOR_quicklookArray)
            End Get
        End Property

        'GORFREE
        Private _GORFREEArray() As Double
        Public ReadOnly Property GORFREEArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_GORFREEArray)
            End Get
        End Property

        'Dpvalve
        Private _DpvalveArray() As Double
        Public ReadOnly Property DpvalveArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_DpvalveArray)
            End Get
        End Property

        'Ptrcalc
        Private _PtrcalcArray() As Double
        Public ReadOnly Property PtrcalcArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_PtrcalcArray)
            End Get
        End Property

        'Dp
        Private _DpArray() As Double
        Public ReadOnly Property DpArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_DpArray)
            End Get
        End Property

        'PI
        Private _PIArray() As Double
        Public ReadOnly Property PIArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_PIArray)
            End Get
        End Property

        'Qgcrit
        Private _QgcritArray() As Double
        Public ReadOnly Property QgcritArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_QgcritArray)
            End Get
        End Property

        'Qcporcent
        Private _QcporcentArray() As Double
        Public ReadOnly Property QcporcentArray() As List(Of Double)
            Get
                'Return _Wc.Val
                Return getLista(_QcporcentArray)
            End Get
        End Property

        'HTC
        Private _HTCArray() As Double
        Public ReadOnly Property HTCArray() As List(Of Double)
            Get
                Return getLista(_HTCArray)
            End Get
        End Property

        'HTC
        Private _Ind_EstArray() As Double
        Public ReadOnly Property Ind_EstArray() As List(Of Double)
            Get
                Return getLista(_Ind_EstArray)
            End Get
        End Property


        'Comentario
        Private _ComentarioArray() As Double
        Public ReadOnly Property ComentarioArray() As List(Of Double)
            Get
                Return getLista(_ComentarioArray)
            End Get
        End Property

        'Árbol de descisión Caso As Integer)
        'Private _Pwh_max As New MaxMinDouble
        'Public WriteOnly Property Pwh_max() As Double
        '    Set(ByVal value As Double)
        '        _Pwh_max.Val = value
        '    End Set
        'End Property

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

        Property ListaParametrosEstabilidad As New List(Of Parametros_Estabilidad)

        Public Sub New(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer) ' ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer)
            Me.Ptr_min = Ptr_min
            Me.Ptr_max = Ptr_max
            Me.Ptr_num = Ptr_num
            Me.Pwh_min = Pwh_Min
            Me.Pwh_max = Pwh_Max
            Me.Pwh_num = Pwh_num
            Me.Qgi_min = Qgi_min
            Me.Qgi_max = Qgi_max
            Me.Qgi_num = Qgi_num
            Me.Dvalve_min = Dvalve_min
            Me.Dvalve_max = Dvalve_max
            Me.Dvalve_num = Dvalve_num
            Me.DiaVal_min = DiaVal_min
            Me.DiaVal_max = DiaVal_max
            Me.DiaVal_num = DiaVal_num
            Me.Wc_min = Wc_min
            Me.Wc_max = Wc_max
            Me.Wc_num = Wc_num
            Me.Cd = Cd
            Me.Ptr_Act = Ptr_Act
            Me.Pwh_Act = Pwh_Act
            Me.Qgi_Act = Qgi_Act
            Me.Dvalve_Act = Dvalve_Act
            Me.DiaValve_Act = DiaValve_Act
            Me.Wc_Act = Wc_Act
            Me.Caso = Caso

            inicializarMaximosMinimos()
        End Sub
        Public Sub inicializarMaximosMinimos()
            _Ptr_max.Nomb = "Máxima presión en la Tubería de revestimiento [Kg/cm2]" : _Ptr_max.Min = 0 : _Ptr_max.Max = 10000
            _Ptr_min.Nomb = "Mínima presión en la Tubería de revestimiento [Kg/cm2]" : _Ptr_min.Min = 0 : _Ptr_min.Max = 10000
            _Ptr_num.Nomb = "Número de pasos en la variación de presión en TR [Adimensional]" : _Ptr_num.Min = 1 : _Ptr_num.Max = 100
            _Pwh_max.Nomb = "Máxima presión en la cabeza del pozo [Kg/cm2]" : _Pwh_max.Min = 0 : _Pwh_max.Max = 10000
            _Pwh_min.Nomb = "Mínima presión en la cabeza del pozo [Kg/cm2]" : _Pwh_min.Min = 0 : _Pwh_min.Max = 10000
            _Pwh_num.Nomb = "Número de pasos en la variación de presión en la cabeza del pozo [Adimensional]" : _Pwh_num.Min = 1 : _Pwh_num.Max = 100
            _Qgi_max.Nomb = "Máximo gasto de Inyección de gas de bombeo neumático [MMscfd]" : _Qgi_max.Min = 0.2 : _Qgi_max.Max = 20
            _Qgi_min.Nomb = "Mínimo gasto de Inyección de gas de bombeo neumático [MMscfd]" : _Qgi_min.Min = 0.2 : _Qgi_min.Max = 20
            _Qgi_num.Nomb = "Número de pasos en la variación del gasto de inyección de gas de bombeo neumatico [Adimensional]" : _Qgi_num.Min = 1 : _Qgi_num.Max = 100
            _Dvalve_max.Nomb = "Máxima profundidad de la válvula de inyección de gas [mD]" : _Dvalve_max.Min = 500 : _Dvalve_max.Max = 10000
            _Dvalve_min.Nomb = "Mínima profundidad de la válvula de inyección de gas [mD]" : _Dvalve_min.Min = 500 : _Dvalve_min.Max = 10000
            _Dvalve_num.Nomb = "Número de pasos en la variación de profundidad de la valvula de bombeo neumático" : _Dvalve_num.Min = 1 : _Dvalve_num.Max = 100
            _DiaVal_max.Nomb = "Máximo diámetro de la válvula de bombeo neumático [64avos]" : _DiaVal_max.Min = 5 : _DiaVal_max.Max = 128
            _DiaVal_min.Nomb = "Mínimo diámetro de la válvula de bombeo neumático [64avos]" : _DiaVal_min.Min = 5 : _DiaVal_min.Max = 128
            _DiaVal_num.Nomb = "Número de pasos en la variación de diámetro la válvula de bombeo neumático [Adimensional]" : _DiaVal_num.Min = 1 : _DiaVal_num.Max = 100
            _Wc_max.Nomb = "Máximo corte de agua [%]" : _Wc_max.Min = 0 : _Wc_max.Max = 100
            _Wc_min.Nomb = "Mínimo corte de agua [%]" : _Wc_min.Min = 0 : _Wc_min.Max = 100
            _Wc_num.Nomb = "Número de pasos en la variación de corte de agua [Adimensional]" : _Wc_num.Min = 0 : _Wc_num.Max = 100
            _Cd.Nomb = "Coeficiente de descarga de la válvula de bombeo neumático [Adimensional]" : _Cd.Min = 0 : _Cd.Max = 1

        End Sub

        Sub Principal()
            'Lista de variables
            Dim i, j, k, L, m, n, cont As Integer 'contadores de for
            Dim Ptr As Double           'Presión en la TR en la red de BN en kg/cm2g**
            Dim Pwh As Double           'Presión en la cabeza del pozo o estrangulador en kg/cm2g
            Dim Wc As Double            'Porcentaje de agua
            Dim DiaVal As Double        'Diametro del orificio en 64avos de pulgas
            Dim GOR As Double           'Relación gas aceite por pvt en m3/m3
            Dim Ql As Double            'Gasto de liquido en STBD
            Dim Qg As Double            'Gasto de gas en MMSCFD
            Dim Pwf_IPR As Double       'Presión de fondo fluyente del IPR en kg/cm2g
            Dim Twh As Double           'Temperatura en la cabeza del pozo en °C
            Dim TotalQgas As Double      'Qg+Qgi en MMSCFD

            Dim Pwf_quicklook As Double 'Presión de fondo fluyente del Quicklook en kg/cm2g
            Dim Pws As Double           'Presión de fondo cerrado en kg/cm2g
            Dim Pti As Double           'Presión en la Tp a la profundidad de la valvula kg/cm2g
            Dim Ptri As Double          'Presión en la Tr a la profundidad de la valvula en kg/cm3g
            Dim Tvalv As Double         'Temperatura a la altura de la valvula en °C
            Dim GOR_quicklook As Double 'Relación gas-aceite de quicklook en m3/m3
            Dim GORFREE As Double       'Relación gas-aceite libre del quicklook en m3/m3
            Dim Dpvalve As Double       'Caida de presión a traves de la valvula de BN en kg/cm2
            Dim Ptrcalc As Double       'Presión en TR calculada como mínimo para inyectar kg/cm2g
            Dim Dp As Double            'Pws-Pwf del quicklook en kg/cm2
            Dim PI As Double            'Indice de productividad en STBD/psi
            Dim Qgcrit As Double        'Gas de BN critico en MMSCFD
            Dim Qcporcent As Double     'Porcentaje de gas de BN critco %
            Dim HTC As Double           'Coeficiente de transferencia termica BTU/h/ft2
            Dim Ind_Est As Integer       'Indicador de estabilidad -1 inoperante; 0 inestable; 1 estable
            Dim Drgi As Double          'Densidad relativa del gas de BN
            'Dim Cd As Double            'Coefiente de descarga de la valvula de orificio***
            Dim F1 As Double            'Priemer Criterio de estabilidad de Asheim
            Dim F2 As Double            'Segundo Criterio de estabilidad de Asheim

            Try
                Connect(ProgramPath)
                'Server = CreateObject("PX32.OpenServer.1")
                Nombre = "PROSPER"
                'DoCmd("PROSPER.SETUNITSYS(""Pemex"")")

                Dim num As Integer
                If (_Ptr_num.Val = 1) Then
                    num = 1
                Else
                    num = _Ptr_num.Val - 1
                End If
                Ptr_incremento = (_Ptr_max.Val - _Ptr_min.Val) / (num) '**
                'Ptr = _Ptr_min.Val

                If (_Pwh_num.Val = 1) Then
                    num = 1
                Else
                    num = _Pwh_num.Val - 1
                End If
                Pwh_incremento = (_Pwh_max.Val - _Pwh_min.Val) / (num)

                If (_Qgi_num.Val = 1) Then
                    num = 1
                Else
                    num = _Qgi_num.Val - 1
                End If
                Qgi_incremento = (_Qgi_max.Val - _Qgi_min.Val) / (num)

                If (_Dvalve_num.Val = 1) Then
                    num = 1
                Else
                    num = _Dvalve_num.Val - 1
                End If
                Dvalve_incremento = (_Dvalve_max.Val - _Dvalve_min.Val) / (num)
                Dvalve = _Dvalve_min.Val

                If (_DiaVal_num.Val = 1) Then
                    num = 1
                Else
                    num = _DiaVal_num.Val - 1
                End If
                DiaVal_incremento = (_DiaVal_max.Val - _DiaVal_min.Val) / (num)
                DiaVal = _DiaVal_min.Val

                If (_Wc_num.Val = 1) Then
                    num = 1
                Else
                    num = _Wc_num.Val - 1
                End If
                Wc_incremento = (_Wc_max.Val - _Wc_min.Val) / (num)
                Wc = _Wc_min.Val

                'Extracción de los datos del modelo en Prosper
                GOR = Val(DoGet("PROSPER.SIN.IPR.Single.totgor"))
                Drgi = Val(DoGet("PROSPER.SIN.GLF.Gravity"))
                Call VolTP_VolTR()

                'cont = 15
                'Dim indice As Integer = 0



                cont = 0
                For i = 0 To _Ptr_num.Val - 1
                    Ptr = _Ptr_min.Val + _Ptr_incremento.Val * i
                    Format(Ptr, "0.00000")

                    For j = 0 To _Pwh_num.Val - 1
                        Pwh = _Pwh_min.Val + _Pwh_incremento.Val * j
                        Format(Pwh, "0.00000")

                        For k = 0 To _Qgi_num.Val - 1
                            Qgi = _Qgi_min.Val + _Qgi_incremento.Val * k 'COMMIT crear arreglos de las variables
                            Format(Qgi, "0.00000")

                            For L = 0 To _Dvalve_num.Val - 1
                                Dvalve = _Dvalve_min.Val + _Dvalve_incremento.Val * L 'Prof. Valv (md)
                                Format(Dvalve, "0.00000")
                                DoSet("PROSPER.SIN.GLF.ValveDepth", Str(Dvalve))
                                If (_Dvalve_incremento.Val > 0) Then Call VolTP_VolTR()

                                For m = 0 To _DiaVal_num.Val - 1
                                    DiaVal = _DiaVal_min.Val + _DiaVal_incremento.Val * m 'Dia. Orificio [64avos]
                                    Format(DiaVal, "0.00000")

                                    For n = 0 To _Wc_num.Val - 1
                                        Wc = _Wc_min.Val + _Wc_incremento.Val * n
                                        Format(Wc, "0.00000")
                                        'WcArray(cont) = Wc
                                        DoSet("PROSPER.SIN.IPR.Single.Wc", Str(Wc))
                                        DoCmd("PROSPER.IPR.CALC")

                                        'Se establecen los parámetros del análisis de 3 varibles
                                        DoSet("PROSPER.ANL.SYS.PRES", Str(Pwh))
                                        DoSet("PROSPER.ANL.SYS.WC", Str(Wc))
                                        DoSet("PROSPER.ANL.SYS.GOR", Str(GOR))
                                        DoSet("PROSPER.ANL.SYS.RateMethod", Str(0))
                                        DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
                                        DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars [0]", 22)
                                        DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[0]", Str(Qgi))
                                        ' Se realizan los calculos
                                        DoCmd("PROSPER.ANL.SYS.CALC")
                                        'Se obitiene los resultado del analisis de 3 variables
                                        Ql = Val(DoGet("PROSPER.OUT.SYS.Results[0][0][0].Sol.LiqRate"))

                                        If (Ql > 0) Then

                                            Qg = Val(DoGet("PROSPER.OUT.SYS.Results[0][0][0].Sol.GasRate"))

                                            Pwf_IPR = Val(DoGet("PROSPER.OUT.SYS.Results[0][0][0].Sol.BHP"))

                                            Twh = Val(DoGet("PROSPER.OUT.SYS.Results[0][0][0].Sol.WHTemperature"))
                                            TotalQgas = Qgi + Qg

                                            'Se establece los parametros para el análisis del Quicklook
                                            DoSet("PROSPER.ANL.QLG.Surface[0][0]", CStr(Pwh))
                                            DoSet("PROSPER.ANL.QLG.Surface[1][0]", CStr(Twh))
                                            DoSet("PROSPER.ANL.QLG.Surface[2][0]", CStr(Ql))
                                            DoSet("PROSPER.ANL.QLG.Surface[3][0]", CStr(Wc))
                                            DoSet("PROSPER.ANL.QLG.Surface[4][0]", CStr(TotalQgas))
                                            DoSet("PROSPER.ANL.QLG.Surface[5][0]", CStr(Qgi))
                                            DoSet("PROSPER.ANL.QLG.Surface[6][0]", CStr(Ptr))
                                            DoSet("PROSPER.ANL.QLG.Gaslift[0]", CStr(DiaVal))
                                            DoSet("PROSPER.ANL.QLG.Gaslift[1]", CStr(Dvalve))
                                            ' Se realizan los calculos
                                            DoCmd("PROSPER.ANL.QLG.CALC")

                                            'Se obtiene los resultados del QuickLook
                                            ' Presion a la prof de la valv.
                                            Pti = DoGet("PROSPER.OUT.QLG.Output[0]")
                                            ' Temperatura a la profundidad de la valvula
                                            Tvalv = DoGet("PROSPER.OUT.QLG.Output[1]")
                                            ' Relacion gas aceite
                                            GOR_quicklook = DoGet("PROSPER.OUT.QLG.Output[2]")
                                            ' GOR Libre
                                            GORFREE = DoGet("PROSPER.OUT.QLG.Output[3]")
                                            ' Delta P a travez de la valvula
                                            Dpvalve = DoGet("PROSPER.OUT.QLG.Output[4]")
                                            ' Presion en la TR teórica superf
                                            Ptrcalc = DoGet("PROSPER.OUT.QLG.Output[5]")
                                            ' Delta P en el yacimiento
                                            Dp = DoGet("PROSPER.OUT.QLG.Output[6]")
                                            ' Indice de productividad
                                            PI = DoGet("PROSPER.OUT.QLG.Output[7]")
                                            ' Gasto de gas crítico
                                            Qgcrit = DoGet("PROSPER.OUT.QLG.Output[8]")
                                            ' Porcentaje de gvasto de gas crítico
                                            Qcporcent = DoGet("PROSPER.OUT.QLG.Output[9]")
                                            ' Coeficiente de transferencia de calor ajustdo
                                            HTC = DoGet("PROSPER.OUT.QLG.Output[10]")
                                            ' Presión en la TR a la profundidad de la valvula
                                            Ptri = DoGet("PROSPER.OUT.QLG.Output[11]")
                                            ' Presión de fondo estatico
                                            Pws = DoGet("PROSPER.OUT.QLG.Output[12]")
                                            ' Presión de fondo fluyente
                                            Pwf_quicklook = DoGet("PROSPER.OUT.QLG.Output[13]")

                                            ' F1 de Asheim
                                            F1 = Criterio_F1(Drgi, Qgi, PI, _Cd.Val, DiaVal, (Ql * (100 - Wc) / 100), Pti, Tvalv)
                                            F2 = 0
                                            If (F1 < 1) Then
                                                Call Gradiente(Dvalve, Pwh, Wc, GOR, Ql, Bgi, Qfpvalve, Dfpvalve, Dgivalve)
                                                F2 = Criterio_F2(VolTP, VolTR, Pti * 14.223, Dfpvalve, Dgivalve, Dvalve * 3.2808, Bgi, Qfpvalve * 86400, F1)
                                            End If

                                        Else
                                            GOR = 0
                                            Ql = 0
                                            Qg = 0
                                            Twh = 0
                                            Pwf_IPR = 0
                                            TotalQgas = 0
                                            Pwf_quicklook = 0
                                            Pws = 0
                                            Pti = 0
                                            Ptri = 0
                                            Tvalv = 0
                                            GOR_quicklook = 0
                                            GORFREE = 0
                                            Dpvalve = 0
                                            Ptrcalc = 0
                                            Dp = 0
                                            PI = 0
                                            Qgcrit = 0
                                            Qcporcent = 0
                                            HTC = 0

                                        End If

                                        If (F1 >= 1 Or F2 >= 1) Then
                                            If (Ptri <= Pti Or Ql <= 0) Then
                                                Ind_Est = -2                'Inoperante
                                                If (Qgcrit < Qgi) Then
                                                    Ind_Est = -1            'Gasto de gas mayor al critico
                                                End If
                                            ElseIf (Ptr < Ptrcalc) Then
                                                Ind_Est = 1                 'Presión Insuficiente para inyectar el Qgi deseado
                                            Else
                                                Ind_Est = 2                 'Estable
                                            End If
                                        Else
                                            Ind_Est = 0                     'Inestable
                                        End If

                                        'Ir agregando a la lista
                                        ReDim Preserve _PtrArray(cont)
                                        _PtrArray(cont) = Ptr
                                        ReDim Preserve _PwhArray(cont)
                                        _PwhArray(cont) = Pwh
                                        ReDim Preserve _QgiArray(cont)
                                        _QgiArray(cont) = Qgi 'Gas de BN (MMSCFD)
                                        ReDim Preserve _DvalveArray(cont)
                                        _DvalveArray(cont) = Dvalve
                                        ReDim Preserve _DiaValArray(cont)
                                        _DiaValArray(cont) = DiaVal
                                        ReDim Preserve _WcArray(cont)
                                        _WcArray(cont) = Wc
                                        ReDim Preserve _GORArray(cont)
                                        _GORArray(cont) = GOR
                                        ReDim Preserve _QlArray(cont)
                                        _QlArray(cont) = Ql
                                        ReDim Preserve _QgArray(cont)
                                        _QgArray(cont) = Qg
                                        ReDim Preserve _Pwf_IPRArray(cont)
                                        _Pwf_IPRArray(cont) = Pwf_IPR
                                        ReDim Preserve _TwhArray(cont)
                                        _TwhArray(cont) = Twh
                                        ReDim Preserve _TotalQgasArray(cont)
                                        _TotalQgasArray(cont) = TotalQgas
                                        ReDim Preserve _Pwf_quicklookArray(cont)
                                        _Pwf_quicklookArray(cont) = Pwf_quicklook
                                        ReDim Preserve _PwsArray(cont)
                                        _PwsArray(cont) = Pws
                                        ReDim Preserve _PtiArray(cont)
                                        _PtiArray(cont) = Pti
                                        ReDim Preserve _PtriArray(cont)
                                        _PtriArray(cont) = Ptri
                                        ReDim Preserve _TvalvArray(cont)
                                        _TvalvArray(cont) = Tvalv
                                        ReDim Preserve _GOR_quicklookArray(cont)
                                        _GOR_quicklookArray(cont) = GOR_quicklook
                                        ReDim Preserve _GORFREEArray(cont)
                                        _GORFREEArray(cont) = GORFREE
                                        ReDim Preserve _DpvalveArray(cont)
                                        _DpvalveArray(cont) = Dvalve
                                        ReDim Preserve _PtrcalcArray(cont)
                                        _PtrcalcArray(cont) = Ptrcalc
                                        ReDim Preserve _DpArray(cont)
                                        _DpArray(cont) = Dp
                                        ReDim Preserve _PIArray(cont)
                                        _PIArray(cont) = PI
                                        ReDim Preserve _QgcritArray(cont)
                                        _QgcritArray(cont) = Qgcrit
                                        ReDim Preserve _QcporcentArray(cont)
                                        _QcporcentArray(cont) = Qcporcent
                                        ReDim Preserve _HTCArray(cont)
                                        _HTCArray(cont) = HTC
                                        ReDim Preserve _Ind_EstArray(cont)
                                        _Ind_EstArray(cont) = Ind_Est


                                        Dim ObjParametrosEstabilidad As New Parametros_Estabilidad With {
                                        .Ptr = Ptr,
                                        .Pwh = Pwh,
                                        .Qgi = Qgi,
                                        .Dvalve = Dvalve,
                                        .DiaVal = DiaVal,
                                        .Wc = Wc,
                                        .GOR = GOR,
                                        .Ql = Ql,
                                        .Qg = Qg,
                                        .Pwf_IPR = Pwf_IPR,
                                        .Twh = Twh,
                                        .TotalQgas = TotalQgas,
                                        .Pwf_quicklook = Pwf_quicklook,
                                        .Pws = Pws,
                                        .Pti = Pti,
                                        .Ptri = Ptri,
                                        .Tvalv = Tvalv,
                                        .GOR_quicklook = GOR_quicklook,
                                        .GORFREE = GORFREE,
                                        .Dpvalve = Dpvalve,
                                        .Ptrcalc = Ptrcalc,
                                        .Dp = Dp,
                                        .PI = PI,
                                        .Qgcrit = Qgcrit,
                                        .Qcporcent = Qcporcent,
                                        .HTC = HTC,
                                        .Ind_Est = Ind_Est
                                    }

                                        ListaParametrosEstabilidad.Add(ObjParametrosEstabilidad)

                                        cont = cont + 1
                                    Next n
                                Next m
                            Next L
                        Next k
                    Next j
                Next i


                'Dim Estable = (From Estab In ListaParametrosEstabilidad
                '               Where Estab.Ind_Est = 2
                '               Select Estab).ToList
                'Dim Inesble = ()

                Arbol_Decision(_Ptr_Act.Val, _Pwh_Act.Val, _Qgi_Act.Val, _Dvalve_Act.Val, _DiaValve_Act.Val, _Wc_Act.Val, _Caso.Val)
                'Arbol_Decision(69, 29, 12.5, 1900, 48, 0, 1)
                Disconnect()
            Catch ex As Exception
                'Server = Nothing
                Disconnect()
                Throw New Exception(ex.Message)
            End Try


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
            Dim Qgi_down As Double = _Qgi_min.Val
            Dim Qgi_up As Double = _Qgi_min.Val
            Dim Dvalve_down As Double = _Dvalve_min.Val
            Dim Dvalve_up As Double = _Dvalve_min.Val
            Dim Diavalve_down As Double = _DiaVal_min.Val
            Dim Diavalve_up As Double = _DiaVal_min.Val
            Dim Wc_down As Double = _Wc_min.Val
            Dim Wc_up As Double = _Wc_min.Val
            Dim i As Integer
            Dim dif As Double = 100
            Dim dif2 As Double = 100

            For i = 0 To _Ptr_num.Val - 1
                Dim ptraux As Double = _Ptr_min.Val + i * _Ptr_incremento.Val
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
                Dim pwhaux As Double = _Pwh_min.Val + i * _Pwh_incremento.Val
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

            For i = 0 To _Qgi_num.Val - 1
                Dim Qgiaux As Double = _Qgi_min.Val + i * _Qgi_incremento.Val
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
            For i = 0 To _Dvalve_num.Val - 1
                Dim Dvalveaux As Double = _Dvalve_min.Val + i * _Dvalve_incremento.Val
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
            For i = 0 To _DiaVal_num.Val - 1
                Dim Diavalaux As Double = _DiaVal_min.Val + i * _DiaVal_incremento.Val
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
            For i = 0 To _Wc_num.Val - 1
                Dim Wcaux As Double = _Wc_min.Val + i * _Wc_incremento.Val
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

            Dim parametrosDown As List(Of Parametros_Estabilidad) 'Se Cambio
            Dim parametrosUp As List(Of Parametros_Estabilidad) 'Se cambio

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

            If local.Count > 0 Then

                For i = 0 To 1
                    If (local(i).d1.Ind_Est = 2) Then
                        cnt += 1
                    End If
                    If (local(i).d2.Ind_Est = 2) Then
                        cnt += 1
                    End If
                Next i
            End If



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

        Private Function Criterio_F1(ByVal Drgi As Double, ByVal Qgi As Double, IP As Double, ByVal Cd As Double, ByVal Dori As Double, ByVal Qo As Double, ByVal p As Double, ByVal t As Double) As Double
            'Calcula el primer criterio de Asheim
            'Recibe de gasto de gas de BN en MMSCFD
            'Indice de productividad IP en STB/(D-psi)
            'Coeficiente de descarga de la valvula de orificio Cd adimensional
            'Diametro de la valvula de orificio en 64avos de pulgas
            'Gasto de liquido Ql en STBD

            Dim Ao As Double
            Bgi = Bg(p * 14.223, ((t * 9 / 5) + 32), Drgi)

            Ao = 3.1416 * (Dori / (64 * 12)) ^ 2 / 4
            Criterio_F1 = (0.00220723) * Drgi * Bgi * (Qgi) ^ 2 * IP / ((Cd * Ao) ^ 2 * Qo)

        End Function

        Private Function Criterio_F2(ByVal VolTP As Double, ByVal VolTR As Double, ByVal Pti As Double, ByVal Dfpvalve As Double, ByVal Dgivalve As Double, ByVal Dvalve As Double, ByVal Bgi As Double, ByVal Qfpvalve As Double, ByVal F1 As Double) As Double
            Criterio_F2 = (VolTP / VolTR) * (144 * (Pti + 14.7) / ((Dfpvalve - Dgivalve) * Dvalve)) * (1 + (Qgi * 1000000 * Bgi) / Qfpvalve) * (1 / (1 - F1))
        End Function

        Private Function Bg(ByVal p As Double, ByVal t As Double, ByVal Drg As Double) As Double
            'Subrutina para calcular el Bg del gas en la cual
            'recibe la presión en psi y t en °F empleando en metodo
            'de Stanging y Katz

            Dim Zgcal, Zgsup, ro, z As Double
            Dim Ppr, Ppc, Tpr, Tpc, cpti As Double
            Dim A1, A2, A3, A4, A5, A6, A7, A8 As Double

            'Propiedades Pseudocríticas según Sutton
            '*****************************************************************
            Tpc = 169.2 + 349.5 * Drg - 74 * Drg ^ 2
            Ppc = 756.8 - 131 * Drg - 3.6 * Drg ^ 2
            A1 = 0.31506 : A2 = -1.0467 : A3 = -0.5783 : A4 = 0.5353
            A5 = -0.6123 : A6 = -0.10489 : A7 = 0.68157 : A8 = 0.68446

            ' Propiedades pseudoreducidas del gas seco
            '******************************************************************
            Tpr = (t + 460) / Tpc
            Ppr = p / Ppc
            Zgsup = 0.27

            Dim iter As Integer
            iter = 1

            Do
                ro = (0.27 * Ppr) / (Zgsup * Tpr)
                Zgcal = 1 + (A1 + A2 / Tpr + A3 / Tpr ^ 3) * ro + (A4 + A5 / Tpr) * ro ^ 2 + A5 * A6 * ro ^ 5 / Tpr + (A7 * ro ^ 2 / Tpr ^ 3) * (1 + A8 * ro ^ 2) * Math.Exp(-A8 * ro ^ 2)
                If iter < 100 Then
                    If Math.Abs(Zgcal - Zgsup) > 0.001 Then
                        Zgsup = Zgcal
                        iter = iter + 1
                    Else
                        z = Zgcal
                        Exit Do
                    End If
                Else
                    MsgBox("Se produjo un error en la subrutina del factor de compresibilidad")
                    Exit Do
                End If
            Loop While (iter < 100)

            cpti = 0.0283 * z * (t + 460)
            Bg = cpti / (p + 14.7)
        End Function


        Private Sub Gradiente(ByRef Dvalve As Double, ByRef Pwh As Double, ByRef Wc As Double, ByRef GOR As Double, ByRef Ql As Double, ByRef Bgi As Double, ByRef Qfpvalve As Double, ByRef Dfpvalve As Double, ByRef Dgivalve As Double)
            Dim cont As Integer
            Dim Deph As Double
            Dim Dephaux As Double
            Dim Bgiaux As Double
            Dim Vsg As Double
            Dim Vsgaux As Double
            Dim Vsl As Double
            Dim Vslaux As Double
            Dim Ao As Double
            Dim Dgivalveaux As Double
            Dim Dfpvalveaux As Double

            DoSet("PROSPER.ANL.GRD.Pres", Str(Pwh))
            DoSet("PROSPER.ANL.GRD.WC", Str(Wc))
            DoSet("PROSPER.ANL.GRD.GOR", Str(GOR))
            DoSet("PROSPER.ANL.GRD.Rate", Str(Ql))
            DoSet("PROSPER.ANL.GRD.Sens.SensDB.Vars[0]", CStr(22))
            DoSet("PROSPER.ANL.GRD.Sens.SensDB.Sens[138].Vals[0]", Str(Qgi))
            DoCmd("PROSPER.ANL.GRD.CALC")
            cont = 0

            Do
                Deph = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].MSD[" + Str(cont) + "]")) 'm
                Vsg = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].Vgnslp[" + Str(cont) + "]")) 'ft/s
                Vsl = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].Vlnslp[" + Str(cont) + "]")) 'ft/s
                Ao = 3.1416 * (Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].Diaint[" + Str(cont) + "]"))) ^ 2 / 576 'ft2
                Qfpvalve = (Vsl + Vsg) * Ao 'ft3/s
                Bgi = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].GASFF[" + Str(cont) + "]")) 'ft3/ft3
                Dgivalve = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].RHGF[" + Str(cont) + "]")) * 0.062427818 'En lb/ft3
                Dfpvalve = Val(DoGet("PROSPER.OUT.GRD.Results[0][0][0].FmuMix[" + Str(cont) + "]")) * 0.062427818 'En lb/ft3

                If (Dvalve = Deph) Then

                    Exit Do
                ElseIf (Dvalve < Deph) Then

                    Vsg = Vsgaux + (Dvalve - Dephaux) * (Vsg - Vsgaux) / (Deph - Dephaux)
                    Vsl = Vslaux + (Dvalve - Dephaux) * (Vsl - Vslaux) / (Deph - Dephaux)
                    Qfpvalve = (Vsl + Vsg) * Ao
                    Bgi = Bgiaux + (Dvalve - Dephaux) * (Bgi - Bgiaux) / (Deph - Dephaux)
                    Dgivalve = Dgivalveaux + (Dvalve - Dephaux) * (Dgivalve - Dgivalveaux) / (Deph - Dephaux)
                    Dfpvalve = Dfpvalveaux + (Dvalve - Dephaux) * (Dfpvalve - Dfpvalveaux) / (Deph - Dephaux)

                    Exit Do
                End If

                Vsgaux = Vsg
                Vslaux = Vsl
                Dephaux = Deph
                Bgiaux = Bgi
                Dgivalveaux = Dgivalve
                Dfpvalveaux = Dfpvalve

                cont = cont + 1
            Loop While (cont < 60)
        End Sub

        Private Sub VolTP_VolTR()
            Dim L As Double          'Longitud de la Tp en m
            Dim L2 As Double         'Control de longitud
            Dim TPID As Double       'Diametro interno de TP en in
            Dim TPOD As Double       'Diametro externo de TP en in
            Dim TRID As Double       'Diametro interno de TR en in
            Dim cont As Integer      'Contador
            cont = 0
            L2 = 0


            Do
                L = Val(DoGet("PROSPER.SIN.EQP.Down.Data[" + cont.ToString() + "].Depth")) * 3.280839895
                TPID = Val(DoGet("PROSPER.SIN.EQP.Down.Data[" + cont.ToString() + "].TID")) / 12
                TPOD = Val(DoGet("PROSPER.SIN.EQP.Down.Data[" + cont.ToString() + "].TOD")) / 12
                TRID = Val(DoGet("PROSPER.SIN.EQP.Down.Data[" + cont.ToString() + "].CID")) / 12
                If (L <= (Dvalve * 3.280839895)) Then
                    VolTP = VolTP + (L - L2) * 3.1416 * (TPID) ^ 2 / 4
                    VolTR = VolTR + (L - L2) * (3.1416 / 4) * ((TRID) ^ 2 - (TPOD) ^ 2)
                    If (L = (Dvalve * 3.280839895)) Then Exit Do
                Else
                    VolTP = VolTP + (Dvalve * 3.280839895 - L2) * 3.1416 * (TPID) ^ 2 / 4
                    VolTR = VolTR + (Dvalve * 3.280839895 - L2) * (3.1416 / 4) * ((TRID) ^ 2 - (TPOD) ^ 2)
                    Exit Do
                End If
                L2 = L
                cont = cont + 1
            Loop While (cont < 18)
        End Sub


        'Public Sub Arbol_Decision(ByVal x() As Double,ByVal y() As Double, )
        '    Dim Ptr_act, Pwh_act, Qgi_act, Dvalvula_act, Orif_act, Wc_act As Double
        '    Dim Ptr_itera, Pwh_itera, Qgi_itera, Dvalvula_itera, Orif_itera, Wc_itera As Double
        '    Dim Ptr_Rec, Pwh_Rec, Qgi_Rec, Dvalvula_Rec, Orif_Rec, Wc_Rec As Double
        '    Dim N_qgi, N_pwh As Integer
        '    Dim marc_est As Boolean
        '    Dim i, j, k, i_aux, Ind_estab As Integer

        '    i = 1
        '    i_aux = 0
        '    'Lee las condiciones de operación actual
        '    Ptr_act = Sheets("Principal").Cells(5, 2).Value
        '    Pwh_act = Sheets("Principal").Cells(5, 3).Value
        '    Qgi_act = Sheets("Principal").Cells(5, 4).Value
        '    Dvalvula_act = Sheets("Principal").Cells(5, 5).Value
        '    Orif_act = Sheets("Principal").Cells(5, 6).Value
        '    Wc_act = Sheets("Principal").Cells(5, 7).Value

        '    'Lee el numero de Qgi
        '    N_qgi = Sheets("Principal").Cells(11, 7).Value
        '    N_pwh = Sheets("principal").Cells(11, 5).Value
        '    marc_est = False

        '    Do
        '        If (Sheets("Principal").Cells(20 + i, 2).Value = "") Then Exit Do

        '        Ptr_itera = Sheets("Principal").Cells(20 + i, 35).Value
        '        Pwh_itera = Sheets("Principal").Cells(20 + i, 2).Value
        '        Qgi_itera = Sheets("Principal").Cells(20 + i, 3).Value
        '        Dvalvula_itera = Sheets("Principal").Cells(20 + i, 13).Value
        '        Orif_itera = Sheets("Principal").Cells(20 + i, 12).Value
        '        Wc_itera = Sheets("Principal").Cells(20 + i, 29).Value
        '        Ind_estab = Sheets("Principal").Cells(20 + i, 47)
        '        'If (i = 249) Then Stop
        '        If (Math.Abs(Ptr_act - Ptr_itera) < 0.0005 And Math.Abs(Pwh_act - Pwh_itera) < 0.0005 And Math.Abs(Qgi_act - Qgi_itera) < 0.0005 And Math.Abs(Dvalvula_act - Dvalvula_itera) < 0.0005 And Math.Abs(Orif_act - Orif_itera) < 0.0005 And Math.Abs(Wc_act - Wc_itera) < 0.0005) Then
        '            If (Ind_estab = 1) Then 'Estable
        '                'Imprimerir resultado "El sistema es estable y se recomienda mantener los parametros"

        '            ElseIf (Ind_estab <> 1) Then 'Inestable e inoperante
        '                'Primero se va ha varia Qgi en pwh
        '                Dim kk As Integer
        '                kk = Fix(i / N_qgi) '9

        '                'i_aux = i - (i / N_qgi - kk) * N_qgi
        '                i_aux = ((i / N_qgi) - kk) * N_qgi
        '                Dim Dif As Double
        '                Dim x_Aux As Double
        '                Dim y_Aux As Double

        '                Dif = 1000
        '                For j = (kk * N_qgi + 1) To ((kk + 1) * N_qgi) - 1
        '                    If (Sheets("Principal").Cells(20 + j, 47).Value = 1) Then
        '                        x_Aux = Sheets("Principal").Cells(20 + j, 3).Value
        '                        If ((Math.Abs(Qgi_act - x_Aux)) < Dif) Then
        '                            Dif = (Math.Abs(Qgi_act - x_Aux))
        '                            Qgi_Rec = x_Aux
        '                        End If
        '                        marc_est = True
        '                    End If
        '                Next j
        '                'Como segundo escenario se va ha varia pwh en Qgi
        '                Dif = 1000
        '                For k = CInt(i_aux) To CInt((N_pwh - 1) * N_qgi + i_aux) Step N_qgi
        '                    Dim prueba As Double
        '                    If (Sheets("Principal").Cells(20 + k, 47).Value = 1) Then
        '                        y_Aux = Sheets("Principal").Cells(20 + k, 2).Value
        '                        If ((Math.Abs(Pwh_act - y_Aux)) < Dif) Then
        '                            Dif = Math.Abs(Pwh_act - y_Aux)
        '                            Pwh_Rec = y_Aux
        '                        End If
        '                        marc_est = True
        '                    End If
        '                Next k
        '            End If
        '        End If
        '        i = i + 1
        '    Loop
        'End Sub

        'Sub Disconect()
        '    Server = Nothing
        'End Sub
        'Sub DoCmd(Cmd)
        '    'Subrutina para ejecutar los comandos de Prosper
        '    '*************************************************************
        '    Try
        '        Dim lErr As Long = 0
        '        lErr = Server.DoCommand(Cmd)
        '        If lErr > 0 Then

        '            Throw New Exception("DoCmd: " & Cmd & "   " & Server.GetErrorDescription(lErr))




        '        End If
        '    Catch ex As Exception

        '        Throw New Exception(ex.Message)


        '    End Try


        '    'Dim lErr As Long
        '    'lErr = Server.Docommand(Cmd)
        '    'If lErr > 0 Then
        '    '    MsgBox(Server.GetErrorDescription(lErr))
        '    '    Server = Nothing

        '    'End If

        'End Sub

        'Sub DoSet(Sv, Val)

        '    'Subrutina para enviar datos de Excel a Prosper
        '    '************************************************************
        '    Dim lErr As Long
        '    lErr = Server.SetValue(Sv, Val)
        '    lErr = Server.GetLastError(Nombre)
        '    If lErr > 0 Then
        '        Throw New Exception(Server.GetErrorDescription(lErr))
        '        'Server = Nothing

        '    End If

        'End Sub

        'Function DoGet(Gv As String) As String
        '    Try
        '        Dim lErr As Long
        '        DoGet = Server.GetValue(Gv)
        '        lErr = Server.GetLastError(Nombre)
        '        If lErr > 0 Then
        '            Throw New Exception(Server.GetErrorDescription(lErr))
        '            'Server = Nothing

        '        End If
        '    Catch ex As Exception
        '        'MsgBox("Revisar que prosper este abierto")
        '        Throw New Exception(ex.Message)
        '    End Try
        '    'Función para enviar datos de Prosper a Excel
        '    '************************************************************

        'End Function
        Private Function getLista(ByVal arreglo() As Double) As List(Of Double)

            Dim result As New List(Of Double)

            For Each elem In arreglo
                result.Add(elem)
            Next

            Return result
        End Function
    End Class

    Class Estabilidad
        Private newPropertyValue As String
        Public Property NewProperty() As String
            Get
                Return newPropertyValue
            End Get
            Set(ByVal value As String)
                newPropertyValue = value
            End Set
        End Property
    End Class

    Class MaxMinDouble
        Public Nomb As String
        Public Val As Double
        Public Max As Double
        Public Min As Double
        Public UnidadMedida As Double
    End Class

    Class MaxMinByte
        Public Nomb As String
        Public Val As Byte
        Public Max As Byte
        Public Min As Byte
    End Class
    Class MaxMinInteger
        Public Nomb As String
        Public Val As Integer
        Public Max As Integer
        Public Min As Integer
    End Class
    Class MaxMinDate
        Public Nomb As String
        Public Val As Date
        Public Max As Date
        Public Min As Date
    End Class
    Class MaxMinArreglo
        Public Nomb As String
        Public Max As Double
        Public Min As Double
        Public Val As Object
    End Class
End Namespace
