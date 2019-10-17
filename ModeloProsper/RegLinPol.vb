'Imports Chan.Biblioteca

Public Class RegLinPol

    'Class Polinomica

    '
    ' Se incluye la derivada                        10/10/2017     Ver 2.0
    '
    '                                    Juan Felipe de Jesus Diaz Jimenez
    '
    ' N-TH Regresión polinómica de oden N con derivada
    '
#Region "Atributos"

    ' Atributos entrada
    Private _NumDat As New MaxMinInteger
    Private _Grado As New MaxMinInteger
    Private _X As New MaxMinArreglo
    Private _Y As New MaxMinArreglo
    Private _Decim As New MaxMinInteger
    Private _Ayuda As Boolean
    Private IErr As Boolean

    'Private _R(_Grado, _Grado + 1) As Double

    ''salida
    Private _Yest() As Double
    Private _YDer() As Double
    Private _Coef() As Double
    Private _CoefDer() As Double
    Private _CoefCor As Double
    Private _ErrEstan As Double
    Private _ValRep As Double
    Private _Ecuacion As String
    Private _EcuacDer As String
    Private _MsgError As String
#End Region


#Region "Propiedades"

    ' Datos de entrada Entrada
    Public WriteOnly Property Grado As Integer
        'Get
        '    Return _Grado
        'End Get
        Set(value As Integer)
            verificarVariable(value, _Grado)
        End Set
    End Property

    Public WriteOnly Property X As Double()
        'Get
        '    Return _X
        'End Get
        Set(value As Double())
            verificaArreglos(value, _X)
        End Set
    End Property

    Public WriteOnly Property Y As Double()
        'Get
        '    Return _Y
        'End Get
        Set(value As Double())
            verificaArreglos(value, _Y)
        End Set
    End Property

    Public WriteOnly Property Decim As Integer
        'Get
        '    Return _Decim
        'End Get
        Set(value As Integer)
            verificarVariable(value, _Decim)
        End Set
    End Property

    Public Property Ayuda As Boolean
        Get
            Return _Ayuda
        End Get
        Set(value As Boolean)
            _Ayuda = value
        End Set
    End Property


    ' Resultados de Salida

    Public ReadOnly Property Yest() As Double()
        Get
            Return _Yest
        End Get
    End Property

    Public ReadOnly Property YDer() As Double()
        Get
            Return _YDer
        End Get
    End Property

    Public ReadOnly Property Coef() As Double()
        Get
            Return _Coef
        End Get
    End Property

    Public ReadOnly Property CoefDer() As Double()
        Get
            Return _CoefDer
        End Get
    End Property

    Public ReadOnly Property CoefCor As Double
        Get
            Return _CoefCor
        End Get
    End Property

    Public ReadOnly Property ErrEstan As Double
        Get
            Return _ErrEstan
        End Get
    End Property

    Public ReadOnly Property Ecuacion() As String
        Get
            Return _Ecuacion
        End Get
    End Property

    Public ReadOnly Property EcuacDer() As String
        Get
            Return _EcuacDer
        End Get
    End Property

    Public ReadOnly Property MsgError() As String
        Get
            Return _MsgError
        End Get
    End Property

    Public ReadOnly Property ValRep() As Double
        Get
            Return _ValRep
        End Get
    End Property

#End Region
    Public Sub New()
        inicializarDatos()
    End Sub
    Private Sub inicializarDatos()
        _NumDat.Nomb = "Numero de Datos" : _NumDat.Min = 5 : _NumDat.Max = 1000000
        _Grado.Nomb = "Grado del polinomio de ajuste" : _Grado.Min = 2 : _Grado.Max = 6
        _X.Nomb = "Arreglo X" : _X.Min = -1000000000 : _X.Max = 1000000000
        _Y.Nomb = "Arreglo Y" : _Y.Min = -1000000000 : _Y.Max = 1000000000
        _Decim.Nomb = "Numero de decimales después del punto" : _Decim.Min = 3 : _Decim.Max = 25
        '_Yest.Nomb = "" : _Yest.Min = 0 : _Yest.Max = 1
        '_YDer.Nomb = "" : _YDer.Min = 0 : _YDer.Max = 1
        '_Coef.Nomb = "" : _Coef.Min = 0 : _Coef.Max = 1
        '_CoefDer.Nomb = "" : _CoefDer.Min = 0 : _CoefDer.Max = 1
        '_CoefCor.Nomb = "" : _CoefCor.Min = 0 : _CoefCor.Max = 1
        '_ErrEstan.Nomb = "" : _ErrEstan.Min = 0 : _ErrEstan.Max = 1
        '_ValRep.Nomb = "" : _ValRep.Min = 0 : _ValRep.Max = 1
    End Sub
    Private Sub verificarVariable(ByRef valor As Object, ByRef variable As Object)
        If valor < variable.Min Or valor > variable.Max Then
            _MsgError = _MsgError & Formato.GetFrmt("Variable:   " & variable.Nomb & " = " & valor.ToString, 1)
            _MsgError = _MsgError & Formato.GetFrmt("Valor mínimo: " & variable.Min.ToString, 3)
            _MsgError = _MsgError & Formato.GetFrmt("Valor máximo: " & variable.Max.ToString, 6)
            IErr = True
        Else
            variable.Val = valor
        End If
    End Sub
    Private Sub verificaArreglos(ByRef valor As Object, ByRef variable As Object)
        Dim MsgErrDatos_aux As String = ""
        For i = 0 To valor.GetUpperBound(0)
            If Not IsNothing(valor(i)) Then
                'If Not IsNumeric(value(i)) Then value(i) = 0
                If valor(i) < variable.Min Or valor(i) > variable.Max Then
                    MsgErrDatos_aux &= Formato.GetFrmt("Variable:" & variable.Nomb & " (" & i & ") = " & valor(i).ToString, 1)
                    MsgErrDatos_aux &= Formato.GetFrmt("Valor mínimo: " & variable.Min.ToString, 3)
                    MsgErrDatos_aux &= Formato.GetFrmt("Valor máximo: " & variable.Max.ToString, 6)
                End If
            Else
                MsgErrDatos_aux &= Formato.GetFrmt("Variable:" & variable.Nomb & " (" & i & ") = Sin valor", 1)
                valor(i) = 0
            End If
        Next
        If MsgErrDatos_aux.Length = 0 Then
            variable.Val = valor
        Else
            _MsgError &= " " & MsgErrDatos_aux
            IErr = True
        End If
    End Sub
    Private Sub verificarValores()

        verificarVariable(_Grado.Val, _Grado)
        verificaArreglos(_X.val, _X)
        verificaArreglos(_Y.val, _Y)
        verificarVariable(_Decim.Val, _Decim)
        If _X.val.Length = _Y.val.Length Then
            _NumDat.Val = _X.val.Length
        Else
            IErr = True
            _MsgError &= "Error en las dimesiones de las matrices X, Y"
        End If

        If IErr Then
            Throw New Exception(_MsgError)
        End If
    End Sub
    Public Sub Regres()
        verificarValores()
        If IErr Then

        Else


            'Public Shared Sub Regres(ByVal NumDat As Long, ByVal Grado As Long, ByVal X() As Double, ByVal Y() As Double, _
            '             ByRef Yest() As Double, ByRef YDer() As Double, ByRef Coef() As Double, ByRef CoefDer() As Double, _
            '             ByVal Decim As Integer, ByRef CoefCor As Double, ByRef ErrEstan As Double, _
            '             ByRef Ecuacion As String, ByRef EcuacDer As String, ByRef MsgError As String)


            '
            ' Regresion polinomica de la forma :
            '
            ' y  = c1 + c2 x + c3 x ^ 2 + ..... + c(Grado + 1) x ^ Grado
            '
            ' y' = c2 x + 2c3 x + 3c4 x ^ 2 + ..... + (Grado) c(Grado + 1) x ^ (Grado-1)
            '
            ' Coloca los limites de la ecuacion de grado para A(2 Grado+1), R(Grado+1,Grado+2), T(Grado+2)
            '
            '
            ' Datos
            '
            ' NumDat    : Numero de puntos conocidos
            ' Grado     : Grado del polinomio al que se desean ajustar los datos
            ' Decim     : Número de decimales a los que se quiere redondear
            '
            ' X()       : Datos de las absisas a ajustar
            ' Y()       : Datos de las ordenadas a ajustar
            '
            ' Resultados
            '
            ' YEst()    : Resultados de las ordenadas ajustadas
            ' YDer()    : Resultados de la derivada de los polinomios ajustados
            ' Coef()    : Coeficientes del polinomio ajustado
            ' CoefDer() : Coeficientes de la derivada de los polinomios ajustados
            '
            ' CoefCor   : Coeficiente de correlacion
            ' Erro      : 0 La solución converge
            '            -1 La solución no converge
            ' ErrEstan  : Error estandar
            '
            ' Coloca los limites de la ecuacion de grado para: A(2 * Grado+1), R(Grado+1, Grado+2), T(Grado+2)
            '
            ' Funciones locales
            '
            ' FilterList( ) : Verifica que no existan valores repetidos del vector X
            '


            Dim A(2 * _Grado.Val) As Double
            Dim R(_Grado.Val, _Grado.Val + 1) As Double
            Dim T(_Grado.Val + 1) As Double

            ReDim _Yest(_NumDat.Val - 1)
            ReDim _YDer(_NumDat.Val - 1)

            ReDim _Coef(_Grado.Val + 1)
            ReDim _CoefDer(_Grado.Val + 1)

            Dim Erro As Boolean
            Dim I As Long
            Dim J As Long
            Dim K As Long
            Dim P As Double
            Dim Q As Double
            Dim Z As Double
            Dim S As Double
            Dim JJ As Double
            Dim Der As Double

            Dim TxtAyuda As String = "Clase: RegLin.Polinomica" & Chr(13) & Chr(13) &
                                     "Metodo empleado: Regresión lineal Polinomica de grado n, con derivada" & Chr(13) & Chr(13) &
                                     "Datos." & Chr(13) & Chr(13) &
                                     "Grado" & Chr(9) & ": Grado del polinomio al que se desea ajustar" & Chr(13) &
                                      Chr(9) & "  los datos [1 -> n]" & Chr(13) &
                                     "NumDat" & Chr(9) & ": Número de puntos" & Chr(13) &
                                     "Decim" & Chr(9) & ": Número de decimales a los que se quiere redondear" & Chr(13) &
                                      Chr(9) & "  los resultados" & Chr(13) &
                                     "X( )" & Chr(9) & ": Datos de las absisas a ajustar" & Chr(13) &
                                      Chr(9) & "  Dimensión: [0 - NumDat-1]" & Chr(13) &
                                     "Y( )" & Chr(9) & ": Datos de las ordenadas a ajustar" & Chr(13) &
                                      Chr(9) & "  Dimensión: [0 - NumDat-1]" & Chr(13) &
                                     "YEst( )" & Chr(9) & ": Resultados de las ordenadas ajustadas" & Chr(13) &
                                      Chr(9) & "  Dimensión: [0 - NumDat-1]" & Chr(13) &
                                     "YDer( )" & Chr(9) & ": Resultados de la derivada del polinomio ajustado" & Chr(13) &
                                      Chr(9) & "  Dimensión: [0 - NumDat-1]" & Chr(13) & Chr(13) &
                                     "Resultados" & Chr(13) & Chr(13) &
                                     "Coef( )" & Chr(9) & ": Coeficientes del polinomio ajustado [0 -> NumDat]" & Chr(13) &
                                     "CoefDer( )" & Chr(9) & ": Coeficientes de la derivada del polinomio " & Chr(13) &
                                      Chr(9) & "  ajustado [0 -> Grado-1]" & Chr(13) &
                                     "CoefCor" & Chr(9) & ": Coeficiente de correlacion" & Chr(13) &
                                     "Erro" & Chr(9) & ": 0 La solución converge" & Chr(13) &
                                      Chr(9) & "  -1 La solución no converge" & Chr(13) &
                                     "ErrEstan" & Chr(9) & ": Error estandar" & Chr(13) &
                                     "ValRep" & Chr(9) & ": Si es diferente de cero, valor repetido del vector X" & Chr(13) & Chr(13) &
                                     "Metodos" & Chr(13) & Chr(13) &
                                     "Regres( )" & Chr(9) & ": Cálculo de la regresión lineal" & Chr(13) & Chr(13) &
                                     "Funciones" & Chr(13) & Chr(13) &
                                     "FilterList( )" & Chr(9) & ": Verifica que no existan valores repetidos del vector X"

            _MsgError = ""
            _Ecuacion = "Y= "
            _EcuacDer = "Y'= "
            ' _Ayuda = True

            Erro = False

            If Not _Ayuda Then
                Try
                    If _NumDat.Val < 2 Then
                        Erro = True
                        MsgBox("Regres: El número de datos debe ser mayor de Uno: " & _NumDat.ToString, vbInformation)
                        Exit Sub
                    End If

                    If _Grado.Val < 1 Then
                        Erro = True
                        MsgBox("Regres: El grado de la ecuación debe ser mayor de Cero: " & _Grado.ToString, vbInformation)
                        Exit Sub
                    End If

                    'MsgVerc(_NumDat, _X, _Y)

                    A(0) = _NumDat.Val

                    For I = 0 To _NumDat.Val - 1
                        ' Se determinan las matrices con un sistema de ecuaciones
                        For J = 1 To 2 * _Grado.Val
                            A(J) = A(J) + _X.val(I) ^ J
                        Next J

                        For K = 0 To _Grado.Val
                            R(K, _Grado.Val + 1) = T(K) + _Y.val(I) * _X.val(I) ^ K
                            T(K) = T(K) + _Y.val(I) * _X.val(I) ^ K
                        Next K
                        T(_Grado.Val + 1) = T(_Grado.Val + 1) + _Y.val(I) ^ 2
                    Next I

                    'MsgVerc(Grado + 1, A, T)

                    'Se resuelven los sistemas de ecuaciones por matrices
                    For J = 0 To _Grado.Val
                        For K = 0 To _Grado.Val
                            R(J, K) = A(J + K)
                        Next K
                    Next J

                    'Call MsgVerc(Grado + 1, A, A)

                    For J = 0 To _Grado.Val
                        For K = J To _Grado.Val
                            If R(K, J) = 0 Then
                                Erro = True ' No existe solucion
                                _MsgError = _MsgError & Chr(13) & "Clase -> Regres: No existe solución"
                                'MsgBox("K : " & Str(K) & "  J : " & Str(J))
                                Exit Sub
                            Else
                                Exit For
                            End If
                        Next K
                        For I = 0 To _Grado.Val + 1
                            S = R(J, I)
                            R(J, I) = R(K, I)
                            R(K, I) = S
                        Next I
                        Z = 1 / R(J, J)
                        For I = 0 To _Grado.Val + 1
                            R(J, I) = Z * R(J, I)
                        Next I
                        For K = 0 To _Grado.Val
                            If K <> J Then
                                Z = -R(K, J)
                                For I = 0 To _Grado.Val + 1
                                    R(K, I) = R(K, I) + Z * R(J, I)
                                Next I
                            End If
                        Next K
                    Next J

                    ' Primer Coeficiente del polinomio
                    _Coef(0) = R(0, _Grado.Val + 1) ' Constante
                    Dim div() As String = _Coef(0).ToString().Split(".")
                    If (div.Length > 1 AndAlso div(1).Length > _Decim.Val) Then
                        _Ecuacion = _Ecuacion & Str(Math.Round(_Coef(0), _Decim.Val))
                    Else
                        _Ecuacion = _Ecuacion & Str(_Coef(0))
                    End If

                    '_Ecuacion = _Ecuacion & Str(Math.Round(_Coef(0), _Decim.Val))

                    For J = 1 To _Grado.Val
                        _Coef(J) = R(J, _Grado.Val + 1)                     ' Coeficiente de grado J
                        EcuacPolin(J, _Coef(J), _Decim.Val, _Ecuacion)
                    Next J

                    'MsgBox("Ecuación: " & Ecuacion)

                    ' Coeficientes de la Derivada
                    For J = 0 To _Grado.Val - 1
                        ' Coeficientes de la derivada
                        ' y' = c2 x + 2c3 x + 3c4 x ^ 2 + ..... + (Grado) c(Grado + 1) x ^ (Grado-1)
                        _CoefDer(J) = _Coef(J + 1) * (J + 1)
                        EcuacPolin(J, _CoefDer(J), _Decim.Val, _EcuacDer)
                    Next J

                    'MsgBox("Ecuación Derivada: " & EcuacDer)

                    ' Calcula el analisis de la regresion
                    P = 0
                    For J = 1 To _Grado.Val
                        P = P + R(J, _Grado.Val + 1) * (T(J) - A(J) * T(0) / _NumDat.Val)
                    Next J
                    Q = T(_Grado.Val + 1) - T(0) ^ 2 / _NumDat.Val
                    Z = Q - P
                    I = _NumDat.Val - _Grado.Val - 1

                    ' Coeficiente de determinacion: R^2
                    JJ = P / Q

                    ' Coeficiente de correlacion de Pearson R
                    If JJ <> 0 Then
                        _CoefCor = Math.Sqrt(JJ)
                    Else
                        _CoefCor = 0
                        _MsgError = _MsgError & Chr(13) & "Error: Coeficiente de correlación: Raíz cuadrada de un número negativo"
                    End If

                    ' Error estandar estimado
                    If (I > 0) And (Z > 0) Then
                        _ErrEstan = Math.Sqrt(Z / I)
                    Else
                        _ErrEstan = 0
                        _MsgError = _MsgError & Chr(13) & "Error: Error estándar: "
                        If (I < 0) Or (Z < 0) Then
                            _MsgError = _MsgError & "Raíz cuadrada de un número negativo"
                        ElseIf (I = 0) Then
                            _MsgError = _MsgError & "División por cero"
                        End If
                    End If

                    ' Calcula la ordenada Y de la absisa X (datos)
                    For I = 0 To _NumDat.Val - 1
                        ' Termino independiente.
                        P = R(0, _Grado.Val + 1)
                        For J = 0 To _Grado.Val - 1
                            P = P + R(J + 1, _Grado.Val + 1) * _X.val(I) ^ (J + 1)
                        Next J
                        _Yest(I) = P
                    Next I

                    ' Derivada
                    For I = 0 To _NumDat.Val - 1
                        Der = 0
                        For J = 0 To _Grado.Val - 1
                            Der = Der + _CoefDer(J) * _X.val(I) ^ J
                        Next J
                        _YDer(I) = Der
                    Next I

                    If _MsgError = "" Then _MsgError = "Sin error"

                Catch ex As Exception
                    MsgBox("Clase: Regres." & Chr(13) & Chr(13) &
                           "I: " & I.ToString & Chr(13) &
                           "J: " & J.ToString & Chr(13) &
                           "K: " & K.ToString & Chr(13) & Chr(13) &
                           ex.ToString)
                End Try
            Else
                MsgBox(TxtAyuda.ToString, MsgBoxStyle.Information, "Ayuda de la clase: RegLin.Polinomica")
            End If

        End If
    End Sub

    Public Sub EcuacPolin(ByVal Indice As Long, ByVal Coeficien As Double, ByVal Decim As Integer, ByRef Ecuac As String)

        Dim div() As String = _Coef(0).ToString().Split(".")
        If Coeficien >= 0 Then
            If (div.Length > 1 AndAlso div(1).Length > _Decim.Val) Then
                Ecuac = Ecuac & " + (" & Str(Math.Round(Coeficien, Decim))
            Else
                Ecuac = Ecuac & " + (" & Coeficien
            End If
            'Ecuac = Ecuac & " + (" & Str(Math.Round(Coeficien, Decim))
        Else
            If (div.Length > 1 AndAlso div(1).Length > _Decim.Val) Then
                Ecuac = Ecuac & " - (" & Str(Math.Abs(Math.Round(Coeficien, Decim)))
            Else
                Ecuac = Ecuac & " - (" & Str(Math.Abs(Coeficien))
            End If
            'Ecuac = Ecuac & " - (" & Str(Math.Abs(Math.Round(Coeficien, Decim)))
        End If
        Select Case Indice
            Case 0
                Ecuac = Ecuac & ")"
            Case 1
                Ecuac = Ecuac & " * X)"
            Case Else
                Ecuac = Ecuac & " * X ^ " & Str(Indice) & ")"
        End Select

        'MsgBox("Indice:      " & Str(Indice) & Chr(13) & _
        '       "Coeficiente: " & Str(Coeficien) & Chr(13) & _
        '       "Decimales:   " & Str(Decim) & Chr(13) & _
        '       Ecuac, vbInformation)

    End Sub

    Public Sub EvalPolin(ByVal Xeval() As Double, ByRef Yeval() As Double)

        Dim P As Double
        Dim Nd As Integer = Xeval.Count

        ' Calcula la ordenada Y de la absisa X (datos)
        For I = 0 To Nd - 1
            ' Termino independiente.
            P = _Coef(0) 'R(0, _Grado + 1)
            For J = 1 To _Grado.Val
                P = P + _Coef(J) * Xeval(I) ^ (J)
            Next J
            Yeval(I) = P
        Next I

    End Sub

    Public Sub EvalPolinDeriv(ByVal Xeval() As Double, ByRef Yeval() As Double)

        Dim P As Double
        Dim A As Double
        Dim Nd As Integer = Xeval.Count
        Try

            ' Calcula la ordenada Y de la absisa X (datos)
            For I = 0 To Nd - 2
                ' Termino independiente.
                P = _CoefDer(0) 'R(0, _Grado + 1)
                For J = 1 To _Grado.Val - 1
                    A = _CoefDer(J) * Xeval(I) ^ (J)
                    P = P + _CoefDer(J) * Xeval(I) ^ (J)
                Next J
                Yeval(I) = P
            Next I
        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try

    End Sub

    Public Sub MsgVerc(ByVal N As Long, ByVal X() As Double, ByVal Y() As Double)

        Dim Aux As String
        Dim I As Integer
        Aux = ""
        Try
            For I = 0 To N - 1
                Aux = Aux & "X(" & Str(I) & ") = " & Str(X(I)) & "   Y(" & Str(I) & ") = " & Str(Y(I)) & Chr(13)
            Next
            MsgBox(Aux, vbInformation)

        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try

    End Sub

    Public Function FilterList() As Boolean
        '
        ' Verifica que no existan valores de X repetidos
        '
        Dim I As Long
        Dim K As Long

        Try
            ' Detect if there are repeated Xi:
            For I = 0 To _NumDat.Val - 1
                _ValRep = _X.val(I)
                For K = I + 1 To _NumDat.Val - 1
                    If _ValRep = _X.val(K) Then
                        ' En cuanto encuentre una X repetida, sale de la funcion.
                        Return True
                    End If
                Next K
            Next I
        Catch ex As Exception
            MsgBox("Clase: Regres." & Chr(13) & Chr(13) &
                   "Función: FilterList: " & Chr(13) & ex.ToString)
        End Try
        Return False

    End Function

    'End Class
    Class Formato
        Public Shared Function GetFrmt(ByVal Cadena As String, ByVal Tipo As Byte) As String
            Select Case Tipo
                Case 1 '  Un enter al final
                    Cadena = Cadena & Chr(13)
                Case 2 ' Dos enters al final
                    Cadena = Cadena & Chr(13) & Chr(13)
                Case 3 ' Un tabulador antes
                    Cadena = Chr(9) & Cadena
                Case 4 ' Dos tabuladortes antes
                    Cadena = Chr(9) & Chr(9) & Cadena
                Case 5 ' Un tabulador antes y un enter al final
                    Cadena = Chr(9) & Cadena & Chr(13)
                Case 6 ' Un tabulador antes y dos enters al final
                    Cadena = Chr(9) & Cadena & Chr(13) & Chr(13)
                Case 7 ' Dos tabuladores antes y un enter al final
                    Cadena = Chr(9) & Chr(9) & Cadena & Chr(13)
                Case 8 ' Dos tabuladores antes y dos enters al final
                    Cadena = Chr(9) & Chr(9) & Cadena & Chr(13) & Chr(13)
                Case Else
            End Select
            Return Cadena
        End Function
    End Class
    Class MaxMinDouble
        Public Nomb As String
        Public Val As Double
        Public Max As Double
        Public Min As Double
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
        Public val As Object
    End Class



End Class