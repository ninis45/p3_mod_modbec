Public Class Lagrange
    Class ClassFlaGr

        ' Se incluye la derivada Numerica                 10/10/2017     Ver 2.0
        '                                 
        '                                      Juan Felipe de Jesus Diaz Jimenez
        '
        ' Clase de interpolación polinómica de Lagrange
        '

#Region "Datos"

        'Atributos Globales
        Private _X() As Double
        Private _Y() As Double
        Private _XArg As Double
        Private _IDeg As Integer
        Private _NPts As Integer
        Private _IErr As Integer
        Private _Ayuda As Boolean
        Private _Diagnostics As Boolean

#End Region

#Region "Propiedades"
        ' Datos de entrada Entrada
        <System.ComponentModel.Description("Numero de datos"), System.ComponentModel.Category("Datos")>
        <System.ComponentModel.DefaultValue(2)>
        Public Property X() As Double()
            Get
                Return _X
            End Get
            Set(value As Double())
                _X = value
            End Set
        End Property
        Public Property Y() As Double()
            Get
                Return _Y
            End Get
            Set(value As Double())
                _Y = value
            End Set
        End Property
        Public Property XArg As Double
            Get
                Return _XArg
            End Get
            Set(value As Double)
                _XArg = value
            End Set
        End Property
        Public Property IDeg As Integer
            Get
                Return _IDeg
            End Get
            Set(value As Integer)
                _IDeg = value
            End Set
        End Property
        Public Property NPts() As Integer
            Get
                Return _NPts
            End Get
            Set(value As Integer)
                _NPts = value
            End Set
        End Property
        Public Property IErr() As Integer
            Get
                Return _IErr
            End Get
            Set(value As Integer)
                _IErr = value
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
        Public Property Diagnostics As Boolean
            Get
                Return _Diagnostics
            End Get
            Set(value As Boolean)
                _Diagnostics = value
            End Set
        End Property

#End Region

        '
        '  THIS FUNCTION PERFORMS 1-DIMENSIONAL INTERPOLATION
        '  LAST REVISION: February 1989.
        '  This function uses the Lagrange Formula to evaluate the
        '  interpolating polynomial of degree IDEG for argument XArg using
        '  the data values X(MIN).....X(MAX) and Y(MIN).....Y(MAX) where
        '  MIN = MAX - IDEG. THE X(I) values are not necessarily evenly
        '  spaced and can be in either increasing or decreasing order.
        '  Interpolation routine similar to FlaGr in "Applied Numerical
        '  Methods" by Carnahan, Luther and Wilkes.
        '
        '                             REFERENCE
        '                             ---------
        '  1. Carnahan, Luther and Wilkes.: "Applied Numerical Methods",
        '          John Wiley and Sons (1969) 29-34.
        '                          SUBPROGRAM CALLED
        '                          -----------------
        '    FlaGr = This function performs one dimensional interpolation.
        '                        VARIABLE DESCRIPTION
        '                        --------------------
        '   *IDeg  = Degree interpolating polynomial (1 is linear, 2 is
        '            quadratic, etc).
        '
        '    IErr        = Error Code. 0 = OK
        '                              1 = Input variables out of range
        '                              2 = Extrapolation of correlation occuring
        '
        '   *NPts   = The number of data points in x and y.
        '   *X()    = The array of independent variable data points.
        '   *XArg   = The argument for which an interpolated value is desired.
        '   *Y()    = The array of dependent variable data points.
        '    N,N1,L = Dummy variables.
        '    I,J = Loop variables.
        '
        '    (* Indicates input variables)
        '
        Public Function FlaGr() As Double
            'Public Shared Function FlaGr(ByVal X() As Double, ByVal Y() As Double, ByVal XArg As Double,
            '                            ByVal IDeg As Integer, ByVal NPts As Integer, ByRef IErr As Integer) As Double

            If (Ayuda) Then
                MsgBox(ClaseAyuda.getAyuda(1))
                Return 0
            ElseIf Diagnostics Then

            End If

            Dim ValTmp As Double
            Dim A, B, I, J, Min, Max As Integer
            Dim Factor, YEst, Term As Double

            If FlaGr_Val(NPts, IErr, IDeg) Then Return 0

            ValTmp = Rango_X(NPts, X, Y, XArg, IDeg, Max, IErr)

            If ValTmp > 0 Then Return ValTmp

            If IErr = 2 Then Return 0

            ' Compute value of Factor.
            Min = Max - IDeg - 1 : Factor = 1

            A = UBound(X)
            B = UBound(Y)

            For I = Min To Max 'Para Buscar la interpolacion exacta
                If (XArg = X(I)) Then
                    ' La interpolacion es exacta
                    'MsgBox(X(I).ToString, MsgBoxStyle.Information)
                    Return Y(I)
                End If
                Factor = Factor * (XArg - X(I))
            Next I

            ' Evaluate interpolating polynomial.
            YEst = 0
            For I = Min To Max
                Term = Y(I) * Factor / (XArg - X(I))
                For J = Min To Max
                    If (I <> J) Then Term = Term / (X(I) - X(J))
                Next J
                YEst = YEst + Term
            Next I

            Dim objClassFlaGr As New Lagrange()

            If Diagnostics Then objClassFlaGr.verEcuacion(Diagnostics, Me)

            Return YEst

        End Function

    End Class

    Class ClassFlaGr2
        Dim _V() As Double
        Dim _H() As Double
        Dim _F(,) As Double
        Dim _NV As Integer
        Dim _NH As Integer
        Dim _IV As Integer
        Dim _IH As Integer
        Dim _VArg As Double
        Dim _HArg As Double
        Dim _IErr As Integer
        Dim _Ayuda As Boolean
        Dim _Y() As Double
        Private _Diagnostics As Boolean

#Region "Propiedades"
        ' Datos de entrada Entrada
        <System.ComponentModel.Description("Numero de datos"), System.ComponentModel.Category("Datos")>
        <System.ComponentModel.DefaultValue(2)>
        Public Property V() As Double()
            Get
                Return _V
            End Get
            Set(value As Double())
                _V = value
            End Set
        End Property
        Public Property H() As Double()
            Get
                Return _H
            End Get
            Set(value As Double())
                _H = value
            End Set
        End Property
        Public Property F() As Double(,)
            Get
                Return _F
            End Get
            Set(value As Double(,))
                _F = value
            End Set
        End Property
        Public Property NV As Integer
            Get
                Return _NV
            End Get
            Set(value As Integer)
                _NV = value
            End Set
        End Property
        Public Property NH As Integer
            Get
                Return _NH
            End Get
            Set(value As Integer)
                _NH = value
            End Set
        End Property
        Public Property IV As Integer
            Get
                Return _IV
            End Get
            Set(value As Integer)
                _IV = value
            End Set
        End Property
        Public Property IH As Integer
            Get
                Return _IH
            End Get
            Set(value As Integer)
                _IH = value
            End Set
        End Property
        Public Property VArg As Double
            Get
                Return _VArg
            End Get
            Set(value As Double)
                _VArg = value
            End Set
        End Property
        Public Property HArg As Double
            Get
                Return _HArg
            End Get
            Set(value As Double)
                _HArg = value
            End Set
        End Property
        Public Property IErr As Double
            Get
                Return _IErr
            End Get
            Set(value As Double)
                _IErr = value
            End Set
        End Property
        Public Property Ayuda As Boolean
            Get
                Return _IErr
            End Get
            Set(value As Boolean)
                _IErr = value
            End Set
        End Property
        Public Property Y() As Double()
            Get
                Return _Y
            End Get
            Set(value As Double())
                _Y = value
            End Set
        End Property
        Public Property Diagnostics As Boolean
            Get
                Return _Diagnostics
            End Get
            Set(value As Boolean)
                _Diagnostics = value
            End Set
        End Property
#End Region


        '
        '                                                              Ver 1.0
        '                                    Juan Felipe de Jesus Diaz Jimenez
        '  THIS FUNCTION PERFORMS 2-DIMENSIONAL INTERPOLATION
        '  LAST REVISION: February 1989
        '  This function performs double interpolation.
        '
        '                          SUBPROGRAM CALLED
        '                          -----------------
        '    FLAGR = This function performs one dimensional interpolation.
        '                        VARIABLE DESCRIPTION
        '                        --------------------
        '    IErr        = Error Code. 0 = OK
        '                              1 = Input variables out of range
        '                              2 = Extrapolation of correlation occuring
        '
        '   *V()         = Row (vertical) array. (X)
        '   *H()         = Column (horizontal) array. (Z)
        '   *F(,)         = Function value matrix. Y(,)
        '   *NV,NH       = Dimensions of the vertical and horizontal arrays.
        '   *IV,IH       = Deggres of interpolation in the V and H arrays.
        '   *VArg,HArg   = Arguments for which interpolated function values
        '                  are desired.
        '
        '    I,J          = Loop variables.
        '    K,Dimensions = Dummy variable.
        '    X(I),Y(J)    = Dummy Arrays.
        '
        '    (* Indicates input variables *)
        '
        Public Function FlaGr2() As Double
            'Public Shared Function FlaGr2(ByVal V() As Double, ByVal H() As Double, ByVal F(,) As Double,
            '                             ByVal NV As Integer, ByVal NH As Integer, ByVal IV As Integer, ByVal IH As Integer,
            '                            ByVal VArg As Double, ByVal HArg As Double, ByRef IErr As Integer) As Double

            If (Ayuda) Then
                MsgBox(ClaseAyuda.getAyuda(2))
                Return 0
            End If

            Dim I, J, Dimensions As Integer
            Dim Res_Flagr2 As Double = 0
            NH = H.Length
            NV = V.Length

            If Flagr2_Val(NH, NV, IErr) Then Return Res_Flagr2 'Checa que las dimesiones horizontales y verticales nos sean menor a 1

            Dim obclassFlaGr As New ClassFlaGr

            Dimensions = (NV * NH)
            Dim X(Dimensions) As Double
            ReDim Y(Dimensions)
            For J = 0 To NH - 1 'For J = 1 To NH

                For I = 0 To NV - 1 'For I = 1 To NV
                    X(I) = F(I, J)
                Next I
                obclassFlaGr.X = V

                obclassFlaGr.Y = X
                obclassFlaGr.XArg = VArg
                obclassFlaGr.IDeg = IV
                obclassFlaGr.NPts = NV
                'obclassFlaGr.IErr = IErr
                Y(J) = obclassFlaGr.FlaGr()

                If FlaGr_Err(NV, V, VArg, IErr) Then Return 0
            Next J
            obclassFlaGr.X = H
            obclassFlaGr.Y = Y
            obclassFlaGr.XArg = HArg
            obclassFlaGr.IDeg = IH
            obclassFlaGr.NPts = NH
            'obclassFlaGr.IErr = IErr
            obclassFlaGr.Ayuda = Ayuda
            Res_Flagr2 = obclassFlaGr.FlaGr()

            If FlaGr_Err(NH, H, HArg, IErr) Then Return 0

            If IErr Then Res_Flagr2 = 0 'I
            Dim objLagrange As New Lagrange()
            If Diagnostics Then objLagrange.verEcuacion2(Diagnostics, Me)

            Return Res_Flagr2

        End Function

    End Class

    Public Class ClaseAyuda
        Public Shared Function getAyuda(ByVal tipo As Integer) As String
            Dim textAyuda As String = ""
            Select Case tipo
                Case 1
                    textAyuda = textAyuda & GetFrmt("Clase: Ayuda FlaGr                                                ", 2)
                    textAyuda = textAyuda & GetFrmt("Funcion de Intepolacion en 1 dimensiones                          ", 2)
                    textAyuda = textAyuda & GetFrmt("Datos de Entrada:                                                 ", 2)
                    textAyuda = textAyuda & GetFrmt("X()    = Arreglo X() tipo Double, filas (Verticales)              ", 2)
                    textAyuda = textAyuda & GetFrmt("Y()    = Arreglo Y() tipo Double, columna (Horizontal)            ", 2)
                    textAyuda = textAyuda & GetFrmt("IDeg   = Grado de interpolación[valor Dimension de V-2]           ", 2)
                    textAyuda = textAyuda & GetFrmt("XArg   = Argumento deseado para el valor de la función interpolada", 2)
                    textAyuda = textAyuda & GetFrmt("en X [Encontrados entre el rango de los valores de X()]           ", 2)
                    textAyuda = textAyuda & GetFrmt("NPts   = Cantidad de puntos de los Arregos X,Y                    ", 2)
                    textAyuda = textAyuda & GetFrmt("Ayuda  = Despleiga el presente mesaje [Valor booleno]             ", 2)
                    textAyuda = textAyuda & GetFrmt("Diagnostico = Desplega los datos de entrada y resultados de salida", 2)
                    textAyuda = textAyuda & GetFrmt("Resultados", 2)
                    textAyuda = textAyuda & GetFrmt("FlaGr, Es el punto de Interpolacion de acuerdo al argumento ", 2)
                Case 2
                    textAyuda = textAyuda & GetFrmt("Clase: Ayuda FlaGr2                                              ", 2)
                    textAyuda = textAyuda & GetFrmt("Funcion de Intepolacion en 2-Dimensiones                         ", 2)
                    textAyuda = textAyuda & GetFrmt("Datos de Entrada:                                                ", 2)
                    textAyuda = textAyuda & GetFrmt("V()    = Arreglo X() tipo Double, filas (Verticales)             ", 2)
                    textAyuda = textAyuda & GetFrmt("H()    = Arreglo Z() tipo Double, columna (Horizontal)           ", 2)
                    textAyuda = textAyuda & GetFrmt("F(,)   = Arreglo Y(,) tipo Double, columna y filas (valor Y)     ", 2)
                    textAyuda = textAyuda & GetFrmt("IV     = Grado de interpolación para el arreglo V de tipo Integer", 2)
                    textAyuda = textAyuda & GetFrmt("[valor Dimension de V-2]                                         ", 5)
                    textAyuda = textAyuda & GetFrmt("IH     = Grado de interpolación para el arreglo H de tipo Integer", 2)
                    textAyuda = textAyuda & GetFrmt("[valor Dimension de H-2]                                         ", 5)
                    textAyuda = textAyuda & GetFrmt("VArg   = Argumento deseado para el valor de la función interpolada", 2)
                    textAyuda = textAyuda & GetFrmt("en V [Encontrados entre el rango de los valores de V()]           ", 5)
                    textAyuda = textAyuda & GetFrmt("HArg   = Argumento deseado para el valor de la función interpolada", 2)
                    textAyuda = textAyuda & GetFrmt("Ayuda  = Despleiga el presente mesaje [Valor booleno]             ", 2)
                    textAyuda = textAyuda & GetFrmt("Diagnostico = Desplega los datos de entrada y resultados de salida", 2)
                    textAyuda = textAyuda & GetFrmt("Resultados                                                        ", 2)
                    textAyuda = textAyuda & GetFrmt("EL punto interpolado en Y(,), definido por VArg y HArg            ", 1)
                Case Else
                    textAyuda = "NO SE ENCONTRO AYUDA"
            End Select

            Return textAyuda

        End Function
    End Class

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

    Public Shared Function Bisec(ByVal N As Integer, ByRef X() As Double, ByRef Y() As Double, ByVal U As Double) As Double
        '                                                              Ver 1.0
        '                                    Juan Felipe de Jesus Diaz Jimenez
        ' N   : Numero de puntos
        ' X() : Valores de las abscisas ordenadas de menor a mayor
        ' Y() : Valores de las abscisas ordenadas de menor a mayor
        ' U   :

        Dim I, J, K As Integer
        Dim V As Double
        ' Binary search.
        I = 1
        J = N + 1
        Do
            K = (I + J) / 2 ' Se obtiene el valor de I por biseccion
            V = X(K)
            If U < X(K) Then ' Valores de X en forma ascendente
                If X(1) < X(N) Then
                    J = K
                Else
                    I = K
                End If
            Else ' Valores de X en forma descendente
                If X(1) < X(N) Then
                    I = K
                Else
                    J = K
                End If
            End If
        Loop While (J > (I + 1))
        Return InterRecta(U, X(I), Y(I), X(J), Y(J))
    End Function

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    ' Interpolacion de una linea recta
    '
    Public Shared Function InterRecta(ByVal X As Double, ByVal X1 As Double, ByVal Y1 As Double,
                                 ByVal X2 As Double, ByVal Y2 As Double) As Double
        Return ((Y2 - Y1) / (X2 - X1)) * (X - X1) + Y1
    End Function

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    '
    ' Se necesitan dos puntos de cada recta, uno inicial y uno final
    ' Primera recta : Pto 1 : (X11, Y11)
    '                 Pto 2 : (X12, Y12)
    ' Segunda recta : Pto 1 : (X21, Y21)
    '                 Pto 2 : (X22, Y22)
    Public Shared Sub InterseccionRecta1(ByVal X11 As Double, ByVal Y11 As Double,
                                     ByVal X12 As Double, ByVal Y12 As Double,
                                     ByVal X21 As Double, ByVal Y21 As Double,
                                     ByVal X22 As Double, ByVal Y22 As Double,
                                     ByRef XInt As Double, ByRef YInt As Double)

        Dim m1, m2 As Double

        m1 = (Y12 - Y11) / (X12 - X11)
        m2 = (Y22 - Y21) / (X22 - X21)

        XInt = ((m1 * X11) - (m2 * X21) + Y21 - Y11) / (m1 - m2)
        YInt = m1 * (XInt - X11) + Y11
    End Sub

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    '
    ' Se necesitan dos puntos de cada recta, uno inicial y uno final
    ' Primera recta : Pto 1 : (X11, Y11)
    '                 Pto 2 : (X12, Y12)
    ' Segunda recta : Pto 1 : (X21, Y21)
    '                 Pto 2 : (X22, Y22)
    '
    Public Shared Sub InterseccionRecta2(ByVal m1 As Double, ByVal m2 As Double,
                              ByVal X11 As Double, ByVal Y11 As Double,
                              ByVal X21 As Double, ByVal Y21 As Double,
                              ByRef XInt As Double, ByRef YInt As Double)

        XInt = ((m1 * X11) - (m2 * X21) + Y21 - Y11) / (m1 - m2)
        YInt = m1 * (XInt - X11) + Y11
    End Sub

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    Public Function verEcuacion(ByVal impr As Boolean, ByVal obj As Object) As String 'Muestra el tipo de Ecuacion en un Mensaje
        Dim tEcuac As String = "DIMENSIONAL INTERPOLATION"
        Dim Ecua As String = "ClassFlagr: " & Chr(13)
        Dim i As Integer = 0

        For i = 0 To obj.NPts - 1
            Ecua = Ecua & (i + 1).ToString & Chr(9) & "  X: " & obj.X(i).ToString & Chr(9) & "  Y: " & obj.Y(i).ToString & Chr(13)
        Next

        If (impr) Then
            MsgBox("Numero de datos: " & obj.NPts & obj.NPts & Chr(13) &
               "Tipo de ecuación: " & tEcuac & Chr(13) &
                Ecua, MsgBoxStyle.Information)
        End If
        Return tEcuac
    End Function

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    Public Function verEcuacion2(ByVal impr As Boolean, ByVal obj As Object) As String 'Muestra el tipo de Ecuacion en un Mensaje
        Dim tEcuac As String = "2-DIMENSIONAL INTERPOLATION"
        Dim Ecua As String = "ClassFlagr2: " & Chr(13)
        Dim i As Integer = 0
        If (obj.NV <= obj.NH) Then
            For i = 0 To obj.NV - 1
                Ecua = Ecua & (i + 1).ToString & Chr(9) & "  V: " & obj.V(i).ToString & Chr(9) & "  H: " & obj.H(i).ToString & Chr(9) & "  Y: " & obj.Y(i).ToString & Chr(13)
            Next
            If (obj.NV < obj.NH) Then
                For i = i To obj.NH - 1
                    Ecua = Ecua & (i + 1).ToString & Chr(9) & Chr(9) & "  NH: " & obj.Y(i).ToString & Chr(9) & "  Y: " & obj.Y(i).ToString & Chr(13)
                Next
            End If
        Else
            For i = 0 To obj.NH - 1
                Ecua = Ecua & (i + 1).ToString & Chr(9) & "  V: " & obj.V(i).ToString & Chr(9) & "  H: " & obj.H(i).ToString & Chr(9) & "  Y: " & obj.Y(i).ToString & Chr(13)
            Next

            For i = i To obj.NV - 1
                Ecua = Ecua & (i + 1).ToString & Chr(9) & "  V: " & obj.V(i).ToString & Chr(9) & Chr(9) & "  Y: " & obj.Y(i).ToString & Chr(13)
            Next

        End If
        If (impr) Then
            MsgBox("Numero de datos: V " & obj.NV & " H:" & obj.NH & Chr(13) &
               "Tipo de ecuación: " & tEcuac & Chr(13) &
                Ecua, MsgBoxStyle.Information)
        End If

        Return tEcuac

    End Function

    Public Shared Function FlaGr_Val(ByVal NPts As Integer, ByRef IErr As Integer, ByVal IDeg As Integer) As Integer
        Dim Msg As String
        '  Check input variables for valid range.
        IErr = 0 : Msg = ""
        If (IDeg < 1) Then
            Msg = "Interpolacion :  El grado del polinomio de interpolacion"
            Msg = Msg + "esta fuera rango : " & Str(IDeg) & " "
            Msg = Msg + "Imposible continuar." : IErr = 1
        ElseIf (IDeg >= NPts - 1) Then
            Msg = "Interpolacion :  El grado del polinomio de interpolacion"
            Msg = Msg + "esta fuera rango : " & Str(IDeg) & " "
            Msg = Msg + "Imposible continuar." : IErr = 1
        ElseIf (NPts < 3) Then
            Msg = "Interpolacion :  El numero de puntos para interpolar"
            Msg = Msg + "esta fuera rango : " & Str(NPts) & " "
            Msg = Msg + "Imposible continuar." : IErr = 1
        End If
        If IErr Then
            MsgBox(Msg)
            Return True ' Exit Function
        Else
            Return False
        End If
        '  End of validity check.
    End Function

    Public Shared Function Rango_X(ByVal NPts As Integer, ByVal X() As Double, ByVal Y() As Double,
                       ByVal XArg As Double, ByVal IDeg As Integer, ByRef Max As Integer,
                       ByRef IErr As Integer) As Double

        Dim N, N1 As Integer
        Dim Res_Rango_X As Double
        Res_Rango_X = 0
        N = Math.Abs(NPts) - 1 : N1 = IDeg + 1 'N = Math.Abs(NPts) : N1 = IDeg + 1
        If (X(1) > X(0)) Then
            ' Check to be sure that XArg is within range of X(I) values
            ' for interpolation porposes. If it is not, set FlaGr equal
            ' to the appropriate Terminal value (Y(1)) or Y(N) and return.
            ' Note that this precludes extrapolation of data.
            If (XArg < X(0)) Then 'Argumento de interpolacion menor que limite inferior
                Res_Rango_X = Y(0) : IErr = 2 : Return Res_Rango_X
            Else
                If (XArg > X(N)) Then 'Argumento de interpolacion mayor que limite inferior
                    Res_Rango_X = Y(N) : IErr = 2 : Return Res_Rango_X
                Else
                    ' Data are in order of increasing value of x.
                    For Max = N1 To N
                        If (XArg < X(Max)) Then
                            Return Res_Rango_X
                        End If
                    Next Max
                End If
            End If
        Else
            If (XArg > X(0)) Then
                Res_Rango_X = Y(0) : IErr = 2 : Return Res_Rango_X
            Else
                If (XArg < X(N)) Then
                    Res_Rango_X = Y(N) : IErr = 2 : Return Res_Rango_X
                Else
                    ' Data are in order of decreasing value of x.
                    For Max = N1 To N
                        If (XArg > X(Max)) Then
                            Return Res_Rango_X
                        End If
                    Next Max
                End If
            End If
        End If
        Return Res_Rango_X
    End Function

    Private Shared Function Flagr2_Val(ByVal NH As Integer, ByVal NV As Integer, ByRef IErr As Integer) As Boolean
        Dim Msg As String = ""
        ' Check input variables for valid range.
        IErr = 0
        If (NH < 1) Then
            Msg += "Interpolacion en 2D :  El dimensionamiento del arreglo horizontal"
            Msg = Msg + " esta fuera rango : " + NH.ToString
            Msg = Msg + " Imposible continuar."
            IErr = 1
        ElseIf (NV < 1) Then
            Msg += "Interpolacion en 2D :  El dimensionamiento del arreglo vertical"
            Msg = Msg + " esta fuera rango : " + NV.ToString
            Msg = Msg + " Imposible continuar."
            IErr = 1
        End If
        If IErr Then
            MsgBox(Msg)
            Return True
        Else
            Return False
        End If
        ' End of validity check.
    End Function

    '                                                              Ver 1.0
    '                                    Juan Felipe de Jesus Diaz Jimenez
    '
    '    IErr = Error Code. 0 = OK
    '                       1 = Input variables out of range
    '                       2 = Extrapolation of correlation occuring
    '
    Private Shared Function FlaGr_Err(ByVal NPts As Integer, ByVal X() As Double,
                                 ByVal XArg As Double, ByRef IErr As Integer) As Boolean
        Dim Msg As String

        If (IErr) Then
            Msg = "Criterio de interpolacion fuera de los limites, Continua Tomando el limite más cercano"
        End If

        Msg = ""
        If IErr = 1 Then
            Msg = "Interpolocion : Variable de entrada fuera de rango. "
            Msg = Msg + "Imposible continuar."
        ElseIf IErr = 2 Then
            Msg = "La subrutina de Lagrange intento extrapolar. "
            Msg = Msg + "Imposible continuar.    "
            Msg = Msg + "Rango de evaluacion: " + Str(X(1)) + " - " + Str(X(NPts)) + "   "
            Msg = Msg + "Valor a evaluar: " + Str(XArg)
        End If

        If IErr Then
            MsgBox(Msg)
            Return True
        Else
            Return False
        End If
    End Function


End Class