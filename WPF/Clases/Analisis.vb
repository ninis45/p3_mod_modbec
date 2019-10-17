Imports System.Data
Imports Lagrange.Lagrange
Namespace Generales
    Public Class Analisis
        Public Shared Function conversion(ByVal dt As DataTable, ByRef TableVal(,) As String, ByRef CountReg As Integer, ByRef SinReg As Boolean) As Boolean
            Dim resul As Boolean = False
            Dim AuxTipo As String = ""
            Dim ValorDr As String = ""
            Dim Dimension_1 As Integer = UBound(TableVal, 1)
            Dim Dimension_2 As Integer = UBound(TableVal, 2)
            'Total = dt.Columns.Count - 1
            If dt.Rows.Count > 0 And Dimension_1 >= dt.Rows.Count And Dimension_2 >= dt.Columns.Count Then  'dr.HasRows Then
                For Each row In dt.Rows
                    For I = 0 To dt.Columns.Count - 1
                        AuxTipo = row.Item(I).GetType().ToString
                        If AuxTipo <> "System.DBNull" Then
                            ValorDr = row.Item(I).ToString
                        Else
                            ValorDr = ""
                        End If
                        TableVal(CountReg, I) = VerifCero(ValorDr, AuxTipo)
                    Next I
                    CountReg += 1
                Next
                resul = True
            Else
                SinReg = True
            End If
            Return resul
        End Function
        Public Shared Function seleccionPuntostrayectoria(ByVal dt As DataTable, ByVal fin As Double) As String(,)
            Dim resultado(17, 1) As String
            If (dt.Rows.Count > 0) Then
                resultado(0, 0) = dt.Rows(0).Item(0)
                resultado(0, 1) = dt.Rows(0).Item(1)
                Dim finMD As Double = fin
                Dim finMV As Double = InterpolarProfundidadesVertical(getArreglo(dt, 0, 1), getArreglo(dt, 1, 1), fin)
                eliminarPuntosSobrantes(dt, fin)
                'eliminamos el primero
                dt.Rows.RemoveAt(0)
                Dim view As New DataView(dt)
                view.Sort = "SEVERIDAD"
                Dim indice As Integer = 1 '2
                Dim i As Integer = dt.Rows.Count - 1
                Do While (indice < 17)
                    resultado(indice, 0) = view.Item(i).Item(0)
                    resultado(indice, 1) = view.Item(i).Item(1)
                    indice += 1
                    i -= 1
                Loop
                ReDim Preserve resultado(indice, 1)
                resultado(indice, 0) = finMD
                resultado(indice, 1) = finMV
                ordenarMatriz(resultado, 0)
            End If
            Return resultado
        End Function
        Public Shared Sub eliminarPuntosSobrantes(ByRef dt As DataTable, ByVal fin As Double)
            Try
                For i = dt.Rows.Count - 1 To 0 Step -1
                    If (dt.Rows(i).Item(0) > fin) Then
                        dt.Rows.RemoveAt(i)
                    Else
                        Exit For
                    End If
                Next
            Catch ex As Exception
                Throw New Exception(ex.ToString)
                'MsgBox(ex.ToString)
            End Try
        End Sub
        Public Shared Function InterpolarProfundidadesVertical(ByRef arrProfMD() As Double, ByRef arrMVert() As Double, ByVal desarrollado As Double) As Double
            Dim interpola As New ClassFlaGr
            Dim nFilas As Integer = arrProfMD.Length
            interpola.X = arrProfMD
            interpola.Y = arrMVert
            interpola.XArg = desarrollado
            interpola.IDeg = 3
            interpola.NPts = nFilas
            InterpolarProfundidadesVertical = interpola.FlaGr()
        End Function

        Private Sub obtenerPuntoIntermedio(ByRef dt As DataTable, ByRef resultado(,) As String)
            'Dim i = 0
            'Dim valor As Boolean = True
            'While (i < dt.Rows.Count And valor)
            '    If (Math.Round(dt.Rows(0).Item(0), 0) <> Math.Round(dt.Rows(0).Item(1), 0)) Then
            '        resultado(1, 0) = dt.Rows(0).Item(0)
            '        resultado(1, 1) = dt.Rows(0).Item(1)
            '        valor = False
            '    End If
            '    dt.Rows.RemoveAt(0)
            'End While
        End Sub

        Public Shared Function obtenerTemperatura(ByVal arreglo_trayectoria(,) As String, ByVal a As Double, b As Double) As String(,)
            Dim arreglo() As String = getArregloCol(arreglo_trayectoria, 1, 0)
            Dim resultado(arreglo.Length - 1, 1) As String
            For i = 0 To arreglo.Length - 1
                resultado(i, 0) = arreglo_trayectoria(i, 0) 'arreglo(i)
                resultado(i, 1) = (arreglo(i) * a) + b
            Next
            Return resultado
        End Function

        Public Shared Function VerifCero(ByVal Valor As String, ByVal Tipo As String) As String
            Dim AuxValor As String = Valor
            Try
                Select Case Tipo
                    Case "System.Boolean"
                        If Valor = "" Then AuxValor = CStr(CBool(0))
                    Case "System.Byte"
                        If Valor = "" Then AuxValor = CStr(CByte(0))
                    Case "System.Double"
                        If Valor = "" Then AuxValor = CStr(CDbl(0))
                    Case "System.DBNull"
                        If Valor = "" Then AuxValor = ""
                    Case "System.DBNull2"
                        If Valor = "" Then AuxValor = CStr(CDbl(0))
                    Case "System.Int16"
                        If Valor = "" Then AuxValor = CStr(CInt(0))
                    Case "System.Int32"
                        If Valor = "" Then AuxValor = CStr(CLng(0))
                    Case "System.Int64"
                        If Valor = "" Then AuxValor = CStr(CLng(0))
                    Case "System.String"
                    Case "System.DateTime"
                        If Valor = "" Then AuxValor = ""
                    Case "System.Byte[]"
                    Case Else
                        Throw New Exception("Funcion VerifCero:" & Chr(13) &
                              "No se tiene declarado el tipo: " & Tipo)
                        'MsgBox("Funcion VerifCero:" & Chr(13) &
                        '           "No se tiene declarado el tipo: " & Tipo)
                End Select
            Catch ex As Exception
                'MsgBox(ex.ToString)
                Throw New Exception(ex.ToString)
            End Try
            Return AuxValor
        End Function
        Public Shared Function getArreglo(ByVal dt As DataTable, ByVal col As Integer, tipo As Integer)
            Dim arreglo As New ArrayList
            If (col < dt.Columns.Count) Then
                Try
                    For i = 0 To dt.Rows.Count - 1
                        If (tipo = 0) Then
                            arreglo.Add(CType(dt.Rows(i).Item(col), String))
                        ElseIf (tipo = 1) Then
                            arreglo.Add(CType(dt.Rows(i).Item(col), Double))
                        Else
                            arreglo.Add(CType(dt.Rows(i).Item(col), Integer))
                        End If
                    Next
                Catch ex As Exception
                    'MsgBox(ex.ToString)
                    Throw New Exception(ex.ToString)
                End Try
            End If
            If (tipo = 0) Then
                Return arreglo.ToArray(GetType(String))
            ElseIf (tipo = 1) Then
                Return arreglo.ToArray(GetType(Double))
            Else
                Return arreglo.ToArray(GetType(Integer))
            End If
        End Function
        Public Shared Function getArregloCol(ByVal arreglo As Object, ByVal col As Integer, ByVal tipo As Integer)
            Dim resul As New ArrayList
            Dim a = arreglo.GetUpperBound(1)
            If (col <= arreglo.GetUpperBound(1)) Then
                Try
                    For i = 0 To arreglo.GetUpperBound(0) ' - 1
                        If (tipo = 0) Then
                            resul.Add(CType(arreglo(i, col), String))
                        ElseIf (tipo = 1) Then
                            resul.Add(CType(arreglo(i, col), Double))
                        Else
                            resul.Add(CType(arreglo(i, col), Integer))
                        End If
                    Next
                Catch ex As Exception
                    'MsgBox(ex.ToString)
                    Throw New Exception(ex.ToString)
                End Try
            End If
            If (tipo = 0) Then
                Return resul.ToArray(GetType(String))
            ElseIf (tipo = 1) Then
                Return resul.ToArray(GetType(Double))
            Else
                Return resul.ToArray(GetType(Integer))
            End If
        End Function

        Public Shared Sub recortarNombre(ByRef edoMec_SQL(,) As String, columna As Integer, longitud As Integer)
            For i = 0 To edoMec_SQL.GetUpperBound(0)
                Try
                    edoMec_SQL(i, columna) = edoMec_SQL(i, columna).Substring(0, 15)
                Catch ex As Exception
                    'MsgBox(ex.ToString)
                End Try
            Next

        End Sub

        Public Shared Function getArregloFila(ByVal arreglo As Object, ByVal fila As Integer)
            Dim resul(arreglo.GetUpperBound(1)) As String
            If (fila <= arreglo.GetUpperBound(0)) Then
                Try
                    For i = 0 To arreglo.GetUpperBound(1) ' - 1
                        resul(i) = arreglo(fila, i)
                    Next
                Catch ex As Exception
                    'MsgBox(ex.ToString)
                    Throw New Exception(ex.ToString)
                End Try
            End If
            Return resul
        End Function
        Public Shared Function ConvertirDatatableMatrizDouble(ByVal dt As DataTable) As Double(,)
            Dim resul(dt.Rows.Count - 1, dt.Columns.Count - 1) As Double 'Pq el 
            For i = 0 To dt.Rows.Count - 1
                For j = 0 To dt.Columns.Count - 1
                    resul(i, j) = dt.Rows(i).Item(j)
                Next
            Next
            Return resul
        End Function
        Public Shared Function ConvertirDatatableMatrizString(dt As DataTable) As String(,)
            Dim resul(dt.Rows.Count - 1, dt.Columns.Count - 1) As String
            For i = 0 To dt.Rows.Count - 1
                For j = 0 To dt.Columns.Count - 1
                    resul(i, j) = IIf(IsDBNull(dt.Rows(i).Item(j)), "", dt.Rows(i).Item(j))
                Next
            Next
            Return resul
        End Function
        Public Shared Function getObtenerEncabezado(dt As DataTable) As String()
            Dim resul(dt.Columns.Count - 2) As String
            For j = 1 To dt.Columns.Count - 1
                resul(j - 1) = dt.Columns.Item(j).ColumnName
            Next
            Return resul
        End Function

        Public Shared Function ReyenarID(id As String, cantidad As Integer) As String()
            Dim resul(cantidad) As String
            For i = 0 To cantidad
                resul(i) = id
            Next
            Return resul
        End Function
        Public Shared Sub ordenarMatriz(ByRef matriz As Object, ByVal columna As Integer)
            If (columna < matriz.GetUpperBound(1)) Then
                For i = 0 To matriz.GetUpperBound(0)
                    For j = 0 To matriz.GetUpperBound(0) - 1
                        If (CDbl(matriz(j, columna)) > CDbl(matriz(j + 1, columna))) Then
                            For k = 0 To matriz.GetUpperBound(1)
                                Dim temp As Double = matriz(j, k)
                                matriz(j, k) = matriz(j + 1, k)
                                matriz(j + 1, k) = temp
                            Next
                        End If
                    Next
                Next
            End If
        End Sub
        Public Shared Function InvertirFilaColumnaMatrizString(ByRef matriz As Object) As Double(,)
            Dim resul(matriz.GetUpperBound(1) - 1, matriz.GetUpperBound(0)) As Double
            For i = 0 To matriz.GetUpperBound(0)
                For j = 0 To matriz.GetUpperBound(1) - 1
                    resul(j, i) = matriz(i, j + 1)
                Next
            Next
            Return resul
        End Function
        Public Shared Sub eliminarPrimerCero(ByRef a() As Double, ByRef b() As Double)
            Dim array1 As New ArrayList
            Dim array2 As New ArrayList
            For i = 0 To a.GetUpperBound(0)
                If (a(i) = 0 And b(i) = 0) Then
                    'pediente
                Else
                    array1.Add(CType(a(i), Double))
                    array2.Add(CType(b(i), Double))
                End If
            Next
            a = array1.ToArray(GetType(Double))
            b = array2.ToArray(GetType(Double))
        End Sub
        Private Shared Function validarLongitud(ar As ArrayList, head As ArrayList) As Boolean
            Dim resp As Boolean = True
            For i = 0 To ar.Count - 1
                If ar(0).length <> ar(i).length Then
                    resp = False
                    Exit For
                End If
            Next
            Return resp
        End Function

        Public Shared Function CrearXML(datos As ArrayList, nombrecolumna As ArrayList, tabla As String) As String
            Dim builder As New System.Text.StringBuilder
            If (validarLongitud(datos, nombrecolumna) And datos.Count = nombrecolumna.Count) Then
                builder.Append("<datos>" & vbLf)
                Dim x As Object = datos(0)
                For i = 0 To x.length - 1
                    builder.Append("<registro ")
                    For j = 0 To nombrecolumna.Count - 1
                        builder.Append(nombrecolumna(j) & "=""" & datos(j)(i) & """ ")
                    Next
                    builder.Append("></registro>" & vbLf)
                Next
                builder.Append("</datos>")
            Else
                Throw New System.Exception("Longitudes incorrectas")
            End If
            Return builder.ToString
        End Function
    End Class
End Namespace
