Imports System.Data
Imports System.Data.OleDb

Public Class BaseDatos

    Class BD

        ' Creacion de la coneccion a la base de datos de Acces 2010
        'Public Shared conn As New OleDb.OleDbConnection("Provider=Microsoft.ACE.OLEDB.12.0; Data Source=BaseSimoDi.accdb; Persist Security Info=False")
        Public Shared conn As New OleDb.OleDbConnection

        ' Command
        Public Shared cmd As New OleDb.OleDbCommand

        ' Data adapter
        Public Shared Da As New OleDb.OleDbDataAdapter

        ' Data Reader
        Public Shared Dr As OleDb.OleDbDataReader

        ' Variable publica con las instrucciones para enviar a la base de datos
        Public Shared Sql As String = ""

        'Public Shared Sub Connect_DB()
        '    Try
        '        conn.Open()
        '        'MsgBox("Coneccion exitosa")
        '    Catch ex As Exception
        '        MsgBox(ex.ToString)
        '    End Try
        'End Sub

        Public Shared Sub setStringConnection(ByVal pathBD As String)
            conn.ConnectionString = "Provider=Microsoft.ACE.OLEDB.12.0; Data Source=" & pathBD & "; Persist Security Info=False"
            '                        Provider=Microsoft.Jet.OLEDB.4.0;  Data Source=d:\Northwind.mdb;User ID=Admin;Password=;                  
        End Sub

        Public Shared Sub InsertQuery(ByVal query As String)
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            cmd.CommandText = query
            Try
                conn.Open()
                Dim i As Integer = cmd.ExecuteNonQuery
            Catch ex As Exception
                MsgBox("Metodo: InsertQuery" & Chr(13) &
                       "Query:  " & query & Chr(13) & Chr(13) &
                       ex.ToString, MsgBoxStyle.Critical)
            Finally
                conn.Close()
            End Try
        End Sub

        Public Shared Sub correrCommand(ByVal command As OleDb.OleDbCommand)
            command.Connection = conn
            Try
                command.ExecuteNonQuery()
            Catch ex As Exception

            End Try
        End Sub

        Public Shared Sub Consultar(ByVal Consulta_SQL As String, ByRef TableVal(,) As String,
                                    ByRef CountReg As Integer, ByRef Total As Integer, ByRef SinReg As Boolean)
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            Dim I As Integer
            Dim Val As String = ""
            Dim AuxTipo As String = ""
            Dim ValorDr As String = ""

            Dim Diagnostics As Boolean = True
            Dim Dimension_1 As Integer = UBound(TableVal, 1)
            Dim Dimension_2 As Integer = UBound(TableVal, 2)

            cmd.CommandText = Consulta_SQL
            CountReg = 0

            Try
                conn.Open()
                Dr = cmd.ExecuteReader()

                ' Numero de campos, (Columnas) en la base de datos
                Total = Dr.VisibleFieldCount - 1

                If Diagnostics Then
                    If Dimension_2 < Total Then
                        MsgBox("Consultar: " & Chr(13) & Chr(13) &
                               "El número de columnas dimensionadas en la matriz: " & Dimension_2.ToString & Chr(13) &
                               "Es menor al numero de columnas " & Chr(13) &
                               "que se desean consultar en la base de datos: " & Total.ToString & Chr(13) &
                               "No es posible realizar la consulta", MsgBoxStyle.Critical)
                        SinReg = True
                        Exit Sub
                    End If
                End If

                If Dr.HasRows Then
                    ' Recorrido del Data reader
                    While Dr.Read()
                        ' Columnas
                        For I = 0 To Total
                            AuxTipo = Dr(I).GetType().ToString
                            If AuxTipo <> "System.DBNull" Then
                                ValorDr = Dr(I).ToString
                            Else
                                ValorDr = ""
                            End If
                            ' Se asignan los valores del registro a la matriz TableVal
                            TableVal(CountReg, I) = VerifCero(ValorDr, AuxTipo)
                            'MsgBox("Registro : " & CountReg.ToString & Chr(13) &
                            '      " Dato : " & I.ToString & Chr(13) &
                            '      " Valor : " & TableVal(CountReg, I) & Chr(13) &
                            '      " Tipo: " & Dr(I).GetType().ToString & Chr(13))

                            'Val = Val & dr(I).ToString & vbTab ' ","
                            'Val = Val & FiltraFecha(dr(I).ToString) & vbTab ' ","
                            'MsgBox("columna " & I.ToString & " -> " & dr(I).ToString & " -> " & _
                            '       " es de tipo: " & dr(I).GetType().ToString)
                            ' MsgBox(dr(0).ToString + " " + dr(1).ToString + " " + dr(2).ToString + " " + dr(3).ToString)
                        Next I
                        'MsgBox(Val)
                        ' Se asignan los valores del registro a la matriz TableVal
                        'Registro_Matriz(CountReg, Total, Val, TableVal)
                        'Val = ""
                        CountReg += 1

                        If Diagnostics Then
                            If Dimension_1 < CountReg Then
                                MsgBox("Consultar: " & Chr(13) & Chr(13) &
                                       "El numero de registros dimensionados en la matriz: " & Dimension_1.ToString & Chr(13) &
                                       "Es menor al numero de registros " & Chr(13) &
                                       "que se desean consultar en la base de datos: " & Total.ToString & Chr(13) &
                                       "No es posible realizar la consulta", MsgBoxStyle.Critical)
                                SinReg = True
                                Exit Sub
                            End If
                        End If

                    End While
                Else
                    MsgBox("No existen registros para la consulta")
                    SinReg = True
                End If
                Dr.Close()
            Catch ex As Exception
                MsgBox("Consultar:" & Chr(13) & Chr(13) & ex.ToString)
            Finally
                conn.Close()
            End Try
        End Sub

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
                    Case "System.String"
                    Case "System.DateTime"
                        If Valor = "" Then AuxValor = ""
                    Case "System.Byte[]"
                    Case Else
                        MsgBox("Funcion VerifCero:" & Chr(13) &
                               "No se tiene declarado el tipo: " & Tipo)
                End Select
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try
            Return AuxValor
        End Function

        Public Shared Function SelectQuery(ByVal Consulta_SQL As String, ByRef Ds As DataSet) As DataTable
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            'Dim Ds As New DataSet
            Dim Dt As New DataTable
            cmd.CommandText = Consulta_SQL
            Try
                Da.SelectCommand = cmd
                conn.Open()
                Da.Fill(Ds)
                Dt = Ds.Tables(0)
                cmd.Cancel()
                conn.Close()
            Catch ex As Exception
                MsgBox("SelectQuery: " & Chr(13) & Chr(13) & ex.ToString)
            Finally
                conn.Close()
            End Try
            Return Dt
        End Function

        Public Shared Function SelectQuery2(ByVal Consulta_SQL As String) As DataTable
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            Dim Ds As New DataSet
            Dim Dt As New DataTable
            cmd.CommandText = Consulta_SQL
            Try
                Da.SelectCommand = cmd
                conn.Open()
                Da.Fill(Ds)
                Dt = Ds.Tables(0)

                conn.Close()
                cmd.Cancel()
            Catch ex As Exception
                MsgBox(Consulta_SQL & Chr(13) & ex.ToString)

            Finally
                conn.Close()
            End Try
            Return Dt
        End Function

        Public Shared Sub Registro_Matriz(ByVal Count As Integer, ByVal Total As Integer,
                                          ByVal Val As String, ByRef TableVal(,) As String)

            'ReDim Preserve TableVal(Count, Total)
            Dim Aux() As String = Split(Val, vbTab)  '",")
            Dim LastNonEmpty As Integer = -1

            For I = 0 To Total
                LastNonEmpty += 1
                If Aux(I) <> "" Then
                    TableVal(Count, LastNonEmpty) = Aux(I)
                Else
                    TableVal(Count, LastNonEmpty) = 0
                End If
            Next I

        End Sub

        Public Shared Function FiltraFecha(ByRef Cadena As String) As String
            Dim Aux As Integer = 0
            Aux = InStr(Cadena, "/")
            If Aux = 3 Then
                Return Mid(Cadena, 1, 10)
            Else
                Return ElimCar(" ", "_", Cadena)
            End If
        End Function

        Public Shared Function ElimCar(ByVal ChrDat As String, ByVal ChrSust As String,
                                ByVal Cadena As String) As String
            Dim Caden As String = ""
            Dim Caract As String = ""
            Dim I As Integer
            Caden = "" : Caract = ""
            For I = 1 To Len(Cadena)
                Caract = Mid(Cadena, I, 1)
                If ChrSust <> "" Then
                    If (Caract <> ChrDat) Then
                        Caden = Caden & Caract
                    Else
                        Caden = Caden & ChrSust
                    End If
                Else
                    If (Caract <> ChrDat) Then Caden = Caden & Caract
                End If
            Next I
            Return Caden
        End Function

        Public Shared Sub consultarPersona(ByRef identificacion As String)
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text

            cmd.CommandText = "SELECT NOMBRES, APELLIDOS, CORREO, DIRECCION FROM PERSONA WHERE IDPERSONA = " + identificacion

            Try
                conn.Open()
                Dr = cmd.ExecuteReader()
                conn.Close()
            Catch ex As Exception
                MsgBox(ex.ToString)
            Finally
                conn.Close()
            End Try
            '
            ' Nota:
            ' Despues de llamar la subrutina, es necesario cerrar 
            ' el Data Reader, de lo contrario se genera un error.
            '     ->    dr.Close()    <-
            '
        End Sub

        Public Shared Sub updateQuery(ByVal sQuery As String)
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            'Dim Ds As New DataSet
            Dim Dt As New DataTable
            cmd.CommandText = sQuery
            Try
                conn.Open()
                Dim I As Boolean = cmd.ExecuteNonQuery
                If Not I Then
                    MsgBox("Metodo: UpdateQuery." & Chr(13) & Chr(13) &
                           "No fue posible actualizar el registro." & Chr(13) & Chr(13) &
                           "Query:  " & sQuery & Chr(13) & Chr(13) & I.ToString)
                End If
            Catch ex As Exception
                MsgBox("Metodo: UpdateQuery." & Chr(13) & Chr(13) &
                       "Query:  " & sQuery & Chr(13) & Chr(13) &
                       ex.ToString, MsgBoxStyle.Critical)
            Finally
                conn.Close()
            End Try
        End Sub

        Public Shared Sub deleteQuery(ByVal query As String)
            cmd.Connection = conn
            cmd.CommandType = CommandType.Text
            cmd.CommandText = query
            Try
                conn.Open()
                Dim i As Integer = cmd.ExecuteNonQuery
            Catch ex As Exception
                Throw New Exception(ex.Message)
            Finally
                conn.Close()
            End Try
        End Sub

        Public Shared Function Fecha_Doble(ByVal Fecha As String) As Double
            Dim aFecha As Date = Convert.ToDateTime(Fecha)
            Return CDbl(aFecha.ToOADate)
        End Function

    End Class

End Class