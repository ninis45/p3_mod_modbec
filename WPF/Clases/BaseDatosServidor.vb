Imports System.Data
Imports System.Data.OleDb
Imports System.Text
Imports System.Xml
Imports DBConnection
'Imports Generales.Generales
Imports System.Web.UI.WebControls
Imports WPF.Generales

Public Class BaseDatosServidor
    Public conn As New SqlClient.SqlConnection
    Public cmd As New SqlClient.SqlCommand
    Public Da As New SqlClient.SqlDataAdapter
    Public dr As SqlClient.SqlDataReader
    'Variable publica con las instrucciones para enviar a la base de datos
    Public Sql As String = ""
    Dim db As SqlServer

    Public Sub New(ByVal Stringconexion As String)
        conn.ConnectionString = Stringconexion
        db = New SqlServer(conn.ConnectionString)
    End Sub
    Public Function InsertUpdateDeletQuery(ByVal query As String) As Boolean
        InsertUpdateDeletQuery = False
        cmd.Connection = conn
        cmd.CommandType = CommandType.Text
        cmd.CommandText = query
        Try
            conn.Open()
            Dim i As Integer = cmd.ExecuteNonQuery
            InsertUpdateDeletQuery = True
        Catch ex As Exception
            MsgBox("Metodo: InsertUpdateDeletQuery" & Chr(13) &
                       "Query:  " & query & Chr(13) & Chr(13) &
                       ex.ToString, MsgBoxStyle.Critical)
        Finally
            conn.Close()
        End Try
    End Function
    Public Function InsertUpdateDeletQuery2(ByVal query As String) As String
        Dim resultado As String = "-1"
        cmd.Connection = conn
        cmd.CommandType = CommandType.Text
        cmd.CommandText = query
        Try
            conn.Open()
            resultado = cmd.ExecuteScalar()
            conn.Close()
            cmd.Cancel()
        Catch ex As Exception
            MsgBox(ex.ToString, MsgBoxStyle.Critical)
        Finally
            conn.Close()
        End Try
        Return resultado
    End Function
    Public Function SelectQuery(ByVal Consulta_SQL As String) As DataTable
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
    Public Function getResultadoModeloGeneral(ByVal IDMODELOPOZO As String) As DataTable
        Dim Query As String = "Select T1.NOMBRE, T2.NUMERO, T3.FECHAMODELO, T4.* 
                            From POZO As T1, AGUJERO As T2, PROSPER.MOD_POZO As T3, PROSPER.MOD_POZO_GENERAL As T4
                            Where T1.IDPOZO = T2.IDPOZO And T2.IDAGUJERO = T3.IDAGUJERO And T3.IDMODPOZO = T4.IDMODPOZO And T4.IDMODPOZO = '" & IDMODELOPOZO & "' AND T4.ENDRECORD=''"
        Return SelectQuery(Query)
    End Function
    Public Function getResultadoMecanico(ByVal IDAGUJERO As String, IDMODPOZO As String) As DataTable
        Dim Query As String = "Select T1.NOMBRE, T2.NUMERO, T3.FECHAMODELO, t4.IDMODPOZOTUBERIA, t3.IDMODPOZO, t4.ETIQUETA, T5.NUMERO, T4.MD, T4.TIDIAM,T4.TIROUG, T4.TODIAM, T4.TOROUG, T4.CIDIAM, T4.CIROUG
                            From POZO As T1, AGUJERO As T2, PROSPER.MOD_POZO As T3, PROSPER.MOD_POZO_TUBERIA As T4, PROSPER.CAT_TIPO_TUBERIA AS T5 
                            Where T1.IDPOZO = T2.IDPOZO And T2.IDAGUJERO = T3.IDAGUJERO And T2.IDAGUJERO = T4.IDAGUJERO And T4.IDTIPOTUBERIA=T5.IDTIPOTUBERIA AND T2.IDAGUJERO ='" & IDAGUJERO & "' AND T3.IDMODPOZO='" & IDMODPOZO & "' AND T4.ENDRECORD IS NULL ORDER BY T4.MD"
        Return SelectQuery(Query)
    End Function

    Public Function getResultadoTrayectoria(ByVal ID As String) As DataTable
        Dim Query As String = "SELECT T2.PROFUNDIDADMD, T2.PROFMV, T2.SEVERIDAD FROM DBO.TRAYECTORIA AS T1, DBO.TRAYECTORIA_GENERAL AS T2 WHERE T1.IDTRAYECTORIA=T2.IDTRAYECTORIA AND T1.IDAGUJERO='" & ID & "' AND T2.ENDRECORD IS NULL ORDER BY T2.PROFUNDIDADMD"
        Return SelectQuery(Query)
    End Function

    Friend Sub guardarTrayectoria(trayecto(,) As String, iDMODPOZO As String)
        Dim datos As New ArrayList()
        datos.Add(Analisis.ReyenarID(iDMODPOZO, trayecto.GetUpperBound(0)))
        datos.Add(Analisis.getArregloCol(trayecto, 0, 1))
        datos.Add(Analisis.getArregloCol(trayecto, 1, 1))
        Dim titulo As New ArrayList()
        titulo.Add("IDMODPOZO")
        titulo.Add("PROFUNDIDADMD")
        titulo.Add("PROFUNDIDADMV")
        Dim HT2 As New Hashtable
        HT2.Add("tabla", "MOD_POZO_TRAYEC")
        HT2.Add("cadena", Analisis.CrearXML(datos, titulo, "MOD_POZO_TRAYEC"))
        Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
    End Sub
    Friend Sub guardarTemperatura(temperatura(,) As String, iDMODPOZO As String)
        Dim datos As New ArrayList()
        datos.Add(Analisis.ReyenarID(iDMODPOZO, temperatura.GetUpperBound(0)))
        datos.Add(Analisis.getArregloCol(temperatura, 0, 1))
        datos.Add(Analisis.getArregloCol(temperatura, 1, 1))
        Dim titulo As New ArrayList()
        titulo.Add("IDMODPOZO")
        titulo.Add("PROFUNDIDADMD")
        titulo.Add("TEMPERATURA")

        Dim HT2 As New Hashtable
        HT2.Add("tabla", "MOD_POZO_TEMP")
        HT2.Add("cadena", Analisis.CrearXML(datos, titulo, "MOD_POZO_TEMP"))
        Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
    End Sub
    Public Function actualizarGrafica1(ByVal IDMODPOZO As String, ByVal titulo1 As String, ByVal titulo2 As String, QL As Double(), QO As Double(), PWFVLP As Double(), PWFIPR As Double()) 'As DataTable
        Dim resultado As Boolean
        Dim Consultar As String = "INSERT INTO PROSPER.VLP_IPR(IDMODPOZO,titulo1,titulo2)"
        Consultar &= " OUTPUT INSERTED.IDVLPIPR"
        Consultar &= " VALUES('" & IDMODPOZO & "','" & titulo1 & "','" & titulo2 & "')"
        Dim id As String = InsertUpdateDeletQuery2(Consultar)
        'If () Then
        Dim objArray As New ArrayList
        objArray.Add(Analisis.ReyenarID(id, QL.GetUpperBound(0)))
        objArray.Add(QL)
        objArray.Add(QO)
        objArray.Add(PWFVLP)
        objArray.Add(PWFIPR)
        Dim objArray2 As New ArrayList
        objArray2.Add("IDVLPIPR")
        objArray2.Add("QL")
        objArray2.Add("QO")
        objArray2.Add("PWFVLP")
        objArray2.Add("PWFIPR")
        Dim HT2 As New Hashtable
        HT2.Add("tabla", "VLP_IPR_DETALLE")
        HT2.Add("cadena", Analisis.CrearXML(objArray, objArray2, "VLP_IPR_DETALLES"))
        Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
        'End if
        Return resultado
    End Function
    Public Function actualizarGrafica2(IDMODPOZO As String, IDCATCORELACION As String(), profmd As Double(,), pres As Double(,)) As Boolean
        Dim resultado As Boolean = False
        For I = 0 To IDCATCORELACION.Length - 1
            Dim Consultar As String = "INSERT INTO PROSPER.CORRELACION (IDMODPOZO,IDCATCORRELACION)"
            Consultar &= " OUTPUT INSERTED.IDCORRELACION"
            Consultar &= " VALUES('" & IDMODPOZO & "','" & IDCATCORELACION(I) & "')"
            Dim id As String = InsertUpdateDeletQuery2(Consultar)
            Dim datos As New ArrayList()
            datos.Add(Analisis.ReyenarID(id, profmd.GetUpperBound(1)))
            datos.Add(Analisis.getArregloFila(profmd, I))
            datos.Add(Analisis.getArregloFila(pres, I))
            Dim titulo As New ArrayList()
            titulo.Add("IDCORRELACION")
            titulo.Add("PROFMD")
            titulo.Add("PRES")
            Dim HT As New Hashtable
            HT.Add("tabla", "CORRELACION_GENERAL")
            HT.Add("cadena", Analisis.CrearXML(datos, titulo, "CORRELACION_GENERAL"))
            Dim result = db.ExecuteSP("dbo.InsertXML", HT)
        Next
        Return resultado
    End Function
    Public Function actualizarGrafica3(ByVal IDMODPOZO As String, ByVal QL As Double(), ByVal QO As Double(), ByVal QliqVLP As Double(,), ByVal PwfVLP As Double(,), ByVal TITULO As Double()) As Boolean
        Dim resultado As Boolean = False
        If (IDMODPOZO.Length > 0) Then
            For I = 0 To QL.Length - 1
                Dim Consultar As String = "INSERT INTO PROSPER.VLP_IPR_GASTO_INYECCION(IDMODPOZO,QL,QO)"
                Consultar &= " OUTPUT INSERTED.IDVLPIPRGASTOINYECCION"
                Consultar &= " VALUES('" & IDMODPOZO & "','" & QL(I) & "','" & QO(I) & "')"
                Dim id As String = InsertUpdateDeletQuery2(Consultar)
                'Dim tb As DataTable = db.ExecuteSP("PROSPER.sp_InsertGastoInyeccion", HT).Tables(0)

                Dim datos As New ArrayList()
                datos.Add(Analisis.ReyenarID(id, QliqVLP.GetUpperBound(0)))
                datos.Add(Analisis.getArregloCol(QliqVLP, I, 1))
                datos.Add(Analisis.getArregloCol(PwfVLP, I, 1))
                datos.Add(TITULO)
                Dim encabezado As New ArrayList()
                encabezado.Add("IDVLPIPRGASTOINYECCION")
                encabezado.Add("PWFVLP")
                encabezado.Add("PWFIPR")
                encabezado.Add("TITULO")
                Dim HT As New Hashtable
                HT.Add("tabla", "VLP_IPR_GASTO_DETALLE")
                HT.Add("cadena", Analisis.CrearXML(datos, encabezado, "VLP_IPR_GASTO_DETALLE"))
                Dim result = db.ExecuteSP("dbo.InsertXML", HT)
            Next
        End If
        Return resultado
    End Function
    Public Function actualizarGrafica4(ByVal IDMODPOZO As String, ByVal Qgi As Double(), ByVal Qliq As Double()) As Boolean
        Dim resultado As Boolean = False
        If (IDMODPOZO.Length > 0) Then
            Dim datos As New ArrayList()
            datos.Add(Analisis.ReyenarID(IDMODPOZO, Qgi.GetUpperBound(0)))
            datos.Add(Qliq)
            datos.Add(Qgi)
            Dim titulo As New ArrayList()
            titulo.Add("IDMODPOZO")
            titulo.Add("QLIQ")
            titulo.Add("QGI")
            Dim HT2 As New Hashtable
            HT2.Add("tabla", "COMPORTAMIENTO_GASBN")
            HT2.Add("cadena", Analisis.CrearXML(datos, titulo, "COMPORTAMIENTO_GASBN"))
            Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
        End If
        Return resultado
    End Function
    Public Function actualizarGrafica5(ByVal IDMODPOZO As String, ByVal PRESION1 As Double(), ByVal PROFMD1 As Double(), ByVal PRESION2 As Double(), ByVal PROFMD2 As Double(), ByVal PRESION3 As Double(), ByVal PROFMD3 As Double(), ByVal PRESION4 As Double(), ByVal PROFMD4 As Double(), ByVal dt As DataTable)
        Dim resultado As Boolean = False
        Dim Consultar As String = "INSERT INTO PROSPER.MOD_POZO_QUICK(IDMODPOZO)"
        Consultar &= " OUTPUT INSERTED.IDMODPOZOQUICK"
        Consultar &= " VALUES('" & IDMODPOZO & "')"
        Dim id As String = InsertUpdateDeletQuery2(Consultar)
        Dim areglo As New ArrayList()
        areglo.Add(PRESION1)
        areglo.Add(PRESION2)
        areglo.Add(PRESION3)
        areglo.Add(PRESION4)
        areglo.Add(PROFMD1)
        areglo.Add(PROFMD2)
        areglo.Add(PROFMD3)
        areglo.Add(PROFMD4)
        For i = 0 To 3
            Dim datos As New ArrayList()
            datos.Add(Analisis.ReyenarID(id, areglo(i).GetUpperBound(0)))
            datos.Add(Analisis.ReyenarID(dt.Rows(i).Item(0), areglo(i).GetUpperBound(0)))
            datos.Add(areglo(i))
            datos.Add(areglo(i + 4))
            'datos.Add(PRESION1)
            'datos.Add(PROFMD1)
            Dim titulo As New ArrayList()
            titulo.Add("IDMODPOZOQUICK")
            titulo.Add("IDCATMODPOZOQUICK")
            titulo.Add("PRESION")
            titulo.Add("PROFMD")
            Dim HT As New Hashtable
            HT.Add("tabla", "MOD_POZO_QUICK_GEN")
            HT.Add("cadena", Analisis.CrearXML(datos, titulo, "MOD_POZO_QUICK_GEN"))
            Dim result = db.ExecuteSP("dbo.InsertXML", HT)
        Next
        Return resultado
    End Function
    Public Function actualizarGrafica6(ByVal IDMODPOZO As String, ByVal Wc_Res As Double(), ByVal Qliq_Res As Double(,), ByVal Qgi_Res As Double()) As Boolean
        Dim resultado As Boolean = False
        If (IDMODPOZO.Length > 0) Then
            For I = 0 To Wc_Res.Length - 1
                Dim Consultar As String = "INSERT INTO PROSPER.PRODUCTIVIDAD(IDMODPOZO,WC_RES,QGI_RES)"
                Consultar &= " OUTPUT INSERTED.IDPRODUCTIVIDAD"
                Consultar &= " VALUES('" & IDMODPOZO & "','" & Wc_Res(I) & "','" & Qgi_Res(I) & "')"
                Dim id As String = InsertUpdateDeletQuery2(Consultar)
                Dim datos As New ArrayList()
                datos.Add(Analisis.ReyenarID(id, Qliq_Res.GetUpperBound(1)))
                datos.Add(Analisis.getArregloFila(Qliq_Res, I))
                Dim titulo As New ArrayList()
                titulo.Add("IDPRODUCTIVIDAD")
                titulo.Add("QLIQ_RES1")
                Dim objXML As New Xml()
                Dim HT2 As New Hashtable
                HT2.Add("tabla", "PRODUCTIVIDAD_DETALLE")
                HT2.Add("cadena", Analisis.CrearXML(datos, titulo, "PRODUCTIVIDAD_DETALLE"))
                Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
            Next
        End If
        Return resultado
    End Function
    Public Function getCatCorrelacion()
        Dim Query As String = "Select T1.IDCATCORRELACION, T1.NOMBRE FROM PROSPER.CAT_CORRELACION As T1 WHERE T1.ENDRECORD IS NULL ORDER BY T1.NUM"
        Return SelectQuery(Query)
    End Function
    Public Function getCAT_MOD_POZO_QUICK()
        Dim Query As String = "Select IDCATMODPOZOQUICK, NOMBRE FROM PROSPER.CAT_MOD_POZO_QUICK WHERE ENDRECORD IS NULL ORDER BY NUM"
        Return SelectQuery(Query)
    End Function
    Public Function getDatosFormacion(ByVal IDAGUJERO As String, ByVal Fecha As String) As DataTable
        Dim query As String = ";WITH columnacima as (SELECT A.IDAGUJERO, CG.CIMAREAL, CG.CIMAREAL + CG.ESPESOR AS BASECG, D.CIMAMD, D.BASEMD, CG.IDFORMACION, 
        COALESCE(D.FECHACIERRE,CONVERT(VARCHAR(10),GETDATE(),120)) AS FECHA
        FROM  DISPARO D
        INNER JOIN AGUJERO A ON A.IDAGUJERO = D.IDAGUJERO
        INNER JOIN COLUMNA_GEOLOGICA CG ON CG.IDAGUJERO = A.IDAGUJERO
        WHERE '" & Fecha & "' BETWEEN D.FECHAAPERTURA AND CASE WHEN
        D.FECHACIERRE  IS NOT NULL
        THEN
        CONVERT(VARCHAR(10),D.FECHACIERRE,120)
        ELSE
        CONVERT(VARCHAR(10),GETDATE(),120)
        END AND
        D.IDAGUJERO = '" & IDAGUJERO & "'AND CG.PRODUCTORA = 1)
        SELECT C.IDAGUJERO, F.TEMPPLANOREF, F.PRESPLANOREF, F.PROFPLANOREF, F.GRADIENTEP,F.GRADIENTET, F.OBSERVACIONES
        from columnacima C
        INNER JOIN FORMACION F ON F.IDFORMACION =C.IDFORMACION
        WHERE CIMAMD >= CIMAREAL AND BASEMD <= BASECG"
        Return SelectQuery(query)
    End Function
End Class
