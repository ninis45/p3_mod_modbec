Imports System.Data
Imports System.Data.OleDb
Imports System.Text
Imports DBConnection
'Imports CampoPozos.Analisis
Imports UsrControl.Generales

Public Class BaseDatos
    Public conn As New SqlClient.SqlConnection
    Public cmd As New SqlClient.SqlCommand
    Public Da As New SqlClient.SqlDataAdapter
    Public dr As SqlClient.SqlDataReader
    'Variable publica con las instrucciones para enviar a la base de datos
    Public Sql As String = ""
    Dim db As SqlServer

    Public Sub New(ByVal Stringconexion As String)
        conn.ConnectionString = Stringconexion  '"SERVER=" & pathBD & ";Integrated Security=True;Connect Timeout=10;Initial Catalog=dbo_CI_Prueba2;"
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
    Public Function getDatosAgujero(ByVal IDAGUJERO As String) As DataTable
        Dim Query As String = "SELECT T2.NOMBRE, T1.NUMERO FROM [dbo].[AGUJERO] T1 INNER JOIN  POZO T2 ON T1.IDPOZO = T2.IDPOZO WHERE T1.IDAGUJERO='" & IDAGUJERO & "' AND T1.ENDRECORD IS NULL"
        Return SelectQuery(Query)
    End Function
    Public Function getConfiguracion() As DataTable
        Dim Query As String = "SELECT dbo_TBL_CONFIGURACION.ACTIVO, dbo_TBL_CONFIGURACION.CAMPO, dbo_TBL_CONFIGURACION.FORMACION, dbo_TBL_CONFIGURACION.BLOQUE FROM dbo_TBL_CONFIGURACION WHERE dbo_TBL_CONFIGURACION.ENDRECORD IS NULL"
        Return SelectQuery(Query)
    End Function
    Public Function getPozos(ByVal Campo As String, ByVal Formacion As String) As DataTable
        Dim Query As String = "SELECT T1.IDPOZO, T1.NOMBRE FROM POZO AS T1, PLATAFORMA AS T2, CAMPO AS T3, FORMACION AS T4, CAT_FORMACION AS T5
                            WHERE T1.IDPLATAFORMA=T2.IDPLATAFORMA AND T2.IDCAMPO=T3.IDCAMPO AND T3.IDCAMPO=T4.IDCAMPO AND T4.IDCATFORMACION=T5.IDCATFORMACION AND
                            T3.NOMBRE='" & Campo & "' AND T5.SERIE_NT='" & Formacion & "' AND T1.ENDRECORD IS NULL"
        Return SelectQuery(Query)
    End Function
    Public Function getResultadoModeloGeneral(ByVal IDMODELOPOZO As String) As DataTable
        Dim Query As String = "Select T1.NOMBRE, T2.NUMERO, T3.FECHAMODELO, T4.* 
                            From POZO As T1, AGUJERO As T2, PROSPER.MOD_POZO As T3, PROSPER.MOD_POZO_GENERAL As T4
                            Where T1.IDPOZO = T2.IDPOZO And T2.IDAGUJERO = T3.IDAGUJERO And T3.IDMODPOZO = T4.IDMODPOZO And T4.IDMODPOZO = '" & IDMODELOPOZO & "' AND T4.ENDRECORD=''"
        Return SelectQuery(Query)
    End Function
    Public Function getResultadoGeneral(ByVal ID As String) As DataTable
        Dim Query As String = "Select T1.NOMBRE, T2.NUMERO, T3.FECHAMODELO, T4.* 
                            From POZO T1
            INNER JOIN AGUJERO T2 ON T2.IDPOZO = T1.IDPOZO
            INNER JOIN PROSPER.MOD_POZO T3 ON T3.IDAGUJERO = T2.IDAGUJERO
            INNER JOIN PROSPER.MOD_POZO_GENERAL T4 ON T4.IDMODPOZO = T3.IDMODPOZO
            WHERE T2.IDAGUJERO = '" & ID & "' 
            AND T3.FECHAMODELO = (
            Select MAX(TT3.FECHAMODELO)
                            From POZO TT1
            INNER JOIN AGUJERO TT2 ON TT2.IDPOZO = TT1.IDPOZO
            INNER JOIN PROSPER.MOD_POZO TT3 ON TT3.IDAGUJERO = TT2.IDAGUJERO
            INNER JOIN PROSPER.MOD_POZO_GENERAL TT4 ON TT4.IDMODPOZO = TT3.IDMODPOZO
            WHERE TT2.IDAGUJERO = T2.IDAGUJERO) AND T4.ENDRECORD=''"

        Return SelectQuery(Query)
    End Function
    Public Function guardarModeloPozoGeneral(ByVal ht As Hashtable, ByVal IDMODPOZO As String) As String
        Dim fecha As DateTime = ht.Item("DatGenDate")
        Dim Query As String = "INSERT INTO PROSPER.MOD_POZO_GENERAL (IDMODPOZO, FLUID, PVTMODEL, SEPARATOR, EMULSION, HYDRATE,WATVIS, VISMOD, FLOWTYPE, WELLTYPE,
        LIFTMETHOD, LIFTYPE, PREDICT, TEMPMODEL, RANGESYSTEM, OUTPUTRES, COMPLETION, GRAVELPACK, INFLOWTYPE, GASCONING,DATGENDATE, COMENTA, IPRMETHOD, PRES, TRES, 
        WC, GORDISUELTO, COMPACT, IRElK, QTEST, PTEST, MGSKINMETHOD, MGSKINMODEL, RESPERM, THICKNESS, DRAINAGE, DIETZ, WBR, SKIN, ENTRY, METHOD, GRAVITY, H2S, CO2, 
        N2, GLRINY, GLRATE, VALVEDEPTH, DIAMVALBNC, HTC, THPD, THTD, RGATOTALAFORO, TRPRES, QGINYMIN, QGINYMAX, THTE, ENDRECORD)
        OUTPUT INSERTED.IDMODPOZOGENERAL VALUES('" & IDMODPOZO & "'," & ht.Item("FLUID") & "," & ht.Item("PVTMODEL") & "," & ht.Item("SEPARATOR") & "," &
        ht.Item("EMULSION") & "," & ht.Item("HYDRATE") & "," & ht.Item("WATVIS") & "," & ht.Item("VISMOD") & "," & ht.Item("FLOWTYPE") & "," & ht.Item("WELLTYPE") & "," &
        ht.Item("LIFTMETHOD") & "," & ht.Item("LIFTYPE") & "," & ht.Item("PREDICT") & "," & ht.Item("TEMPMODEL") & "," & ht.Item("RANGESYSTEM") & "," &
        ht.Item("OUTPUTRES") & "," & ht.Item("COMPLETION") & "," & ht.Item("GRAVELPACK") & "," & ht.Item("INFLOWTYPE") & "," & ht.Item("GASCONING") & ",'" &
        fecha.ToString("yyyy/MM/dd") & "','" & ht.Item("COMENTA") & "'," & ht.Item("IPRMETHOD") & "," & ht.Item("PRES") & "," & ht.Item("TRES") & "," &
        ht.Item("WC") & "," & ht.Item("GORDISUELTO") & "," & ht.Item("COMPACT") & "," & ht.Item("IRElK") & "," & ht.Item("QTEST") & "," & ht.Item("PTEST") & "," &
        ht.Item("MGSKINMETHOD") & "," & ht.Item("MGSKINMODEL") & "," & ht.Item("RESPERM") & "," & ht.Item("THICKNESS") & "," & ht.Item("DRAINAGE") & "," &
        ht.Item("DIETZ") & "," & ht.Item("WBR") & "," & ht.Item("SKIN") & "," & ht.Item("ENTRY") & "," & ht.Item("METHOD") & "," & ht.Item("GRAVITY") & "," &
        ht.Item("H2S") & "," & ht.Item("CO2") & "," & ht.Item("N2") & "," & ht.Item("GLRINY") & "," & ht.Item("GLRATE") & "," & ht.Item("VALVEDEPTH") & "," &
        ht.Item("DIAMVALBNC") & "," & ht.Item("HTC") & "," & ht.Item("THPD") & "," & ht.Item("THTD") & "," & ht.Item("RGATOTALAFORO") & "," & ht.Item("TRPRES") & "," &
        ht.Item("QGINYMIN") & "," & ht.Item("QGINYMAX") & "," & ht.Item("THTE") & ",'" & ht.Item("ENDRECORD") & "')"
        Return InsertUpdateDeletQuery2(Query.ToString)
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
    Public Function getResultadoTemperatura(ByVal ID As String) As DataTable
        Dim Query As String = "SELECT T1.NOMBRE, T2.NUMERO, T3.FECHAMODELO, T4.*
                            FROM POZO AS T1, AGUJERO AS T2, PROSPER.MOD_POZO AS T3, PROSPER.MOD_POZO_TEMP AS T4
                            WHERE T1.IDPOZO = T2.IDPOZO AND T2.IDAGUJERO = T3.IDAGUJERO AND T3.IDMODPOZO = T4.IDMODPOZO AND T2.IDAGUJERO = '" & ID & "' AND T4.ENDRECORD='' ORDER BY T4.TEMPERATURA"
        Return SelectQuery(Query)
    End Function

    ''' <summary>
    ''' cream el modelopozo y regresa el idmodpozo del modelopozo
    ''' </summary>
    ''' <param name="IDAGUJERO"></param>
    ''' <returns>es el id relacionado</returns>
    Public Function crearIdModPozo(ByVal IDAGUJERO As String) As String
        Dim HT As New Hashtable
        HT.Add("IDAGUJERO", IDAGUJERO)
        HT.Add("OBSERVACIONES", "")
        Dim tb As DataTable = db.ExecuteSP("PROSPER.proc_insertModPozo", HT).Tables(0)
        Return tb.Rows(0).Item(0)
    End Function
    Public Function actualizarGrafica1(ByVal IDMODPOZO As String, ByVal titulo1 As String, ByVal titulo2 As String, QL As Double(), QO As Double(), PWFVLP As Double(), PWFIPR As Double()) 'As DataTable
        Dim resultado As Boolean
        'Dim HT As New Hashtable
        'HT.Add("IDMODPOZO", IDMODPOZO)
        'HT.Add("titulo1", titulo1)
        'HT.Add("titulo2", titulo2)
        ''HT.Add("IDVLPIPR", 0)
        ''HT.Add("IDMODPOZOR", 0)

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

            ''Dim tb As DataTable = db.ExecuteSP("PROSPER.sp_InsertCorrelacion", HT).Tables(0)
            'If (tb.Rows.Count > 0 AndAlso tb.Rows(0).Item(0) <> "ERROR DE DATOS") Then
            '    Dim query As String = "INSERT INTO PROSPER.CORRELACION_GENERAL(IDCORRELACION, PROFMD, PRES) VALUES " & formateoGrafica2(tb.Rows(0).Item(0), profmd, pres, I)


            '    resultado = InsertUpdateDeletQuery(query)
            'End If
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
                Dim HT2 As New Hashtable
                HT2.Add("tabla", "PRODUCTIVIDAD_DETALLE")
                HT2.Add("cadena", Analisis.CrearXML(datos, titulo, "PRODUCTIVIDAD_DETALLE"))
                Dim result = db.ExecuteSP("dbo.InsertXML", HT2)
            Next
        End If
        Return resultado
    End Function
    Public Function getIDMODELO(IDAGUJERO As String) As String
        Dim Query As String = "Select IDMODPOZO FROM PROSPER.MOD_POZO WHERE FECHAMODELO=(SELECT MAX(FECHAMODELO) FROM PROSPER.MOD_POZO WHERE IDAGUJERO='" & IDAGUJERO & "') AND IDAGUJERO='" & IDAGUJERO & "' AND PROSPER.MOD_POZO.ENDRECORD IS NULL"
        Return SelectQuery(Query).Rows(0).Item(0)
    End Function
    Public Function getCatCorrelacion()
        Dim Query As String = "Select T1.IDCATCORRELACION, T1.NOMBRE FROM PROSPER.CAT_CORRELACION As T1 WHERE T1.ENDRECORD IS NULL ORDER BY T1.NUM"
        Return SelectQuery(Query)
    End Function
    Public Function getCAT_MOD_POZO_QUICK()
        Dim Query As String = "Select IDCATMODPOZOQUICK, NOMBRE FROM PROSPER.CAT_MOD_POZO_QUICK WHERE ENDRECORD IS NULL ORDER BY NUM"
        Return SelectQuery(Query)
    End Function
    Public Function getVLP_IPR(ByVal IDAGUJERO As String) As DataTable
        Dim Query As String = "Select V.IDVLPIPR, V.TITULO1, V.TITULO2  FROM PROSPER.MOD_POZO M INNER JOIN PROSPER.VLP_IPR V On V.IDMODPOZO = M.IDMODPOZO WHERE M.IDAGUJERO ='" & IDAGUJERO & "' AND M.FECHAMODELO = (Select MAX(M1.FECHAMODELO)  FROM PROSPER.MOD_POZO M1 INNER JOIN PROSPER.VLP_IPR V1 On V1.IDMODPOZO = M1.IDMODPOZO WHERE M1.IDAGUJERO = M.IDAGUJERO) AND V.ENDRECORD IS NULL"
        Return SelectQuery(Query)
    End Function
    Public Function getVLP_IPR_DETALLE(ByVal ID As String) As DataTable
        Dim Query As String = "SELECT QL, QO, PWFVLP, PWFIPR FROM PROSPER.VLP_IPR_DETALLE WHERE IDVLPIPR='" & ID & "' AND PROSPER.VLP_IPR_DETALLE.ENDRECORD IS NULL ORDER BY QL"
        Return SelectQuery(Query)
    End Function
    Public Function getMAXMD(ByVal IDAGUJERO As String) As Double
        Dim Query As String = "SELECT MAX(MD) FROM PROSPER.MOD_POZO_TUBERIA WHERE IDAGUJERO='" & IDAGUJERO & "' AND PROSPER.MOD_POZO_TUBERIA.ENDRECORD IS NULL"
        Dim dt As DataTable = SelectQuery(Query)
        If (dt.Rows.Count > 0) Then
            Return dt.Rows(0).Item(0)
        Else
            Return 0.0
        End If
    End Function

    Public Function getCorrelacionGeneral(ByVal ID As String, ByVal nombrepro As String) As DataTable
        Dim tb As New DataTable
        If (ID.Length > 0) Then
            Dim HT As New Hashtable
            HT.Add("IDMODPOZO", ID)
            Dim ds As DataSet = db.ExecuteSP(nombrepro, HT)
            tb = db.ExecuteSP(nombrepro, HT).Tables(0)
        End If
        Return tb
    End Function
    Public Function getDatosFormacion(ByVal IDAGUJERO As String, ByVal Fecha As String) As DataTable
        Try
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

        SELECT C.IDAGUJERO, F.TEMPERATURA,F.PLANOREFERENCIA
        from columnacima C
        INNER JOIN FORMACION_GENERAL F ON F.IDFORMACION =C.IDFORMACION
        WHERE CIMAMD >= CIMAREAL AND BASEMD <= BASECG"
            ' F.TEMPPLANOREF, F.PRESPLANOREF, F.PROFPLANOREF, F.GRADIENTEP,F.GRADIENTET, F.OBSERVACIONES
            Return SelectQuery(query)
        Catch ex As Exception
            Throw New Exception("Error en los datos formacion: " + IDAGUJERO + " - " + Fecha)
        End Try

    End Function

    Public Function getDatosTemperatura(ByVal IDAGUJERO As String, ByVal fechaprueba As String) As DataTable
        Dim tb As New DataTable
        If (IDAGUJERO.Length > 0) Then
            Dim HT As New Hashtable
            HT.Add("IDAGUJERO", IDAGUJERO)
            HT.Add("FECHAPRUEBA", fechaprueba)
            tb = db.ExecuteSP("PROSPER.proc_ConsultaPWSC", HT).Tables(0)
        End If
        Return tb
    End Function
    Public Function getDatosVLPIPRGastoInyeccion(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDVLPIPRGASTOINYECCION,QL,QO FROM PROSPER.VLP_IPR_GASTO_INYECCION WHERE IDMODPOZO='" & IDMODPOZO & "' AND PROSPER.VLP_IPR_GASTO_INYECCION.ENDRECORD IS NULL ORDER BY QL"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getDatosVLPIPRGastoDetalle(ByVal IDVLPIPRGASTOINYECCION As String) As DataTable
        Dim Query As String = "SELECT PWFVLP, PWFIPR, TITULO FROM PROSPER.VLP_IPR_GASTO_DETALLE WHERE IDVLPIPRGASTOINYECCION='" & IDVLPIPRGASTOINYECCION & "' AND PROSPER.VLP_IPR_GASTO_DETALLE.ENDRECORD IS NULL ORDER BY TITULO"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getDatosGasBNC(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT QLIQ, QGI FROM PROSPER.COMPORTAMIENTO_GASBN WHERE IDMODPOZO = '" & IDMODPOZO & "' AND PROSPER.COMPORTAMIENTO_GASBN.ENDRECORD IS NULL ORDER BY QLIQ"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getIDMODPOZOQUICK(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDMODPOZOQUICK FROM PROSPER.MOD_POZO_QUICK WHERE IDMODPOZO = '" & IDMODPOZO & "' AND PROSPER.MOD_POZO_QUICK.ENDRECORD IS NULL "
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getModPozoQuickGen(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDCATMODPOZOQUICK, PROFMD, PRESION FROM PROSPER.MOD_POZO_QUICK_GEN WHERE IDMODPOZOQUICK = (SELECT IDMODPOZOQUICK FROM PROSPER.MOD_POZO_QUICK WHERE IDMODPOZO = '" & IDMODPOZO & "') AND PROSPER.MOD_POZO_QUICK_GEN.ENDRECORD IS NULL ORDER BY PROFMD"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getCatModPozoQuick(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDCATMODPOZOQUICK, NOMBRE FROM PROSPER.CAT_MOD_POZO_QUICK WHERE ENDRECORD IS NULL"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getCountCatModPozoQuick(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDCATMODPOZOQUICK, count(IDCATMODPOZOQUICK) FROM PROSPER.MOD_POZO_QUICK_GEN WHERE IDMODPOZOQUICK = (SELECT IDMODPOZOQUICK FROM PROSPER.MOD_POZO_QUICK WHERE IDMODPOZO = '" & IDMODPOZO & "') AND ENDRECORD IS NULL GROUP BY IDCATMODPOZOQUICK"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getProductividadDetalle(ByVal IDPRODUCTIVIDAD As String) As DataTable
        Dim Query As String = "SELECT QLIQ_RES1 FROM PROSPER.PRODUCTIVIDAD_DETALLE WHERE IDPRODUCTIVIDAD = '" & IDPRODUCTIVIDAD & "' AND ENDRECORD IS NULL ORDER BY QLIQ_RES1 DESC"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getProductividad(ByVal IDMODPOZO As String) As DataTable
        Dim Query As String = "SELECT IDPRODUCTIVIDAD, WC_RES, QGI_RES FROM PROSPER.PRODUCTIVIDAD WHERE IDMODPOZO = '" & IDMODPOZO & "' AND ENDRECORD IS NULL order by WC_RES;"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function actualizarModelo(ByVal IDMODPOZOGENERAL As String, ByVal COLUMNA As String, ByVal VALOR As String) As Boolean
        Dim Consultar As String = "UPDATE PROSPER.MOD_POZO_GENERAL SET " & COLUMNA & "=" & VALOR & " WHERE IDMODPOZOGENERAL ='" & IDMODPOZOGENERAL & "'"
        Return InsertUpdateDeletQuery(Consultar)
    End Function
    ''-------------
    Public Function getAgujeroPozo(ByVal idpozo As String) As DataTable
        Dim Query As String = "SELECT * FROM dbo.Agujero WHERE IDPOZO = '" & idpozo & "' AND ENDRECORD IS NULL"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function getCatClasificacion() As DataTable
        Dim Query As String = "SELECT IDCATCLASIFAGUJERO,NOMBRE FROM dbo.CAT_CLASIF_AGUJERO"
        Dim dt As DataTable = SelectQuery(Query)
        Return dt
    End Function
    Public Function agregarAgujero(ByVal idpozo As String, ByVal NUMERO As String, ByVal FECHAAPERTURA As Date, ByVal FECHACIERRE As Date, ByVal COMENTARIO As String, ByVal COORDENADAX As String, ByVal COORDENADAY As String, ByVal ADITEP As String, ByVal PROFUNDIDADMD As String, ByVal IDCATCLASIFAGUJERO As String) 'As DataTable
        Dim fa As String = FECHAAPERTURA.Year & "-" & FECHAAPERTURA.Month & "-" & FECHAAPERTURA.Day
        Dim fc As String = FECHACIERRE.Year & "-" & FECHACIERRE.Month & "-" & FECHACIERRE.Day
        Dim Consultar As String = "INSERT INTO dbo.Agujero(IDPOZO ,  NUMERO , FECHAAPERTURA , FECHACIERRE , COMENTARIO , COORDENADAX , COORDENADAY , ADITEP , PROFUNDIDADMD , IDCATCLASIFAGUJERO)"
        Consultar &= " OUTPUT INSERTED.IDAGUJERO"
        Consultar &= " VALUES('" & idpozo & "'," & NUMERO & ",'" & fa & "','" & fc & "','" & COMENTARIO & "'," & COORDENADAX & "," & COORDENADAY & ",'" & ADITEP & "'," & PROFUNDIDADMD & ",'" & IDCATCLASIFAGUJERO & "')"
        Dim id As String = InsertUpdateDeletQuery2(Consultar)
        Return id
    End Function
    Public Function eliminarAgujero(ID As String, IDUSUARIO As String) As Boolean
        eliminarAgujero = False
        Dim Consultar As String = "UPDATE DBO.AGUJERO SET ENDRECORD='" & IDUSUARIO & ":" & Date.Today & "' where IDAGUJERO ='" & ID & "'"
        eliminarAgujero = InsertUpdateDeletQuery(Consultar)
    End Function
    Public Function actualizarAgujero(ByVal ID As String, ByVal COLUMNA As String, ByVal VALOR As String) As Boolean
        Dim Consultar As String = "UPDATE dbo.Agujero SET " & COLUMNA & "=" & VALOR & " WHERE IDAGUJERO ='" & ID & "'"
        Return InsertUpdateDeletQuery(Consultar)
    End Function
    ''ESTAS TRES FUNCIONES SON TEMPORAR
    Public Function ValidarEstadoMecanico(ByVal IDAGUJERO As String, IDMODPOZO As String) As Boolean
        Dim resultado As Boolean = False
        If (getResultadoMecanico(IDAGUJERO, IDMODPOZO).Rows.Count > 0) Then resultado = True
        Return resultado
    End Function
    Public Function ValidarTrayectoria(ByVal IDAGUJERO As String) As Boolean
        Dim resultado As Boolean = False
        If (getResultadoTrayectoria(IDAGUJERO).Rows.Count > 0) Then resultado = True
        Return resultado
    End Function
    Public Function ValidarFormacion(ByVal IDAGUJERO As String, fecha As String) As Boolean
        Dim resultado As Boolean = False
        If (getDatosFormacion(IDAGUJERO, fecha).Rows.Count > 0) Then resultado = True
        Return resultado
    End Function


    Public Function obtenerTRsAgujero(agujero As String)
        Dim query As String = " SELECT" &
                                " TRI.COMPONENTE," &
                                " TRC.DIAMETRONOMINAL AS DIAMETRO_NOMINAL," &
                                " TRC.DIAMETROINTERIOR AS DIAMETRO_INTERIOR," &
                                " dbo.TR.PROFUNDIDADINICIO AS PROFUNDIDAD_INICIO," &
                                " dbo.TR.PROFUNDIDADFIN AS PROFUNDIDAD_FIN," &
                                " TRC.DIAMEXT AS DIAM_EXT," &
                                " TRC.DIAMINT AS DIAM_INT," &
                                " TRC.IDTRDETALLE AS IDTRCATALOGO," &
                                " dbo.TR.VISIBLE," &
                                " dbo.TR.IDTR AS IDTUBERIAREVESTIMIENTO," &
                                " dbo.TR.IDPERFORAETAPA AS IDETAPAPERFORACION," &
                                " TRI.IDCATTRIMAGEN AS IDTRIMAGEN, DBO.TR.TITULO" &
                                "" &
                                " FROM" &
                                " dbo.HISTO_PERFORA_TERMINA AS HPT" &
                                " INNER JOIN dbo.PERFORA_ETAPA AS EP ON HPT.IDHISTOPERFORATERMINA = EP.IDHISTOPERFORATERMINA" &
                                " INNER JOIN dbo.TR ON EP.IDPERFORAETAPA = dbo.TR.IDPERFORAETAPA" &
                                " INNER JOIN dbo.TR_DETALLE AS TRC ON dbo.TR.IDTRDETALLE = TRC.IDTRDETALLE" &
                                " INNER JOIN dbo.CAT_TR_IMAGEN AS TRI ON TRC.IDCATTRIMAGEN = TRI.IDCATTRIMAGEN" &
                                " WHERE" &
                                " (HPT.IDAGUJERO = '" + agujero + "')" &
                                " ORDER BY" &
                                " tr.PROFUNDIDADFIN ASC," &
                                " tr.PROFUNDIDADINICIO ASC"

        Return db.GetDataTable(query)
    End Function

    Public Function obtenerTPsAgujero(agujero As String)
        Dim query As String = String.Empty
        query = query & " SELECT"
        query = query & "	TPCO.TITULO,"
        query = query & "	TPC.DIAMETRONOMINAL AS DIAMETRO_NOMINAL,"
        query = query & "	TPC.DIAMETROINTERIOR AS DIAMETRO_INTERIOR,"
        query = query & "	TPCO.LONGITUD,"
        query = query & "	TPC.DIAMEXT AS DIAM_EXT,"
        query = query & "	TPC.DIAMINT AS DIAM_INT,"
        query = query & "	TP.IDTP,"
        query = query & "	TPCO.IDTPCOMPONENTE, "
        query = query & "	TPCO.PROFUNDIDAD, "
        query = query & "   TPI.IDCATTPIMAGEN as IDTPIMAGEN,"
        query = query & "   TPCO.SECUENCIA, TPI.COMPONENTE"
        query = query & " FROM"
        query = query & "	TP_COMPONENTE AS TPCO"
        query = query & "	INNER JOIN TP_DETALLE AS TPC ON TPCO.IDTPDETALLE = TPC.IDTPDETALLE"
        query = query & "	INNER JOIN dbo.TP AS TP ON TPCO.IDTP = TP.IDTP"
        query = query & "	INNER JOIN dbo.CAT_TP_IMAGEN AS TPI ON TPC.IDCATTPIMAGEN = TPI.IDCATTPIMAGEN "
        query = query & " WHERE"
        query = query & "	TP.IDAGUJERO = '" & agujero & "' order by tpco.secuencia"
        Return db.GetDataTable(query)
    End Function
End Class