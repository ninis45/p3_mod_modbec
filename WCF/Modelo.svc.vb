' NOTA: puede usar el comando "Cambiar nombre" del menú contextual para cambiar el nombre de clase "Modelo" en el código, en svc y en el archivo de configuración a la vez.
' NOTA: para iniciar el Cliente de prueba WCF para probar este servicio, seleccione Modelo.svc o Modelo.svc.vb en el Explorador de soluciones e inicie la depuración.

Imports System.Data
Imports ModeloProsper
Imports WCF.Generales
Imports WCF.Helpers
Imports ModeloCI
Imports System.Data.Entity
Imports System.Threading.Tasks

Public Class Modelo
    Implements IModelo
    Private db As New Entities_ModeloCI()
    Private conexion As BaseDatosServidor
    Private MPrsp As Crea.Modelo
    Private IdLog As String
    'Private IdModPozo As String
    'Private IdAgujero As String
    Private Inicio As DateTime
    Private Fin As DateTime
    Private User As String
    Private Logger As ModeloProsper.Logger



    Public Sub New()

        'conexion = New BaseDatosServidor("SERVER=10.85.35.113\SQLDESA01;User=CamposInteligentes;Password=ciGaitep; Database=CI;")

    End Sub
    Public Sub Reset(ByVal IdModPozo As String, ByVal MaxIntentos As Integer) Implements IModelo.Reset
        Try
            Dim modelo As New ModeloProsper.Modelo(IdModPozo)




            modelo.Reset(IdModPozo, MaxIntentos)


        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Sub
    Public Sub Delete(ByVal IdModPozo As String, ByVal IdUsuario As String) Implements IModelo.Delete
        Try
            'Dim mod_configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
            'If mod_configuracion Is Nothing Then
            '    Throw New Exception("El modelo no existe o fue borrado: " + IdModPozo)
            'End If

            'db.CONFIGURACION_ADMINISTRADOR.Remove(mod_configuracion)
            'db.SaveChanges()

            Dim modelo As New ModeloProsper.Modelo()
            modelo.Delete(IdModPozo)

        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try


    End Sub
    Public Function Execute(ByVal IdModPozo As String, ByVal User As String) As Boolean Implements IModelo.Execute
        Try
            Dim modelo As New ModeloProsper.Modelo(IdModPozo)
            Dim inicio As DateTime = DateTime.Now

            Logger = New ModeloProsper.Logger(IdModPozo, User)
            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If



            Logger.SetEstatus(2)

            Dim result = modelo.Create()

            If result Then
                Logger.SetEstatus(3, "Ejecución correcta(Remoto)")
            Else
                Logger.SetEstatus(-1, "Hubo un error al realizar la operación")
            End If

            Return result
        Catch ex As Exception
            If Logger IsNot Nothing AndAlso Logger.Configuracion.ESTATUS = 2 Then
                Logger.SetEstatus(-1, ex.Message)
            End If

            Throw New Exception(ex.Message)
        End Try




    End Function
    Public Function Condicion(ByVal IdModPozo As String, ByVal User As String) As Boolean Implements IModelo.Condicion
        Try
            Dim modelo As New ModeloProsper.Modelo(IdModPozo)
            Dim inicio As DateTime = DateTime.Now

            Logger = New ModeloProsper.Logger(IdModPozo, User)
            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If



            Logger.SetEstatus(2)

            Dim result = modelo.Update()

            If result Then
                Logger.SetEstatus(3, "Ejecución correcta(Remoto)")
            Else
                Logger.SetEstatus(-1, "Hubo un error al realizar la operación")
            End If
            Return True
        Catch ex As Exception
            If Logger IsNot Nothing AndAlso Logger.Configuracion.ESTATUS = 2 Then
                Logger.SetEstatus(-1, ex.Message)
            End If

            Throw New Exception(ex.Message)
        End Try
    End Function
    Public Function Reading(ByVal LiftMethod As Integer, ByVal FileUpload As Byte(), ByVal FileName As String) As List(Of String) Implements IModelo.Reading
        Dim modelo As New ModeloProsper.Modelo()

        If ModeloProsper.Modelo.Dispose = False Then
            Throw New Exception("Open server ocupado")
        End If

        Return modelo.Reading(LiftMethod, FileUpload, FileName)
    End Function

    Public Function Estabilidad(ByVal IdModPozo As String) As Boolean Implements IModelo.Estabilidad
        Dim Conf As CONFIGURACION = db.CONFIGURACION.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        Dim modelo As New ModeloProsper.Estabilidad(Conf)
        Dim result As Boolean
        Try
            result = modelo.Execute()
            'Dim Resultados = modelo.Mapa



            modelo.Save()


            Return result

        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try



    End Function



    Private Sub Program() Implements IModelo.Program
        Dim path = "C:\Program Files (x86)\Petroleum Experts\IPM 11\prosper.exe"
        Dim proceso = Shell(path, AppWinStyle.NormalFocus, False, -1)

    End Sub
    Public Function Sensibilidad_BNF(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal QGI_Min As Double, ByVal QGI_Max As Double, ByVal FileD As Byte(), ByVal FileName As String) As List(Of Gas) Implements IModelo.Sensibilidad_BNF
        Try
            Dim Result As New List(Of Gas)

            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If


            Dim modelo As New ModeloProsper.Modelo()

            Dim TmpResult = modelo.Sensibilidad_BN(Qgi, Pwh, WC, QGI_Min, QGI_Max, FileD, FileName)

            If TmpResult.GetUpperBound(0) > 0 Then
                For i = 0 To TmpResult.GetUpperBound(0)
                    Result.Add(New Gas() With {.Xaux = TmpResult(i, 0), .Yaux = TmpResult(i, 1)})
                Next
            End If

            Return Result
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function

    Public Function Sensibilidad_BN(ByVal IdModPozo As String) As Boolean Implements IModelo.Sensibilidad_BN
        Try

            Dim modelo As New ModeloProsper.Modelo(IdModPozo)
            Dim inicio As DateTime = DateTime.Now

            Logger = New ModeloProsper.Logger(IdModPozo, modelo.ModPozo.CONFIGURACION_ADMINISTRADOR.SingleOrDefault().IDUSUARIO)

            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If

            Logger.SetEstatus(2)

            'Dim License = My.Settings.LicensePROSPER


            Dim result = modelo.Sensibilidad_BN()

            If result Then

                Dim conds = db.PA_operacionPozosFecha(modelo.ModPozo.AGUJERO.IDPOZO).ToList()
                Dim cond As PA_operacionPozosFecha_Result = conds(0)
                Dim cabezera = db.CabeceraPozoGBN.Where(Function(w) w.bajaLogica Is Nothing And w.idPozo = modelo.ModPozo.AGUJERO.IDPOZO).SingleOrDefault()

                If result And cabezera IsNot Nothing And cond.FEC_CONDICION = modelo.ModPozo.FECHAMODELO Then


                    Dim inyeccion = db.DatosInyeccion.Where(Function(w) w.idCabeceraPozoGBN = cabezera.idCabeceraPozoGBN).ToList()


                    inyeccion.ForEach(Function(e) db.DatosInyeccion.Remove(e))
                    db.SaveChanges()


                    Dim QGI = db.COMPORTAMIENTO_GAS.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

                    If QGI IsNot Nothing Then
                        Dim QGIDetalles = db.COMPORTAMIENTO_GAS_DETALLES.Where(Function(w) w.IDCOMPORTAMIENTOGAS = QGI.IDCOMPORTAMIENTOGAS).OrderBy(Function(o) o.XAUX).ToList()


                        If QGIDetalles.Count > 0 Then

                            For Each dt In QGIDetalles

                                db.DatosInyeccion.Add(New DatosInyeccion() With {.idCabeceraPozoGBN = cabezera.idCabeceraPozoGBN, .qLiq = dt.YAUX.GetValueOrDefault(), .qGasBN = dt.XAUX.GetValueOrDefault()})

                            Next
                            db.SaveChanges()


                        End If

                    End If

                    cabezera.porc_agua = cond.gastoagua
                    cabezera.presionCabeza = cond.PRESION_TP.GetValueOrDefault()
                    cabezera.qGasBN = cond.VOLUMEN_BN.GetValueOrDefault()
                    cabezera.fecha = cond.FEC_CONDICION.GetValueOrDefault()


                    db.Entry(cabezera).State = EntityState.Modified
                    db.SaveChanges()
                End If

                Logger.SetEstatus(3, "Ejecución correcta(Remoto)")
            Else
                Logger.SetEstatus(-1, "Hubo un error al realizar la operación")




            End If

            Return result
        Catch ex As Exception
            If Logger.Configuracion.ESTATUS = 2 Then
                Logger.SetEstatus(-1, ex.Message)
            End If

            Throw New Exception(ex.Message)
        End Try

    End Function
    ''Public Function Sensibilidad_BN(ByVal IdModPozo As String, ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double) As List(Of Gas) Implements IModelo.SensGas
    ''    Try
    ''        Dim Prosper = Settings.GetBy("open_server")

    ''        If Prosper = 0 Then
    ''            Throw New Exception("Open Server ocupado, verificar si se encuentra apagado o bloqueado")
    ''        End If
    ''        Dim Modelo As New ModeloProsper.Modelo(IdModPozo)
    ''        Dim ListGas As New List(Of Gas)


    ''        Dim Results = Modelo.Sensibilidad_BN(IdModPozo)

    ''        'For i = 0 To Results.GetUpperBound(1)
    ''        '    ListGas.Add(New Gas() With {.Xaux = Results(0, i), .Yaux = Results(1, i)})
    ''        'Next




    ''        Return ListGas

    ''    Catch ex As Exception
    ''        Throw New Exception(ex.Message)
    ''    End Try

    ''End Function

    Public Function Execute(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal File As Byte) As Boolean

        Return False
    End Function
    Public Sub Scheduled()

    End Sub
    Public Function Condiciones() As Dictionary(Of String, List(Of String)) Implements IModelo.Condiciones

        Dim Usuario = db.USUARIO.Where(Function(w) w.USUARIO1 = "richi").SingleOrDefault()

        If Usuario Is Nothing Then
            Throw New Exception("El usuario no existe o fue removido")
        End If


        'CONDICIONES DE OPERACION PARA LOS MODELOS
        '===========================================================================================================================================
        Dim Modelos = db.VW_MOD_POZO.Where(Function(w) w.ENDRECORD Is Nothing And (w.FUNCION = 6 And w.ESTATUS = 3) Or w.FUNCION = 2).OrderByDescending(Function(o) o.FECHAMODELO).GroupBy(Function(g) g.IDPOZO).ToList()
        Dim CatSaps As Dictionary(Of Integer, Integer) = db.CAT_SAP.ToDictionary(Function(k) k.IDCATSAP, Function(d) d.PROSPER.GetValueOrDefault())
        Dim Messages As Dictionary(Of String, List(Of String)) = New Dictionary(Of String, List(Of String)) From {
            {"success", New List(Of String)},
            {"error", New List(Of String)},
            {"info", New List(Of String)}
        }

        If Modelos.Count > 0 Then


            For Each Pozo In Modelos
                Try
                    Dim VwModPozo = Pozo.OrderByDescending(Function(o) o.FECHAMODELO).ToList()(0)
                    Dim Condicion = (From cond In db.VW_CONDICIONES_OPERACION Where cond.IDPOZO = Pozo.Key And cond.FECHA_CONTABLE = (db.VW_CONDICIONES_OPERACION.Where(Function(w) w.IDPOZO = Pozo.Key).Max(Function(m) m.FECHA_CONTABLE)) Select cond).SingleOrDefault()
                    Dim Archivo = (From ar In db.ARCHIVOS_PROSPER Where ar.borradoLogico Is Nothing And ar.idModPozo = VwModPozo.IDMODPOZO And ar.fecha = (db.ARCHIVOS_PROSPER.Where(Function(w) w.idModPozo = VwModPozo.IDMODPOZO And w.borradoLogico Is Nothing).Max(Function(w) w.fecha)) Select ar).SingleOrDefault()
                    Dim General = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = VwModPozo.IDMODPOZO).SingleOrDefault()


                    If General Is Nothing Then
                        Throw New Exception("No hay datos generales en " + VwModPozo.POZO)
                    End If
                    If Archivo Is Nothing Then
                        Throw New Exception("No hay archivo en " + VwModPozo.POZO)
                        Continue For
                    End If


                    If Condicion IsNot Nothing AndAlso Condicion.FECHA_CONTABLE <> VwModPozo.FECHAMODELO Then
                        Dim ModPozo As MOD_POZO = New MOD_POZO() With {
                                    .IDMODPOZO = Guid.NewGuid().ToString().ToUpper(),
                                    .FECHAMODELO = Condicion.FECHA_CONTABLE,
                                    .FUNCION = 2,
                                    .OBSERVACIONES = "ACTUALIZACION DE MODELO",
                                    .IDAGUJERO = VwModPozo.IDAGUJERO,
                                    .ARCHIVO = VwModPozo.ARCHIVO
                        }
                        db.Entry(Archivo).State = Entity.EntityState.Detached

                        'If Archivo IsNot Nothing Then
                        Archivo.idModPozo = ModPozo.IDMODPOZO
                        Archivo.fecha = DateTime.Now
                        ModPozo.ARCHIVOS_PROSPER.Add(Archivo)
                        'End If


                        Dim ModTray = (From tray In db.MOD_POZO_TRAYEC Where tray.IDMODPOZO = VwModPozo.IDMODPOZO And tray.PROFUNDIDADMV = (db.MOD_POZO_TRAYEC.Where(Function(w) w.IDMODPOZO = VwModPozo.IDMODPOZO).Max(Function(m) m.PROFUNDIDADMV)) Select tray).SingleOrDefault()

                        If ModTray Is Nothing Then
                            Throw New Exception("No hay registro de trayectorias: " + VwModPozo.POZO)
                        End If

                        Dim PT = CIPresion.Presion.GetResultado(ModPozo.FECHAMODELO.GetValueOrDefault(), ModPozo.IDAGUJERO, ModTray.PROFUNDIDADMV.GetValueOrDefault())
                        Dim WC As Double = Water.Clases.Aforo.GetRAA(VwModPozo.IDPOZO, Condicion.FECHA_CONTABLE)

                        ModPozo.MOD_POZO_GENERAL.Add(New MOD_POZO_GENERAL() With {
                                .IDMODPOZOGENERAL = Guid.NewGuid().ToString().ToUpper(),
                                .IDMODPOZO = ModPozo.IDMODPOZO,
                                .LIFTMETHOD = General.LIFTMETHOD,
                                .WC = IIf(WC > 0, WC, General.WC),
                                .THPD = Condicion.P_TP.GetValueOrDefault(),
                                .TRPRES = Condicion.P_TR.GetValueOrDefault(),
                                .TRES = PT("TRES"), ' Necesario para RGA
                                .PRES = PT("PRES"), 'Necesario para RGA
                                .TOTGOR = 0, ' (Condicion.GASTO_GAS.GetValueOrDefault() * 1000000) / (5.615 * Condicion.GASTO_ACEITE.GetValueOrDefault() / 100),
                                .RGATOTALAFORO = 0 ' (Condicion.GASTO_GAS.GetValueOrDefault() * 1000000) / (5.615 * Condicion.GASTO_ACEITE.GetValueOrDefault() / 100)
                        })

                        ModPozo.MOD_POZO_TRAYEC.Add(New MOD_POZO_TRAYEC() With {
                                    .IDMODPOZO = ModPozo.IDMODPOZO,
                                    .IDMODPOZOTRAYEC = Guid.NewGuid().ToString().ToUpper(),
                                    .PROFUNDIDADMD = ModTray.PROFUNDIDADMD,
                                    .PROFUNDIDADMV = ModTray.PROFUNDIDADMV
                        })

                        Select Case CatSaps(VwModPozo.IDCATSAP)

                            Case 1
                                ModPozo.MOD_POZO_BNC.Add(New MOD_POZO_BNC() With {.IDMODPOZO = ModPozo.IDMODPOZO, .GLRINY = Condicion.GASTO_GAS})
                            Case 2
                                ModPozo.MOD_POZO_BEC.Add(New MOD_POZO_BEC() With {.IDMODPOZO = ModPozo.IDMODPOZO, .IDMODPOZOBEC = Guid.NewGuid().ToString().ToUpper(), .FREC_BEC = Condicion.FREC_OPER})
                            Case Else
                                ' Messages.Add("SAP no encontrado: " + VW_MOD_POZO.IDMODPOZO);
                        End Select


                        ModPozo.CONFIGURACION_ADMINISTRADOR.Add(New CONFIGURACION_ADMINISTRADOR() With {.IDMODPOZO = ModPozo.IDMODPOZO, .IDUSUARIO = Archivo.idUsuario, .FECHA_REGLA = ModPozo.FECHAMODELO, .FECHA_PROGRAMACION = DateTime.Now.AddMinutes(30), .ESTATUS = 1, .MAXREINTENTOS = 1, .IDCONFIGURACION = Guid.NewGuid().ToString().ToUpper()})
                        db.MOD_POZO.Add(ModPozo)
                        db.SaveChanges()

                        Messages("success").Add("Modelo agregado :" + VwModPozo.POZO)

                    End If

                Catch ex As Exception
                    Messages("error").Add(ex.Message)
                End Try

            Next
        End If

        'CONDICION DE OPERACION PARA DISTRIBUCION DE GAS
        '=============================================================================================================================================
        Dim Items As List(Of CabeceraPozoGBN) = db.CabeceraPozoGBN.Where(Function(w) w.bajaLogica Is Nothing).ToList()

        For Each Item In Items
            Dim conds = db.PA_operacionPozosFecha(Item.idPozo).OrderByDescending(Function(o) o.FEC_CONDICION).ToList() ''New ObservableCollection < PA_operacionPozosFecha_Result > (db.PA_operacionPozosFecha(Item.idPozo).ToList());
            If conds.Count > 0 Then
                Try
                    Dim ModPozo As MOD_POZO = New MOD_POZO() With {
                                .IDMODPOZO = Guid.NewGuid().ToString().ToUpper(),
                                .OBSERVACIONES = "CONDICION DISTRIBUCION DE GAS",
                                .FUNCION = 1
                    }

                    Dim cond As PA_operacionPozosFecha_Result = conds(0)


                    If Item.archivo Is Nothing Then
                        Throw New Exception("No hay archivo del modelo")
                    End If

                    If cond.FEC_CONDICION > Item.fecha Then

                        Dim agujeros = db.AGUJERO.Where(Function(w) w.IDPOZO = Item.idPozo).OrderByDescending(Function(o) o.FECHAAPERTURA).ToList()

                        If agujeros.Count > 0 Then
                            ModPozo.FECHAMODELO = cond.FEC_CONDICION
                            ModPozo.IDAGUJERO = agujeros(0).IDAGUJERO

                            ModPozo.CONFIGURACION_ADMINISTRADOR.Add(New CONFIGURACION_ADMINISTRADOR() With {.IDMODPOZO = ModPozo.IDMODPOZO, .IDUSUARIO = Usuario.IDUSUARIO, .FECHA_REGLA = ModPozo.FECHAMODELO, .FECHA_PROGRAMACION = DateTime.Now.AddMinutes(30), .ESTATUS = 1, .MAXREINTENTOS = 1, .IDCONFIGURACION = Guid.NewGuid().ToString().ToUpper()})
                            ModPozo.ARCHIVOS_PROSPER.Add(New ARCHIVOS_PROSPER() With {.idModPozo = ModPozo.IDMODPOZO, .idUsuario = Usuario.IDUSUARIO, .archivo = Item.archivo, .nombreArchivo = Item.nombreArchivo, .fecha = DateTime.Now})
                            ModPozo.MOD_POZO_GENERAL.Add(New MOD_POZO_GENERAL() With {.IDMODPOZO = ModPozo.IDMODPOZO, .IDMODPOZOGENERAL = Guid.NewGuid().ToString().ToUpper(), .WC = cond.gastoagua, .THPD = cond.PRESION_TP})
                            ModPozo.MOD_POZO_BNC.Add(New MOD_POZO_BNC() With {.IDMODPOZO = ModPozo.IDMODPOZO, .GLRINY = cond.VOLUMEN_BN, .QGIMIN = 0, .QGIMAX = 10})


                            db.MOD_POZO.Add(ModPozo)
                            db.SaveChanges()

                            Item.porc_agua = cond.gastoagua
                            Item.presionCabeza = cond.PRESION_TP.GetValueOrDefault()
                            Item.qGasBN = cond.VOLUMEN_BN.GetValueOrDefault()
                            Item.fecha = cond.FEC_CONDICION.GetValueOrDefault()
                            db.Entry(Item).State = EntityState.Modified
                            db.SaveChanges()
                            Messages("success").Add("Modelo agregado (Distribución de gas):" + Item.POZO.NOMBRE)
                        Else
                            Throw New Exception("No hay datos del agujero del pozo")
                        End If
                    End If



                    'If (agujeros.Count > 0) Then
                    '                                    {

                    '                MOD_POZO.FECHAMODELO = cond.FEC_CONDICION;
                    '                MOD_POZO.IDAGUJERO = agujeros[0].IDAGUJERO;

                    '                MOD_POZO.CONFIGURACION_ADMINISTRADOR.Add(New CONFIGURACION_ADMINISTRADOR() {IDMODPOZO = MOD_POZO.IDMODPOZO, IDUSUARIO = IdUsuario, FECHA_REGLA = MOD_POZO.FECHAMODELO, FECHA_PROGRAMACION = DateTime.Now.AddMinutes(30), ESTATUS = 1, MAXREINTENTOS = 1, IDCONFIGURACION = Guid.NewGuid().ToString().ToUpper()});
                    '                MOD_POZO.ARCHIVOS_PROSPER.Add(New ARCHIVOS_PROSPER() {idModPozo = MOD_POZO.IDMODPOZO, archivo = Item.archivo, nombreArchivo = Item.nombreArchivo, idUsuario = IdUsuario, fecha = DateTime.Now});
                    '                MOD_POZO.MOD_POZO_GENERAL.Add(New MOD_POZO_GENERAL() {IDMODPOZO = MOD_POZO.IDMODPOZO, IDMODPOZOGENERAL = Guid.NewGuid().ToString().ToUpper(), WC = cond.gastoagua, THPD = cond.PRESION_TP});
                    '                MOD_POZO.MOD_POZO_BNC.Add(New MOD_POZO_BNC() {IDMODPOZO = MOD_POZO.IDMODPOZO, GLRINY = cond.VOLUMEN_BN, QGIMIN = 0, QGIMAX = 10});


                    '                db.MOD_POZO.Add(MOD_POZO);
                    '                db.SaveChanges();



                    '                Item.porc_agua = cond.gastoagua;
                    '                Item.presionCabeza = cond.PRESION_TP.GetValueOrDefault();
                    '                Item.qGasBN = cond.VOLUMEN_BN.GetValueOrDefault();
                    '                Item.fecha = cond.FEC_CONDICION.GetValueOrDefault();
                    '                db.Entry(Item).State = EntityState.Modified;
                    '                db.SaveChanges();

                    '                Messages.Add("Programación correcta: " + MOD_POZO.IDMODPOZO + " con funcion 1");

                    '            }
                    '            Else
                    '            {
                    '                Throw New Exception("No existe el agujero o fue borrado.");
                    '            }
                Catch ex As Exception
                    Messages("error").Add(ex.Message)
                End Try


            End If


        Next
        Return Messages


    End Function
    Public Function Monitor(ByRef OpenServer As String) As List(Of String) Implements IModelo.Monitor
        Dim Prosper As Integer
        Dim Messages As New List(Of String)
        Dim Estatus() As Integer = {0, 1, 2, -1}



        If ModeloProsper.Modelo.Dispose() Then
            Settings.SetBy("open_server", "1")
            Prosper = 1
        Else
            Settings.SetBy("open_server", "0")
            Prosper = 0
        End If

        If OpenServer <> Prosper Then

            If ModeloProsper.Modelo.Dispose() Then
                Messages.Add(String.Format("Open Server: {0} | {1} ", "Disponible", DateTime.Now.ToString()))
            Else
                Messages.Add(String.Format("Open Server: {0} | {1} ", "Ocupado", DateTime.Now.ToString()))
            End If


            Dim Modelos As New Dictionary(Of Integer, Integer) From {
                {1, 0},
                {2, 0}
            }
            Dim ListModelos = db.VW_MOD_POZO.Where(Function(w) Estatus.Contains(w.ESTATUS)).GroupBy(Function(g) g.ESTATUS).Select(Function(s) New With {.Total = s.Count(), .Descripcion = s.Key}).ToList()


            If ListModelos.Count > 0 Then
                For Each Mode In ListModelos
                    Modelos(Mode.Descripcion) = Mode.Total
                Next
            End If



            If Modelos.Count > 0 Then
                Dim Message As String = "En cola " + Modelos(1).ToString() + ", proceso  " + Modelos(2).ToString()
                Messages.Add(Message)
            End If

            OpenServer = Prosper
        End If






        Return Messages
    End Function




    Public Function Dispose() As Boolean Implements IModelo.Dispose
        Return ModeloProsper.Modelo.Dispose()
    End Function
    Public Sub ShutDown() Implements IModelo.ShutDown
        ModeloProsper.Modelo.ShutDown()
    End Sub
    Public Sub Tester(ByVal Command As String) Implements IModelo.Tester
        Dim ObjModelo = New ModeloProsper.Modelo()

        Dim result = ObjModelo.Tester(Command)
    End Sub



    'Listado de modelos
    Public Function CountBy(ByVal Estatus As Integer) As Integer Implements IModelo.CountBy
        Dim List = db.VW_MOD_POZO.AsEnumerable()
        Select Case Estatus
            Case 0
                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS Is Nothing)



            Case 1
                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS.GetValueOrDefault() = 2)
            Case 3

                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS.GetValueOrDefault() = -1)


        End Select

        Return List.Count()
    End Function



    Public Function GetList(ByVal Estatus As Integer, Optional ByVal Page As Integer = 1) As List(Of String) Implements IModelo.GetList
        Dim List = db.VW_MOD_POZO.OrderBy(Function(o) o.FECHAMODELO).AsEnumerable()
        Dim Result As List(Of String) = New List(Of String)
        Dim PerPage As Integer = Settings.GetBy("records_per_page")

        Page = IIf(Page = 0, 1, Page)

        Dim PFrom As Integer = (Page * PerPage) - PerPage
        'Dim PTo As Integer = Page * PerPage

        Select Case Estatus
            Case 0
                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS Is Nothing)



            Case 1
                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS.GetValueOrDefault() = 2)
            Case 3

                List = List.Where(Function(w) w.ESTATUS.GetValueOrDefault() = Estatus Or w.ESTATUS.GetValueOrDefault() = -1)


        End Select

        'Dim r = List.Select(Function(s) Result.Add(New Dictionary(Of String, String) From {{"", ""}}))
        For Each w In List.Skip(PFrom).Take(PerPage).ToList()



            '                {"Id", w.IDMODPOZO}
            Result.Add(Newtonsoft.Json.JsonConvert.SerializeObject(w))
        Next

        Return Result
    End Function
End Class
