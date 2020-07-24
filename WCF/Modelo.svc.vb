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

            If Logger.Configuracion.ESTATUS = 3 Then
                Throw New Exception("El modelo ha sido ejecutado anteriormente")
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
            If Logger IsNot Nothing Then
                If Logger.Configuracion.ESTATUS = 2 Then
                    Logger.SetEstatus(-1, ex.Message)
                End If

                If Logger.Intentos < Logger.Configuracion.MAXREINTENTOS Then
                    Logger.SetEstatus(1, Logger.Configuracion.FECHA_PROGRAMACION.AddMinutes(10))
                End If
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
    Public Function Reading(ByVal LiftMethod As Integer, ByVal FileUpload As Byte(), ByVal FileName As String) As Dictionary(Of String, String) Implements IModelo.Reading
        Dim modelo As New ModeloProsper.Modelo()
        Dim Result As New Dictionary(Of String, String) From {
                {"errors", ""},
                {"data", ""},
                {"mecanico", ""}
            }

        Try
            If ModeloProsper.Modelo.Dispose = False Then
                Throw New Exception("Open server ocupado")
            End If
            'Newtonsoft.Json.JsonConvert.SerializeObject(
            Dim Errors = modelo.Reading(LiftMethod, FileUpload, FileName)
            Result("errors") = Newtonsoft.Json.JsonConvert.SerializeObject(Errors)
            Result("data") = Newtonsoft.Json.JsonConvert.SerializeObject(modelo.ModPozo)
            Result("mecanico") = Newtonsoft.Json.JsonConvert.SerializeObject(modelo.ModTuberias)
            Return Result
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function

    Public Function Estabilidad(ByVal IdModPozo As String) As Boolean Implements IModelo.Estabilidad
        Dim db As New Entities_ModeloCI()
        Dim Conf As CONFIGURACION = db.CONFIGURACION.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        Dim ModEstabilidad As New ModeloProsper.Estabilidad(Conf)
        Dim result As Boolean
        Try
            result = ModEstabilidad.Execute()




            ModEstabilidad.Save()


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
            Dim db As New Entities_ModeloCI()
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


    Public Sub Scheduled()

    End Sub
    Public Function Condiciones() As Dictionary(Of String, List(Of String)) Implements IModelo.Condiciones
        Try
            Dim Modelo As New ModeloProsper.Modelo()
            Return Modelo.Condiciones()
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try



    End Function
    Public Function Monitor(ByRef OpenServer As String) As List(Of String) Implements IModelo.Monitor

        Dim Modelo = New ModeloProsper.Modelo()
        Try

            Return Modelo.Monitor(OpenServer)
        Catch ex As Exception
            Return New List(Of String) From {"Error: " + ex.Message}
        End Try











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
        Dim db As New Entities_ModeloCI()
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
        Dim db As New Entities_ModeloCI()
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
