Imports System.Resources
Imports System.IO
Imports System.Security.Permissions
Imports ModeloCI
'Lista de cambios - Por Bernardo Cauich
'===================================================================
'-La columna TRPRESS se migra de la tabla ModGeneral  a la tabla ModBNC
Public Class Modelo

    Private db As New Entities_ModeloCI()
    Private conexion As BaseDatosServidor
    Private MPrsp As Crea.Modelo
    Private MPrsu As UpdateModelo()
    Private IdLog As String
    Private Inicio As DateTime
    Private Fin As DateTime
    Private User As String
    Private Logger As Logger
    Private Environment As String = Settings.GetBy("enviroment")
    Property Messages As String
    Property Conected As Boolean
    Private ProgramPath As String = "C:\Program Files (x86)\Petroleum Experts\" + Settings.GetBy("prosper_version") + "\prosper.exe"
    Property ModPozo As MOD_POZO
    Private Config As CONFIGURACION_ADMINISTRADOR
    Private ModArchivo As ARCHIVOS_PROSPER

    Private Path As String = "C:\PVTs\Tmps\"
    Private DeleteFile As Boolean
    Private FilePvt As String = ""
    Private FileAnl As String = ""
    Private FileOut As String = ""
    Private FileSin As String = ""

    Private TimeLimitT As Integer = 30
    Private IPM As String = Settings.GetBy("prosper_version")

    Public Sub New()
        CreateFolder()
    End Sub
    Public Sub New(ByVal IdModPozo As String)
        ModPozo = db.MOD_POZO.Include("CONFIGURACION_ADMINISTRADOR").Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        If ModPozo Is Nothing Then
            Throw New Exception("No existe el modelo o fue borrado")
        End If


        ModArchivo = (From f In db.ARCHIVOS_PROSPER Where f.borradoLogico Is Nothing And f.idModPozo = IdModPozo And f.fecha = (db.ARCHIVOS_PROSPER.Where(Function(w) w.borradoLogico Is Nothing And w.idModPozo = IdModPozo).Max(Function(m) m.fecha))).SingleOrDefault()

        DeleteFile = IIf(Settings.GetBy("del_pvt") = "1", True, False)
        CreateFolder()


    End Sub
    Public Sub Reset(ByVal IdModPozo As String, ByVal Fecha As DateTime)
        Dim configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
        If configuracion IsNot Nothing Then
            Dim intentos = db.EJECUCION_PROCESOS.Where(Function(w) w.IDCONFIGURACION = configuracion.IDCONFIGURACION).ToList()

            configuracion.FECHA_PROGRAMACION = Fecha

            configuracion.ESTATUS = 0





            db.Entry(configuracion).State = Entity.EntityState.Modified
            db.SaveChanges()


        Else
            Throw New Exception("No existe la configuración o fue borrada")
        End If
    End Sub
    ''' <summary>
    ''' Regresar a la cola el modelo de pozo
    ''' </summary>
    ''' <param name="IdModPozo">Id del modelo pozo</param>
    ''' <param name="MaxIntentos">Máximo de intentos para la ejecucion de PROSPER</param>
    Public Sub Reset(ByVal IdModPozo As String, ByVal MaxIntentos As Integer)
        Try
            'Eliminar logs
            Dim configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            If configuracion IsNot Nothing Then

                configuracion.ESTATUS = 1

                Dim intentos = db.EJECUCION_PROCESOS.Where(Function(w) w.IDCONFIGURACION = configuracion.IDCONFIGURACION And w.ENDRECORD Is Nothing).ToList()

                If MaxIntentos > 0 Then


                    If intentos.Count > 0 Then
                        For Each e In intentos
                            e.ENDRECORD = DateTime.Now + ": " + e.IDEJECUCION
                            db.Entry(e).State = Entity.EntityState.Modified
                        Next
                    End If

                    db.SaveChanges()

                    configuracion.MAXREINTENTOS = MaxIntentos
                    db.Entry(configuracion).State = Entity.EntityState.Modified
                    db.SaveChanges()
                Else

                    'If (intentos.Count + 1) > configuracion.MAXREINTENTOS Then
                    '    configuracion.ESTATUS = 0
                    'Else
                    configuracion.FECHA_PROGRAMACION = configuracion.FECHA_PROGRAMACION.AddMinutes(5)
                    'End If



                    db.Entry(configuracion).State = Entity.EntityState.Modified
                    db.SaveChanges()

                End If
            Else
                Throw New Exception("No existe la configuración o fue borrada")
            End If

            'eliminar iprs
            Dim iprs = db.VLP_IPR.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            iprs.ForEach(Function(e) db.VLP_IPR.Remove(e))
            db.SaveChanges()

            'eliminar correlaciones
            Dim corrs = db.CORRELACION.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            corrs.ForEach(Function(e) db.CORRELACION.Remove(e))
            db.SaveChanges()

            'eliminar vlps
            Dim vlps = db.VLP_IPR_GASTO_INYECCION.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            vlps.ForEach(Function(e) db.VLP_IPR_GASTO_INYECCION.Remove(e))
            db.SaveChanges()

            'eliminar gasto de gas
            Dim gastos = db.COMPORTAMIENTO_GAS.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            gastos.ForEach(Function(e) db.COMPORTAMIENTO_GAS.Remove(e))
            db.SaveChanges()

            'eliminar Diagnosticos
            Dim diagnosticos = db.DIAGNOSTICOS.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            diagnosticos.ForEach(Function(e) db.DIAGNOSTICOS.Remove(e))
            db.SaveChanges()

            'eliminar wc
            Dim prods = db.PRODUCTIVIDAD.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            prods.ForEach(Function(e) db.PRODUCTIVIDAD.Remove(e))
            db.SaveChanges()

            'eliminar quicklook
            Dim quick = db.MOD_POZO_QUICK.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
            quick.ForEach(Function(e) db.MOD_POZO_QUICK.Remove(e))
            db.SaveChanges()

        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Sub
    Public Sub Delete(ByVal IdModPozo As String)
        Try

            Reset(IdModPozo, 1)

            Dim mod_configuracion = db.CONFIGURACION_ADMINISTRADOR.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
            If mod_configuracion Is Nothing Then
                Throw New Exception("El modelo no existe o fue borrado: " + IdModPozo)
            End If

            db.CONFIGURACION_ADMINISTRADOR.Remove(mod_configuracion)
            db.SaveChanges()
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try


    End Sub
    ''' <summary>
    ''' Actualizacion a traves de las condiciones de operación.
    ''' </summary>
    ''' <returns></returns>
    Public Function Update() As Boolean
        Try
            Dim Result As Boolean
            Dim ModGeneral = db.VW_EDO_GENERAL.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()
            Dim vw_mod_pozo = db.VW_MOD_POZO.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()
            Dim MPrsu As New UpdateModelo()

            MPrsu.Frecuencia_BEC = ModGeneral.FREC_BEC.GetValueOrDefault()
            MPrsu.THPD = ModGeneral.THPD.GetValueOrDefault()
            MPrsu.PTR = ModGeneral.TRPRES.GetValueOrDefault()
            MPrsu.Qginy = ModGeneral.GLRINY.GetValueOrDefault()
            MPrsu.Corte_Agua = ModGeneral.WC.GetValueOrDefault()


            If ModArchivo IsNot Nothing Then

                Dim Names = ModArchivo.nombreArchivo.Split("\".ToCharArray())
                Dim NameMaster As String = Names(Names.Length - 1)
                MPrsu.ArchivoPVT = "C:\PVTs\Tmps\" + NameMaster
                File.WriteAllBytes(MPrsu.ArchivoPVT, ModArchivo.archivo)
                FileOut = MPrsu.ArchivoPVT
            Else
                Throw New Exception("Falta el archivo .Out: " + MPrsu.ArchivoPVT)
            End If

            If MPrsu.Execute() Then

                'Actualizamos QuickLook
                '==================================================

                '*********Guardado de resultados generales****************/
                MPrsu.GralQuickLook.IDMODPOZO = ModPozo.IDMODPOZO
                db.MOD_POZO_QUICK.Add(MPrsu.GralQuickLook)
                db.SaveChanges()

                SaveGAS(ModPozo.IDMODPOZO, MPrsu)
                SaveDiag(ModPozo.IDMODPOZO, MPrsu)


                Result = True
            End If


            DeleteFiles()

            Return Result
        Catch ex As Exception


            DeleteFiles()
            Throw New Exception(ex.Message)
        End Try


    End Function
    Public Function Create() As Boolean



        Try

            Dim Result As Boolean

            Dim vw_mod_pozo = db.VW_MOD_POZO.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()


            If vw_mod_pozo Is Nothing Then
                Throw New Exception("El modelo no existe o fue borrado")
            End If



            Dim correlaciones = db.CAT_CORRELACION.Where(Function(w) w.ENDRECORD = Nothing).ToList()
            Dim mod_general = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = vw_mod_pozo.IDMODPOZO).SingleOrDefault()
            If mod_general Is Nothing Then
                Throw New Exception("Falta los datos de configuración: MOD_GENERAL")
            End If


            Dim edo_mecanico = db.VW_EDO_MECANICO.Where(Function(w) w.IDAGUJERO = vw_mod_pozo.IDAGUJERO).OrderBy(Function(o) o.ORDEN).ToList()
            Dim edo_trayectoria = db.MOD_POZO_TRAYEC.Where(Function(w) w.IDMODPOZO = vw_mod_pozo.IDMODPOZO).OrderBy(Function(o) o.PROFUNDIDADMD).ToList()
            Dim edo_temperatura = db.MOD_POZO_TEMP.Where(Function(w) w.IDMODPOZO = vw_mod_pozo.IDMODPOZO).OrderBy(Function(o) o.PROFUNDIDADMD).ToList()



            If ModArchivo IsNot Nothing AndAlso ModArchivo.equipment = True Then
                MPrsp = New Crea.Modelo()
            Else


                MPrsp = New Crea.Modelo(edo_mecanico.Count, edo_trayectoria.Count, edo_temperatura.Count)
            End If

            MPrsp.ProgramPath = ProgramPath ' IIf(Environment = "production", "C:\Program Files (x86)\Petroleum Experts\" + Settings.GetBy("prosper_version") + "\prosper.exe", "C:\Program Files (x86)\Petroleum Experts\IPM 7.5\prosper.EXE")  ' "C:\Program Files (x86)\Petroleum Experts\" + Settings.GetBy("prosper_version") + "\prosper.EXE" '' Settings.GetBy("prosper_version") Settings.GetBy("program_prosper")

            'Select Case mod_general.LIFTMETHOD
            '    Case 1
            '        MPrsp.ArchivoPVT = Settings.GetBy("pvt_bnc")
            '    Case 2
            '        MPrsp.ArchivoPVT = Settings.GetBy("pvt_bec")
            '    Case Else

            'End Select

            If ModArchivo IsNot Nothing Then
                Dim Paths = ModArchivo.nombreArchivo.Split("\".ToCharArray())
                Dim NameMaster As String = Paths(Paths.Length - 1)
                Dim Names = NameMaster.Split(".".ToCharArray())


                MPrsp.Equipment = ModArchivo.equipment.GetValueOrDefault()
                MPrsp.ArchivoPVT = "C:\PVTs\Tmps\" + NameMaster
                File.WriteAllBytes(MPrsp.ArchivoPVT, ModArchivo.archivo)

                FileAnl = Path + Names(0) + ".Anl"
                FilePvt = Path + Names(0) + ".Pvt"
                FileSin = Path + Names(0) + ".Sin"
                FileOut = Path + Names(0) + ".Out"

                If MPrsp.Equipment = False Then
                    MPrsp.SaveFile = True
                End If
            Else

                'Talvez se tenga que migrar  -->> If ModArchivo isnot nothing
                If MPrsp.ArchivoPVT = "" Or MPrsp.ArchivoPVT Is Nothing Then
                    Dim ModPvt = db.MOD_POZO_PVT.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()

                    If ModPvt Is Nothing Then
                        Throw New Exception("No hay datos en PVT")
                    End If
                    MPrsp.SaveFile = True
                    MPrsp.ArchivoPVT = "C:\PVTs\Tmps\" + vw_mod_pozo.POZO + ".Out"

                    FileOut = MPrsp.ArchivoPVT

                    Select Case IPM
                        Case "IPM 11"
                            'File.WriteAllBytes(MPrsp.ArchivoPVT, My.Resources.Generic_7_5)
                        Case "IPM 7.5"
                            File.WriteAllBytes(MPrsp.ArchivoPVT, My.Resources.Generic_7_5)
                    End Select



                    SetPvt(vw_mod_pozo.IDAGUJERO, ModPvt.IDPVTGENERAL)


                End If

                MPrsp.SaveFile = IIf(Settings.GetBy("save_pvt") = "1", True, False)


            End If


            LoadGeneral(mod_general, edo_mecanico, edo_trayectoria, edo_temperatura)
            MPrsp.Ayuda = False
            MPrsp.Diagnostico = False
            MPrsp.Rastreo = False


            If MPrsp.MsgErrDatos.Length = 0 Then


                'Dim InitTime As New Task(AddressOf TimeLimit(MPrsp))
                'Task.Run(Sub() TimeLimit(ModPozo, MPrsp))
                If MPrsp.Execute() Then

                    ''Depreciar los datos ya se guardan en el QuickLook
                    'If MPrsp.LiftMethod.Val = 2 Then
                    '    Dim mod_bec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = vw_mod_pozo.IDMODPOZO).SingleOrDefault()

                    '    mod_bec.Qpromedio = MPrsp.QPromedio
                    '    mod_bec.General = MPrsp.CGenerada

                    '    db.Entry(mod_bec).State = Entity.EntityState.Modified
                    '    db.SaveChanges()
                    'End If

                    SaveIPR(ModPozo.IDMODPOZO)
                    SaveCorrelacion(ModPozo.IDMODPOZO)
                    If MPrsp.FlagVlpIpr = 1 Then SaveVLP(ModPozo.IDMODPOZO)

                    If MPrsp.FlagQuickLook = 1 Then SaveDiag(ModPozo.IDMODPOZO)
                    If MPrsp.FlagSensWc = 1 Then SaveWC(ModPozo.IDMODPOZO)

                    'If MPrsp.LiftMethod.Val = 1 Then
                    SaveGAS(ModPozo.IDMODPOZO)
                    'End If
                    'If MPrsp.LiftMethod.Val = 1 Then
                    'SaveCarta(ModPozo.IDMODPOZO)
                    'End If

                    If ModArchivo Is Nothing Then
                        db.ARCHIVOS_PROSPER.Add(New ARCHIVOS_PROSPER() With {
                            .archivo = File.ReadAllBytes(MPrsp.ArchivoPVT),
                            .idModPozo = ModPozo.IDMODPOZO,
                            .idUsuario = ModPozo.CONFIGURACION_ADMINISTRADOR(0).IDUSUARIO,
                            .nombreArchivo = MPrsp.ArchivoPVT,
                            .equipment = False,
                            .fecha = DateTime.Now
                        })
                        db.SaveChanges()

                        ' File.Delete(MPrsp.ArchivoPVT)
                    Else
                        If MPrsp.SaveFile = True Then
                            ModArchivo.archivo = File.ReadAllBytes(MPrsp.ArchivoPVT)

                        End If
                        ModArchivo.equipment = False
                        db.Entry(ModArchivo).State = Entity.EntityState.Modified
                        db.SaveChanges()


                        If MPrsp.Equipment Then
                            Task.Run(Sub() SaveEquipment(ModPozo.IDMODPOZO))
                        End If





                    End If


                    Result = True
                End If

            Else
                Throw New Exception(MPrsp.MsgErrDatos)

            End If



            'Borrado de todos los archivos generados por PROSPER
            '====================================================================================

            DeleteFiles()

            Return Result


        Catch ex As Exception
            DeleteFiles()
            Throw New Exception("Execute: " + ex.Message)
        End Try

    End Function
    Private Sub DeleteFiles()
        If DeleteFile Then
            If File.Exists(FileOut) Then
                File.Delete(FileOut)
            End If
            If File.Exists(FilePvt) Then
                File.Delete(FilePvt)
            End If
            If File.Exists(FileAnl) Then
                File.Delete(FileAnl)
            End If
            If File.Exists(FileSin) Then
                File.Delete(FileSin)
            End If
        End If
    End Sub
    Public Function Reading(ByVal FileB As Byte(), ByVal FileName As String) As List(Of String)
        Try
            Dim Paths = FileName.Split("\".ToCharArray())
            Dim NameMaster As String = Paths(Paths.Length - 1)
            Dim Names = NameMaster.Split(".".ToCharArray())



            MPrsp = New Crea.Modelo()

            'MPrsp.SaveEquip = ModArchivo.equipment.GetValueOrDefault()
            MPrsp.Equipment = True
            MPrsp.SaveFile = False
            MPrsp.Reading = True
            MPrsp.ArchivoPVT = "C:\PVTs\Tmps\" + NameMaster

            If File.Exists(MPrsp.ArchivoPVT) = False Then
                File.WriteAllBytes(MPrsp.ArchivoPVT, FileB)
            End If

            MPrsp.Execute()
            MPrsp.Validating()



            If DeleteFile = False Then
                MPrsp.ArchivoPVT = ""
            End If

            If File.Exists(MPrsp.ArchivoPVT) Then
                File.Delete(MPrsp.ArchivoPVT)
            End If

            Return MPrsp.Errors


        Catch ex As Exception
            If DeleteFile = False Then
                MPrsp.ArchivoPVT = ""
            End If
            If File.Exists(MPrsp.ArchivoPVT) Then
                File.Delete(MPrsp.ArchivoPVT)
            End If
            Throw New Exception(ex.Message)
        End Try



        Return MPrsp.Errors
    End Function
    'Depreciado temporalmente al parecer no sirve
    Public Function Sensibilidad_BN(ByVal IdModPozo As String, ByVal Archivo As FileStream) As Boolean

        Dim PathFile As String = ""
        Try
            Dim ModPozo = db.VW_MOD_POZO.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()
            If ModPozo Is Nothing Then
                Throw New Exception("No existe el modelo o fue borrado")
            End If

            Dim NameMaster As String = Archivo.Name
            PathFile = "C:/PVTs/Tmps/" + NameMaster
            ' Dim Bits As Byte = File.ReadAllBytes()

            'File.WriteAllBytes(PathFile)

            File.Delete(PathFile)
            Return True
        Catch ex As Exception

            If File.Exists(PathFile) Then
                File.Delete(PathFile)
            End If

            Throw New Exception(ex.Message)
        End Try


    End Function
    Public Function GetServer() As Object
        Return MPrsp.Server
    End Function

    Private Function SetPvt(ByVal IdAgujero As String, ByVal IdPvtGeneral As String) As Boolean
        Try
            Dim Formacion = db.VW_PVT.Where(Function(w) w.IDAGUJERO = IdAgujero And w.IDPVTGENERAL = IdPvtGeneral).SingleOrDefault()

            If Formacion Is Nothing Then
                Throw New Exception("PVT: No hay datos de formación")
            End If

            If Formacion.SALINIDAD < 1 Or Formacion.SALINIDAD Is Nothing Then
                Throw New Exception("PVT: No hay datos de Salinidad")
            End If

            'Carga de PVT
            MPrsp.Pvt = New Pvt()

            MPrsp.Pvt.GOR = Formacion.GOR
            MPrsp.Pvt.API = Formacion.API
            MPrsp.Pvt.Drg = Formacion.DRG
            MPrsp.Pvt.Salinidad = Formacion.SALINIDAD
            MPrsp.Pvt.Tpvt = Formacion.TEMP

            Dim PSat As Double = 0

            Dim Pvts = db.VW_PVT_GRAFICA.Where(Function(w) w.IDPVTGENERAL = Formacion.IDPVTGENERAL And w.TEMPERATURA = Formacion.TEMP).OrderBy(Function(w) w.PPRUEBA).ToList()

            If Pvts.Count = 0 Then
                Throw New Exception("PVT: No hay registros de PVT")
            End If

            ReDim MPrsp.Pvt.Pprueba(Pvts.Count - 1)
            ReDim MPrsp.Pvt.Rs(Pvts.Count - 1)
            ReDim MPrsp.Pvt.Bo(Pvts.Count - 1)
            ReDim MPrsp.Pvt.Muo(Pvts.Count - 1)

            For i = 0 To Pvts.Count - 1
                MPrsp.Pvt.Pprueba(i) = Pvts(i).PPRUEBA
                MPrsp.Pvt.Rs(i) = Pvts(i).RS
                MPrsp.Pvt.Bo(i) = Pvts(i).BO
                MPrsp.Pvt.Muo(i) = Pvts(i).VISCOSIDADACEITE

                PSat += Pvts(i).PB 'CAMBIAR POR PB
            Next


            MPrsp.Pvt.Psat = PSat / Pvts.Count

            'MPrsp.Pvt.Execute()


            Return True
        Catch ex As Exception
            Throw New Exception("SetPVT: " + ex.Message)
        End Try
    End Function
    Private Sub SaveEquipment(ByVal IdModPozo As String)




        Dim TipoTuberias = db.CAT_TIPO_TUBERIA.ToDictionary(Function(d) d.NUMERO, Function(d) d.IDTIPOTUBERIA)


        'Borrado de equipamiento 
        '================================================================================
        Dim Tubs = db.MOD_POZO_TUBERIA.Where(Function(w) w.IDAGUJERO = ModPozo.IDAGUJERO).ToList()
        Dim Trays = db.MOD_POZO_TRAYEC.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
        Dim Temps = db.MOD_POZO_TEMP.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()

        Tubs.ForEach(Function(e) db.MOD_POZO_TUBERIA.Remove(e))
        db.SaveChanges()

        Trays.ForEach(Function(e) db.MOD_POZO_TRAYEC.Remove(e))
        db.SaveChanges()

        Temps.ForEach(Function(e) db.MOD_POZO_TEMP.Remove(e))
        db.SaveChanges()



        'Insercion del nuevo equipamiento
        '================================================================================

        Try

            For I = 0 To MPrsp.NumDatEdoMec.Val - 1

                If MPrsp.Label(I) IsNot Nothing Then
                    db.MOD_POZO_TUBERIA.Add(New MOD_POZO_TUBERIA() With {
                     .IDAGUJERO = ModPozo.IDAGUJERO,
                     .IDMODPOZOTUBERIA = Guid.NewGuid().ToString().ToUpper(),
                     .ETIQUETA = MPrsp.Label(I),
                     .IDTIPOTUBERIA = TipoTuberias(MPrsp.DType.Val(I)),
                     .ORDEN = I,
                     .MD = Double.Parse(MPrsp.Depth.Val(I)),
                     .TIDIAM = Double.Parse(MPrsp.TID.Val(I)),
                     .TIROUG = Double.Parse(MPrsp.TIR.Val(I)),
                     .TODIAM = Double.Parse(MPrsp.TOD.Val(I)),
                     .TOROUG = Double.Parse(MPrsp.TOR.Val(I)),
                     .CIDIAM = Double.Parse(MPrsp.CID.Val(I)),
                     .CIROUG = Double.Parse(MPrsp.CIR.Val(I))
                    })
                End If


            Next
            db.SaveChanges()


            'MPrsp.RDEnable.Val(I) = 1


            For I = 0 To MPrsp.NumDatTrayecto.Val - 1
                db.MOD_POZO_TRAYEC.Add(New MOD_POZO_TRAYEC() With {
                    .IDMODPOZO = IdModPozo,
                    .IDMODPOZOTRAYEC = Guid.NewGuid().ToString().ToUpper(),
                    .PROFUNDIDADMD = Double.Parse(MPrsp.RDMd.Val(I)),
                    .PROFUNDIDADMV = Double.Parse(MPrsp.RDTvd.Val(I))
                })
            Next I
            db.SaveChanges()

            For I = 0 To MPrsp.NumDatTemp.Val - 1
                db.MOD_POZO_TEMP.Add(New MOD_POZO_TEMP() With {
                    .IDMODPOZO = IdModPozo,
                    .IDMODPOZOTEMP = Guid.NewGuid().ToString().ToUpper(),
                    .PROFUNDIDADMD = Double.Parse(MPrsp.PTMd.Val(I)),
                    .TEMPERATURA = Double.Parse(MPrsp.PTTmp.Val(I))
                })
            Next I
            db.SaveChanges()


            Dim ModGeneral = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            ModGeneral.PTEST = MPrsp.Ptest.Val
            ModGeneral.PRES = MPrsp.PRes.Val
            ' ModGeneral.CO2 = MPrsp.CO2.Val
            ModGeneral.COMENTA = MPrsp.Comenta
            ModGeneral.COMPLETION = MPrsp.Completion.Val
            'ModGeneral.DIAMVALBNC = MPrsp.DiamValBNC.Val
            ModGeneral.DIETZ = MPrsp.Dietz.Val
            ModGeneral.DRAINAGE = MPrsp.Drainage.Val
            ModGeneral.EMULSION = MPrsp.Emulsion.Val
            'ModGeneral.ENTRY = MPrsp.Entry.Val
            ModGeneral.FLOWTYPE = MPrsp.FlowType.Val
            ModGeneral.FLUID = MPrsp.Fluid.Val
            ModGeneral.GASCONING = MPrsp.GasConing.Val
            'ModGeneral.GLRATE = MPrsp.GLRate.Val
            ' ModGeneral.GLRINY = MPrsp.GLRiny.Val
            ModGeneral.GRAVELPACK = MPrsp.GravelPack.Val
            'ModGeneral.GRAVITY = MPrsp.Gravity.Val
            'ModGeneral.H2S = MPrsp.H2S.Val
            ModGeneral.HTC = MPrsp.Htc.Val
            ModGeneral.HYDRATE = MPrsp.Hydrate.Val
            ModGeneral.IPRMETHOD = MPrsp.IPRMethod.Val
            ModGeneral.IRElK = MPrsp.IRELK.Val
            ModGeneral.LIFTMETHOD = MPrsp.LiftMethod.Val
            ModGeneral.LIFTYPE = MPrsp.LiftType.Val
            'ModGeneral.METHOD = MPrsp.Method.Val
            ModGeneral.MGSKINMETHOD = MPrsp.MGSkinMethod.Val
            ModGeneral.QTEST = MPrsp.QTest.Val
            ModGeneral.WELLTYPE = MPrsp.WellType.Val
            ModGeneral.WELL = MPrsp.Well
            ModGeneral.WC = MPrsp.Wc.Val
            ModGeneral.WBR = MPrsp.WBR.Val
            ModGeneral.WATVIS = MPrsp.WatVis.Val
            ModGeneral.VISMOD = MPrsp.VisMod.Val
            ModGeneral.VALVEDEPTH = MPrsp.ValveDepth.Val
            ModGeneral.TRES = MPrsp.TRes.Val
            ModGeneral.RGATOTALAFORO = MPrsp.RGA_Aforo.Val
            ModGeneral.TOTGOR = MPrsp.TotGor.Val
            ModGeneral.THPD = MPrsp.THPres.Val
            ModGeneral.THTD = MPrsp.THTemp.Val
            ModGeneral.QG = MPrsp.Qg.Val
            ModGeneral.QO = MPrsp.Qo.Val
            ModGeneral.QW = MPrsp.Qw.Val
            ModGeneral.PREDICT = MPrsp.Predict.Val


            db.Entry(ModGeneral).State = Entity.EntityState.Modified
            db.SaveChanges()

            Select Case MPrsp.LiftMethod.Val
                Case 1
                    Dim ModBNC = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

                    If ModBNC Is Nothing Then
                        Throw New Exception("No existe la tabla ModBNC para los datos en modo lectura del archivo (.Out)")
                    End If

                    ModBNC.CO2 = MPrsp.CO2.Val
                    ModBNC.DIAMVAL = MPrsp.DiamValBNC.Val
                    ModBNC.ENTRY = MPrsp.Entry.Val
                    ModBNC.GLRATE = MPrsp.GLRate.Val
                    ModBNC.GLRINY = MPrsp.GLRiny.Val
                    ModBNC.GRAVITY = MPrsp.Gravity.Val
                    ModBNC.H2S = MPrsp.H2S.Val
                    ModBNC.METHOD = MPrsp.Method.Val
                    ModBNC.N2 = MPrsp.N2.Val
                    ModBNC.QGIMAX = MPrsp.QgiMax.Val
                    ModBNC.QGIMIN = MPrsp.QgiMin.Val
                    ModBNC.RANGESYSTEM = MPrsp.RangeSystem.Val
                    ModBNC.TRPRES = MPrsp.TRPres.Val
                    ModBNC.VALVEDEPTH = MPrsp.ValveDepth.Val


                    db.Entry(ModBNC).State = Entity.EntityState.Modified
                    db.SaveChanges()
                Case 2
                    Dim ModBec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

                    If ModBec Is Nothing Then
                        Throw New Exception("No existe la tabla ModBNC para los datos en modo lectura del archivo (.Out)")
                    End If

                    ModBec.FRECMIN = MPrsp.FrecMin.Val
                    ModBec.FRECMAX = MPrsp.FrecMax.Val
                    ModBec.PROF_BEC = MPrsp.Prof_BEC.Val
                    ModBec.FREC_BEC = MPrsp.Frec_BEC.Val
                    ModBec.ODMAX_BEC = MPrsp.ODMax_BEC.Val
                    ModBec.LONGCABLE_BEC = MPrsp.LongCable_BEC.Val
                    ModBec.EFISEPGAS_BEC = MPrsp.EfiSepGas_BEC.Val
                    ModBec.ETAPAS_BEC = MPrsp.Etapas_BEC.Val
                    ModBec.VOLTSUP_BEC = MPrsp.VoltSup_BEC.Val
                    ModBec.DESGASTE_BEC = MPrsp.Desgaste_BEC.Val
                    ModBec.REDUCGAS_BEC = MPrsp.ReducGas_BEC.Val
                    ModBec.BOMBA_BEC = MPrsp.Bomba_BEC.Val
                    ModBec.MOTOR_BEC = MPrsp.Motor_BEC.Val
                    ModBec.POTENCIAMOTOR_BEC = MPrsp.PotenciaMotor_BEC.Val
                    ModBec.CABLE_BEC = MPrsp.Cable_BEC.Val
                    ModBec.PRESUC_BEC = MPrsp.PreSuc_BEC.Val
                    ModBec.CORRIENTE_BEC = MPrsp.Corriente_BEC.Val
                    ModBec.POTENCIA_BEC = MPrsp.Potencia_BEC.Val
                    ModBec.PREDES_BEC = MPrsp.PreDes_BEC.Val
                    ModBec.Qpromedio = MPrsp.PumpRate.Val
                    ModBec.General = MPrsp.PumpHead.Val



            End Select
            'Guardamos PVT
            '===================================================
            Dim Pvt = db.MOD_POZO_PVT.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

            If Pvt Is Nothing Then
                Pvt = New MOD_POZO_PVT() With
                {
                    .IDMODPOZO = IdModPozo,
                    .CREATED_ON = DateTime.Now,
                    .IDMODPOZOPVT = Guid.NewGuid().ToString().ToUpper(),
                    .API = MPrsp.Pvt.API,
                    .CO2 = MPrsp.Pvt.CO2,
                    .DRG = MPrsp.Pvt.Drg,
                    .GOR = MPrsp.Pvt.GOR,
                    .H2S = MPrsp.Pvt.H2S,
                    .N2 = MPrsp.Pvt.N2,
                    .WSAL = MPrsp.Pvt.Salinidad,
                    .IDPVTGENERAL = "NA"
                }
                db.MOD_POZO_PVT.Add(Pvt)
            Else
                db.Entry(Pvt).State = Entity.EntityState.Modified

            End If
            db.SaveChanges()



        Catch ex As Exception
            Throw New Exception("SaveEquipment: " + ex.Message)
        End Try

    End Sub

    Public Function Sensibilidad_BN(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal QGI_Min As Double, ByVal QGI_Max As Double, ByVal FileB As Byte(), ByVal FileName As String) As Double(,)
        Dim Result(,) As Double

        Dim Names = FileName.Split("\".ToCharArray())
        Dim NameMaster As String = Names(Names.Length - 1)
        Dim PathFile As String = "C:\PVTs\Tmps\" + NameMaster



        Try
            If File.Exists(PathFile) = False Then
                File.WriteAllBytes(PathFile, FileB)
            End If

            Dim Modelo As New ModeloProsper.Crea.Modelo()
            Modelo.ProgramPath = ProgramPath
            Modelo.ArchivoPVT = PathFile '"C:/PVTs/Tmps/" + NameMaster


            Modelo.THPres.Val = Pwh
            Modelo.Wc.Val = WC
            Modelo.GLRiny.Val = Qgi


            Dim Oper = Modelo.Sensibilidad_BN(QGI_Min, QGI_Max)
            If Oper AndAlso Modelo.QliqVec.Count() > 0 Then
                ReDim Result(Modelo.QliqVec.Count() - 1, 2)
                For i = 0 To Modelo.QliqVec.Count() - 1

                    Result(i, 0) = Math.Round(Modelo.QgiVec(i), 2)
                    Result(i, 1) = Math.Round(Modelo.QliqVec(i), 2)

                Next i

            Else
                Throw New Exception("Al parecer no trajo resultados la operación")
            End If

            PathFile = IIf(Settings.GetBy("del_pvt") = 1, PathFile, "")

            If File.Exists(PathFile) Then
                File.Delete(PathFile)
            End If
            Return Result
        Catch ex As Exception
            If File.Exists(PathFile) Then
                File.Delete(PathFile)
            End If
            Throw New Exception(ex.Message)
        End Try
    End Function

    Public Function Sensibilidad_BN() As Boolean
        Dim PathFile As String = ""
        Dim Result(,) As Double

        Try

            Dim ModGeneral = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()
            Dim ModBnc = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()


            If ModPozo.CONFIGURACION_ADMINISTRADOR.Count = 0 Then
                Throw New Exception("No existe la configuración del modelo")
            End If
            If ModGeneral Is Nothing Then
                Throw New Exception("No existe los datos generales del modelo")
            End If
            If ModBnc Is Nothing Then
                Throw New Exception("No existe los datos del modelo BNC")
            End If

            If ModPozo.CONFIGURACION_ADMINISTRADOR.SingleOrDefault().ESTATUS.GetValueOrDefault() <> 1 Then
                Throw New Exception("No es posible ejecutar este modelo: Estado " + ModPozo.CONFIGURACION_ADMINISTRADOR.SingleOrDefault().ESTATUS.GetValueOrDefault())
            End If

            'If PxServer.Count() > 0 Then
            '    Throw New Exception("Open Server ocupado")
            'End If



            'Tomamos el ultimo archivo (Verificar por la nueva implementacion del ModArchivo en el constructor)
            '==================================================================================================
            'Dim Archivo = (From Fil In db.ARCHIVOS_PROSPER Where Fil.idModPozo = ModPozo.IDMODPOZO And Fil.fecha = (db.ARCHIVOS_PROSPER.Where(Function(w) w.idModPozo = ModPozo.IDMODPOZO).Max(Function(m) m.fecha))).SingleOrDefault() ' db.ARCHIVOS_PROSPER.Where(Function(w) w.idModPozo = IdModPozo).SingleOrDefault()

            If ModArchivo Is Nothing Then
                Throw New Exception("Para esta operación es necesario el registro del Archivo PVT(*.OUT)")
            End If

            Dim Names = ModArchivo.nombreArchivo.Split("\".ToCharArray())
            Dim NameMaster As String = Names(Names.Length - 1)
            PathFile = "C:\PVTs\Tmps\" + NameMaster

            Dim Bytes() As Byte = ModArchivo.archivo

            File.WriteAllBytes(PathFile, Bytes)

            ''Empezamos con la operacion PROSPER
            ''===============================================================

            Dim Modelo As New ModeloProsper.Crea.Modelo()
            Modelo.ProgramPath = ProgramPath
            Modelo.ArchivoPVT = PathFile '"C:/PVTs/Tmps/" + NameMaster

            'ModeloGas.GOR_Total = GOR
            Modelo.THPres.Val = ModGeneral.THPD
            Modelo.Wc.Val = ModGeneral.WC
            Modelo.GLRiny.Val = ModBnc.GLRINY 'Aca va GLRINY


            Dim Oper = Modelo.Sensibilidad_BN(ModBnc.QGIMIN, ModBnc.QGIMAX)





            If Oper AndAlso Modelo.QliqVec.Count() > 0 Then
                'Eliminamos resultados
                Dim Gastos = db.COMPORTAMIENTO_GAS.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).ToList()

                If Gastos IsNot Nothing Then
                    Gastos.ForEach(Function(e) db.COMPORTAMIENTO_GAS.Remove(e))
                    db.SaveChanges()
                End If


                Dim gasto As New COMPORTAMIENTO_GAS() With {
                          .IDMODPOZO = ModPozo.IDMODPOZO,
                          .TITULO = "Qgi",
                          .MIN = ModBnc.QGIMIN,
                          .MAX = ModBnc.QGIMAX
                }

                ReDim Result(1, Modelo.QliqVec.Count() - 1)

                db.COMPORTAMIENTO_GAS.Add(gasto)
                db.SaveChanges()

                For i = 0 To Modelo.QliqVec.Count() - 1
                    Dim item = New COMPORTAMIENTO_GAS_DETALLES() With {
                        .IDCOMPORTAMIENTOGAS = gasto.IDCOMPORTAMIENTOGAS,
                        .XAUX = Math.Round(Modelo.QgiVec(i), 2),
                        .YAUX = Math.Round(Modelo.QliqVec(i), 2)
                    }
                    db.COMPORTAMIENTO_GAS_DETALLES.Add(item)
                    db.SaveChanges()
                    ' Result(0, i) = Math.Round(Modelo.QgiVec(i), 2)
                    'Result(1, i) = Math.Round(Modelo.QliqVec(i), 2)

                Next i






            Else

                Throw New Exception("Al parecer no trajo resultados la operación")
            End If


            PathFile = IIf(Settings.GetBy("del_pvt") = "1", PathFile, "")

            If File.Exists(PathFile) Then
                File.Delete(PathFile)
            End If

            Return True

        Catch ex As Exception
            If File.Exists(PathFile) Then
                File.Delete(PathFile)
            End If

            Throw New Exception(ex.Message)
        End Try
    End Function

    Private Sub SaveIPR(ByVal IdModPozo As String)
        'IPR

        Try
            Dim Vlp As New VLP_IPR() With {
                     .IDVLPIPR = Guid.NewGuid().ToString().ToUpper(),
                     .IDMODPOZO = IdModPozo,
                     .TITULO1 = MPrsp.NomVLPIPR(0),
                     .TITULO2 = MPrsp.NomVLPIPR(1)
                                 }
            db.VLP_IPR.Add(Vlp)
            db.SaveChanges()


            For i = 0 To 19 ' MPrsp.VLP_PWF.GetUpperBound(0)
                db.VLP_IPR_DETALLE.Add(New VLP_IPR_DETALLE() With {
                    .IDVLPIPRDETALLE = Guid.NewGuid().ToString().ToUpper(),
                    .IDVLPIPR = Vlp.IDVLPIPR,
                    .IPR_RTEL = MPrsp.IPR_RTEL(i),
                    .IPR_PWF = MPrsp.IPR_PWF(i),
                    .VLP_RTEL = MPrsp.VLPIPR_RTEL(0, i),
                    .VLP_PWF = MPrsp.VLPIPR_PWF(0, i)
                })
            Next i
            db.SaveChanges()


        Catch ex As Exception
            Throw New Exception("SaveIPR: " + ex.Message)
        End Try


    End Sub
    Private Sub SaveCorrelacion(ByVal IdModPozo As String)
        Dim cat_correlaciones = db.CAT_CORRELACION.Where(Function(w) w.ENDRECORD Is Nothing).OrderBy(Function(o) o.NUM).ToDictionary(Function(d) d.NUM, Function(d) d.IDCATCORRELACION)

        Try

            Dim NdatAux As Integer
            For i = 0 To MPrsp.NumCorr.Val - 1 '1 to 9
                NdatAux = MPrsp.NumDatCorr(MPrsp.NumCor(i)) - 1
                Dim Xaux(NdatAux + 1), Yaux(NdatAux + 1) As Double

                If cat_correlaciones.ContainsKey(MPrsp.NumCor(i)) Then
                    Dim correlacion As New CORRELACION() With {
                   .IDCATCORRELACION = cat_correlaciones.Values(MPrsp.NumCor(i)),
                   .IDCORRELACION = Guid.NewGuid().ToString().ToUpper(),
                   .IDMODPOZO = IdModPozo
                    }
                    db.CORRELACION.Add(correlacion)
                    db.SaveChanges()


                    'Guardamos correlaciones generales
                    For j = 0 To MPrsp.NumDatCorr(MPrsp.NumCor(i))
                        ' PresWf, ProfDesa
                        db.CORRELACION_GENERAL.Add(New CORRELACION_GENERAL() With {
                            .IDCORRELACIONGENERAL = Guid.NewGuid().ToString().ToUpper(),
                            .IDCORRELACION = correlacion.IDCORRELACION,
                            .PROFMD = MPrsp.ProfDesa(MPrsp.NumCor(i), j),
                            .PRES = MPrsp.PresWf(MPrsp.NumCor(i), j)
                        })


                    Next
                    db.SaveChanges()
                End If


            Next i
        Catch ex As Exception
            Throw New Exception("SaveCorrelacion: " + ex.Message)
        End Try

    End Sub
    Private Sub SaveVLP(ByVal IdModPozo As String)
        Dim NumGraf As Integer = 4
        Dim NumDat As Integer = 20


        Dim NdatAux As Integer
        NdatAux = NumDat - 1
        Dim Xaux(NdatAux), Yaux(NdatAux) As Double
        Try
            Select Case MPrsp.LiftMethod.Val
                Case 1
                    '<!-- Inicia guardado del IPR
                    Dim vlp As New VLP_IPR_GASTO_INYECCION() With {
                            .IDMODPOZO = IdModPozo,
                            .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
                            .TITULO = MPrsp.NomVLPIPR(1),
                            .IS_VLP = 1
                    }
                    db.VLP_IPR_GASTO_INYECCION.Add(vlp)
                    db.SaveChanges()


                    For J = 0 To NdatAux
                        db.VLP_IPR_GASTO_DETALLE.Add(New VLP_IPR_GASTO_DETALLE() With {
                                    .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
                                    .IDVLPIPRGASTOINYECCION = vlp.IDVLPIPRGASTOINYECCION,
                                    .XAUX = MPrsp.IPR_RTEL(J),'Revisar
                                    .YAUX = MPrsp.IPR_PWF(J) ' Revisar            
                        })

                        db.SaveChanges()
                    Next
                    'Termina guardado IPR-->

                    'Guardar frecuencias
                    For i = 0 To 9

                        'Insercion VLP
                        Dim vlp_inyeccion As New VLP_IPR_GASTO_INYECCION() With {
                            .IDMODPOZO = IdModPozo,
                            .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
                            .TITULO = Format(MPrsp.QgiVec(i), "0.00"),
                            .IS_VLP = 0
                        }
                        db.VLP_IPR_GASTO_INYECCION.Add(vlp_inyeccion)
                        db.SaveChanges()


                        For j = 0 To NdatAux '19 lugares
                            Dim vlp_detalles As New VLP_IPR_GASTO_DETALLE() With {
                                    .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
                                    .IDVLPIPRGASTOINYECCION = vlp_inyeccion.IDVLPIPRGASTOINYECCION,
                                    .XAUX = MPrsp.QliqVLP(i, j),'Revisar
                                    .YAUX = MPrsp.PwfVLP(i, j) ' Revisar            
                            }

                            db.VLP_IPR_GASTO_DETALLE.Add(vlp_detalles)
                            db.SaveChanges()
                        Next j




                    Next i
                Case 2
                    'Guardar vlp gasto inyeccion
                    Dim vlp As New VLP_IPR_GASTO_INYECCION() With {
                        .IDMODPOZO = IdModPozo,
                        .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
                        .TITULO = MPrsp.NomVLPIPR(0),
                        .IS_VLP = 1
                    }
                    db.VLP_IPR_GASTO_INYECCION.Add(vlp)
                    db.SaveChanges()
                    For j = 0 To NdatAux
                        'Xaux(j) = MPrsp.QliqVLP(1, j)
                        'Yaux(j) = MPrsp.PwfVLP(1, j)

                        db.VLP_IPR_GASTO_DETALLE.Add(New VLP_IPR_GASTO_DETALLE() With {
                                    .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
                                    .IDVLPIPRGASTOINYECCION = vlp.IDVLPIPRGASTOINYECCION,
                                    .XAUX = MPrsp.QliqVLP(1, j),'Revisar
                                    .YAUX = MPrsp.PwfVLP(1, j) ' Revisar            
                        })

                        db.SaveChanges()
                    Next


                    'Guardar frecuencias
                    For i = 0 To 9

                        'Insercion VLP
                        Dim vlp_inyeccion As New VLP_IPR_GASTO_INYECCION() With {
                            .IDMODPOZO = IdModPozo,
                            .IDVLPIPRGASTOINYECCION = Guid.NewGuid().ToString().ToUpper(),
                            .TITULO = Format(MPrsp.FrecVec(i), "0.00"),
                            .IS_VLP = 0
                        }
                        db.VLP_IPR_GASTO_INYECCION.Add(vlp_inyeccion)
                        db.SaveChanges()


                        For j = 0 To NdatAux '19 lugares
                            Dim vlp_detalles As New VLP_IPR_GASTO_DETALLE() With {
                                    .IDVLPIPRGASTODETALLE = Guid.NewGuid().ToString().ToUpper(),
                                    .IDVLPIPRGASTOINYECCION = vlp_inyeccion.IDVLPIPRGASTOINYECCION,
                                    .XAUX = MPrsp.QliqVLP(i, j),'Revisar
                                    .YAUX = MPrsp.Pdescarga_BEC(i, j) ' Revisar            
                            }

                            db.VLP_IPR_GASTO_DETALLE.Add(vlp_detalles)
                            db.SaveChanges()
                        Next j




                    Next i
                    'Guardar vlp gasto inyeccion detalles
                    'For i = 0 To 9
                    '    For j = 0 To NdatAux
                    '        Xaux(j) = MPrsp.QliqVLP(i, j)
                    '        Yaux(j) = MPrsp.Pdescarga_BEC(i, j)
                    '    Next
                    'Next i
            End Select
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Private Sub SaveCarta(ByVal IdModPozo As String)

    End Sub

    Private Sub SaveGAS(ByVal IdModPozo As String, ByVal MPrsu As UpdateModelo)
        Try
            Select Case MPrsu.LiftMethod
                Case 1


                    Dim gasto As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Qgi"
                    }



                    For i = 0 To MPrsu.QLiq.GetUpperBound(1)



                        gasto.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                            .XAUX = MPrsu.QLiq(0, i),
                            .YAUX = MPrsu.QLiq(1, i)
                        })


                    Next i

                    db.COMPORTAMIENTO_GAS.Add(gasto)
                    db.SaveChanges()

                Case 2

                    'Dim xHz As Double()
                    'Dim yHz As Double()
                    Dim titulo As String = ""
                    For j = 0 To MPrsu.CartaX.GetUpperBound(0)
                        '    ReDim xHz(99), yHz(99)

                        Select Case j
                            Case 0 '40 Hz
                                titulo = "35 Hz"
                            Case 1 '50 Hz
                                titulo = "40 Hz"
                            Case 2 '60 Hz
                                titulo = "50 Hz"
                            Case 3 '70 Hz
                                titulo = "60 Hz"
                            Case 4 '70 Hz
                                titulo = "70 Hz"
                            Case 5 '70 Hz
                                titulo = "75 Hz"
                            Case Else
                                titulo = "ND"

                        End Select

                        Dim gasto As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = titulo
                        }

                        For i = 0 To MPrsu.CartaX.GetUpperBound(1)
                            gasto.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                                .XAUX = MPrsu.CartaX(j, i),
                                .YAUX = MPrsu.CartaY(j, i)
                            })
                        Next i

                        db.COMPORTAMIENTO_GAS.Add(gasto)
                        db.SaveChanges()


                    Next j

                    ''=========rango minimo==============
                    Dim rango_min As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Rango Mínimo"
                    }

                    For i = 0 To MPrsu.RangeX.GetUpperBound(1)
                        rango_min.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                        .IDCOMPORTAMIENTOGAS = rango_min.IDCOMPORTAMIENTOGAS,
                        .XAUX = MPrsu.RangeX(0, i),
                        .YAUX = MPrsu.RangeY(0, i)
                      })
                    Next i
                    db.COMPORTAMIENTO_GAS.Add(rango_min)
                    db.SaveChanges()
                    '=========rango maximo==============
                    Dim rango_max As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Rango Máximo"
                    }
                    For i = 0 To MPrsu.RangeX.GetUpperBound(1)
                        rango_max.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                               .IDCOMPORTAMIENTOGAS = rango_max.IDCOMPORTAMIENTOGAS,
                               .XAUX = MPrsu.RangeX(1, i),
                               .YAUX = MPrsu.RangeY(1, i)
                        })

                    Next i
                    db.COMPORTAMIENTO_GAS.Add(rango_max)
                    db.SaveChanges()
                    '=========Eficiencia==============
                    Dim eficiencia As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Mejor Eficiencia"
                    }

                    For i = 0 To MPrsu.RangeX.GetUpperBound(1)
                        eficiencia.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                               .IDCOMPORTAMIENTOGAS = eficiencia.IDCOMPORTAMIENTOGAS,
                               .XAUX = MPrsu.RangeX(2, i),
                               .YAUX = MPrsu.RangeY(2, i)
                        })
                    Next i
                    db.COMPORTAMIENTO_GAS.Add(eficiencia)
                    db.SaveChanges()

            End Select

        Catch ex As Exception
            Throw New Exception("SaveGAS: " + ex.Message)
        End Try
    End Sub
    Private Sub SaveGAS(ByVal IdModPozo As String)
        Try
            Select Case MPrsp.LiftMethod.Val
                Case 1


                    Dim gasto As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Qgi"
                    }

                    db.COMPORTAMIENTO_GAS.Add(gasto)
                    db.SaveChanges()

                    For i = 0 To MPrsp.QgiVec.Count() - 1

                        If MPrsp.QliqVec(i) > 0 Then

                            db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                            .IDCOMPORTAMIENTOGAS = gasto.IDCOMPORTAMIENTOGAS,
                            .XAUX = MPrsp.QgiVec(i),
                            .YAUX = MPrsp.QliqVec(i)
                        })
                        End If

                    Next i

                Case 2

                    Dim titulo As String = ""
                    For j = 0 To MPrsp.CartaX.GetUpperBound(0)
                        '    ReDim xHz(99), yHz(99)
                        If j = 0 Or j = 5 Then
                            Continue For
                        End If

                        Select Case j
                            Case 0 '40 Hz
                                titulo = "35 Hz"
                            Case 1 '50 Hz
                                titulo = "40 Hz"
                            Case 2 '60 Hz
                                titulo = "50 Hz"
                            Case 3 '70 Hz
                                titulo = "60 Hz"
                            Case 4 '70 Hz
                                titulo = "70 Hz"
                            Case 5 '70 Hz
                                titulo = "75 Hz"
                            Case Else
                                titulo = "ND"

                        End Select

                        Dim gasto As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = titulo
                        }

                        For i = 0 To MPrsp.CartaX.GetUpperBound(1)
                            gasto.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                                .XAUX = MPrsp.CartaX(j, i),
                                .YAUX = MPrsp.CartaY(j, i)
                            })
                        Next i

                        db.COMPORTAMIENTO_GAS.Add(gasto)
                        db.SaveChanges()


                    Next j

                    ''=========rango minimo==============
                    Dim rango_min As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Rango Mínimo"
                    }

                    For i = 0 To MPrsp.RangeX.GetUpperBound(1)
                        rango_min.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                        .IDCOMPORTAMIENTOGAS = rango_min.IDCOMPORTAMIENTOGAS,
                        .XAUX = MPrsp.RangeX(0, i),
                        .YAUX = MPrsp.RangeY(0, i)
                      })
                    Next i
                    db.COMPORTAMIENTO_GAS.Add(rango_min)
                    db.SaveChanges()
                    '=========rango maximo==============
                    Dim rango_max As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Rango Máximo"
                    }
                    For i = 0 To MPrsp.RangeX.GetUpperBound(1)
                        rango_max.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                               .IDCOMPORTAMIENTOGAS = rango_max.IDCOMPORTAMIENTOGAS,
                               .XAUX = MPrsp.RangeX(1, i),
                               .YAUX = MPrsp.RangeY(1, i)
                        })

                    Next i
                    db.COMPORTAMIENTO_GAS.Add(rango_max)
                    db.SaveChanges()
                    '=========Eficiencia==============
                    Dim eficiencia As New COMPORTAMIENTO_GAS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Mejor Eficiencia"
                    }

                    For i = 0 To MPrsp.RangeX.GetUpperBound(1)
                        eficiencia.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                               .IDCOMPORTAMIENTOGAS = eficiencia.IDCOMPORTAMIENTOGAS,
                               .XAUX = MPrsp.RangeX(2, i),
                               .YAUX = MPrsp.RangeY(2, i)
                        })
                    Next i
                    db.COMPORTAMIENTO_GAS.Add(eficiencia)
                    db.SaveChanges()

                    'Dim xHz As Double()
                    'Dim yHz As Double()
                    'Dim titulo As String = ""
                    'For j = 0 To 3
                    '    ReDim xHz(99), yHz(99)

                    '    Select Case j
                    '        Case 0 '40 Hz
                    '            titulo = "40 Hz"
                    '        Case 1 '50 Hz
                    '            titulo = "50 Hz"
                    '        Case 2 '60 Hz
                    '            titulo = "60 Hz"
                    '        Case 3 '70 Hz
                    '            titulo = "70 Hz"
                    '    End Select
                    '    Dim gasto As New COMPORTAMIENTO_GAS() With {
                    '        .IDMODPOZO = IdModPozo,
                    '        .TITULO = titulo
                    '    }

                    '    db.COMPORTAMIENTO_GAS.Add(gasto)
                    '    db.SaveChanges()

                    '    For i = 0 To 99
                    '        db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                    '            .IDCOMPORTAMIENTOGAS = gasto.IDCOMPORTAMIENTOGAS,
                    '            .XAUX = MPrsp.Xcarta_BEC(j)(i),
                    '            .YAUX = MPrsp.Ycarta_BEC(j)(i)
                    '        })
                    '    Next i



                    'Next j

                    ''=========rango minimo==============
                    'Dim rango_min As New COMPORTAMIENTO_GAS() With {
                    '        .IDMODPOZO = IdModPozo,
                    '        .TITULO = "Rango Mínimo"
                    '}
                    'db.COMPORTAMIENTO_GAS.Add(rango_min)
                    'db.SaveChanges()
                    'For i = 0 To MPrsp.Rango_MinX_BEC.Count() - 1
                    '    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                    '           .IDCOMPORTAMIENTOGAS = rango_min.IDCOMPORTAMIENTOGAS,
                    '           .XAUX = MPrsp.Rango_MinX_BEC(i),
                    '           .YAUX = MPrsp.Rango_MinY_BEC(i)
                    '    })
                    'Next i
                    'db.SaveChanges()
                    ''=========rango maximo==============
                    'Dim rango_max As New COMPORTAMIENTO_GAS() With {
                    '        .IDMODPOZO = IdModPozo,
                    '        .TITULO = "Rango Máximo"
                    '}
                    'db.COMPORTAMIENTO_GAS.Add(rango_max)
                    'db.SaveChanges()

                    'For i = 0 To MPrsp.Rango_MaxX_BEC.Count() - 1
                    '    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                    '           .IDCOMPORTAMIENTOGAS = rango_max.IDCOMPORTAMIENTOGAS,
                    '           .XAUX = MPrsp.Rango_MaxX_BEC(i),
                    '           .YAUX = MPrsp.Rango_MaxY_BEC(i)
                    '    })

                    'Next i
                    'db.SaveChanges()
                    ''=========Eficiencia==============
                    'Dim eficiencia As New COMPORTAMIENTO_GAS() With {
                    '        .IDMODPOZO = IdModPozo,
                    '        .TITULO = "Mejor Eficiencia"
                    '}
                    'db.COMPORTAMIENTO_GAS.Add(eficiencia)
                    'db.SaveChanges()

                    'For i = 0 To MPrsp.Mejor_EficX_BEC.Count() - 1
                    '    db.COMPORTAMIENTO_GAS_DETALLES.Add(New COMPORTAMIENTO_GAS_DETALLES() With {
                    '           .IDCOMPORTAMIENTOGAS = eficiencia.IDCOMPORTAMIENTOGAS,
                    '           .XAUX = MPrsp.Mejor_EficX_BEC(i),
                    '           .YAUX = MPrsp.Mejor_EficY_BEC(i)
                    '    })
                    'Next i
                    'db.SaveChanges()

            End Select

        Catch ex As Exception
            Throw New Exception("SaveGAS: " + ex.Message)
        End Try

    End Sub
    Private Sub SaveDiag(ByVal IdModPozo As String, ByVal MPrsu As UpdateModelo)
        Try
            Select Case MPrsu.LiftMethod
                Case 1

                    Dim Bottom_Up As New DIAGNOSTICOS() With {
                              .IDMODPOZO = IdModPozo,
                              .TITULO = "Abajo - Arriba"
                    }
                    Dim Up_Bottom As New DIAGNOSTICOS() With
                    {
                      .IDMODPOZO = IdModPozo,
                      .TITULO = "Arriba - Abajo"
                    }

                    Dim Temp As New DIAGNOSTICOS() With
                    {
                          .IDMODPOZO = IdModPozo,
                          .TITULO = "Temperatura"
                    }

                    Dim Tr As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Presión en la TR"
                    }
                    Dim DIAGNOSTICOS As New DIAGNOSTICOS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Ptr Medida"
                    }
                    Dim Teo As New DIAGNOSTICOS() With {
                            .IDMODPOZO = IdModPozo,
                            .TITULO = "Ptr teorica"
                    }

                    For Each item In MPrsu.Quick_Look
                        If item.Sentido = 0 Then
                            Bottom_Up.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.PRES, .YAUX = item.MV})
                            Temp.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.TEMP, .YAUX = item.MV})
                        End If

                        If item.Sentido = 1 Then
                            Up_Bottom.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.PRES, .YAUX = item.MV})
                        End If

                    Next


                    For Each item In MPrsu.PresTR

                        Tr.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.Pres, .YAUX = item.Prof_MV})
                        Teo.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.Pres_Teo, .YAUX = item.Prof_MV})
                    Next

                    db.DIAGNOSTICOS.Add(Bottom_Up)
                    db.DIAGNOSTICOS.Add(Up_Bottom)
                    db.DIAGNOSTICOS.Add(Temp)
                    db.DIAGNOSTICOS.Add(Tr)
                    db.DIAGNOSTICOS.Add(Teo)
                    db.SaveChanges()





                Case 2

                    'Arriba - Abajo
                    Dim bottom As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Abajo - Arriba"
                    }
                    'Abajo - Arriba
                    Dim top As New DIAGNOSTICOS() With {
                       .IDMODPOZO = IdModPozo,
                           .TITULO = "Arriba - Abajo"
                    }
                    'Temperatura
                    Dim temp As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Temp [°C]"
                    }


                    For Each item In MPrsu.Quick_Look
                        If item.Sentido = 0 Then
                            bottom.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.PRES, .YAUX = item.MV})
                            temp.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.TEMP, .YAUX = item.MV})
                        End If

                        If item.Sentido = 1 Then
                            top.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {.XAUX = item.PRES, .YAUX = item.MV})
                        End If

                    Next


                    db.DIAGNOSTICOS.Add(top)
                    db.DIAGNOSTICOS.Add(bottom)
                    db.DIAGNOSTICOS.Add(temp)

                    db.SaveChanges()

            End Select

        Catch ex As Exception
            Throw New Exception("SaveDiag: " + ex.Message)
        End Try
    End Sub
    ''' <summary>
    ''' Resultado del   QUICKLOOK
    ''' </summary>
    ''' <param name="IdModPozo">Id del modelo de pozo</param>
    Private Sub SaveDiag(ByVal IdModPozo As String)
        Dim titulo As String = ""
        Try
            Select Case MPrsp.LiftMethod.Val
                Case 1
                    'TChart5.Text = "Diagnóstico de BNC"
                    Dim Xaux(MPrsp.NumDatGrad), Yaux(MPrsp.NumDatGrad) As Double


                    'Arriba - Abajo
                    Dim arriba As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Arriba-Abajo"
                    }
                    'Abajo - Arriba
                    Dim abajo As New DIAGNOSTICOS() With {
                       .IDMODPOZO = IdModPozo,
                           .TITULO = "Abajo-Arriba"
                    }
                    'Ptr Medida
                    Dim medida As New DIAGNOSTICOS() With {
                       .IDMODPOZO = IdModPozo,
                       .TITULO = "Ptr Medida"
                    }
                    'Ptr Teorica
                    Dim teorica As New DIAGNOSTICOS() With {
                       .IDMODPOZO = IdModPozo,
                       .TITULO = "Ptr Teoríca"
                    }
                    db.DIAGNOSTICOS.Add(arriba)
                    db.DIAGNOSTICOS.Add(abajo)
                    db.DIAGNOSTICOS.Add(medida)
                    db.DIAGNOSTICOS.Add(teorica)
                    db.SaveChanges()


                    For j = 0 To MPrsp.NumDatGrad - 1
                        'Xaux(J) = MPrsp.PresQL(i, J)
                        'Yaux(J) = MPrsp.TVDQL(i, J)

                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                            .IDDIAGNOSTICO = arriba.IDDIAGNOSTICO,
                            .XAUX = MPrsp.PresQL(0, j),
                            .YAUX = MPrsp.TVDQL(0, j)
                        })
                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                          .IDDIAGNOSTICO = abajo.IDDIAGNOSTICO,
                          .XAUX = MPrsp.PresQL(1, j),
                          .YAUX = MPrsp.TVDQL(1, j)
                       })
                    Next


                    ' Dim Xaux1(MPrsp.NumDatGG), Yaux1(MPrsp.NumDatGG) As Double
                    For j = 0 To MPrsp.NumDatGG - 1

                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                            .IDDIAGNOSTICO = medida.IDDIAGNOSTICO,
                            .XAUX = MPrsp.PresGG(j),
                            .YAUX = MPrsp.TVDGG(j)
                        })

                        'Xaux1(I) = MPrsp.PresGG(I)
                        'Yaux1(I) = MPrsp.MSDGG(I)
                    Next

                    For j = 0 To MPrsp.GGPres.Length - 1
                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                            .IDDIAGNOSTICO = teorica.IDDIAGNOSTICO,
                            .XAUX = MPrsp.GGPres(j),
                            .YAUX = MPrsp.GGTVD(j)
                        })
                    Next


                Case 2
                    'Arriba - Abajo
                    Dim arriba As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Arriba-Abajo"
                    }
                    'Abajo - Arriba
                    Dim abajo As New DIAGNOSTICOS() With {
                       .IDMODPOZO = IdModPozo,
                           .TITULO = "Abajo-Arriba"
                    }
                    'Temperatura
                    Dim temp As New DIAGNOSTICOS() With {
                        .IDMODPOZO = IdModPozo,
                        .TITULO = "Temp [°C]"
                    }
                    db.DIAGNOSTICOS.Add(arriba)
                    db.DIAGNOSTICOS.Add(abajo)
                    db.DIAGNOSTICOS.Add(temp)
                    db.SaveChanges()
                    For J = 0 To MPrsp.NumDatGrad - 1

                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                            .IDDIAGNOSTICO = arriba.IDDIAGNOSTICO,
                            .XAUX = MPrsp.PresQL(0, J),
                            .YAUX = MPrsp.TVDQL(0, J)
                        })

                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                           .IDDIAGNOSTICO = abajo.IDDIAGNOSTICO,
                           .XAUX = MPrsp.PresQL(1, J),
                           .YAUX = MPrsp.TVDQL(1, J)
                        })

                        db.DIAGNOSTICO_DETALLES.Add(New DIAGNOSTICO_DETALLES() With {
                           .IDDIAGNOSTICO = temp.IDDIAGNOSTICO,
                           .XAUX = MPrsp.TempQL(0, J),
                           .YAUX = MPrsp.TVDQL(0, J)
                       })

                    Next
                    db.SaveChanges()




            End Select

        Catch ex As Exception
            Throw New Exception("SaveDiag: " + ex.Message)
        End Try

    End Sub
    Private Sub SaveWC(ByVal IdModPozo As String)
        Try
            Dim i, j, k As Integer
            Dim NumVar1 As Integer = 9
            Dim NumVar2 As Integer = 9
            Dim NumVar3 As Integer = 0

            Dim Xaux(NumVar1), Yaux(NumVar2) As Double




            Select Case MPrsp.LiftMethod.Val
                Case 1

                    For i = 0 To NumVar1
                        Dim wc As New PRODUCTIVIDAD() With {
                            .IDPRODUCTIVIDAD = Guid.NewGuid().ToString().ToUpper(),
                            .IDMODPOZO = IdModPozo,
                            .TITULO = Math.Round(MPrsp.Qgi_Res(i), 1).ToString
                        }
                        db.PRODUCTIVIDAD.Add(wc)
                        db.SaveChanges()

                        For j = 0 To NumVar2
                            Xaux(j) = MPrsp.Wc_Res(j)
                            For k = 0 To NumVar3
                                Yaux(j) = MPrsp.Qliq_Res(i, j, k)
                            Next k


                        Next j

                        For j = 0 To Xaux.Count - 1
                            db.PRODUCTIVIDAD_DETALLE.Add(New PRODUCTIVIDAD_DETALLE() With {
                                .IDPRODUCTIVIDADDETALLE = Guid.NewGuid().ToString().ToUpper(),
                                .IDPRODUCTIVIDAD = wc.IDPRODUCTIVIDAD,
                                .WC_RES = Xaux(j),
                                .QGI_RES = Yaux(j)
                             })
                        Next j
                        db.SaveChanges()
                    Next i
                Case 2
                    For i = 0 To NumVar1
                        Dim wc As New PRODUCTIVIDAD() With {
                            .IDPRODUCTIVIDAD = Guid.NewGuid().ToString().ToUpper(),
                            .IDMODPOZO = IdModPozo,
                            .TITULO = Math.Round(MPrsp.Frec_Res(i), 1).ToString
                        }
                        db.PRODUCTIVIDAD.Add(wc)
                        db.SaveChanges()

                        For j = 0 To NumVar2
                            Xaux(j) = MPrsp.Wc_Res(j)
                            For k = 0 To NumVar3
                                Yaux(j) = MPrsp.Qliq_Res(i, j, k)
                            Next k


                        Next j

                        For j = 0 To Xaux.Count - 1
                            db.PRODUCTIVIDAD_DETALLE.Add(New PRODUCTIVIDAD_DETALLE() With {
                                .IDPRODUCTIVIDADDETALLE = Guid.NewGuid().ToString().ToUpper(),
                                .IDPRODUCTIVIDAD = wc.IDPRODUCTIVIDAD,
                                .WC_RES = Xaux(j),
                                .QGI_RES = Yaux(j)
                             })
                        Next j
                        db.SaveChanges()
                    Next i

            End Select

        Catch ex As Exception
            Throw New Exception("SaveWC: " + ex.Message)
        End Try

    End Sub

    Private Function LoadGeneral(ByVal general As MOD_POZO_GENERAL, ByVal edo_mecanico As List(Of VW_EDO_MECANICO), ByVal edo_trayectoria As List(Of MOD_POZO_TRAYEC), ByVal edo_temp As List(Of MOD_POZO_TEMP)) As Boolean

        Dim mod_bnc = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = general.IDMODPOZO).SingleOrDefault()
        Dim mod_bec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = general.IDMODPOZO).SingleOrDefault()






        Try
            ' Fluido: 0 = Oil and Water
            '         1 = Dry and Wet Gas
            '         2 = Retrograde Condensate
            '
            MPrsp.Fluid.Val = general.FLUID.GetValueOrDefault()

            '
            ' PVTModel: 0 = Black Oil
            '           1 = Equation of State
            '
            MPrsp.PVTModel.Val = general.PVTMODEL.GetValueOrDefault()

            '
            ' Separator: 0 = Single Stage Separator
            '            1 = Two Stage Separator
            '
            MPrsp.Separator.Val = general.SEPARATOR.GetValueOrDefault()

            '
            ' Emulsion: 0 = No
            '           1 = Emulsion + Pump Viscosity Correction
            '
            MPrsp.Emulsion.Val = general.EMULSION.GetValueOrDefault()

            '
            ' Hydrate: 0 = Disable Warning
            '          1 = Enable Warning
            '
            MPrsp.Hydrate.Val = general.HYDRATE.GetValueOrDefault()

            '
            ' WatVis: 0 = Use Default Correlation
            '         1 = Use Pressure Corrected Correlation
            '
            MPrsp.WatVis.Val = general.WATVIS.GetValueOrDefault()

            '
            ' VisMod: 0 = Newtonian Fluid
            '         1 = Non Newtonian Fluid
            '
            MPrsp.VisMod.Val = general.VISMOD.GetValueOrDefault()

            '
            ' FlowType: 0 = Tubing Flow
            '           1 = Annular Flow
            '
            MPrsp.FlowType.Val = general.FLOWTYPE.GetValueOrDefault()

            '
            ' WellType: 0 = Producer
            '           1 = Inyector
            '           2 = Water Inyector
            '
            MPrsp.WellType.Val = general.WELLTYPE.GetValueOrDefault()

            '
            ' LiftMethod: 0 = None
            '             1 = Gas Lift (Continous)
            '             2 = Electrical Sumersible Pump
            '             3 = Hydraulic Drive Downhole Pump
            '             4 = Progressive Cavity Pump
            '             5 = Coiled Tubing Gas Lift
            '             6 = Diluent Injection
            '             7 = Jet Pump
            '             8 = MultiPhase Pump
            '             9 = Sucker Rod Pump
            '             10 = Gas Lift (Intermttent)
            '
            MPrsp.LiftMethod.Val = general.LIFTMETHOD.GetValueOrDefault()

            '
            ' LiftType: 0 = No Friction Loss In Annulus
            '           1 = Friction Loss In Annulus
            '           2 = Safety Equipment
            '
            MPrsp.LiftType.Val = general.LIFTYPE.GetValueOrDefault()

            '
            ' Predict: 0 = Pressure Only
            '          1 = Pressure and Temperature (offshore)
            '          2 = Pressure and Temperature (on Land)        '
            MPrsp.Predict.Val = general.PREDICT.GetValueOrDefault()

            '
            ' TempModel: 0 = Rough Approximation
            '            1 = Enthalpy Balance
            '            2 = Improved Approximation
            MPrsp.TempModel.Val = general.TEMPMODEL.GetValueOrDefault()

            '
            ' RangeSystem: 0 = Full System
            '              1 = Pipeline Only
            MPrsp.RangeSystem.Val = general.RANGESYSTEM.GetValueOrDefault()

            '
            ' OutputRes: 0 = Show Calculating Data
            '            1 = Hide Calculating Data
            MPrsp.OutputRes.Val = general.OUTPUTRES.GetValueOrDefault()

            '
            ' Completion: 0 = Cased Hole
            '             1 = Open hole
            MPrsp.Completion.Val = general.COMPLETION.GetValueOrDefault()

            '
            ' GravelPack: 0 = None
            '             1 = Gravel Pack
            '             2 = Pre Packed Screen
            '             3 = Wire Wrapped Screen
            '             4 = Slotted Liner
            MPrsp.GravelPack.Val = general.GRAVELPACK.GetValueOrDefault()

            '
            ' InflowType: 0 = Single Branch
            '             1 = MultiLateral Well
            MPrsp.InflowType.Val = general.INFLOWTYPE.GetValueOrDefault()

            '
            ' GasConing: 0 = No
            '            1 = Yes
            MPrsp.GasConing.Val = general.GASCONING.GetValueOrDefault()

            MPrsp.Company = general.COMPANY
            MPrsp.Field = general.FIELD
            MPrsp.Locations = general.LOCATIONS
            MPrsp.Well = general.WELL
            MPrsp.Platform = general.PLATAFORM
            MPrsp.Analyst = general.ANALYST
            'MPrsp.DatGenDate = general.DATGENDATE

            MPrsp.Comenta = general.COMENTA



            '
            ' IPR
            '
            MPrsp.IPRMethod.Val = general.IPRMETHOD.GetValueOrDefault()

            ' Datos
            ' Presión de Fondo Estática
            MPrsp.PRes.Val = general.PRES.GetValueOrDefault() ' Kg/cm2
            ' Temperatura del Yacimiento
            MPrsp.TRes.Val = general.TRES.GetValueOrDefault()  ' oC
            ' Porcentaje de agua en la capa
            MPrsp.Wc.Val = general.WC.GetValueOrDefault()  ' %
            ' Relación Gas Aceite tomada de la columna de los datos del yacimiento debido a que se debe filtrar.
            MPrsp.TotGor.Val = general.TOTGOR.GetValueOrDefault()
            ' Modelo de reduccion de permeabilidad por compactación
            ' 0: No
            ' 1: Si
            MPrsp.Compact.Val = general.COMPACT.GetValueOrDefault()
            ' Considerar las permeabilidades relativas
            ' 0: No
            ' 1: Si
            MPrsp.IRELK.Val = general.IRElK.GetValueOrDefault()

            ' Datos del modelo de Vogel
            '================================================
            ' Producción de liquido
            MPrsp.QTest.Val = general.QTEST.GetValueOrDefault() ' bl/dia
            ' Presión de fondo fluyendo
            MPrsp.Ptest.Val = general.PTEST.GetValueOrDefault() ' Kg/cm2

            ' Datos del modelo de Darcy
            '================================================
            ' Daño Mecánico / Geometrico
            ' 0: Enter Skin by Hand
            ' 1: Loke
            ' 2: MacLeod
            ' 3: Karakas - Tariq
            MPrsp.MGSkinMethod.Val = general.MGSKINMETHOD.GetValueOrDefault()

            ' Daño por desviación y penetración parcial
            ' 0: Cinco / Martín-Bronz
            ' 1: Wong-Cliford
            ' 2: Cinco (2) / Martín-Bronz
            MPrsp.DPSkinMethod.Val = general.MGSKINMETHOD.GetValueOrDefault()

            ' Permeabilidad del yacimiento
            MPrsp.ResPerm.Val = general.RESPERM.GetValueOrDefault()
            ' Espesor del yacimiento
            MPrsp.Thickness.Val = general.THICKNESS.GetValueOrDefault()
            ' Area de drene
            MPrsp.Drainage.Val = general.DRAINAGE.GetValueOrDefault()
            ' Factor de forma
            MPrsp.Dietz.Val = general.DIETZ.GetValueOrDefault()
            ' Radio del pozo
            MPrsp.WBR.Val = general.WBR.GetValueOrDefault()
            ' Factor de daño total
            MPrsp.Skin.Val = general.SKIN.GetValueOrDefault()

            'Datos de modelo IP
            '========================================
            MPrsp.PI.Val = general.PI.GetValueOrDefault()


            Select Case general.LIFTMETHOD
                Case 1
                    ' DATOS DEL BNC
                    '
                    ' Emplear RGIL o Qgi
                    '      0: Usar RGIL
                    '      1: Usar Qgi
                    MPrsp.Entry.Val = mod_bnc.ENTRY.GetValueOrDefault()
                    ' Metodo de  calculo del BN.
                    ' 0: Profundidad de Inyección Fija
                    ' 1: Profundidad de Inyeccion Optima
                    ' 2: Especificar la profundidad de las valvulas
                    MPrsp.Method.Val = mod_bnc.METHOD.GetValueOrDefault()

                    ' Densidad del gas de bombeo neumatico
                    MPrsp.Gravity.Val = mod_bnc.GRAVITY.GetValueOrDefault()
                    '  Mol de H2S
                    MPrsp.H2S.Val = mod_bnc.H2S.GetValueOrDefault()
                    '  Mol de CO2
                    MPrsp.CO2.Val = mod_bnc.CO2.GetValueOrDefault()
                    '  Mol de N2
                    MPrsp.N2.Val = mod_bnc.N2.GetValueOrDefault()
                    ' Qgi
                    MPrsp.GLRiny.Val = mod_bnc.GLRINY.GetValueOrDefault()

                    ' Relación gas inyectado liquido m3/m3
                    MPrsp.GLRate.Val = mod_bnc.GLRATE.GetValueOrDefault()

                    ' Profundidad de la valvula de inyeccion
                    MPrsp.ValveDepth.Val = mod_bnc.VALVEDEPTH.GetValueOrDefault()
                    ' Diametro de la válvula de BNC
                    MPrsp.DiamValBNC.Val = mod_bnc.DIAMVAL.GetValueOrDefault()

                    MPrsp.QgiMin.Val = mod_bnc.QGIMIN.GetValueOrDefault()

                    MPrsp.QgiMax.Val = mod_bnc.QGIMAX.GetValueOrDefault()

                    ' Presion en la TR (BNC)
                    MPrsp.TRPres.Val = mod_bnc.TRPRES.GetValueOrDefault()

                Case 2
                    '************************DATOS BEC************************************

                    ' Frecuencia Mínima
                    MPrsp.FrecMin.Val = mod_bec.FRECMIN.GetValueOrDefault()
                    ' Frecuencia Máxima
                    MPrsp.FrecMax.Val = mod_bec.FRECMAX.GetValueOrDefault()
                    ' Profundidad del BEC
                    MPrsp.Prof_BEC.Val = mod_bec.PROF_BEC.GetValueOrDefault()
                    ' Frecuencia de operación del BEC
                    MPrsp.Frec_BEC.Val = mod_bec.FREC_BEC.GetValueOrDefault()
                    ' Diámetro exterior máximo de BEC
                    MPrsp.ODMax_BEC.Val = mod_bec.ODMAX_BEC.GetValueOrDefault()
                    ' Longitud del Cable
                    MPrsp.LongCable_BEC.Val = mod_bec.LONGCABLE_BEC.GetValueOrDefault()
                    ' Eficiencia del separación de gas del BEC
                    MPrsp.EfiSepGas_BEC.Val = mod_bec.EFISEPGAS_BEC.GetValueOrDefault()
                    ' Etapas de BEC
                    MPrsp.Etapas_BEC.Val = mod_bec.ETAPAS_BEC.GetValueOrDefault()
                    ' Voltaje en superficie del BEC
                    MPrsp.VoltSup_BEC.Val = mod_bec.VOLTSUP_BEC.GetValueOrDefault()
                    ' Factor de desgaste del BEC
                    MPrsp.Desgaste_BEC.Val = mod_bec.DESGASTE_BEC.GetValueOrDefault()
                    ' Modelo de reducción del gas
                    MPrsp.ReducGas_BEC.Val = mod_bec.REDUCGAS_BEC.GetValueOrDefault()
                    ' Número de bomba del catálogo de bombas
                    MPrsp.Bomba_BEC.Val = mod_bec.BOMBA_BEC.GetValueOrDefault()
                    ' Número de motor del catálogo de motres
                    MPrsp.Motor_BEC.Val = mod_bec.MOTOR_BEC.GetValueOrDefault()
                    ' Número de la potencia del motor del catálogo de motores-potencia
                    MPrsp.PotenciaMotor_BEC.Val = mod_bec.POTENCIAMOTOR_BEC.GetValueOrDefault()
                    ' Número de cable del catálogo de cables
                    MPrsp.Cable_BEC.Val = Int32.Parse(mod_bec.CABLE_BEC.GetValueOrDefault())
                    ' Presión de Succión de la bomba
                    MPrsp.PreSuc_BEC.Val = mod_bec.PRESUC_BEC.GetValueOrDefault()
                    ' Corriente de la bomba BEC
                    MPrsp.Corriente_BEC.Val = mod_bec.CORRIENTE_BEC.GetValueOrDefault()
                    ' Potencia de la bomba BEC
                    MPrsp.Potencia_BEC.Val = mod_bec.POTENCIA_BEC.GetValueOrDefault()
                    ' Presión de Descarga de la bomba
                    MPrsp.PreDes_BEC.Val = mod_bec.PREDES_BEC.GetValueOrDefault()

                    'CARGAR CATALOGO 
                    MPrsp.Bombas = New Dictionary(Of Integer, BOMBA)
                    For Each b In db.BOMBA.Where(Function(w) w.PROSPER IsNot Nothing).ToList()
                        If MPrsp.Bombas.ContainsKey(b.PROSPER.GetValueOrDefault()) = False Then
                            MPrsp.Bombas.Add(b.PROSPER.GetValueOrDefault(), b)
                        End If
                    Next
                    'MPrsp.Bombas = db.BOMBA.Where(Function(w) w.).ToDictionary(Function(d) d.PROSPER.GetValueOrDefault(), Function(d) d)


            End Select


            'DATOS ESTADO MECANICO
            If MPrsp.Equipment = False Then
                For i = 0 To edo_mecanico.Count - 1
                    ' Etiqueta
                    MPrsp.Label(i) = edo_mecanico(i).ETIQUETA
                    ' Tipo de elemento ->
                    '      0: E.M.R
                    '      1: Tubing
                    '      2: SSSV
                    '      3: Restriction
                    '      4: Casing
                    If i > 0 Then MPrsp.DType.Val(i) = edo_mecanico(i).NUMERO.ToString()
                    ' Profundidad desarrollada
                    MPrsp.Depth.Val(i) = IIf(edo_mecanico(i).ETIQUETA.Contains("SSSV"), "0", edo_mecanico(i).MD.ToString())
                    ' Diametro interior de la tuberia de produccion
                    MPrsp.TID.Val(i) = edo_mecanico(i).TIDIAM.ToString()
                    ' Rugosidad del interior de la tuberia de produccion
                    MPrsp.TIR.Val(i) = edo_mecanico(i).TIROUG.ToString()
                    ' Diametro exterior de la tuberia de produccion
                    MPrsp.TOD.Val(i) = edo_mecanico(i).TODIAM.ToString()
                    ' Rugosidad del exterior de la tuberia de produccion
                    MPrsp.TOR.Val(i) = edo_mecanico(i).TOROUG.ToString()
                    ' Diametro interior de la tuberia de revestimiento
                    MPrsp.CID.Val(i) = edo_mecanico(i).CIDIAM.ToString()
                    ' Rugosidad del interior de la tuberia de revestimiento
                    MPrsp.CIR.Val(i) = edo_mecanico(i).CIROUG.ToString()

                    'MsgBox("Elemento No:  " & I.ToString & Chr(13) & _
                    '       "Etiqueta:     " & MPrsp.Label(I).ToString & Chr(13) & _
                    '       "Tipo Tub:     " & MPrsp.DType(I).ToString & Chr(13) & _
                    '       "Profund:      " & MPrsp.Depth(I).ToString & Chr(13) & _
                    '       "Diam Int TP:  " & MPrsp.TID(I).ToString & Chr(13) & _
                    '       "Rug Int TP:   " & MPrsp.TIR(I).ToString & Chr(13) & _
                    '       "Diam Ext TP:  " & MPrsp.TOD(I).ToString & Chr(13) & _
                    '       "Rug Ext TP:   " & MPrsp.TOR(I).ToString & Chr(13) & _
                    '       "Diam Int TR:  " & MPrsp.CID(I).ToString & Chr(13) & _
                    '       "Rug Int TR:   " & MPrsp.CIR(I).ToString & Chr(13))

                Next i


                'DATOS TRAYECTORIA

                ' Registro de Desviaciones
                '
                ' Trayecto_SQL(0, 0): Nombre del pozo
                ' Trayecto_SQL(0, 1): Numero de agujero
                ' Trayecto_SQL(0, 2): Fecha del modelo
                ' Trayecto_SQL(0, 3): IDTRAYECTORIAPROSPER
                ' Trayecto_SQL(0, 4): IDDATOSMODELOPROSPER
                '
                For I = 0 To edo_trayectoria.Count - 1
                    MPrsp.RDEnable.Val(I) = 1
                    ' Profundidad Desarrollada
                    MPrsp.RDMd.Val(I) = edo_trayectoria(I).PROFUNDIDADMD
                    ' Profundidad Vertical
                    MPrsp.RDTvd.Val(I) = edo_trayectoria(I).PROFUNDIDADMV
                Next I

                'DATOS TEMPERATURA

                For I = 0 To edo_temp.Count - 1
                    ' Profundidad Desarrollada
                    MPrsp.PTMd.Val(I) = edo_temp(I).PROFUNDIDADMD

                    ' Temperatura
                    MPrsp.PTTmp.Val(I) = edo_temp(I).TEMPERATURA
                Next I
            End If



            ' Coeficiente de Transferencia de Calor Inicial
            MPrsp.Htc.Val = general.HTC.GetValueOrDefault()
            ' Presion en la cabeza del pozo
            MPrsp.THPres.Val = general.THPD.GetValueOrDefault()
            ' Temperatura en la cabeza del pozo (Fluyendo)
            MPrsp.THTemp.Val = general.THTD.GetValueOrDefault()
            ' RGA del aforo
            MPrsp.RGA_Aforo.Val = general.RGATOTALAFORO.GetValueOrDefault()


            Return True

        Catch ex As Exception
            Throw New Exception(ex.Message)

            Return False
        End Try
    End Function


    Private Async Sub TimeLimit(ByVal ModPozo As MOD_POZO, ByVal MPrsp As Crea.Modelo)


        Dim TimeStop As DateTime = DateTime.Now.AddSeconds(TimeLimitT)
        Logger = New ModeloProsper.Logger(ModPozo.IDMODPOZO, ModPozo.CONFIGURACION_ADMINISTRADOR(0).IDUSUARIO)

        While TimeStop > DateTime.Now

            Await Task.Delay(100)

        End While

        ''MPrsp.Disconnect()



    End Sub




    Public Function Tester(ByVal Command As String) As Object

        Dim Modelo As New ModeloProsper.Crea.Modelo()
        Modelo.ProgramPath = ProgramPath

        Return Modelo.Tester(Command)
    End Function

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub
    ''' <summary>
    ''' Se forza el apagado del Open Server, matando todos los procesos activos.
    ''' </summary>
    Public Shared Sub ShutDown()
        Dim PxServer = Process.GetProcessesByName("pxserver")
        Prosper.Disconnect()
        If PxServer.Count() > 0 Then

            For i = 0 To PxServer.Count()
                PxServer(i).Kill()
            Next i

        End If


        ModeloProsper.Settings.SetBy("open_server", "1")
    End Sub

    Public Function PxServer() As Process()
        Dim Px = Process.GetProcessesByName("pxserver")

        Return Px
    End Function
    Public Shared Function Dispose() As Boolean
        Dim Px = Process.GetProcessesByName("pxserver")

        If Px.Count() > 0 Then
            Return False
        Else
            Return True
        End If
    End Function

    Private Function CreateFolder() As Boolean
        If (Not System.IO.Directory.Exists(Path)) Then
            System.IO.Directory.CreateDirectory(Path)
        End If
        Return True
    End Function


End Class
