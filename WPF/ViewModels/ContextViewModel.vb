' VERSIONES
' Version                                 Comentarios
'==============================================================


Imports System.Windows.Forms.Integration
Imports ModeloCI
'Imports ModeloProsper
Imports System.Data
Imports WPF.Generales
Imports Telerik.Windows.Controls
Imports Prism.Mvvm
Imports UC_EstadoMecanico



Public Class ContextViewModel
    Inherits BindableBase
    'Inherits ViewModelBase



    Private GastoInyeccion() As Double
    Private tablacatalogo As DataTable
    'Private MPrsp As ModeloProsper.Crea.Modelo
    Private Resu_SQL(1, 70) As String
    Private EdoMec_SQL(20, 15) As String
    Private Trayecto_SQL(20, 10) As String
    Private Temperatura_SQL(20, 10) As String
    Private NumDatEdoMec As Integer
    Private NumDatTrayecto As Integer
    Private NumDatTemp As Integer
    ' Private IDagujero As String
    'Private fechaprueba As String


    Public grfProductividad As New grfProductividad()
    Public grfCorrelacion As New grfCorrelacion()
    Public grfVpl As New grfVlp()
    Public grfGas As New grfGas()
    Public grfDiag As New grfDiag()
    Public grfWc As New grfWc()
    'Private MPrsp As Crea.Modelo
    Private db As New Entities_ModeloCI()
    Private objconsulta As New BaseDatos("SERVER=10.85.35.113\SQLDESA01;User=CamposInteligentes;Password=ciGaitep; Database=CI;")
    'Private agujero As AgujeroModel
    Private VwGeneral As VW_EDO_GENERAL 'Ultimo modelo generado

    Private EdoMecanico As New UC_EstadoMecanico.EstadoMecanico()
    Private AgujeroModel As AgujeroModel



    Sub New()



        'IDAGUJERO                                    LIFTMETHOD           FECHA P.
        '55A5DD64-F543-4B5F-874A-8E1C49CDEF4A             1               2016-01-25    
        '57EB0E3D-1A72-45B3-A3BF-377B504A238B             2               2019-02-19
        '677069CB-7862-43BD-B3D8-7E41B5895ED1             1               2018-08-10
        'DDB3A31F-0143-42E3-819B-9B4A4B631D33             2               2019-03-25
        '24ACEE43-3CC1-4E8E-9535-76D049D2DDCC  
        '817A460B-3856-4837-98A4-ACF6F622D33F             1               2019-02-11              MAALOB-492
        '1A9B6D8D-50ED-4719-BA34-37D8337B7A22             1               2018-08-11              BALAM-41
        '56B74925-CE90-4E2D-8FAC-9933EBC4899D             1               2018-01-02              KU-288




        IdAgujero = "56B74925-CE90-4E2D-8FAC-9933EBC4899D"
        FechaPrueba = "2018-01-02"

        'Inicializamos propiedades
        '============================================
        _on_load_aforo = New DelegateCommand(AddressOf LoadAforo)


        _mod_general = New MOD_POZO_GENERAL() With {
            .DATGENDATE = DateTime.Now
        }
        _mod_bnc = New MOD_POZO_BNC()
        _mod_bec = New MOD_POZO_BEC()

        _configuraciones = db.VW_MOD_POZO.Where(Function(w) w.IDAGUJERO = IdAgujero And w.ESTATUS = 3).ToList()

        'Consultamos el Agujero
        '============================================
        'Dim _agujero = AgujeroModel.GetAgujero(IdAgujero, FechaPrueba)

        AgujeroModel = New AgujeroModel(db, IdAgujero, FechaPrueba)




        'agujero = ObjAgujero.GetAgujero(IdAgujero) ' Casi obsoleto
        '_pozo = agujero.pozo & " | "
        'IdPozo = agujero.id_pozo
        LiftMethod = AgujeroModel.LiftMethod
        Titulo = "Nueva configuración"
        Pozo = AgujeroModel.Pozo

        If AgujeroModel.IdModPozo IsNot Nothing Then
            IdModPozo = AgujeroModel.IdModPozo
            Titulo = "Última configuración " + AgujeroModel.CreatedOn
        Else
            Estatus = AgujeroModel.Estatus
            Datgendate = DateTime.Now
        End If











        _command_save = New DelegateCommand(AddressOf OnSave)


        _aforos = db.AFORO.Where(Function(w) w.ENDRECORD = Nothing And w.IDAGUJERO = AgujeroModel.IdAgujero).ToList()



    End Sub
    Public Sub LoadAforo(ByVal e As Object)

        Dim aforo = CType(e.Source.SelectedItem, AFORO)

        If aforo Is Nothing Then
            Exit Sub
        End If

        Dim result = MessageBox.Show("¿Desea cargar los datos del aforo con fecha " + aforo.FECHA + " a la configuración?", "Confirmar datos", MessageBoxButton.YesNoCancel)

        If result = MessageBoxResult.Yes Then
            Qtest = aforo.PRODLIQ
            RgaTotalAforo = aforo.PRODGASFORM
            GlRiny = aforo.VOLGASINY
            Thpd = aforo.PTP1
            Trpres = aforo.PTR1
            Wc = aforo.FRACAGUA
            Thtd = aforo.TEMP
        End If


    End Sub
    Public Sub LoadCharts(ByVal IdModPozo As String)

        Dim mod_general = db.VW_EDO_GENERAL.Where(Function(w) w.IDMODPOZO = IdModPozo).SingleOrDefault()

        Estatus = mod_general.ESTATUS.GetValueOrDefault()
        VwGeneral = mod_general

        If IdModPozo = AgujeroModel.IdModPozo Then


            LoadConfig()

            If Estatus = 1 Then
                Prog = "Programado: " + mod_general.FECHA_PROGRAMACION
            End If

        End If



        If Estatus = 3 Then
            LoadIPR(mod_general)
            LoadCorrela(mod_general)
            LoadVLPIPR(mod_general)
            LoadGAS(mod_general)
            LoadDiag(mod_general)
            LoadWC(mod_general)
        End If

    End Sub
    Public Sub LoadConfig()
        'DATOS ENTRADA
        'ModGeneral.DATGENDATE = VwGeneral.DATGENDATE

        Pres = VwGeneral.PRES.GetValueOrDefault()
        Tres = VwGeneral.TRES.GetValueOrDefault()
        Wc = VwGeneral.WC.GetValueOrDefault()
        Totgor = VwGeneral.TOTGOR.GetValueOrDefault()
        Qtest = VwGeneral.QTEST.GetValueOrDefault()
        Ptest = VwGeneral.PTEST.GetValueOrDefault()
        Resperm = VwGeneral.RESPERM.GetValueOrDefault()
        Thickness = VwGeneral.THICKNESS.GetValueOrDefault()
        Drainage = VwGeneral.DRAINAGE.GetValueOrDefault()
        Dietz = VwGeneral.DIETZ.GetValueOrDefault()
        Wbr = VwGeneral.WBR.GetValueOrDefault()
        Skin = VwGeneral.SKIN.GetValueOrDefault()
        Htc = VwGeneral.HTC.GetValueOrDefault()





        Fluid = VwGeneral.FLUID
        OutputRes = VwGeneral.OUTPUTRES
        PvtModel = VwGeneral.PVTMODEL
        IprMethod = VwGeneral.IPRMETHOD
        Completion = VwGeneral.COMPLETION
        Separator = VwGeneral.SEPARATOR
        GravelPack = VwGeneral.GRAVELPACK
        Emulsion = VwGeneral.EMULSION
        InflowType = VwGeneral.INFLOWTYPE
        Hydrate = VwGeneral.HYDRATE
        GasConing = VwGeneral.GASCONING
        Datgendate = VwGeneral.DATGENDATE.GetValueOrDefault()
        TempModel = VwGeneral.TEMPMODEL
        IprMethod = VwGeneral.IPRMETHOD
        RangeSystem = VwGeneral.RANGESYSTEM
        LiftType = VwGeneral.LIFTYPE
        Predict = VwGeneral.PREDICT
        Comenta = VwGeneral.COMENTA
        Htc = VwGeneral.HTC
        Thpd = VwGeneral.THPD
        Thtd = VwGeneral.THTD
        Trpres = VwGeneral.TRPRES
        RgaTotalAforo = VwGeneral.RGATOTALAFORO
        LiftMethod = VwGeneral.LIFTMETHOD


        Select Case VwGeneral.LIFTMETHOD
            Case 1
                _mod_bnc = db.MOD_POZO_BNC.Where(Function(w) w.IDMODPOZO = VwGeneral.IDMODPOZO).SingleOrDefault()

                If _mod_bnc Is Nothing Then
                    _mod_bnc = New MOD_POZO_BNC()
                End If

                Method = _mod_bnc.METHOD
                Entry = _mod_bnc.ENTRY
            Case 2
                _mod_bec = db.MOD_POZO_BEC.Where(Function(w) w.IDMODPOZO = VwGeneral.IDMODPOZO).SingleOrDefault()

                If _mod_bec Is Nothing Then
                    _mod_bec = New MOD_POZO_BEC()
                End If

        End Select

    End Sub
    Public Sub LoadDefault()

        'Datgendate = DateTime.Now
        ModGeneral.COMPANY = ht.Item("COMPANY")
        ModGeneral.FIELD = ht.Item("FIELD")
        ModGeneral.LOCATIONS = ht.Item("LOCATIONS")
        ModGeneral.WELL = ht.Item("WELL")
        ModGeneral.PLATAFORM = ht.Item("PLATAFORM")
        ModGeneral.ANALYST = ht.Item("ANALYST")


        'DATOS DE SISTEMA
        'ModGeneral.PRES = ht.Item("PRES")
        'ModGeneral.TRES = ht.Item("TRES")
        'ModGeneral.WC = ht.Item("WC")
        'ModGeneral.TOTGOR = ht.Item("TOTGOR")
        'ModGeneral.QTEST = ht.Item("QTEST")
        'ModGeneral.PTEST = ht.Item("PTEST")

        'ModGeneral.RESPERM = ht.Item("RESPERM")
        'ModGeneral.THICKNESS = ht.Item("THICKNESS")
        'ModGeneral.DRAINAGE = ht.Item("DRAINAGE")
        'ModGeneral.DIETZ = ht.Item("DIETZ")
        'ModGeneral.WBR = ht.Item("WBR")
        'ModGeneral.SKIN = ht.Item("SKIN")


        'ModGeneral.HTC = ht.Item("HTC")
        'ModGeneral.THPD = ht.Item("THPD")
        'ModGeneral.THTD = ht.Item("THTD")
        'ModGeneral.TRPRES = ht.Item("TRPRES")
        'ModGeneral.RGATOTALAFORO = ht.Item("RGATOTALAFORO")
        'ModGeneral.DATGENDATE = DateTime.Now


        'ModBEC.CABLE_BEC = ht.Item("CABLE")
        'ModBEC.PRESUC_BEC = ht.Item("PRESUC")
        'ModBEC.CORRIENTE_BEC = ht.Item("CORRIENTE")
        'ModBEC.POTENCIA_BEC = ht.Item("POTENCIA")
        'ModBEC.PREDES_BEC = ht.Item("PREDES")



    End Sub
    'Acciones
    '=============================================
    Private _on_load_aforo As ICommand
    Public Property OnLoadAforo As ICommand
        Get
            Return _on_load_aforo
        End Get
        Set(value As ICommand)
            _on_load_aforo = value
        End Set
    End Property
    'Utilerias de funcionamiento
    Private _refresh As Boolean
    Public Property Refresh As Boolean
        Get
            Return _refresh
        End Get
        Set(value As Boolean)
            _refresh = value
        End Set
    End Property
    Private _estatus As String
    Public Property Estatus As String
        Get
            Return _estatus
        End Get
        Set(value As String)
            _estatus = value
            RaisePropertyChanged("Estatus")
        End Set
    End Property
    Private _prog As String
    Public Property Prog() As String
        Get
            Return _prog
        End Get
        Set(value As String)
            _prog = value
            RaisePropertyChanged("Prog")
        End Set
    End Property
    Private _titulo As String
    Public Property Titulo As String
        Get
            Return _titulo
        End Get
        Set(value As String)
            _titulo = value
        End Set
    End Property

    'Modelo BNC
    Private _mod_bnc As MOD_POZO_BNC
    Public Property ModBNC() As MOD_POZO_BNC
        Get
            Return _mod_bnc
        End Get
        Set(value As MOD_POZO_BNC)
            _mod_bnc = value
        End Set
    End Property
    'Modelo BEC
    Private _mod_bec As MOD_POZO_BEC
    Public Property ModBEC() As MOD_POZO_BEC
        Get
            Return _mod_bec
        End Get
        Set(value As MOD_POZO_BEC)
            If _mod_bec Is Nothing Then
                _mod_bec = New MOD_POZO_BEC()
            End If

            _mod_bec = value
            RaisePropertyChanged("ModBEC")
        End Set
    End Property
    'Modelo General
    Private _mod_general As MOD_POZO_GENERAL
    Public Property ModGeneral() As MOD_POZO_GENERAL
        Get


            Return _mod_general
        End Get
        Set(value As MOD_POZO_GENERAL)
            _mod_general = value

            RaisePropertyChanged("ModGeneral")
        End Set
    End Property


    Private _aforos As List(Of AFORO)
    Public Property Aforos As List(Of AFORO)
        Get
            Return _aforos
        End Get
        Set(value As List(Of AFORO))
            _aforos = value
            RaisePropertyChanged("Aforos")
        End Set
    End Property
    'Configuracion Administrador
    Private _id_mod_pozo As String
    Public Property IdModPozo() As String
        Get
            Return _id_mod_pozo
        End Get
        Set(value As String)
            _id_mod_pozo = value


            LoadCharts(_id_mod_pozo)
            RaisePropertyChanged("IdModPozo")
        End Set
    End Property
    'Private _mod_configuracion As CONFIGURACION_ADMINISTRADOR
    'Public Property ModConfiguracion() As CONFIGURACION_ADMINISTRADOR
    '    Get
    '        Return _mod_configuracion
    '    End Get
    '    Set(value As CONFIGURACION_ADMINISTRADOR)




    '        _mod_configuracion = value

    '        If _mod_configuracion IsNot Nothing Then

    '            Select Case _mod_configuracion.ESTATUS
    '                Case 0
    '                    If VwGeneral IsNot Nothing Then
    '                        Prog = "Última configuración: " & VwGeneral.FECHAMODELO.GetValueOrDefault().ToString("MM/dd/yyyy")
    '                    Else
    '                        Prog = "Ninguno"
    '                    End If

    '                Case 1
    '                    Prog = "Programado: " & _mod_configuracion.FECHA_PROGRAMACION.ToString("MM/dd/yyyy")
    '                Case 2
    '                    Prog = "Generado: " & _mod_configuracion.FECHA_PROGRAMACION.ToString("MM/dd/yyyy")

    '            End Select
    '        End If

    '        'If _mod_configuracion IsNot Nothing AndAlso _mod_configuracion.ESTATUS = 1 Then
    '        '    Prog = "Programado: " & _mod_configuracion.FECHA_PROGRAMACION.ToString("MM/dd/yyyy")
    '        'End If

    '        RaisePropertyChanged("ModConfiguracion")
    '    End Set
    'End Property
    Private _pozo As String
    Public Property Pozo As String
        Get
            Return _pozo
        End Get

        Set(value As String)
            _pozo = value
        End Set
    End Property
    Private _configuraciones As List(Of VW_MOD_POZO)
    Public Property Configuraciones As List(Of VW_MOD_POZO)
        Get
            Return _configuraciones
        End Get
        Set(value As List(Of VW_MOD_POZO))
            _configuraciones = value
            RaisePropertyChanged("Configuraciones")
        End Set
    End Property

    'GRAFICAS
    '============================================================
    'Datos Chart1 
    Sub LoadIPR(ByVal model As VW_EDO_GENERAL)
        Dim PTest1(2), RTEOTest1(2) As Double


        PTest1(0) = 0
        PTest1(1) = model.PTEST
        PTest1(2) = model.PTEST
        RTEOTest1(0) = model.QTEST
        RTEOTest1(1) = model.QTEST
        RTEOTest1(2) = 0




        Dim vlp = db.VLP_IPR.Where(Function(w) w.IDMODPOZO = model.IDMODPOZO And w.ENDRECORD Is Nothing).SingleOrDefault()
        Dim vlp_detalles = db.VLP_IPR_DETALLE.Where(Function(w) w.IDVLPIPR = vlp.IDVLPIPR And w.ENDRECORD Is Nothing).OrderBy(Function(o) o.VLP_RTEL).ToList()


        Dim IPR_RTEL(vlp_detalles.Count - 1) As Double
        Dim IPR_WPF(vlp_detalles.Count - 1) As Double

        Dim NdatAux As Integer
        NdatAux = 19
        Dim Xaux(vlp_detalles.Count - 1), Yaux(vlp_detalles.Count - 1) As Double



        For i = 0 To vlp_detalles.Count - 1
            IPR_RTEL(i) = vlp_detalles(i).IPR_RTEL
            IPR_WPF(i) = vlp_detalles(i).IPR_PWF


            Xaux(i) = vlp_detalles(i).VLP_RTEL
            Yaux(i) = vlp_detalles(i).VLP_PWF
        Next i

        'line1
        grfProductividad.TChart1.Series(0).Title = vlp.TITULO2 'IPR
        grfProductividad.TChart1.Series(0).Add(IPR_RTEL, IPR_WPF)

        'line2
        grfProductividad.TChart1.Series(1).Add(RTEOTest1, PTest1)
        grfProductividad.TChart1.Series(1).Legend.Visible = False

        'line3
        grfProductividad.TChart1.Series(2).Add(Xaux, Yaux)
        grfProductividad.TChart1.Series(2).Title = vlp.TITULO1




    End Sub
    'Datos Chart2
    Sub LoadCorrela(ByVal model As VW_EDO_GENERAL)
        Dim PTest1(2), PrfTest1(2) As Double
        Dim PTest2(1), PrfTest2(1) As Double
        Dim xaux As New Dictionary(Of String, ArrayList)
        Dim yaux As New Dictionary(Of String, ArrayList)


        Dim cat_correlas = db.CAT_CORRELACION.Where(Function(w) w.ENDRECORD Is Nothing).OrderBy(Function(o) o.NUM).ToDictionary(Function(d) d.IDCATCORRELACION, Function(d) d.NOMBRE)
        Dim correlaciones = (From correlas In db.VW_CORRELACIONES Where correlas.IDMODPOZO = model.IDMODPOZO Order By correlas.IDCORRELACION, correlas.PROFMD).ToList() 'db.VW_CORRELACIONES.Where(Function(w) w.IDMODPOZO = IdModPozo).OrderBy(Function(o) o.IDCORRELACION).OrderBy(Function(o) o.IDCORRELACION).OrderBy(Function(o) o.PROFMD).ToList()



        For Each correlacion In correlaciones
            If xaux.ContainsKey(correlacion.IDCATCORRELACION) = False Then



                xaux.Add(correlacion.IDCATCORRELACION, New ArrayList())
                yaux.Add(correlacion.IDCATCORRELACION, New ArrayList())
            End If
            xaux(correlacion.IDCATCORRELACION).Add(Math.Round(correlacion.PRES, 2))
            yaux(correlacion.IDCATCORRELACION).Add(Math.Round(correlacion.PROFMD, 2))
        Next


        Select Case model.LIFTMETHOD
            Case 1 'BN
                'PrfTest1(0) = agujero.mecanicos(agujero.mecanicos.Count - 1).MD  '100 '.NivMedDisp.Val
                ' PrfTest1(1) = agujero.mecanicos(agujero.mecanicos.Count - 1).MD '100'.NivMedDisp.Val
                'PrfTest1(0) = VwGeneral '.Prof_BEC.Val Esperar nuevo formulario
               ' PrfTest1(1) = bec.PROF_BEC '.Prof_BEC.Val Esperar nuevo formulario

            Case 2 'BEC
                PrfTest1(0) = model.PROF_BEC '.Prof_BEC.Val Esperar nuevo formulario
                PrfTest1(1) = model.PROF_BEC '.Prof_BEC.Val Esperar nuevo formulario

                '    Case 9
        End Select

        PTest1(0) = 0
        PTest1(1) = model.PTEST
        PTest1(2) = model.PTEST

        PrfTest1(2) = 0


        grfCorrelacion.TChart1.Series(0).Add(PTest1, PrfTest1) 'Revisar el PrfTest1 esta erroneo
        grfCorrelacion.TChart1.Series(0).Legend.Visible = False



        For i = 0 To xaux.Count - 1

            grfCorrelacion.TChart1.Series(i + 1).Title = cat_correlas(xaux.Keys(i))



            Dim x = xaux.Values(i).ToArray(GetType(Double))
            Dim y = yaux.Values(i).ToArray(GetType(Double))
            grfCorrelacion.TChart1.Series(i + 1).Add(x, y)
        Next i




    End Sub
    'Datos TChart3
    Sub LoadVLPIPR(ByVal model As VW_EDO_GENERAL)

        '================CONSULTAS==================
        'Dim vlp_gastos = db.VLP_IPR_GASTO_INYECCION.Where(Function(w) w.IDMODPOZO = IdModPozo And w.ENDRECORD Is Nothing).OrderBy(Function(o) o.QL).ToList()
        Dim vlp_gastos = db.VW_VLP_GASTOS.Where(Function(w) w.IDMODPOZO = model.IDMODPOZO).OrderBy(Function(o) o.XAUX).ToList()
        ' (From vlp In db.VW_VLP_GASTOS Where vlp.IDMODPOZO = IdModPozo Order By vlp.TITULO).ToList() ' db.VW_VLP_GASTOS.Where(Function(w) w.IDMODPOZO = IdModPozo).ToList()
        '===========TERMINA CONSULTAS===============
        Dim xaux As New Dictionary(Of String, ArrayList)
        Dim yaux As New Dictionary(Of String, ArrayList)

        Dim NumGraf As Integer = 4
        Dim NumDat As Integer = 20

        Dim RTEOTest1(2), PTest1(2) As Double


        RTEOTest1(0) = 0
        RTEOTest1(1) = model.QTEST
        RTEOTest1(2) = model.QTEST

        PTest1(0) = model.PTEST
        PTest1(1) = model.PTEST
        PTest1(2) = 0
        'line1
        grfVpl.TChart1.Series(0).Add(RTEOTest1, PTest1)
        grfVpl.TChart1.Series(0).Legend.Visible = False


        Dim NdatAux As Integer
        NdatAux = NumDat - 1



        For Each gasto In vlp_gastos
            If gasto.IS_VLP And xaux.ContainsKey(gasto.TITULO.ToString()) = False Then
                xaux.Add(gasto.TITULO.ToString(), New ArrayList())
                yaux.Add(gasto.TITULO.ToString(), New ArrayList())
                Exit For
            End If

        Next


        For Each gasto In vlp_gastos

            If xaux.ContainsKey(gasto.TITULO.ToString()) = False And gasto.IS_VLP = 0 Then
                xaux.Add(gasto.TITULO.ToString(), New ArrayList())
                yaux.Add(gasto.TITULO.ToString(), New ArrayList())
            End If

            xaux(gasto.TITULO.ToString()).Add(gasto.XAUX)
            yaux(gasto.TITULO.ToString()).Add(gasto.YAUX)

        Next


        Select Case model.LIFTMETHOD
            Case 1
                'grfVpl.TChart1.Text = "VLP/IPR-Sensibilidad de QgIny"
                'grfVpl.TChart1.Axes.Left.Title.Text = "Presión de Fondo Fluyendo [Kg/cm2]"
                'grfVpl.TChart1.Axes.Bottom.Title.Text = "Gasto de Líquido [STBPD]"

                'For i = 0 To vlp_gastos.Count - 1
                '    Xaux(i) = vlp_gastos(i).QL ' .VLPIPR_RTEL(1, J)
                '    Yaux(i) = vlp_gastos(i).QO '.VLPIPR_PWF(1, J)
                'Next


                ''grfVpl.TChart1.Series.Add(getLinea())
                'grfVpl.TChart1.Series(1).Add(Xaux, Yaux)
                'grfVpl.TChart1.Series(1).Title = "VLP"

                'Dim titulo_g As String = ""
                'Dim serie As Integer = 0
                'Dim npoints As Integer = 0


                'For Each vlp In vlp_gasto_detalles

                '    If titulo_g <> Format(vlp.TITULO, "0.00") Then

                '        If titulo_g <> "" Then
                '            grfVpl.TChart1.Series(serie + 2).Title = titulo_g
                '            grfVpl.TChart1.Series(serie + 2).Add(Xaux, Yaux)
                '            serie += 1
                '        End If


                '        'Yaux as New Double

                '        titulo_g = Format(vlp.TITULO, "0.00")
                '        npoints = 0
                '        Xaux(npoints) = vlp.PWFVLP
                '        Yaux(npoints) = vlp.PWFIPR



                '    Else

                '        Xaux(npoints) = vlp.PWFVLP
                '        Yaux(npoints) = vlp.PWFIPR
                '        npoints += 1
                '    End If
                'Next


                grfVpl.TChart1.Text = "VLP/IPR-Sensibilidad de QgIny"
                grfVpl.TChart1.Axes.Left.Title.Text = "Presión de Fondo Fluyendo [Kg/cm2]"
                grfVpl.TChart1.Axes.Bottom.Title.Text = "Gasto de Líquido [STBPD]"

            Case 2
                grfVpl.TChart1.Text = "VLP/Presión de Descargar Sensibilizando Frecuencia"
                grfVpl.TChart1.Axes.Left.Title.Text = "Presión de Descarga [Kg/cm2]"
                grfVpl.TChart1.Axes.Bottom.Title.Text = "Gasto de Líquido [STBPD]"
        End Select

        Dim titulos As List(Of String) = xaux.Keys.ToList()

        For i = 0 To titulos.Count - 1

            Dim x = xaux.Values(i).ToArray(GetType(Double))
            Dim y = yaux.Values(i).ToArray(GetType(Double))
            grfVpl.TChart1.Series(i + 1).Title = titulos(i) 'xaux.Keys(i)
            grfVpl.TChart1.Series(i + 1).Add(x, y)

        Next i
    End Sub
    'Datos Tchart4
    Sub LoadGAS(ByVal model As VW_EDO_GENERAL)
        Dim comportamientos = (From com In db.COMPORTAMIENTO_GAS Join det In db.COMPORTAMIENTO_GAS_DETALLES On com.IDCOMPORTAMIENTOGAS Equals det.IDCOMPORTAMIENTOGAS Where com.IDMODPOZO = model.IDMODPOZO).ToList()

        Dim xaux As New Dictionary(Of String, ArrayList)
        Dim yaux As New Dictionary(Of String, ArrayList)

        For Each com In comportamientos
            If xaux.ContainsKey(com.com.TITULO.ToString()) = False Then
                xaux.Add(com.com.TITULO.ToString(), New ArrayList())
                yaux.Add(com.com.TITULO.ToString(), New ArrayList())
            End If
            xaux(com.com.TITULO.ToString()).Add(com.det.XAUX)
            yaux(com.com.TITULO.ToString()).Add(com.det.YAUX)

        Next

        Select Case model.LIFTMETHOD
            Case 1
                grfGas.TChart1.Text = "Comportamiento del Gas de BN"
                grfGas.TChart1.Axes.Left.Title.Text = "Gasto de Líquido [STBPD]"
                grfGas.TChart1.Axes.Bottom.Title.Text = "Gasto de Inyección de gas [MMSCFPD]"

                Dim QgiTest1(1), QliqTest1(1) As Double
                Dim QgiTest2(1), QliqTest2(1) As Double

                Dim GlRate As Double = model.GLRATE

                QgiTest1(0) = GlRate
                QgiTest1(1) = GlRate
                QliqTest1(0) = model.QTEST
                QliqTest1(1) = model.QTEST
                QgiTest2(0) = 0
                QgiTest2(1) = GlRate
                QliqTest2(0) = model.QTEST
                QliqTest2(1) = model.QTEST

                grfGas.TChart1.Series(0).Add(QgiTest1, QliqTest1)
                grfGas.TChart1.Series(1).Add(QgiTest2, QliqTest2)
                grfGas.TChart1.Series(2).Title = "Qgi"

                Dim x = xaux.Values(0).ToArray(GetType(Double))
                Dim y = yaux.Values(0).ToArray(GetType(Double))
                grfGas.TChart1.Series(2).Add(x, y)


            Case 2
                grfGas.TChart1.Text = "Carta de la Bomba "
                grfGas.TChart1.Axes.Left.Title.Text = "Carga de la Bomba (m)"
                grfGas.TChart1.Axes.Bottom.Title.Text = "Gasto de Líquido [RBPD]"

                Dim titulos As List(Of String) = xaux.Keys.ToList()



                For i = 0 To titulos.Count - 1

                    Dim x = xaux.Values(i).ToArray(GetType(Double))
                    Dim y = yaux.Values(i).ToArray(GetType(Double))
                    grfGas.TChart1.Series(i).Title = titulos(i) 'xaux.Keys(i)
                    grfGas.TChart1.Series(i).Add(x, y)

                Next i

                grfGas.TChart1.Series(7).Legend.Visible = False

        End Select
    End Sub
    'Datos Tchart5
    Sub LoadDiag(ByVal model As VW_EDO_GENERAL)
        Dim diagnosticos = db.VW_DIAGNOSTICOS.Where(Function(w) w.IDMODPOZO = model.IDMODPOZO).OrderBy(Function(o) o.YAUX).ToList()
        Dim xaux As New Dictionary(Of String, ArrayList)
        Dim yaux As New Dictionary(Of String, ArrayList)


        For Each diag In diagnosticos
            If xaux.ContainsKey(diag.TITULO.ToString()) = False Then
                xaux.Add(diag.TITULO.ToString(), New ArrayList())
                yaux.Add(diag.TITULO.ToString(), New ArrayList())
            End If
            xaux(diag.TITULO.ToString()).Add(diag.XAUX)
            yaux(diag.TITULO.ToString()).Add(diag.YAUX)

        Next

        Select Case model.LIFTMETHOD
            Case 1
                grfDiag.TChart1.Text = "Diagnóstico de BNC"
                Dim titulos As List(Of String) = xaux.Keys.ToList()



                For i = 0 To titulos.Count - 1

                    Dim x = xaux.Values(i).ToArray(GetType(Double))
                    Dim y = yaux.Values(i).ToArray(GetType(Double))
                    grfDiag.TChart1.Series(i).Title = titulos(i) 'xaux.Keys(i)
                    grfDiag.TChart1.Series(i).Add(x, y)

                Next i

                grfDiag.TChart1.Series(3).Legend.Visible = False
            Case 2
                grfDiag.TChart1.Text = "Diagnóstico de BEC"

                Dim titulos As List(Of String) = xaux.Keys.ToList()



                For i = 0 To titulos.Count - 1

                    Dim x = xaux.Values(i).ToArray(GetType(Double))
                    Dim y = yaux.Values(i).ToArray(GetType(Double))
                    grfDiag.TChart1.Series(i).Title = titulos(i) 'xaux.Keys(i)
                    grfDiag.TChart1.Series(i).Add(x, y)

                Next i

                grfDiag.TChart1.Series(3).Legend.Visible = False
        End Select

    End Sub
    'Datos Tchart6
    Sub LoadWC(ByVal model As VW_EDO_GENERAL)
        Dim productividad = db.VW_PRODUCTIVIDAD.Where(Function(w) w.IDMODPOZO = model.IDMODPOZO And w.ENDRECORD Is Nothing).OrderBy(Function(o) o.WC_RES).ToList()

        Dim xaux As New Dictionary(Of String, ArrayList)
        Dim yaux As New Dictionary(Of String, ArrayList)

        For Each prod In productividad
            If xaux.ContainsKey(prod.TITULO.ToString()) = False Then
                xaux.Add(prod.TITULO.ToString(), New ArrayList())
                yaux.Add(prod.TITULO.ToString(), New ArrayList())
            End If
            xaux(prod.TITULO.ToString()).Add(prod.WC_RES)
            yaux(prod.TITULO.ToString()).Add(prod.QGI_RES)

        Next

        Select Case model.LIFTMETHOD
            Case 1
                grfWc.TChart1.Text = "Sensibilidad de Corte de Agua-Gasto de Gas de Inyección"
                grfWc.TChart1.Axes.Left.Title.Text = "Gasto de Líquido [STBPD]"
                grfWc.TChart1.Axes.Bottom.Title.Text = "Corte de Agua [%]"
                Dim titulos As List(Of String) = xaux.Keys.ToList()



                For i = 0 To titulos.Count - 1

                    Dim x = xaux.Values(i).ToArray(GetType(Double))
                    Dim y = yaux.Values(i).ToArray(GetType(Double))
                    grfWc.TChart1.Series(i).Title = titulos(i) 'xaux.Keys(i)
                    grfWc.TChart1.Series(i).Add(x, y)

                Next i
            Case 2

                grfWc.TChart1.Text = "Sensibilidad de Corte de Agua-Frecuencia de BEC"
                grfWc.TChart1.Axes.Left.Title.Text = "Gasto de Líquido [STBPD]"
                grfWc.TChart1.Axes.Bottom.Title.Text = "Corte de Agua [%]"


                Dim titulos As List(Of String) = xaux.Keys.ToList()



                For i = 0 To titulos.Count - 1

                    Dim x = xaux.Values(i).ToArray(GetType(Double))
                    Dim y = yaux.Values(i).ToArray(GetType(Double))
                    grfWc.TChart1.Series(i).Title = titulos(i) 'xaux.Keys(i)
                    grfWc.TChart1.Series(i).Add(x, y)

                Next i
        End Select

    End Sub




    Sub LoadGeneral()

        'Inicial.Init()


        'ModGeneral.COMPANY = ht.Item("COMPANY")
        'ModGeneral.FIELD = ht.Item("FIELD")
        'ModGeneral.LOCATIONS = ht.Item("LOCATIONS")
        'ModGeneral.WELL = ht.Item("WELL")
        'ModGeneral.PLATAFORM = ht.Item("PLATAFORM")
        'ModGeneral.ANALYST = ht.Item("ANALYST")


        '''DATOS DE ENTRADA
        'Fluid = ht.Item("FLUID")
        'OutputRes = ht.Item("OUTPUTRES")
        'PvtModel = ht.Item("PVTMODEL")
        'IprMethod = ht.Item("IPRMETHOD")
        'Completion = ht.Item("COMPLETION")
        'Separator = ht.Item("SEPARATOR")
        'GravelPack = ht.Item("GRAVELPACK")
        'Emulsion = ht.Item("EMULSION")
        'InflowType = ht.Item("INFLOWTYPE")
        'Hydrate = ht.Item("HYDRATE")
        'GasConing = ht.Item("GASCONING")


        '_mod_general.PRES = ht.Item("PRES")
        '_mod_general.TRES = ht.Item("TRES")
        '_mod_general.WC = ht.Item("WC")
        '_mod_general.TOTGOR = ht.Item("TOTGOR")
        '_mod_general.QTEST = ht.Item("QTEST")
        '_mod_general.PTEST = ht.Item("PTEST")


        '_mod_general.RESPERM = ht.Item("RESPERM")
        '_mod_general.THICKNESS = ht.Item("THICKNESS")
        '_mod_general.DRAINAGE = ht.Item("DRAINAGE")
        '_mod_general.DIETZ = ht.Item("DIETZ")
        '_mod_general.WBR = ht.Item("WBR")
        '_mod_general.SKIN = ht.Item("SKIN")

        '_mod_general.HTC = ht.Item("HTC")
        '_mod_general.THPD = ht.Item("THPD")
        '_mod_general.THTD = ht.Item("THTD")
        '_mod_general.TRPRES = ht.Item("TRPRES")
        '_mod_general.THTE = ht.Item("THTE")
        '_mod_general.RGATOTALAFORO = ht.Item("RGATOTALAFORO")
        '_mod_general.RANGESYSTEM = ht.Item("RANGESYSTEM")

        '''BEC
        '_mod_bec.FRECMIN = ht.Item("FRECMIN")
        '_mod_bec.FRECMAX = ht.Item("FRECMAX")
        '_mod_bec.PROF_BEC = ht.Item("PROF")
        '_mod_bec.FREC_BEC = ht.Item("FREC")
        '_mod_bec.ODMAX_BEC = ht.Item("ODMAX")
        '_mod_bec.LONGCABLE_BEC = ht.Item("LONGCABLE")
        '_mod_bec.EFISEPGAS_BEC = ht.Item("EFISEPGAS")
        '_mod_bec.ETAPAS_BEC = ht.Item("ETAPAS")
        '_mod_bec.VOLTSUP_BEC = ht.Item("VOLTSUP")
        '_mod_bec.DESGASTE_BEC = ht.Item("DESGASTE")
        '_mod_bec.REDUCGAS_BEC = ht.Item("REDUCGAS")
        '_mod_bec.BOMBA_BEC = ht.Item("BOMBA")
        '_mod_bec.MOTOR_BEC = ht.Item("MOTOR")
        '_mod_bec.POTENCIAMOTOR_BEC = ht.Item("POTENCIAMOTOR")

        '_mod_bec.CABLE_BEC = ht.Item("CABLE")
        '_mod_bec.PRESUC_BEC = ht.Item("PRESUC")
        '_mod_bec.CORRIENTE_BEC = ht.Item("CORRIENTE")
        '_mod_bec.POTENCIA_BEC = ht.Item("POTENCIABEC")
        '_mod_bec.PREDES_BEC = ht.Item("PREDES")

        ''TempModel = general.Item("TEMPMODEL")
        ''IprMethod = general.Item("IPRMETHOD")
        ''RangeSystem = general.Item("RANGESYSTEM")

        '''_mod_bnc.ENTRY = general.Item("ENTRY")
        '''_mod_bnc.GRAVITY = general.Item("GRAVITY")
        '''_mod_bnc.H2S = general.Item("H2S")
        '''_mod_bnc.CO2 = general.Item("CO2")
        '''_mod_bnc.N2 = general.Item("N2")
        '''_mod_bnc.GLRINY = general.Item("GLRINY")
        '''_mod_bnc.GLRATE = general.Item("GLRATE")
        '''_mod_bnc.VALVEDEPTH = general.Item("VALVEDEPTH")
        '''_mod_bnc.DIAMVAL = general.Item("DIAMVALBNC")
        '''_mod_bnc.QGIMIN = general.Item("QGINYMIN")
        '''_mod_bnc.QGIMAX = general.Item("QGINYMAX")

        'Method = ht.Item("METHOD")
        'H2s = ht.Item("H2S")
        'Gravity = ht("GRAVITY")
        'Entry = ht.Item("ENTRY")
        'N2 = ht.Item("N2")
        'Co2 = ht.Item("CO2")
        'QgiMin = ht.Item("QGIMIN")
        'QgiMax = ht.Item("QGIMAX")
        'ValveDepth = ht.Item("VALVEDEPTH")
        'DiamVal = ht.Item("DIAMVAL")
        'GlRiny = ht.Item("GLRINY")
        'GlRate = ht.Item("GLRATE")




        ModGeneral.PRES = Pres
        ModGeneral.TRES = Tres
        ModGeneral.WC = Wc
        ModGeneral.TOTGOR = Totgor
        ModGeneral.QTEST = Qtest
        ModGeneral.PTEST = Ptest
        ModGeneral.THICKNESS = Thickness
        ModGeneral.RESPERM = Resperm
        ModGeneral.DRAINAGE = Drainage
        ModGeneral.DIETZ = Dietz
        ModGeneral.WBR = Wbr
        ModGeneral.SKIN = Skin
        ModGeneral.HTC = Htc
        ModGeneral.THPD = Thpd
        ModGeneral.THTD = Thtd
        ModGeneral.TRPRES = Trpres
        ModGeneral.RGATOTALAFORO = RgaTotalAforo
        ModGeneral.DATGENDATE = Datgendate



        ModGeneral.FLUID = Fluid
        ModGeneral.OUTPUTRES = OutputRes
        ModGeneral.PVTMODEL = PvtModel
        ModGeneral.COMPLETION = Completion
        ModGeneral.SEPARATOR = Separator
        ModGeneral.GRAVELPACK = GravelPack
        ModGeneral.EMULSION = Emulsion
        ModGeneral.INFLOWTYPE = InflowType
        ModGeneral.HYDRATE = Hydrate
        ModGeneral.GASCONING = GasConing
        ModGeneral.WATVIS = WatVis
        ModGeneral.VISMOD = VisMod
        ModGeneral.IPRMETHOD = IprMethod
        ModGeneral.FLOWTYPE = FlowType
        ModGeneral.COMPACT = Compact
        ModGeneral.WELLTYPE = WellType
        ModGeneral.IRElK = Irelk
        ModGeneral.LIFTMETHOD = LiftMethod
        ModGeneral.MGSKINMETHOD = MgSkinMethod
        ModGeneral.LIFTYPE = LiftType
        ModGeneral.MGSKINMODEL = MgSkinModel
        ModGeneral.PREDICT = Predict
        ModGeneral.TEMPMODEL = TempModel
        ModGeneral.RANGESYSTEM = RangeSystem


    End Sub
    'Inicializa componentes de la Grafica
    '=============================================
    Sub Initialize(ByVal hst_prod As WindowsFormsHost, ByVal hst_correla As WindowsFormsHost, ByVal hst_vpl As WindowsFormsHost, ByVal hst_gas As WindowsFormsHost, ByVal hst_diag As WindowsFormsHost, ByVal hst_wc As WindowsFormsHost)

        Inicial.Init(LiftMethod)


        hst_prod.Child = grfProductividad
        hst_correla.Child = grfCorrelacion
        hst_vpl.Child = grfVpl
        hst_gas.Child = grfGas
        hst_diag.Child = grfDiag
        hst_wc.Child = grfWc


    End Sub
#Region "Variables"
    Private _id_agujero As String
    Public Property IdAgujero() As String
        Get
            Return _id_agujero
        End Get
        Set(value As String)
            _id_agujero = value
            RaisePropertyChanged("IdAgujero")
        End Set
    End Property
    Private _id_pozo As String
    Public Property IdPozo() As String
        Get
            Return _id_pozo
        End Get
        Set(value As String)
            _id_pozo = value
            RaisePropertyChanged("IdPozo")
        End Set
    End Property

    Private _fecha_prueba As String
    Public Property FechaPrueba() As String
        Get
            Return _fecha_prueba
        End Get
        Set(value As String)
            _fecha_prueba = value
        End Set
    End Property

    'LECTURA DE DATOS (REVISAR POR DEPRECIACION)
    Private Function Lectura_Datos() As Boolean
        Dim resul As Boolean = False
        Try
            'Resu_SQL = Analisis.ConvertirDatatableMatrizString(objconsulta.getResultadoModeloGeneral(IdModPozo)) 'ESTE SE VA A SUSTITUIR
            'If Resu_SQL.GetUpperBound(0) >= 0 Then

            If IdModPozo IsNot Nothing Then
                ' ESTADO MECANICO
                EdoMec_SQL = Analisis.ConvertirDatatableMatrizString(objconsulta.getResultadoMecanico(IdAgujero, IdModPozo)) 'ESTO FALTA QUE INTEGRE DAVID
                NumDatEdoMec = EdoMec_SQL.GetUpperBound(0) '+ 1
                If NumDatEdoMec > 1 Then
                    Analisis.recortarNombre(EdoMec_SQL, 5, 15)
                    ' TRAYECTORIA
                    Trayecto_SQL = Analisis.seleccionPuntostrayectoria(objconsulta.getResultadoTrayectoria(IdAgujero), EdoMec_SQL(EdoMec_SQL.GetUpperBound(0), 7))
                    NumDatTrayecto = Trayecto_SQL.GetUpperBound(0)
                    If NumDatTrayecto > 0 Then
                        'objconsulta.guardarTrayectoria(Trayecto_SQL, IdPozo) 'AQUI SE DEBERIA DE GUARDAR EN LA BASE DE DATOS LA TRAYECTORIA
                        ' TEMPERATURA
                        Dim tb As DataTable = objconsulta.getDatosFormacion(IdAgujero, FechaPrueba)
                        'Dim tb As DataTable = obBD.getDatosTemperatura(IDAGUJERO, fechaprueba)
                        If tb.Rows.Count > 0 Then
                            Dim b = Resu_SQL(Resu_SQL.GetUpperBound(0), 66) 'TEMPERATURA EN LA PRIMERA CABEZA
                            Dim a = ((tb.Rows(0).Item(1) - b) / (tb.Rows(0).Item(3) - Trayecto_SQL(0, 1)))
                            Temperatura_SQL = Analisis.obtenerTemperatura(Trayecto_SQL, a, b)
                            NumDatTemp = Temperatura_SQL.GetUpperBound(0)
                            ' objconsulta.guardarTemperatura(Temperatura_SQL, IdPozo)
                            resul = True
                        End If
                    End If
                End If
            End If
        Catch ex As Exception
            Throw New Exception(ex.ToString)
        End Try
        Return resul
    End Function
    Private _list_mecanicos As List(Of VW_EDO_MECANICO)

    'VARIABLES 
    Private _pres As Double
    Public Property Pres As Double
        Get
            Return _pres
        End Get
        Set(value As Double)
            _pres = value

            RaisePropertyChanged("Pres")

        End Set
    End Property
    Private _tres As Double
    Public Property Tres As Double
        Get
            Return _tres
        End Get
        Set(value As Double)
            _tres = value
            RaisePropertyChanged("Tres")

        End Set
    End Property

    Private _wc As Double
    Public Property Wc As Double
        Get
            Return _wc
        End Get
        Set(value As Double)
            _wc = value
            RaisePropertyChanged("Wc")

        End Set
    End Property
    Private _totgor As Double
    Public Property Totgor As Double
        Get
            Return _totgor
        End Get
        Set(value As Double)
            _totgor = value
            RaisePropertyChanged("Totgor")

        End Set
    End Property
    Private _qtest As Double
    Public Property Qtest As Double
        Get
            Return _qtest
        End Get
        Set(value As Double)
            _qtest = value
            RaisePropertyChanged("Qtest")

        End Set
    End Property
    Private _ptest As Double
    Public Property Ptest As Double
        Get
            Return _ptest
        End Get
        Set(value As Double)
            _ptest = value
            RaisePropertyChanged("Ptest")

        End Set
    End Property
    Private _resperm As Double
    Public Property Resperm As Double
        Get
            Return _resperm
        End Get
        Set(value As Double)
            _resperm = value
            RaisePropertyChanged("Resperm")

        End Set
    End Property
    Private _thickness As Double
    Public Property Thickness As Double
        Get
            Return _thickness
        End Get
        Set(value As Double)
            _thickness = value
            RaisePropertyChanged("Thickness")

        End Set
    End Property
    Private _drainage As Double
    Public Property Drainage As Double
        Get
            Return _drainage
        End Get
        Set(value As Double)
            _drainage = value
            RaisePropertyChanged("Drainage")
        End Set
    End Property
    Private _dietz As Double
    Public Property Dietz As Double
        Get
            Return _dietz
        End Get
        Set(value As Double)
            _dietz = value
            RaisePropertyChanged("Dietz")
        End Set
    End Property
    Private _wbr As Double
    Public Property Wbr As Double
        Get
            Return _wbr
        End Get
        Set(value As Double)
            _wbr = value
            RaisePropertyChanged("Wbr")
        End Set
    End Property
    Private _skin As Double
    Public Property Skin As Double
        Get
            Return _skin
        End Get
        Set(value As Double)
            _skin = value
            RaisePropertyChanged("Skin")
        End Set
    End Property
    Private _htc As Double
    Public Property Htc As Double
        Get
            Return _htc
        End Get
        Set(value As Double)
            _htc = value
            RaisePropertyChanged("Htc")
        End Set
    End Property
    Private _thpd As Double
    Public Property Thpd As Double
        Get
            Return _thpd
        End Get
        Set(value As Double)
            _thpd = value
            RaisePropertyChanged("Thpd")
        End Set
    End Property
    Private _thtd As Double
    Public Property Thtd As Double
        Get
            Return _thtd
        End Get
        Set(value As Double)
            _thtd = value
            RaisePropertyChanged("Thtd")
        End Set
    End Property
    Private _trpres As Double
    Public Property Trpres As Double
        Get
            Return _trpres
        End Get
        Set(value As Double)
            _trpres = value
            RaisePropertyChanged("Trpres")
        End Set
    End Property
    Private _rgatotalaforo As Double
    Public Property RgaTotalAforo As Double
        Get
            Return _rgatotalaforo
        End Get
        Set(value As Double)
            _rgatotalaforo = value
            RaisePropertyChanged("RgaTotalAforo")
        End Set
    End Property


    Private _comenta As String
    Public Property Comenta As String
        Get
            Return _comenta
        End Get
        Set(value As String)
            _comenta = value
            RaisePropertyChanged("Comenta")
        End Set
    End Property
    Private _fluid As Double
    Public Property Fluid() As Double
        Get
            Return _fluid
        End Get
        Set(value As Double)
            _fluid = value
            RaisePropertyChanged("Fluid")
        End Set
    End Property
    Private _output_res As Double
    Public Property OutputRes() As Double
        Get
            Return _output_res
        End Get
        Set(value As Double)
            _output_res = value
            RaisePropertyChanged("OutputRes")
        End Set
    End Property
    Private _pvt_model As Double
    Public Property PvtModel() As Double
        Get
            Return _pvt_model
        End Get
        Set(value As Double)
            _pvt_model = value
            RaisePropertyChanged("PvtModel")
        End Set
    End Property
    Private _completion As Double
    Public Property Completion() As Double
        Get
            Return _completion
        End Get
        Set(value As Double)
            _completion = value
            RaisePropertyChanged("Completion")
        End Set
    End Property
    Private _separator As Double
    Public Property Separator() As Double
        Get
            Return _separator

        End Get
        Set(value As Double)
            _separator = value
            RaisePropertyChanged("Separator")
        End Set
    End Property
    Private _gravel_pack As Double
    Public Property GravelPack() As Double
        Get
            Return _gravel_pack
        End Get
        Set(value As Double)
            _gravel_pack = value
            RaisePropertyChanged("GravelPack")
        End Set
    End Property
    Private _emulsion As Double
    Public Property Emulsion() As Double
        Get
            Return _emulsion
        End Get
        Set(value As Double)
            _emulsion = value
            RaisePropertyChanged("Emulsion")
        End Set
    End Property
    Private _inflow_type As Double
    Public Property InflowType() As Double
        Get
            Return _inflow_type
        End Get
        Set(value As Double)
            _inflow_type = value
            RaisePropertyChanged("InflowType")
        End Set
    End Property
    Private _hydrate As Double
    Public Property Hydrate() As Double
        Get
            Return _hydrate
        End Get
        Set(value As Double)
            _hydrate = value
            RaisePropertyChanged("Hydrate")
        End Set
    End Property
    Private _gas_coning As Double
    Public Property GasConing() As Double
        Get
            Return _gas_coning
        End Get
        Set(value As Double)
            _gas_coning = value
            RaisePropertyChanged("GasConing")
        End Set
    End Property
    Private _wat_vis As Double
    Public Property WatVis() As Double
        Get
            Return _wat_vis
        End Get
        Set(value As Double)
            _wat_vis = value
            RaisePropertyChanged("WatVis")
        End Set
    End Property
    Private _vis_mod As Double
    Public Property VisMod() As Double
        Get
            Return _vis_mod
        End Get
        Set(value As Double)
            _vis_mod = value
            RaisePropertyChanged("VisMod")
        End Set
    End Property
    Private _ipr_method As Double
    Public Property IprMethod() As Double
        Get
            Return _ipr_method
        End Get
        Set(value As Double)
            _ipr_method = value
            RaisePropertyChanged("IprMethod")
        End Set
    End Property
    Private _flow_type As Double
    Public Property FlowType() As Double
        Get
            Return _flow_type
        End Get
        Set(value As Double)
            _flow_type = value
            RaisePropertyChanged("FlowType")
        End Set
    End Property
    Private _compact As Double
    Public Property Compact() As Double
        Get
            Return _compact
        End Get
        Set(value As Double)
            _compact = value
            RaisePropertyChanged("Compact")
        End Set
    End Property
    Private _well_type As Double
    Public Property WellType() As Double
        Get
            Return _well_type
        End Get
        Set(value As Double)
            _well_type = value
            RaisePropertyChanged("WellType")
        End Set
    End Property
    Private _irelk As Double
    Public Property Irelk() As Double
        Get
            Return _irelk
        End Get
        Set(value As Double)
            _irelk = value
            RaisePropertyChanged("Irelk")
        End Set
    End Property
    Private _lift_method As Double
    Public Property LiftMethod() As Double
        Get
            Return _lift_method
        End Get
        Set(value As Double)
            _lift_method = value
            RaisePropertyChanged("LiftMethod")
        End Set
    End Property
    Private _mg_skin_method As Double
    Public Property MgSkinMethod() As Double
        Get
            Return _mg_skin_method
        End Get
        Set(value As Double)
            _mg_skin_method = value
            RaisePropertyChanged("MgSkinMethod")
        End Set
    End Property
    Private _lift_type As Double
    Public Property LiftType() As Double
        Get
            Return _lift_type
        End Get
        Set(value As Double)
            _lift_type = value
            RaisePropertyChanged("LiftType")
        End Set
    End Property
    Private _mg_skin_model As Double
    Public Property MgSkinModel() As Double
        Get
            Return _mg_skin_model
        End Get
        Set(value As Double)
            _mg_skin_model = value
            RaisePropertyChanged("MgSkinModel")
        End Set
    End Property
    Private _predict As Double
    Public Property Predict() As Double
        Get
            Return _predict
        End Get
        Set(value As Double)
            _predict = value
            RaisePropertyChanged("Predict")
        End Set
    End Property
    Private _temp_model As Double
    Public Property TempModel() As Double
        Get
            Return _temp_model
        End Get
        Set(value As Double)
            _temp_model = value
            RaisePropertyChanged("TempModel")
        End Set
    End Property
    Private _datgendate As DateTime
    Public Property Datgendate() As DateTime
        Get
            Return _datgendate
        End Get
        Set(value As DateTime)
            _datgendate = value
            RaisePropertyChanged("Datgendate")
        End Set
    End Property


    'BNC
    Private _entry As Double
    Public Property Entry() As Double
        Get
            Return _entry
        End Get
        Set(value As Double)
            _entry = value
            RaisePropertyChanged("Entry")
        End Set
    End Property
    Private _method As Double
    Public Property Method() As Double
        Get
            Return _method
        End Get
        Set(value As Double)
            _method = value
            RaisePropertyChanged("Method")
        End Set
    End Property
    Private _gravity As Double
    Public Property Gravity() As Double
        Get
            Return _gravity
        End Get
        Set(value As Double)
            _gravity = value
            RaisePropertyChanged("Gravity")
        End Set
    End Property
    Private _range_system As Double
    Public Property RangeSystem() As Double
        Get
            Return _range_system
        End Get
        Set(value As Double)
            _range_system = value
            RaisePropertyChanged("RangeSystem")
        End Set
    End Property
    Private _h2s As Double
    Public Property H2s() As Double
        Get
            Return _h2s
        End Get
        Set(value As Double)
            _h2s = value
            RaisePropertyChanged("H2s")
        End Set
    End Property
    Private _co2 As Double
    Public Property Co2() As Double
        Get
            Return _co2
        End Get
        Set(value As Double)
            _co2 = value
            RaisePropertyChanged("Co2")
        End Set
    End Property
    Private _n2 As Double
    Public Property N2() As Double
        Get
            Return _n2
        End Get
        Set(value As Double)
            _n2 = value
            RaisePropertyChanged("N2")
        End Set
    End Property
    Private _glriny As Double
    Public Property GlRiny() As Double
        Get
            Return _glriny
        End Get
        Set(value As Double)
            _glriny = value
            RaisePropertyChanged("GlRiny")
        End Set
    End Property
    Private _glrate As Double
    Public Property GlRate() As Double
        Get
            Return _glrate
        End Get
        Set(value As Double)
            _glrate = value
            RaisePropertyChanged("GlRate")
        End Set
    End Property
    Private _valve_depth As Double
    Public Property ValveDepth() As Double
        Get
            Return _valve_depth
        End Get
        Set(value As Double)
            _valve_depth = value
            RaisePropertyChanged("ValveDepth")
        End Set
    End Property
    Private _diam_val As Double
    Public Property DiamVal() As Double
        Get
            Return _diam_val
        End Get
        Set(value As Double)
            _diam_val = value
            RaisePropertyChanged("DiamVal")
        End Set
    End Property
    Private _qgi_min As Double
    Public Property QgiMin() As Double
        Get
            Return _qgi_min
        End Get
        Set(value As Double)
            _qgi_min = value
            RaisePropertyChanged("QgiMin")
        End Set
    End Property
    Private _qgi_max As Double
    Public Property QgiMax() As Double
        Get
            Return _qgi_max
        End Get
        Set(value As Double)
            _qgi_max = value
            RaisePropertyChanged("QgiMax")
        End Set
    End Property
#End Region



    Private Sub OnSave()

        Try

            If AgujeroModel.Estatus = 1 Then

                MsgBox("La configuración ha sido programada por el Administrador, una vez ejecutado podra seguir actualizando")
                Exit Sub
            End If


            Dim ModPozo As New MOD_POZO()
            Dim max_md As Double = 0









            'If LiftMethod < 1 Then
            '    Throw New Exception("El Lift Method es requerido")
            'End If

            Dim trayectorias = db.VW_TRAYECTORIA.Where(Function(w) w.IDAGUJERO = IdAgujero).OrderBy(Function(o) o.PROFUNDIDADMD).ToList()

            'If trayectorias.Count = 0 Then
            '    Throw New Exception("Debe haber al menos una trayectoria registrada: " + IdAgujero)
            'End If


            If AgujeroModel.SaveMecanico() > 0 Then
                max_md = db.VW_EDO_MECANICO.Where(Function(w) w.IDAGUJERO = IdAgujero).Max(Function(m) m.MD)
            End If



            If AgujeroModel.IdModPozo IsNot Nothing AndAlso (AgujeroModel.Estatus = 1 Or AgujeroModel.Estatus = 0) Then
                ModPozo = db.MOD_POZO.Find(AgujeroModel.IdModPozo)
                ModPozo.FECHAMODELO = DateTime.Now
                ModPozo.OBSERVACIONES = Comenta
                db.Entry(ModPozo).State = Entity.EntityState.Modified
                db.SaveChanges()
            Else
                If AgujeroModel.Estatus = 2 Then
                    Throw New Exception("Configuración bloqueada temporalmente")
                End If
                ModPozo.IDMODPOZO = db.proc_insertModPozo(IdAgujero, Comenta).SingleOrDefault()
            End If



            ''GENERAL
            SaveGeneral(ModPozo)
            SaveModelo(ModPozo)

            AgujeroModel.ReloadGeneral(ModPozo.IDMODPOZO, 0)


            If max_md > 0 Then

                Dim puntos = AgujeroModel.SaveTrayectorias(max_md)
                SaveTemperatura(ModPozo, puntos)
            Else
                Throw New Exception("No hay estado mecanico")
            End If




            IdModPozo = ModPozo.IDMODPOZO
            MsgBox("Configuración guardada")
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical)
        End Try



    End Sub
    '80% FUNCIONAL
    Public Sub SaveModelo(ByVal ModPozo As MOD_POZO)
        Select Case LiftMethod
            Case 1
                Dim BncModel As New BncModel(db)
                ModBNC.ENTRY = Entry
                ModBNC.METHOD = Method

                BncModel.Save(ModPozo.IDMODPOZO, ModBNC)

            Case 2
                Dim BecModel As New BecModel(db)

                BecModel.Save(ModPozo.IDMODPOZO, ModBEC)

        End Select
    End Sub
    Public Function SaveGeneral(ByVal ModPozo As MOD_POZO) As Boolean
        Try
            ModGeneral = db.MOD_POZO_GENERAL.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).SingleOrDefault()








            If ModGeneral Is Nothing Then


                'Cargamos nuevamente los parametros
                ModGeneral = New MOD_POZO_GENERAL()
                LoadDefault()
                LoadGeneral()
                ModGeneral.IDMODPOZO = ModPozo.IDMODPOZO
                ModGeneral.IDMODPOZOGENERAL = Guid.NewGuid().ToString().ToUpper()


                db.MOD_POZO_GENERAL.Add(ModGeneral)
            Else
                LoadDefault()
                LoadGeneral()
                'ModGeneral.FLUID = Fluid
                'ModGeneral.OUTPUTRES = OutputRes
                'ModGeneral.PVTMODEL = PvtModel
                'ModGeneral.COMPLETION = Completion
                'ModGeneral.SEPARATOR = Separator
                'ModGeneral.GRAVELPACK = GravelPack
                'ModGeneral.EMULSION = Emulsion
                'ModGeneral.INFLOWTYPE = InflowType
                'ModGeneral.HYDRATE = Hydrate
                'ModGeneral.GASCONING = GasConing
                'ModGeneral.WATVIS = WatVis
                'ModGeneral.VISMOD = VisMod
                'ModGeneral.IPRMETHOD = IprMethod
                'ModGeneral.FLOWTYPE = FlowType
                'ModGeneral.COMPACT = Compact
                'ModGeneral.WELLTYPE = WellType
                'ModGeneral.IRElK = Irelk
                'ModGeneral.LIFTMETHOD = LiftMethod
                'ModGeneral.MGSKINMETHOD = MgSkinMethod
                'ModGeneral.LIFTYPE = LiftType
                'ModGeneral.MGSKINMODEL = MgSkinModel
                'ModGeneral.PREDICT = Predict
                'ModGeneral.TEMPMODEL = TempModel
                'ModGeneral.RANGESYSTEM = RangeSystem
                'ModGeneral.IDMODPOZOGENERAL = VwGeneral.IDMODPOZOGENERAL



                db.Entry(ModGeneral).State = Entity.EntityState.Modified
            End If
            db.SaveChanges()
            Return True
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.Critical)
            Return False
        End Try
    End Function
    Public Function SaveMecanico(ByVal IdAgujero As String) As Boolean
        Try
            'Eliminamos tuberias
            Dim mecanicos = db.MOD_POZO_TUBERIA.Where(Function(w) w.IDAGUJERO = IdAgujero).ToList()
            If mecanicos.Count > 0 Then
                mecanicos.ForEach(Function(e) db.MOD_POZO_TUBERIA.Remove(e))
                db.SaveChanges()
            End If
            Dim mecanico As New Mecanico(db, IdAgujero)

            Dim list_mecanicos = mecanico.GetList()
            Dim num As Integer = 0
            For Each tub In list_mecanicos
                db.MOD_POZO_TUBERIA.Add(New MOD_POZO_TUBERIA() With {
                    .IDMODPOZOTUBERIA = Guid.NewGuid.ToString().ToUpper(),
                    .IDAGUJERO = IdAgujero,
                    .IDTIPOTUBERIA = tub.IDTIPOTUBERIA,
                    .ETIQUETA = tub.ETIQUETA,
                    .CIDIAM = tub.CIDIAM,
                    .CIROUG = tub.CIROUG,
                    .MD = tub.MD,
                    .TIDIAM = tub.TIDIAM,
                    .TODIAM = tub.TODIAM,
                    .TIROUG = tub.TIROUG,
                    .TOROUG = tub.TOROUG
                })

                num += 1
            Next
            db.SaveChanges()


            Return True
        Catch ex As Exception
            MsgBox(ex.Message)
            Return False
        End Try
    End Function
    Public Function GetTemperatura(ByVal trayectoria(,) As Double, ByVal a As Double, b As Double) As Double(,)
        'REVISAR TALVEZ MARQUE FUERA DE INDICE
        Dim result(trayectoria.GetUpperBound(0), 1) As Double
        For i = 0 To trayectoria.GetUpperBound(0)  'Forzosamente se realiza a 17 registros, asi lo pide PROSPER
            result(i, 0) = trayectoria(i, 0)
            result(i, 1) = (trayectoria(i, 1) * a) + b
        Next i
        'Dim arreglo() As String = getArregloCol(arreglo_trayectoria, 1, 0)
        'Dim resultado(arreglo.Length - 1, 1) As String
        'For i = 0 To arreglo.Length - 1
        '    resultado(i, 0) = arreglo_trayectoria(i, 0) 'arreglo(i)
        '    resultado(i, 1) = (arreglo(i) * a) + b
        'Next
        Return result
    End Function
    Private Function SaveTemperatura(ByVal ModPozo As MOD_POZO, ByVal puntos(,) As Double) As Boolean
        Try

            Dim temps = db.MOD_POZO_TEMP.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).ToList()

            If temps.Count > 0 Then
                temps.ForEach(Function(e) db.MOD_POZO_TEMP.Remove(e))
                db.SaveChanges()
            End If


            Dim tb As DataTable = objconsulta.getDatosFormacion(IdAgujero, FechaPrueba) 'Necesario para la temperatura
            If tb.Rows.Count > 0 Then
                Dim b = ModGeneral.THTD 'TEMPERATURA EN LA PRIMERA CABEZA
                Dim a = ((tb.Rows(0).Item(1) - b) / (tb.Rows(0).Item(2) - puntos(0, 1)))

                Dim temperaturas = GetTemperatura(puntos, a, b)

                For i = 0 To temperaturas.GetUpperBound(0)
                    Dim mod_temperatura As New MOD_POZO_TEMP() With {
                        .IDMODPOZOTEMP = Guid.NewGuid().ToString().ToUpper(),
                        .IDMODPOZO = ModPozo.IDMODPOZO,
                        .PROFUNDIDADMD = temperaturas(i, 0),
                        .TEMPERATURA = temperaturas(i, 1)
                    }
                    db.MOD_POZO_TEMP.Add(mod_temperatura)
                Next

                db.SaveChanges()

                'Temperatura_SQL = Analisis.obtenerTemperatura(Trayecto_SQL, a, b)
                'NumDatTemp = Temperatura_SQL.GetUpperBound(0)
                'objConsulta.guardarTemperatura(Temperatura_SQL, IDMODPOZO)
                'resul = True
                Return True
            End If

            Return True
        Catch ex As Exception
            MsgBox(ex.Message & ":" & IdAgujero & " - " & FechaPrueba)
            Return False
        End Try
    End Function


    Private Function SaveTrayectorias(ByVal ModPozo As MOD_POZO, ByVal trayectorias As List(Of VW_TRAYECTORIA), ByVal max_md As Double) As Double(,)
        'Eliminar o resetear parametros en Trayectorias
        '===============================================================================
        Dim olds = db.MOD_POZO_TRAYEC.Where(Function(w) w.IDMODPOZO = ModPozo.IDMODPOZO).ToList()

        If olds.Count > 0 Then
            olds.ForEach(Function(e) db.MOD_POZO_TRAYEC.Remove(e))
            db.SaveChanges()
        End If



        Dim result(17, 1) As Double
        Try




            Dim mds(trayectorias.Count - 1), mvs(trayectorias.Count - 1) As Double
            Dim tmp_trayectorias As New List(Of VW_TRAYECTORIA)







            If trayectorias.Count > 0 Then
                'Indice 0 obligatorio inicialiar PMD a 0
                result(0, 0) = trayectorias(0).PROFUNDIDADMD 'dt.Rows(0).Item(0)
                result(0, 1) = trayectorias(0).PROFMV 'dt.Rows(0).Item(1)


                Dim finMD As Double = max_md



                For i = 0 To trayectorias.Count - 1
                    mds(i) = trayectorias(i).PROFUNDIDADMD
                    mvs(i) = trayectorias(i).PROFMV

                    If trayectorias(i).PROFUNDIDADMD <= max_md Then
                        tmp_trayectorias.Add(trayectorias(i))
                    End If

                Next i

                Dim finMV As Double = Analisis.InterpolarProfundidadesVertical(mds, mvs, max_md)

                tmp_trayectorias = tmp_trayectorias.OrderBy(Function(o) o.SEVERIDAD).ToList()


                Dim indice As Integer = 1 '2
                Dim j As Integer = tmp_trayectorias.Count - 1
                Do While (indice < 17)
                    result(indice, 0) = tmp_trayectorias(j).PROFUNDIDADMD 'View.Item(i).Item(0)
                    result(indice, 1) = tmp_trayectorias(j).PROFMV 'View.Item(i).Item(1)
                    indice += 1
                    j -= 1
                Loop

                ReDim Preserve result(indice, 1)
                result(indice, 0) = finMD
                result(indice, 1) = finMV

                Analisis.ordenarMatriz(result, 0)
            End If



            For i = 0 To result.GetUpperBound(0)
                Dim insert_tray As New MOD_POZO_TRAYEC() With {
                    .IDMODPOZOTRAYEC = Guid.NewGuid().ToString().ToUpper(),
                    .IDMODPOZO = ModPozo.IDMODPOZO,
                    .PROFUNDIDADMD = result(i, 0),
                    .PROFUNDIDADMV = result(i, 1)
                }


                db.MOD_POZO_TRAYEC.Add(insert_tray)

            Next i

            db.SaveChanges()






            Return result
        Catch ex As Exception
            MsgBox(ex.Message)
            Return result
        End Try
    End Function
    Private _command_save As ICommand
    Public Property CommandSave As ICommand
        Get
            Return _command_save
        End Get
        Set(value As ICommand)
            _command_save = value
        End Set
    End Property

    Public Shared Function getDoubleValidado(ByVal objeto As Object) As Double
        getDoubleValidado = 0.0
        Try
            If (objeto.Equals("NeuN")) Then
                getDoubleValidado = 0.0
            ElseIf (objeto.Equals("NaN")) Then
                getDoubleValidado = 0.0
            Else

                getDoubleValidado = CDbl(objeto)
            End If
        Catch ex As Exception

        End Try
    End Function
End Class