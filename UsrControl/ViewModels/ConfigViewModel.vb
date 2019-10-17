Imports WPF.Generales
Imports Telerik.Windows.Controls
Imports ModeloCI
Imports Prism.Mvvm
Public Class ConfigViewModel
    Inherits BindableBase

    Sub New()

    End Sub
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



    'VARIABLES GLOBALES
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

End Class
