Imports ModeloProsper.Prosper
Imports ModeloProsper.Lagrange
Imports ModeloCI

''' <summary>
''' Clase para crear automáticamente un modelo de pozo con BNC 
''' empleando Prosper                                              Versión 2.1
''' </summary>
Public Class Crea

#Region "Comentarios"

    '2345678901234567890123456789012345678901234567890123456789012345678901234567890
    '     *****************************************************************
    '     La clase se emplea para : Crear un modelo de un pozo con BNC empleando Prosper
    '
    '          1.- Yacimiento
    '          2.- VLP/IPR
    '
    '     Escrita  por:                            Juan Felipe de Jesús Díaz Jiménez
    '     Version :                                                              2.1
    '
    '     Version:     Revisada por:       Fecha              Cambios
    '
    '     2.10   Juan Felipe de Jesús   10/Oct/2017   Se incluye el mapa de estabilidad
    '            Díaz Jiménez                      
    '     2.03   Juan Felipe de Jesús   03/Mar/2016   Se pasa el código a visual estudio
    '            Díaz Jiménez                      
    '     2.02   Juan Felipe de Jesús   25/Feb/2006
    '            Díaz Jiménez                      
    '     2.01   Juan Felipe de Jesús   05/Feb/2016
    '            Díaz Jiménez                      
    '     2.0    Juan Felipe de Jesús   06/Nov/2015
    '            Díaz Jiménez                      
    '     1.1    Juan Felipe de Jesús   01/Sep/2015
    '            Díaz Jiménez                      
    '     1.0    Juan Felipe de Jesús   10/Abr/2006  
    '            Díaz Jiménez                          
    '     *****************************************************************
    '
    '                               Referencias                               
    '                              -------------                              
    '
    '     1.- 
    '
    '     ****************************************************************    
    '                                                                         
    '                      INPUT/OUTPUT LOGICAL VARIABLES                   
    '                   -----------------------------------                   
    '                                                                         
    '                            SUBPROGRAM CALLED                            
    '                            -----------------                            
    '     
    '     1.- New         : Inicialización de la clase
    '     2.- Yacimiento  : 
    '
    '                           VARIABLE DESCRIPTION                          
    '                           --------------------                          
    '
    '     *Ayuda       = En caso de ser verdadero, se despliega la ayuda de la clase
    '     *Diagnostico = En caso de ser verdadero, se despliega que información entra
    '                    a la clase.
    '
    '      IErr        = Variable para el control de errores en la clase
    '                    Verdadero : Se genera un error en la clase.
    '
    '      MsgErrDatos = Si los valores de entrada están fuera de rango de despliega
    '                    un mensaje de error. Guarda el error generado durante la 
    '                    verificación de los rangos de los datos.
    '      MsgErrCalc  = Guarda el error generado durante la ejecución de la clase.
    '      MsgAyuda    = Guarda el mensaje de ayuda de la presente clase.
    '      MsgDiagnostico = Guarda el mensaje de ayuda de la presente clase.
    '
    '
    '      I, J                                                   = Dummy variables.
    '
    '      (* Indicates input variables)
    '     ****************************************************************    
    '

#End Region

    Class Modelo
        Inherits PE

#Region "Atributos (Variables Locales, Datos y Resultados)"




        ' Variables locales
        Public IErr As Boolean = False ' Para verificar si se genera un error en la presente clase

        'AjusteVLPIPR
        Public Iniciar As Byte   ' Control de erroresana
        Public GrvGas As Double  '
        Public API As Double     '
        Public Dro As Double     '
        Public GOR_PTy As Double '
        Public GORFree As Double '
        Public Qg_PTy As Double       '
        Public Qg_Aforo As Double     '
        Public Qg_Libre As Double     '
        Public Qg_Total As Double     '
        Public GOR_Total As Double    '
        Public IPR_RTEL() As Double   '
        Public IPR_PWF() As Double    '
        Public Qgi_Res() As Double    '
        Public Wc_Res() As Double     '
        Public Pws_Res() As Double    '
        Public Qliq_Res(,,) As Double '
        Public QliqVLP(,) As Double
        Public PwfVLP(,) As Double
        Public Pdescarga_BEC(,) As Double
        Public QgiVec() As Double
        Public QliqVec() As Double

        ' Datos
        Public Ayuda As Boolean         ' Despliega la ayuda de la presente clase
        Public Diagnostico As Boolean   ' Se desliega el diagnóstico de la presente clase
        Public Rastreo As Boolean       ' Permite seguir los resultados parciales de la clase
        Public ProgramPath As String  ' Localización del programa "Prosper"
        Public ArchivoPVT As String   ' Archivo donde se encuentra el PVT ajustado

        Public Fluid As New MaxMinByte       '
        Public PVTModel As New MaxMinByte    '
        Public Separator As New MaxMinByte   '
        Public Emulsion As New MaxMinByte    '
        Public Hydrate As New MaxMinByte     '
        Public WatVis As New MaxMinByte      '
        Public VisMod As New MaxMinByte      '
        Public FlowType As New MaxMinByte    '
        Public WellType As New MaxMinByte    '
        Public LiftMethod As New MaxMinByte  '
        Public LiftType As New MaxMinByte    '
        Public Predict As New MaxMinByte     '
        Public TempModel As New MaxMinByte   '
        Public RangeSystem As New MaxMinByte '
        Public OutputRes As New MaxMinByte   '
        Public Completion As New MaxMinByte  '
        Public GravelPack As New MaxMinByte  '
        Public InflowType As New MaxMinByte  '
        Public GasConing As New MaxMinByte   '



        Public Company As String         '
        Public Field As String           '
        Public Locations As String       '
        Public Well As String            '
        Public Platform As String        '
        Public Analyst As String         '
        Public DatGenDate As String      '
        Public Comenta As String         '

        Public Qo As New MaxMinDouble        '
        Public QTest As New MaxMinDouble     '
        Public Wc As New MaxMinDouble        '
        Public PRes As New MaxMinDouble      '
        Public TRes As New MaxMinDouble      '
        Public RGA_Aforo As New MaxMinDouble '
        Public GLRate As New MaxMinDouble    '
        Public QgiMin As New MaxMinDouble    '
        Public QgiMax As New MaxMinDouble    '

        Public MGSkinMethod As New MaxMinByte '
        Public DPSkinMethod As New MaxMinByte '
        Public Compact As New MaxMinByte      ' 
        Public IRELK As New MaxMinByte        ' 

        Public IPRMethod As New MaxMinByte    '
        Public Ptest As New MaxMinDouble      '
        Public TotGor As New MaxMinDouble     '
        Public ResPerm As New MaxMinDouble    '
        Public Thickness As New MaxMinDouble  '
        Public Drainage As New MaxMinDouble   '
        Public Dietz As New MaxMinDouble      '
        Public WBR As New MaxMinDouble        '
        Public Skin As New MaxMinDouble       '
        Public PI As New MaxMinDouble

        Public NumDatEdoMec As New MaxMinByte ' Número de datos del estado mecánico
        Public Label() As String          '
        Public DType As New MaxMinArreglo     '
        Public Depth As New MaxMinArreglo     '
        Public TID As New MaxMinArreglo       '
        Public TIR As New MaxMinArreglo       '
        Public TOD As New MaxMinArreglo       '
        Public TOR As New MaxMinArreglo       '
        Public CID As New MaxMinArreglo       '
        Public CIR As New MaxMinArreglo       '

        Public Entry As New MaxMinByte        '
        Public Method As New MaxMinByte       '
        Public Gravity As New MaxMinDouble    '
        Public H2S As New MaxMinDouble        '
        Public CO2 As New MaxMinDouble        '
        Public N2 As New MaxMinDouble         '
        Public GLRiny As New MaxMinDouble     '
        Public ValveDepth As New MaxMinDouble '

        Public NumDatTrayecto As New MaxMinByte ' Numero de datos del registro de desviaciones
        Public RDEnable As New MaxMinArreglo    ' 
        Public RDMd As New MaxMinArreglo        '
        Public RDTvd As New MaxMinArreglo       '

        Public NumDatTemp As New MaxMinByte ' Número de datos del registro de temperatura
        Public Htc As New MaxMinDouble      '
        Public PTMd As New MaxMinArreglo    '
        Public PTTmp As New MaxMinArreglo   '

        Public THPres As New MaxMinDouble     ' Presión en la cabeza del pozo (Pth)
        Public THTemp As New MaxMinDouble     '
        Public TRPres As New MaxMinDouble     '
        Public DiamValBNC As New MaxMinDouble '

        'Public IPRRTEL As New MaxMinArreglo   '
        'Public IPRPWF As New MaxMinArreglo    '
        'Public AOF As New MaxMinDouble        '

        Public NumCorr As New MaxMinInteger   '
        Public ProfDesa As Double(,)          '
        Public TVDCC As Double(,)
        Public PresWf As Double(,)            '
        Public NumDatCorr As Integer()        '
        Public NomCorr As String()            '
        Public NumCor As Integer()
        Public CorrVFP As Integer   '
        Public NivMedDisp As New MaxMinDouble '

        Public NGrafVLPIPR As Integer     '
        Public VLPIPR_RTEL As Double(,)   '
        Public VLPIPR_PWF As Double(,)    '
        Public NumPntsVLPIPR As Integer() '
        Public NomVLPIPR As String()      '

        Public QgiRes As Double()    '
        Public WcRes As Double()     '
        Public PwsRes As Double()    '
        Public QliqRes As Double(,,) '

        Public NumDatGrad As Integer '
        Public NumDatGrad2 As Integer '
        Public NumDatGG As Integer   '
        Public MSDQL As Double(,)    '
        Public TVDQL As Double(,)    '
        Public PresQL As Double(,)   '
        Public GGTVD As Double()     '
        Public GGPres As Double()    '
        Public MSDGG As Double()     '
        Public PresGG As Double()    '
        Public TVDGG As Double()
        Public TempQL As Double(,)

        Public NumDatIPR As Integer() '
        Public NomSerie As String()   '

        '      BEC datos de entrada
        Public FrecMin As New MaxMinDouble    '
        Public FrecMax As New MaxMinDouble    '
        Public Prof_BEC As New MaxMinDouble
        Public Frec_BEC As New MaxMinDouble
        Public ODMax_BEC As New MaxMinDouble
        Public LongCable_BEC As New MaxMinDouble
        Public EfiSepGas_BEC As New MaxMinDouble
        Public Etapas_BEC As New MaxMinDouble
        Public VoltSup_BEC As New MaxMinDouble
        Public Desgaste_BEC As New MaxMinDouble
        Public ReducGas_BEC As New MaxMinDouble
        Public Bomba_BEC As New MaxMinInteger
        Public Motor_BEC As New MaxMinInteger
        Public PotenciaMotor_BEC As New MaxMinDouble
        Public Cable_BEC As New MaxMinInteger
        Public PreSuc_BEC As New MaxMinDouble
        Public Corriente_BEC As New MaxMinDouble
        Public Potencia_BEC As New MaxMinDouble
        Public PreDes_BEC As New MaxMinDouble

        '      BEC Salidas
        Public FrecVec() As Double
        Public Frec_Res() As Double
        Public HPreqBomba_BECq As Double
        Public HPreqMotor_BECq As Double
        Public EfiMotor_BECq As Double
        Public Voltreq_BECq As Double
        Public PreSuccion_BECq As Double
        Public PreDescarga_BECq As Double
        Public QliqCY_BECq As Double
        Public FgasinBomba_BECq As Double
        Public KVA_BECq As Double
        Public TorqEje_BECq As Double
        Public Xcarta_BEC As ArrayList
        Public Ycarta_BEC As ArrayList
        Public Rango_MinX_BEC As Double()
        Public Rango_MinY_BEC As Double()
        Public Mejor_EficX_BEC As Double()
        Public Mejor_EficY_BEC As Double()
        Public Rango_MaxX_BEC As Double()
        Public Rango_MaxY_BEC As Double()
        Public Bombaaux_BEC As String

        'Public Qgi_Min As Double      ' MMpie3/día
        'Public Property Qgi_Max As Double      ' MMpie3/día

        ' Resultados
        Public Property MsgErrDatos As String    ' Guarda el error generado durante la verificación de los rangos de los datos
        Public Property MsgErrCalc As String     ' Guarda el error generado durante la ejecución de la clase.
        Public Property MsgAyuda As String       ' Guarda el mensaje de ayuda de la presente clase.
        Public Property MsgDiagnostico As String ' Guarda el mensaje de ayuda de la presente clase.

        'TMP 
        Public Property Qgi_afor As Double
        Public Property Qliq_aforo As Double

        Public Pvt As Pvt
        Public Reset As Boolean = False
        Public Property OpenProsper As Boolean
        Public Property Conected As Boolean
        Public Property Server As Object
        Public Property SaveFile As Boolean

#End Region

#Region "Propiedades"

        Public WriteOnly Property _Fluid As Byte
            Set(value As Byte)
                If value < Fluid.Min Or value > Fluid.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Fluid.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Fluid.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Fluid.Max.ToString, 6)
                    IErr = True
                Else
                    Fluid.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PVTModel As Byte
            Set(value As Byte)
                If value < PVTModel.Min Or value > PVTModel.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & PVTModel.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & PVTModel.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & PVTModel.Max.ToString, 6)
                    IErr = True
                Else
                    PVTModel.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Separator As Byte
            Set(value As Byte)
                If value < Separator.Min Or value > Separator.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Separator.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Separator.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Separator.Max.ToString, 6)
                    IErr = True
                Else
                    Separator.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Emulsion As Byte
            Set(value As Byte)
                If value < Emulsion.Min Or value > Emulsion.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Emulsion.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Emulsion.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Emulsion.Max.ToString, 6)
                    IErr = True
                Else
                    Emulsion.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Hydrate As Byte
            Set(value As Byte)
                If value < Hydrate.Min Or value > Hydrate.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Hydrate.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Hydrate.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Hydrate.Max.ToString, 6)
                    IErr = True
                Else
                    Hydrate.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _WatVis As Byte
            Set(value As Byte)
                If value < WatVis.Min Or value > WatVis.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & WatVis.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & WatVis.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & WatVis.Max.ToString, 6)
                    IErr = True
                Else
                    WatVis.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _VisMod As Byte
            Set(value As Byte)
                If value < VisMod.Min Or value > VisMod.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & VisMod.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & VisMod.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & VisMod.Max.ToString, 6)
                    IErr = True
                Else
                    VisMod.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _FlowType As Byte
            Set(value As Byte)
                If value < FlowType.Min Or value > FlowType.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & FlowType.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & FlowType.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & FlowType.Max.ToString, 6)
                    IErr = True
                Else
                    FlowType.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _WellType As Byte
            Set(value As Byte)
                If value < WellType.Min Or value > WellType.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & WellType.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & WellType.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & WellType.Max.ToString, 6)
                    IErr = True
                Else
                    WellType.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _LiftMethod As Byte
            Set(value As Byte)
                If value < LiftMethod.Min Or value > LiftMethod.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & LiftMethod.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & LiftMethod.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & LiftMethod.Max.ToString, 6)
                    IErr = True
                Else
                    LiftMethod.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _LiftType As Byte
            Set(value As Byte)
                If value < LiftType.Min Or value > LiftType.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & LiftType.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & LiftType.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & LiftType.Max.ToString, 6)
                    IErr = True
                Else
                    LiftType.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Predict As Byte
            Set(value As Byte)
                If value < Predict.Min Or value > Predict.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Predict.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Predict.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Predict.Max.ToString, 6)
                    IErr = True
                Else
                    Predict.Val = value
                End If
            End Set
        End Property
        Public WriteOnly Property _TempModel As Byte
            Set(value As Byte)
                If value < TempModel.Min Or value > TempModel.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & TempModel.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & TempModel.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & TempModel.Max.ToString, 6)
                    IErr = True
                Else
                    TempModel.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _RangeSystem As Byte
            Set(value As Byte)
                If value < RangeSystem.Min Or value > RangeSystem.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & RangeSystem.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & RangeSystem.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & RangeSystem.Max.ToString, 6)
                    IErr = True
                Else
                    RangeSystem.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _OutputRes As Byte
            Set(value As Byte)
                If value < OutputRes.Min Or value > OutputRes.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & OutputRes.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & OutputRes.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & OutputRes.Max.ToString, 6)
                    IErr = True
                Else
                    OutputRes.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Completion As Byte
            Set(value As Byte)
                If value < Completion.Min Or value > Completion.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Completion.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Completion.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Completion.Max.ToString, 6)
                    IErr = True
                Else
                    Completion.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _GravelPack As Byte
            Set(value As Byte)
                If value < GravelPack.Min Or value > GravelPack.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & GravelPack.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & GravelPack.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & GravelPack.Max.ToString, 6)
                    IErr = True
                Else
                    GravelPack.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _InflowType As Byte
            Set(value As Byte)
                If value < InflowType.Min Or value > InflowType.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & InflowType.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & InflowType.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & InflowType.Max.ToString, 6)
                    IErr = True
                Else
                    InflowType.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _GasConing As Byte
            Set(value As Byte)
                If value < GasConing.Min Or value > GasConing.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & GasConing.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & GasConing.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & GasConing.Max.ToString, 6)
                    IErr = True
                Else
                    GasConing.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Qo As Double
            Set(value As Double)
                If value < Qo.Min Or value > Qo.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Qo.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Qo.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Qo.Max.ToString, 6)
                    IErr = True
                Else
                    Qo.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _QTest As Double
            Set(value As Double)
                If value < QTest.Min Or value > QTest.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & QTest.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & QTest.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & QTest.Max.ToString, 6)
                    IErr = True
                Else
                    QTest.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Wc As Double
            Set(value As Double)
                If value < Wc.Min Or value > Wc.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Wc.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Wc.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Wc.Max.ToString, 6)
                    IErr = True
                Else
                    Wc.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PRes As Double
            Set(value As Double)
                If value < PRes.Min Or value > PRes.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & PRes.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & PRes.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & PRes.Max.ToString, 6)
                    IErr = True
                Else
                    PRes.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _TRes As Double
            Set(value As Double)
                If value < TRes.Min Or value > TRes.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & TRes.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & TRes.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & TRes.Max.ToString, 6)
                    IErr = True
                Else
                    TRes.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _RGA_Aforo As Double
            Set(value As Double)
                If value < RGA_Aforo.Min Or value > RGA_Aforo.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & RGA_Aforo.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & RGA_Aforo.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & RGA_Aforo.Max.ToString, 6)
                    IErr = True
                Else
                    RGA_Aforo.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _GLRate As Double
            Set(value As Double)
                If value < GLRate.Min Or value > GLRate.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & GLRate.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & GLRate.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & GLRate.Max.ToString, 6)
                    IErr = True
                Else
                    GLRate.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _QgiMin As Double
            Set(value As Double)
                If value < QgiMin.Min Or value > QgiMin.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & QgiMin.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & QgiMin.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & QgiMin.Max.ToString, 6)
                    IErr = True
                Else
                    QgiMin.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _QgiMax As Double
            Set(value As Double)
                If value < QgiMax.Min Or value > QgiMax.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & QgiMax.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & QgiMax.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & QgiMax.Max.ToString, 6)
                    IErr = True
                Else
                    QgiMax.Val = value
                End If
            End Set
        End Property


        Public WriteOnly Property _FrecMin As Double
            Set(value As Double)
                If value < FrecMin.Min Or value > FrecMin.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & FrecMin.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & FrecMin.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & FrecMin.Max.ToString, 6)
                    IErr = True
                Else
                    FrecMin.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _FrecMax As Double
            Set(value As Double)
                If value < FrecMax.Min Or value > FrecMax.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & FrecMax.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & FrecMax.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & FrecMax.Max.ToString, 6)
                    IErr = True
                Else
                    FrecMax.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Prof_BEC As Double
            Set(value As Double)
                If value < Prof_BEC.Min Or value > Prof_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Prof_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Prof_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Prof_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Prof_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Frec_BEC As Double
            Set(value As Double)
                If value < Frec_BEC.Min Or value > Frec_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Frec_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Frec_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Frec_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Frec_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _ODMax_BEC As Double
            Set(value As Double)
                If value < ODMax_BEC.Min Or value > ODMax_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & ODMax_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & ODMax_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & ODMax_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    ODMax_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _LongCable_BEC As Double
            Set(value As Double)
                If value < LongCable_BEC.Min Or value > LongCable_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & LongCable_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & LongCable_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & LongCable_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    LongCable_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _EfiSepGas_BEC As Double
            Set(value As Double)
                If value < EfiSepGas_BEC.Min Or value > EfiSepGas_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & EfiSepGas_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & EfiSepGas_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & EfiSepGas_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    EfiSepGas_BEC.Val = value
                End If
            End Set
        End Property
        Public WriteOnly Property _Etapas_BEC As Double
            Set(value As Double)
                If value < Etapas_BEC.Min Or value > Etapas_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Etapas_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Etapas_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Etapas_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Etapas_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _VoltSup_BEC As Double
            Set(value As Double)
                If value < VoltSup_BEC.Min Or value > VoltSup_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & VoltSup_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & VoltSup_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & VoltSup_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    VoltSup_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Desgaste_BEC As Double
            Set(value As Double)
                If value < Desgaste_BEC.Min Or value > Desgaste_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Desgaste_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Desgaste_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Desgaste_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Desgaste_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _ReducGas_BEC As Double
            Set(value As Double)
                If value < ReducGas_BEC.Min Or value > ReducGas_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & ReducGas_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & ReducGas_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & ReducGas_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    ReducGas_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Bomba_BEC As Integer
            Set(value As Integer)
                If value < Bomba_BEC.Min Or value > Bomba_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Bomba_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Bomba_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Bomba_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Bomba_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Motor_BEC As Integer
            Set(value As Integer)
                If value < Motor_BEC.Min Or value > Motor_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Motor_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Motor_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Motor_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Motor_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PotenciaMotor_BEC As Integer
            Set(value As Integer)
                If value < PotenciaMotor_BEC.Min Or value > PotenciaMotor_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & PotenciaMotor_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & PotenciaMotor_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & PotenciaMotor_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    PotenciaMotor_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Cable_BEC As Integer
            Set(value As Integer)
                If value < Cable_BEC.Min Or value > Cable_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Cable_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Cable_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Cable_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Cable_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PreSuc_BEC As Double
            Set(value As Double)
                If value < PreSuc_BEC.Min Or value > PreSuc_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & PreSuc_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & PreSuc_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & PreSuc_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    PreSuc_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Corriente_BEC As Double
            Set(value As Double)
                If value < Corriente_BEC.Min Or value > Corriente_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Corriente_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Corriente_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Corriente_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Corriente_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Potencia_BEC As Double
            Set(value As Double)
                If value < Potencia_BEC.Min Or value > Potencia_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Potencia_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Potencia_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Potencia_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    Potencia_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PreDes_BEC As Double
            Set(value As Double)
                If value < PreDes_BEC.Min Or value > PreDes_BEC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & PreDes_BEC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & PreDes_BEC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & PreDes_BEC.Max.ToString, 6)
                    IErr = True
                Else
                    PreDes_BEC.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _MGSkinMethod As Byte
            Set(value As Byte)
                If value < MGSkinMethod.Min Or value > MGSkinMethod.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & MGSkinMethod.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & MGSkinMethod.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & MGSkinMethod.Max.ToString, 6)
                    IErr = True
                Else
                    MGSkinMethod.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _DPSkinMethod As Byte
            Set(value As Byte)
                If value < DPSkinMethod.Min Or value > DPSkinMethod.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & DPSkinMethod.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & DPSkinMethod.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & DPSkinMethod.Max.ToString, 6)
                    IErr = True
                Else
                    DPSkinMethod.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Compact As Byte
            Set(value As Byte)
                If value < Compact.Min Or value > Compact.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Compact.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Compact.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Compact.Max.ToString, 6)
                    IErr = True
                Else
                    Compact.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _IRELK As Byte
            Set(value As Byte)
                If value < IRELK.Min Or value > IRELK.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & IRELK.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & IRELK.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & IRELK.Max.ToString, 6)
                    IErr = True
                Else
                    IRELK.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _IPRMethod As Byte
            Set(value As Byte)
                If value < IPRMethod.Min Or value > IPRMethod.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & IPRMethod.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & IPRMethod.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & IPRMethod.Max.ToString, 6)
                    IErr = True
                Else
                    IPRMethod.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Ptest As Double
            Set(value As Double)
                If value < Ptest.Min Or value > Ptest.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & Ptest.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Ptest.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Ptest.Max.ToString, 6)
                    IErr = True
                Else
                    Ptest.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _TotGor As Double
            Set(value As Double)
                If value < TotGor.Min Or value > TotGor.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & TotGor.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & TotGor.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & TotGor.Max.ToString, 6)
                    IErr = True
                Else
                    TotGor.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _ResPerm As Double
            Set(value As Double)
                If value < ResPerm.Min Or value > ResPerm.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & ResPerm.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & ResPerm.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & ResPerm.Max.ToString, 6)
                    IErr = True
                Else
                    ResPerm.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Thickness As Double
            Set(value As Double)
                If value < Thickness.Min Or value > Thickness.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & Thickness.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Thickness.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Thickness.Max.ToString, 6)
                    IErr = True
                Else
                    Thickness.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Drainage As Double
            Set(value As Double)
                If value < Drainage.Min Or value > Drainage.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & Drainage.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Drainage.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Drainage.Max.ToString, 6)
                    IErr = True
                Else
                    Drainage.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Dietz As Double
            Set(value As Double)
                If value < Dietz.Min Or value > Dietz.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & Dietz.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Dietz.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Dietz.Max.ToString, 6)
                    IErr = True
                Else
                    Dietz.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _WBR As Double
            Set(value As Double)
                If value < WBR.Min Or value > WBR.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & WBR.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & WBR.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & WBR.Max.ToString, 6)
                    IErr = True
                Else
                    WBR.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Skin As Double
            Set(value As Double)
                If value < Skin.Min Or value > Skin.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:     " & Skin.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Skin.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Skin.Max.ToString, 6)
                    IErr = True
                Else
                    WBR.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _NumDatEdoMec As Byte
            Set(value As Byte)
                If value < NumDatEdoMec.Min Or value > NumDatEdoMec.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & NumDatEdoMec.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & NumDatEdoMec.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & NumDatEdoMec.Max.ToString, 6)
                    IErr = True
                Else
                    NumDatEdoMec.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _DType As Object() ' Tipo Byte
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < DType.Min Or value(i) > DType.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & DType.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & DType.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & DType.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    DType.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _Depth As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < Depth.Min Or value(i) > Depth.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & Depth.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & Depth.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & Depth.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    Depth.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _TID As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < TID.Min Or value(i) > TID.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & TID.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & TID.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & TID.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    TID.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _TIR As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < TIR.Min Or value(i) > TIR.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & TIR.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & TIR.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & TIR.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    TIR.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _TOD As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < TOD.Min Or value(i) > TOD.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & TOD.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & TOD.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & TOD.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    TOD.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _TOR As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < TOR.Min Or value(i) > TOR.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & TOR.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & TOR.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & TOR.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    TOR.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _CID As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < CID.Min Or value(i) > CID.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & CID.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & CID.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & CID.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    CID.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _CIR As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < CIR.Min Or value(i) > CIR.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & CIR.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & CIR.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & CIR.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    CIR.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _Entry As Byte
            Set(value As Byte)
                If value < Entry.Min Or value > Entry.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Entry.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Entry.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Entry.Max.ToString, 6)
                    IErr = True
                Else
                    Entry.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Method As Byte
            Set(value As Byte)
                If value < Method.Min Or value > Method.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Method.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Method.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Method.Max.ToString, 6)
                    IErr = True
                Else
                    Method.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _Gravity As Double
            Set(value As Double)
                If value < Gravity.Min Or value > Gravity.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Gravity.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Gravity.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Gravity.Max.ToString, 6)
                    IErr = True
                Else
                    Gravity.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _H2S As Double
            Set(value As Double)
                If value < H2S.Min Or value > H2S.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & H2S.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & H2S.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & H2S.Max.ToString, 6)
                    IErr = True
                Else
                    H2S.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _CO2 As Double
            Set(value As Double)
                If value < CO2.Min Or value > CO2.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & CO2.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & CO2.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & CO2.Max.ToString, 6)
                    IErr = True
                Else
                    CO2.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _N2 As Double
            Set(value As Double)
                If value < N2.Min Or value > N2.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & N2.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & N2.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & N2.Max.ToString, 6)
                    IErr = True
                Else
                    N2.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _GLRiny As Double
            Set(value As Double)
                If value < GLRiny.Min Or value > GLRiny.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & GLRiny.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & GLRiny.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & GLRiny.Max.ToString, 6)
                    IErr = True
                Else
                    GLRiny.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _ValveDepth As Double
            Set(value As Double)
                If value < ValveDepth.Min Or value > ValveDepth.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & ValveDepth.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & ValveDepth.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & ValveDepth.Max.ToString, 6)
                    IErr = True
                Else
                    ValveDepth.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _RDEnable As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < RDEnable.Min Or value(i) > RDEnable.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & RDEnable.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & RDEnable.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & RDEnable.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    RDEnable.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _RDMd As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < RDMd.Min Or value(i) > RDMd.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & RDMd.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & RDMd.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & RDMd.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    RDMd.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _RDTvd As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < RDTvd.Min Or value(i) > RDTvd.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & RDTvd.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & RDTvd.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & RDTvd.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    RDTvd.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _Htc As Double
            Set(value As Double)
                If value < Htc.Min Or value > Htc.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & Htc.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & Htc.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & Htc.Max.ToString, 6)
                    IErr = True
                Else
                    Htc.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _PTMd As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < PTMd.Min Or value(i) > PTMd.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & PTMd.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & PTMd.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & PTMd.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    PTMd.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _PTTmp As Object() ' Tipo Doble
            Set(value As Object())
                Dim MsgErrDatos_aux As String = ""
                For i = 0 To value.GetUpperBound(0)
                    If Not IsNothing(value(i)) Then
                        'If Not IsNumeric(value(i)) Then value(i) = 0
                        If value(i) < PTTmp.Min Or value(i) > PTTmp.Max Then
                            MsgErrDatos_aux &= GetFrmt("Variable:" & PTTmp.Nomb & " (" & i & ") = " & value(i).ToString, 1)
                            MsgErrDatos_aux &= GetFrmt("Valor mínimo: " & PTTmp.Min.ToString, 3)
                            MsgErrDatos_aux &= GetFrmt("Valor máximo: " & PTTmp.Max.ToString, 6)
                        End If
                    Else
                        value(i) = 0
                    End If
                Next
                If MsgErrDatos_aux.Length = 0 Then
                    PTTmp.Val = value
                Else
                    MsgErrDatos &= " " & MsgErrDatos_aux
                    IErr = True
                End If
            End Set
        End Property

        Public WriteOnly Property _THPres As Double
            Set(value As Double)
                If value < THPres.Min Or value > THPres.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & THPres.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & THPres.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & THPres.Max.ToString, 6)
                    IErr = True
                Else
                    THPres.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _THTemp As Double
            Set(value As Double)
                If value < THTemp.Min Or value > THTemp.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & THTemp.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & THTemp.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & THTemp.Max.ToString, 6)
                    IErr = True
                Else
                    THTemp.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _TRPres As Double
            Set(value As Double)
                If value < TRPres.Min Or value > TRPres.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & TRPres.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & TRPres.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & TRPres.Max.ToString, 6)
                    IErr = True
                Else
                    TRPres.Val = value
                End If
            End Set
        End Property

        Public WriteOnly Property _DiamValBNC As Double
            Set(value As Double)
                If value < DiamValBNC.Min Or value > DiamValBNC.Max Then
                    MsgErrDatos = MsgErrDatos & GetFrmt("Variable:   " & DiamValBNC.Nomb & " = " & value.ToString, 1)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor mínimo: " & DiamValBNC.Min.ToString, 3)
                    MsgErrDatos = MsgErrDatos & GetFrmt("Valor máximo: " & DiamValBNC.Max.ToString, 6)
                    IErr = True
                Else
                    DiamValBNC.Val = value
                End If
            End Set
        End Property

#End Region

        ''' <summary>
        ''' Inicialización de la clase Crea.Modelo
        ''' </summary>
        ''' 
        Public Sub New()

        End Sub
        Public Sub New(ByVal NDatEdoMec As Integer, ByVal NDatTrayecto As Integer, ByVal NDatTemp As Integer)

            ' Es necesario dimensionar los arreglos de:
            ' El estado mecánico 
            ' La temperatura en el pozo
            ' El registro de desviaciones

            Dim MsgSub As String = GetFrmt("Inicialización: ", 2)
            Iniciar = 0


            Try
                If NDatEdoMec > 0 Then
                    NumDatEdoMec.Val = NDatEdoMec
                    ReDim Label(NDatEdoMec), DType.Val(NDatEdoMec), Depth.Val(NDatEdoMec),
                                              TID.Val(NDatEdoMec), TIR.Val(NDatEdoMec), TOD.Val(NDatEdoMec), TOR.Val(NDatEdoMec),
                                              CID.Val(NDatEdoMec), CIR.Val(NDatEdoMec)
                    Iniciar += 1
                Else
                    MsgSub &= GetFrmt("Es necesario tener cuando menos una tubería", 2)
                End If

                If NDatTrayecto > 1 Then
                    NumDatTrayecto.Val = NDatTrayecto
                    ReDim RDEnable.Val(NDatTrayecto)
                    ReDim RDMd.Val(NDatTrayecto)
                    ReDim RDTvd.Val(NDatTrayecto)
                    Iniciar += 1
                Else
                    MsgSub &= GetFrmt("Es necesario tener cuando menos dos datos de la trayectoria del pozo", 2)
                End If

                If NDatTemp > 1 Then
                    NumDatTemp.Val = NDatTemp
                    ReDim PTMd.Val(NDatTemp)
                    ReDim PTTmp.Val(NDatTemp)
                    Iniciar += 1
                Else
                    MsgSub &= GetFrmt("Es necesario tener cuando menos dos datos de temperatura en el pozos", 2)
                End If

                'PntIPR = NumResIPR    ' Número de puntos de la IPR
                MsgErrDatos = ""
                MsgErrCalc = ""
                MsgAyuda = ""
                MsgDiagnostico = ""

                ' Agregar los mínimos y máximos

                ' Fluido: 0 = Oil and Water
                '         1 = Dry and Wet Gas
                '         2 = Retrograde Condensate
                Fluid.Nomb = "Tipo de fluido producido " : Fluid.Min = 0 : Fluid.Max = 2
                ' PVTModel: 0 = Black Oil
                '           1 = Equation of State
                PVTModel.Nomb = "Modelo PVT " : PVTModel.Min = 0 : PVTModel.Max = 1
                ' Separator: 0 = Single Stage Separator
                '            1 = Two Stage Separator
                Separator.Nomb = "Etapas de separación " : Separator.Min = 0 : Separator.Max = 1
                ' Emulsion: 0 = No
                '           1 = Emulsion + Pump Viscosity Correction
                Emulsion.Nomb = "Manejo de emulsiones " : Emulsion.Min = 0 : Emulsion.Max = 1
                ' Hydrate: 0 = Disable Warning
                '          1 = Enable Warning
                Hydrate.Nomb = "Habilitar alarmas en caso de tener presencia de hidratos " : Hydrate.Min = 0 : Hydrate.Max = 1
                ' WatVis: 0 = Use Default Correlation
                '         1 = Use Pressure Corrected Correlation
                WatVis.Nomb = "Correlación para la viscosidad del agua " : WatVis.Min = 0 : WatVis.Max = 1
                ' VisMod: 0 = Newtonian Fluid
                '         1 = Non Newtonian Fluid
                VisMod.Nomb = "Modelo de viscosidad " : VisMod.Min = 0 : VisMod.Max = 1
                ' FlowType: 0 = Tubing Flow
                '           1 = Annular Flow
                FlowType.Nomb = "Flujo por la tubería de producción o por el Espacio Anular " : FlowType.Min = 0 : FlowType.Max = 1
                ' WellType: 0 = Producer
                '           1 = Inyector
                '           2 = Water Inyector
                WellType.Nomb = "Tipo de pozo (productor, inyector o inyector de agua " : WellType.Min = 0 : WellType.Max = 1
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
                LiftMethod.Nomb = "Tipo de sistema artificial " : LiftMethod.Min = 0 : LiftMethod.Max = 10
                ' LiftType: 0 = No Friction Loss In Annulus
                '           1 = Friction Loss In Annulus
                '           2 = Safety Equipment
                LiftType.Nomb = "Considerar o no fricción en el espacio anular " : LiftType.Min = 0 : LiftType.Max = 2
                ' Predict: 0 = Pressure Only
                '          1 = Pressure and Temperature (offshore)
                '          2 = Pressure and Temperature (on Land)        '
                Predict.Nomb = "Predicción de presión y temperatura " : Predict.Min = 0 : Predict.Max = 2
                ' TempModel: 0 = Rough Approximation
                '            1 = Enthalpy Balance
                '            2 = Improved Approximation
                TempModel.Nomb = "Modelo de temperatura " : TempModel.Min = 0 : TempModel.Max = 2
                ' RangeSystem: 0 = Full System
                '              1 = Pipeline Only
                RangeSystem.Nomb = "Cálculos en el sistema completo o únicamente en la línea de descarga " : RangeSystem.Min = 0 : RangeSystem.Max = 1
                ' OutputRes: 0 = Show Calculating Data
                '            1 = Hide Calculating Data
                OutputRes.Nomb = "Mostrar o no los resultados calculados " : OutputRes.Min = 0 : OutputRes.Max = 1
                ' Completion: 0 = Cased Hole
                '             1 = Open hole
                Completion.Nomb = "Terminación en agujero descubierto o en agujero entubado"
                ' GravelPack: 0 = None
                '             1 = Gravel Pack
                '             2 = Pre Packed Screen
                '             3 = Wire Wrapped Screen
                '             4 = Slotted Liner
                GravelPack.Nomb = "Tipo de herramienta de control de arena " : GravelPack.Min = 0 : GravelPack.Max = 4
                ' InflowType: 0 = Single Branch
                '             1 = MultiLateral Well
                InflowType.Nomb = "Tipo de IPR" : InflowType.Min = 0 : InflowType.Max = 1
                ' GasConing: 0 = No
                '            1 = Yes
                GasConing.Nomb = "Considerar conificación de gas " : GasConing.Min = 0 : GasConing.Max = 1
                ' IPRMethod: 0 = PI Entry
                '            1 = Vogel
                '            2 = Composite
                '            3 = Darcy
                '            4 = Fetkovich
                '            5 = MultiRate Fetkovich 
                '            6 = Jones
                '            7 = MultiRate Jones
                '            8 = Transient
                '            9 = Hidraulically Fractured Well
                '           10 = Horizontal Well- No Flow Boundaries
                '           11 = Horizontal Well- Constant Pressure Upper Boundary
                '           12 = MultiLayer Reservoir
                '           13 = External Entry
                '           14 = Horizontal Well- dP Friction Loss in Wellbore
                '           15 = MultiLayer - dP LossIn WellBore
                '           16 = SkinAide (ELF)
                '           17 = Dual Porosity
                '           18 = Horizontal Well - Transverse Vertical Fractures
                '           19 = SPOT
                IPRMethod.Nomb = "Modelo de cálculo de la IPR " : IPRMethod.Min = 0 : IPRMethod.Max = 19

                ' Datos del yacimiento
                ' Presión de Fondo Estática
                PRes.Nomb = "Presión de Fondo Estática [kg/cm2] " : PRes.Min = 500 / 14.223 : PRes.Max = 7000 / 14.223   ' Valores en lb/pg2
                ' Temperatura del Yacimiento
                TRes.Nomb = "Temperatura del Yacimiento [oC]" : TRes.Min = (68 - 32) / 1.8 : TRes.Max = (400 - 32) / 1.8 ' Valores en oF
                ' Porcentaje de agua en la capa
                Wc.Nomb = "Porcentaje de agua producida [%]" : Wc.Min = 0 : Wc.Max = 99
                ' Relación Gas Aceite
                TotGor.Nomb = "Relación Gas Aceite Total (disuelto + libre) [m3/m3]" : TotGor.Min = 20 : TotGor.Max = 5000
                ' Modelo de reducción de permeabilidad por compactación
                ' 0: No
                ' 1: Si
                Compact.Nomb = "Emplear modelo de reducción de permeabilidad por compactación" : Compact.Min = 0 : Compact.Max = 1
                ' Considerar las permeabilidades relativas
                ' 0: No
                ' 1: Si
                IRELK.Nomb = "Considerar las permeabilidades relativas " : IRELK.Min = 0 : IRELK.Max = 1

                'Datos del modelo PI
                PI.Nomb = "Valor del Índice de Productividad" : PI.Min = 0.001 : PI.Max = 5000

                ' Datos del modelo de Vogel

                ' Producción de liquido
                QTest.Nomb = "Producción de líquido [bl/dia]" : QTest.Min = 30 : QTest.Max = 30000
                ' Presión de fondo fluyendo
                Ptest.Nomb = "Presión de fondo fluyendo [kg/cm2]" : Ptest.Min = PRes.Min - 1 : Ptest.Max = PRes.Max - 1

                ' Datos del modelo de Darcy

                ' Daño Mecánico / Geometrico
                ' 0: Enter Skin by Hand
                ' 1: Loke
                ' 2: MacLeod
                ' 3: Karakas - Tariq
                MGSkinMethod.Nomb = "Modelo de daño Mecánico / Geometrico" : MGSkinMethod.Min = 0 : MGSkinMethod.Max = 3
                ' Daño por desviación y penetración parcial
                ' 0: Cinco / Martín-Bronz
                ' 1: Wong-Cliford
                ' 2: Cinco (2) / Martín-Bronz
                DPSkinMethod.Nomb = "Modelo de Daño por desviación y penetración parcial " : DPSkinMethod.Min = 0 : DPSkinMethod.Max = 2
                ' Permeabilidad del yacimiento
                ResPerm.Nomb = "Permeabilidad del yacimiento [md]" : ResPerm.Min = 1 : ResPerm.Max = 5000
                ' Espesor del yacimiento
                Thickness.Nomb = "Espesor del yacimiento [m]" : Thickness.Min = 1 : Thickness.Max = 50
                ' Área de drene
                Drainage.Nomb = "Área de drene [m2]" : Drainage.Min = 100 : Drainage.Max = 5000
                ' Factor de forma
                Dietz.Nomb = "Factor de forma [adim]" : Dietz.Min = 0.008 : Dietz.Max = 32
                ' Radio del pozo
                WBR.Nomb = "Radio del pozo [pg]" : WBR.Min = 3.5 : WBR.Max = 13
                ' Factor de daño total
                Skin.Nomb = "Factor de daño total [adim]" : Skin.Min = -10 : Skin.Max = 10


                ' Datos del BNC

                ' Emplear RGIL o Qgi
                '      0: Usar RGIL
                '      1: Usar Qgi
                Entry.Nomb = "Emplear RGIL o Qgi " : Entry.Min = 0 : Entry.Max = 1
                ' Método de cálculo del BN.
                ' 0: Profundidad de Inyección Fija
                ' 1: Profundidad de Inyección Optima
                ' 2: Especificar la profundidad de las válvulas
                Method.Nomb = "Método de cálculo del BNC" : Method.Min = 0 : Method.Max = 2
                ' Densidad del gas de bombeo neumático
                Gravity.Nomb = "Densidad del gas de bombeo neumático " : Gravity.Min = 0.65 : Gravity.Max = 0.85
                ' % Mol de H2S
                H2S.Nomb = "% Mol de H2S " : H2S.Min = 0 : H2S.Max = 30
                ' % Mol de CO2
                CO2.Nomb = "% Mol de CO2 " : CO2.Min = 0 : CO2.Max = 30
                ' % Mol de N2
                N2.Nomb = "% Mol de N2 " : N2.Min = 0 : N2.Max = 30
                ' Gasto de gas de inyección mmpcd
                GLRiny.Nomb = "Gasto de gas de inyección mmpcd" : GLRiny.Min = 0 : GLRiny.Max = 7
                ' Relación gas inyectado líquido m3/m3
                GLRate.Nomb = "Relación gas inyectado líquido [m3/m3] " : GLRate.Min = 0 : GLRate.Max = 50
                ' Profundidad de la válvula de inyección
                ValveDepth.Nomb = "Profundidad de la válvula de inyección [m]" : ValveDepth.Min = 100 : ValveDepth.Max = 5000
                ' Diametro de la válvula de BNC
                DiamValBNC.Nomb = "Diámetro de la válvula de BNC [pg]" : DiamValBNC.Min = 8 / 64 : DiamValBNC.Max = 3 / 4
                ' Datos para el ajuste
                ' Presión en la cabeza del pozo
                THPres.Nomb = "Presión en la cabeza del pozo [kg/cm2] " : THPres.Min = 3 : THPres.Max = 30
                ' Temperatura en la cabeza del pozo (Fluyendo)
                THTemp.Nomb = "Temperatura en la cabeza del pozo (Fluyendo) [oC] " : THTemp.Min = 30 : THTemp.Max = 130
                ' RGA del aforo
                RGA_Aforo.Nomb = "RGA del aforo m3/m3 " : RGA_Aforo.Min = 30 : RGA_Aforo.Max = 5000
                ' Presion en la TR (BNC)
                TRPres.Nomb = "Presión en la TR (BNC) [kg/cm2] " : TRPres.Min = 0 : TRPres.Max = 5000
                ' Gatos de Aceite BPD
                Qo.Nomb = "Gasto de Aceite [STBPD]" : Qo.Min = 0 : Qo.Max = 100000
                ' Gasto de Gas de inyección de BN mínimo
                QgiMin.Nomb = "Gasto de Gas de inyección de BN mínimo [MMSCFPD]" : QgiMin.Min = 0 : QgiMin.Max = 20
                ' Gasto de Gas de inyección de BN máximo
                QgiMin.Nomb = "Gasto de Gas de inyección de BN máximo [MMSCFPD]" : QgiMax.Min = 0 : QgiMax.Max = 20
                ' Numero de Datos de Estado Mecánico
                NumDatEdoMec.Nomb = "Número de datos de estado Mecánico" : NumDatEdoMec.Min = 0 : NumDatEdoMec.Max = 20
                ' Tipo de accesorio o tubería en el estado mecánico
                ' 0=Ningúno
                ' 1=Tubing
                ' 2=SSSV
                ' 3=Restriction
                ' 4=Cassing
                DType.Nomb = "Tipo de accesorio o tubería en el estado mecánico conteniedo hasta 20 datos" : DType.Min = 0 : DType.Max = 4
                ' Profundidades del estado mecánico
                Depth.Nomb = "Profundidad del estado Mecanico [mD], hasta 20 datos" : Depth.Min = 0 : Depth.Max = 20000
                ' Diámetro Interior de la Tuberías
                TID.Nomb = "Diámetro Interior de las tuberías de producción del estado mecánico [pulgadas], hasta 20 datos" : TID.Min = 0 : TID.Max = 30
                'Coeficiente Rugosidad interior de la tubería de producción
                TIR.Nomb = "Coeficiente de rugosidad interior de las tuberías de producción del estado mecánico [pulgadas], hasta 20 datos" : TIR.Min = 0 : TIR.Max = 1
                ' Diametro Exterior de la tuberías de producción
                TOD.Nomb = "Diámetro Exterior de las tubuerías de producción del estado mecánico [pulgadas], hasta 20 datos" : TOD.Min = 0 : TOD.Max = 36
                'Coeficiente Rugosidad exterior de la tubería de producción
                TOR.Nomb = "Coeficiente de rugosidad exterior de las tuberías de producción del estado mecánico [pulgadas], hasta 20 datos" : TOR.Min = 0 : TOR.Max = 1
                'Diámetro interno de la tubería de revestimiento
                CID.Nomb = "Diámetro interno de la tubería de revestimiento del estado mecánico [pulgadas], hasta 20 datos" : CID.Min = 0 : CID.Max = 36
                'Coeficiente de rugosidad interno de la tubería de revestimiento
                CIR.Nomb = "Coeficiente de rugosidad interno de la tubería de revestimiento del estado mecánico [pulgadas], hasta 20 datos" : CIR.Min = 0 : CIR.Max = 1
                'Numero de Datos de la trayectoria
                NumDatTrayecto.Nomb = "Número de datos de la trayectoria del pozo" : NumDatTrayecto.Min = 2 : NumDatTrayecto.Max = 20
                'Habilitador de datos de la trayectoria
                RDEnable.Nomb = "Habilita de datos de trayectoria del pozo [adimensional], hasta 20 datos" : RDEnable.Min = 0 : RDEnable.Max = 1
                'Datos de trayectoria del pozo en  metros desarrollados
                RDMd.Nomb = "Datos de metros desarrollados [mD], hasta 20 datos" : RDMd.Min = 0 : RDMd.Max = 20000
                'Datos de trayectoria del pozo en metros verticales
                RDTvd.Nomb = "Datos de trayectoria del pozo en metros verticales [mV], hasta 20 datos" : RDTvd.Min = 0 : RDTvd.Max = 20000
                'Número de datos del perfil de temperatura en el pozo
                NumDatTemp.Nomb = "Número de datos del perfil de temperatura del pozo" : NumDatTemp.Min = 2 : NumDatTemp.Max = 20
                'Coeficiente de Transferencia de Calor Inicial
                Htc.Nomb = "Coeficiente de Transferencia de Calor Inicial [W/m2/oK] " : Htc.Min = 1 : Htc.Max = 8
                'Profundidad del perfil de temperatura en metros desarrollados
                PTMd.Nomb = "Profundidad del perfil de temperatura en metros desarrollados [mD], hasta 20 datos" : PTMd.Min = 0 : PTMd.Max = 20000
                'Temperatura del perfil de temperatura en °C
                PTTmp.Nomb = "Temperatura del perfil de temperatura [°C], hasta 20 datos" : PTTmp.Min = 0 : PTTmp.Max = 300
                'Nivel medio del intervalo disparado
                NivMedDisp.Nomb = "Nivel medio del intervalo Disparado [mD]" : NivMedDisp.Min = 0 : NivMedDisp.Max = 20000

                'Datos de entrada exclusivamente de BEC
                'Frecuencia mínima de BEC
                FrecMin.Nomb = "Frecuencia mínima de la bomba BEC [Hz]" : FrecMin.Min = 10 : FrecMin.Min = 200
                'Frecuencia máxima de BEC
                FrecMax.Nomb = "Frecuencia máxima de la bomba BEC [Hz]" : FrecMax.Min = 10 : FrecMax.Min = 200
                ' Profundidad del BEC
                Prof_BEC.Nomb = "Profundidad del BEC [mD]" : Prof_BEC.Min = 0 : Prof_BEC.Max = 20000
                'Frecuencia de operación del BEC
                Frec_BEC.Nomb = "Frecuencia de operación del BEC [Hz]" : Frec_BEC.Min = 10 : Frec_BEC.Max = 200
                ' Diámetro exterior mayor del BEC
                ODMax_BEC.Nomb = "Diámetro exterior mayor del BEC [pulgadas]" : ODMax_BEC.Min = 1 : ODMax_BEC.Max = 20
                'Longitud del cable del BEC
                LongCable_BEC.Nomb = "Longitud del cable BEC [m]" : LongCable_BEC.Min = 0 : LongCable_BEC.Max = 20000
                'Eficiencia del separador del Gas
                EfiSepGas_BEC.Nomb = "Eficiencia del separador de gas del BEC [procentaje]" : EfiSepGas_BEC.Min = 0 : EfiSepGas_BEC.Max = 100
                'Etapas de la bomba BEC
                Etapas_BEC.Nomb = "Etapas de la bomba BEC" : Etapas_BEC.Min = 1 : Etapas_BEC.Max = 1000
                'Voltaje en la superficie para la bomba BEC
                VoltSup_BEC.Nomb = "Voltaje en superficie para el BEC [Volts]" : VoltSup_BEC.Min = 1 : VoltSup_BEC.Max = 20000
                'Grado de desgaste del BEC
                Desgaste_BEC.Nomb = "Grado de desgaste del BEC [fracción]" : Desgaste_BEC.Min = 0 : Desgaste_BEC.Max = 1
                'Modelo de reducción del gas
                '0= ninguno
                ReducGas_BEC.Nomb = "Modelo de reducción del gas" : ReducGas_BEC.Min = 0 : ReducGas_BEC.Max = 1
                'Bomba para el BEC, el cual permite selección una bomba de 0 hasta el 568 de catálogo de las bombas BEC
                Bomba_BEC.Nomb = "Bomba BEC de 0 hasta 568 del catálogo de bombas" : Bomba_BEC.Min = 0 : Bomba_BEC.Max = 568
                'Motor para el BEC, el cual permite selección de un motor de 0 hasta el 810 del catálogo de motores BEC
                Motor_BEC.Nomb = "Motor BEC de 0 hasta 810 del catálogo de motores" : Motor_BEC.Min = 0 : Motor_BEC.Max = 810
                'Cable para el BEC, el cual permite selección de un cable de 0 hasta el 12 del catálogo de cables BEC
                Cable_BEC.Nomb = "Cable BEC de 0 hasta 12 del catálogo de cables" : Cable_BEC.Min = 0 : Cable_BEC.Max = 12
                'Presión succión de la bomba BEC
                PreSuc_BEC.Nomb = "Presión de Succión de la Bomba BEC [kg/cm2]" : PreSuc_BEC.Min = 0 : PreSuc_BEC.Max = 20000
                'Corriente de operación en superficie del BEC
                Corriente_BEC.Nomb = "Corriente de operación del BEC [Amperes]" : Corriente_BEC.Min = 0 : Corriente_BEC.Max = 20000
                'Potencia de operación en superficie del BEC
                Potencia_BEC.Nomb = "Potencia de operación del BEC [HP]" : Potencia_BEC.Min = 0 : Potencia_BEC.Max = 20000
                PreDes_BEC.Nomb = "Presión de Descarga de la Bomba BEC [kg/cm2]" : PreDes_BEC.Min = 0 : PreDes_BEC.Max = 20000


            Catch ex As Exception
                Iniciar = 0
                MsgSub &= GetFrmt(ex.ToString, 2)
                Throw New Exception(MsgSub)
            End Try

        End Sub

        ''' <summary>
        ''' FUNCION PRINCIPAL QUE LLAMA LAS FUNCIONES Y METODOS
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Public Function ModeloProsper() As Boolean

            If Ayuda Then
                MsgAyuda = getAyuda()
                IErr = True
            ElseIf Diagnostico Then
                MsgDiagnostico = getDiagnostics()
                IErr = True
            ElseIf MsgErrDatos.Length > 0 Then
                ' Se genera un error debido a los rangos de los datos
                MsgErrDatos &= GetFrmt("Clase: FormatoClase. Variables fuera de rango.", 2)
                IErr = True
            ElseIf Iniciar < 3 Then
                MsgErrDatos &= GetFrmt("Se necesita datos de la Trayectoria, la Tuberia y la Temperatura del pozo", 2) & MsgErrDatos
                MsgErrDatos &= GetFrmt("Número de datos del estado mecánico        : " & NumDatEdoMec.ToString, 2) & MsgErrDatos
                MsgErrDatos &= GetFrmt("Número de datos de la trayectoria del pozo : " & NumDatTrayecto.ToString, 2) & MsgErrDatos
                MsgErrDatos &= GetFrmt("Número de datos de la temoeratura del pozo : " & NumDatTemp.ToString, 2) & MsgErrDatos
                IErr = True
            Else
                'Dim Prosper_Id As Object
                Dim textRast As String = ""
                Dim title As String = "Rastreo 1/2"
                Dim style As MsgBoxStyle = MsgBoxStyle.DefaultButton1 Or MsgBoxStyle.Information Or MsgBoxStyle.OkCancel
                Dim response As MsgBoxResult

                Try


                    Me.Server = Connect()


                    DoCmd("PROSPER.OPENFILE=""" & ArchivoPVT & """")


                    If Pvt IsNot Nothing Then
                        DoCmd("PROSPER.RESET(ALL)")
                        DoCmd("PROSPER.SETUNITSYS(""Pemex"")")
                        Pvt.Execute()
                    Else
                        DoCmd("PROSPER.SETUNITSYS(""Pemex"")")
                    End If


                    'Tipo de Calculos deseados y Datos Generales
                    DatosGenerales()




                    ' Nota importante para el manejo de los volúmenes de gas:
                    '
                    ' Qg libre  = Qg aforo - Qg Disuelto(PVT)
                    ' RGA libre = (Qg libre * 1E6)/ (Qo * 5.615)
                    ' m3/m3     = (pie3/dia) / (bl/dia)
                    '
                    ' Siempre considerar:
                    '                    
                    ' Qg aforo >= Qg Disuelto(PVT) de lo contrario tomar:
                    ' Qg aforo = Qg Disuelto(PVT)

                    ' Gasto de aceite (QTest es el gasto de líquido)
                    Qo.Val = (QTest.Val * (100 - Wc.Val)) / 100
                    ' RGA del PVT @ Pws y Tws
                    GOR_PTy = RGA_PVT_PwsyTws()
                    'GOR_PTy = RGA_PVT_PwsyTws(PRes, TRes)
                    ' Gasto de gas disuelto @ Pws y Tws (MMpie3/día)
                    Qg_PTy = (GOR_PTy * Qo.Val * 5.615) / 1000000
                    ' Gasto de gas del aforo (MMpie3/día)
                    Qg_Aforo = (5.615 * RGA_Aforo.Val * Qo.Val) / 1000000
                    ' Relacion Gas Libre Aceite
                    GORFree = 0 '((Qg_Aforo - Qg_PTy) * 1000000) / (Qo.Val * 5.615)
                    ' Gasto de gas libre (MMpie3/día)
                    Qg_Libre = (GORFree * Qo.Val * 5.615) / 1000000
                    '  Gasto de gas total (MMpie3/día)
                    Qg_Total = Qg_Libre + Qg_PTy
                    ' Relación gas aceite total
                    GOR_Total = GOR_PTy + GORFree

                    ' RGIL ((Qgi/QL) * 1e6)/5.615
                    GLRiny.Val = GLRate.Val / QTest.Val * 1000000 / 5.615
                    GrvGas = DoGet("PROSPER.PVT.INPUT.GRVGAS")
                    API = DoGet("PROSPER.PVT.INPUT.API")
                    Dro = 141.5 / (131.5 + API)

                    If Rastreo Then
                        textRast &= GetFrmt("Clase: Modelo Prosper.                                                              ", 1)
                        textRast &= GetFrmt(StrDup(75, "-"), 1)
                        textRast &= GetFrmt("Resultados de los cálculos.                                                                    ", 1)
                        textRast &= GetFrmt(StrDup(75, "-"), 1)
                        textRast &= GetFrmt("Produccion de aceite = " & Math.Round(Qo.Val, 3).ToString & " [bl/dia]", 5)
                        textRast &= GetFrmt("Relacion gas aceite @ Pws y Tws = " & Math.Round(GOR_PTy, 3).ToString & " [m3/m3]", 5)
                        textRast &= GetFrmt("Relacion gas libre aceite = " & Math.Round(GORFree, 3).ToString & " [m3/m3]", 5)
                        textRast &= GetFrmt("Gasto de gas disuelto @ Pws y Tws = " & Math.Round(Qg_PTy, 3).ToString & " [MMpie3/día]", 5)
                        textRast &= GetFrmt("Gasto de gas del aforo = " & Math.Round(Qg_Aforo, 3).ToString & " [MMpie3/día]", 5)
                        textRast &= GetFrmt("Gasto de gas libre = " & Math.Round(Qg_Libre, 3).ToString & " [MMpie3/día]", 5)

                        'textRast &= GetFrmt(" = " & CStr() & " []", 5)
                        textRast &= GetFrmt(StrDup(75, "-"), 1)

                        response = MsgBox(textRast, style, title)
                        If response = MsgBoxResult.Cancel Then
                            'Return False
                        Else
                            textRast = ""
                            title = "Rastreo 2/' Comparación de correlaciones"
                        End If
                    End If

                    ' Se actualizan algunos cambios
                    'DoCmd("PROSPER.Refresh")

                    ' Se envian los datos a Prosper
                    EdoMec_Dat()
                    RegDesv()
                    PerfilTemp()
                    TubDescarga(PTTmp.Val(0))

                    ' Nivel medio de los disparos
                    NivMedDisp.Val = Depth.Val(NumDatEdoMec.Val - 1)

                    ' Cálculo de la IPR
                    If Yacimiento() Then Exit Function

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
                    NumCorr.Val = 10
                    Select Case LiftMethod.Val
                        Case 1
                            Configura_BN()  '--------------------->>>>>>>>>>
                            ' Comparación de correlaciones y Ajuste de VLP/IPR
                            If Compara_Correlaciones_BN() Then Exit Function
                            ' Ajuste de las correlaciones
                            If VLPIPR() = False Then Exit Function
                            ReDim NomVLPIPR(1)
                            NomVLPIPR(1) = "IPR" : NomVLPIPR(0) = "VLP, " & NomCorr(0)
                            CorrVFP = NumCorrVFP(CorrVFP)
                            If Sensibilidad_BN() Then Exit Function
                            If QuickLook_BN() Then Exit Function
                            If SensAgua_BN() Then Exit Function
                        Case 2
                            Configura_BEC()  '--------------------->>>>>>>>>>
                            ' Comparación de correlaciones y Ajuste de VLP/IPR
                            If Compara_Correlaciones_BEC() = False Then Exit Function
                            ' Ajuste de las correlaciones

                            ReDim NomVLPIPR(1)
                            NomVLPIPR(1) = "IPR" : NomVLPIPR(0) = "VLP, " & NomCorr(0)
                            CorrVFP = NumCorrVFP(CorrVFP)
                            If Sensibilidad_BEC() Then Exit Function
                            If QuickLook_BEC() Then Exit Function
                            If SensAgua_BEC() Then Exit Function
                            If Carta_BEC() Then Exit Function
                            'If Diseño_BEC() Then Exit Function
                            If VLPIPR() = False Then Exit Function
                        Case Else
                            Throw New Exception("No existe el Sistema Artificial de Producción")
                    End Select

                    If SaveFile Then
                        DoCmd("PROSPER.SAVEFILE")
                    End If

                    Disconnect()
                    Return True
                Catch ex As Exception

                    Disconnect()
                    Throw New Exception("ModeloProsper: " + ex.Message)
                End Try
            End If
            Return IErr
        End Function






        ''' <summary>
        ''' Se envían los datos generales a Prosper
        ''' </summary>
        Private Sub DatosGenerales()

            'If Reset Then

            'End If

            '
            ' Fluido: 0 = Oil and Water
            '         1 = Dry and Wet Gas
            '         2 = Retrograde Condensate
            '
            DoSet("PROSPER.SIN.SUM.Fluid", Fluid.Val)
            '
            ' PVTModel: 0 = Black Oil
            '           1 = Equation of State
            '
            DoSet("PROSPER.SIN.SUM.PVTModel", PVTModel.Val)
            '
            ' Separator: 0 = Single Stage Separator
            '            1 = Two Stage Separator
            '
            DoSet("PROSPER.SIN.SUM.Separator", Separator.Val)
            '
            ' Emulsion: 0 = No
            '           1 = Emulsion + Pump Viscosity Correction
            '
            DoSet("PROSPER.SIN.SUM.Emulsion", Emulsion.Val)
            '
            ' Hydrate: 0 = Disable Warning
            '          1 = Enable Warning
            '
            DoSet("PROSPER.SIN.SUM.Hydrate", Hydrate.Val)
            '
            ' WatVis: 0 = Use Default Correlation
            '         1 = Use Pressure Corrected Correlation
            '
            DoSet("PROSPER.SIN.SUM.WatVis", WatVis.Val)
            '
            ' VisMod: 0 = Newtonian Fluid
            '         1 = Non Newtonian Fluid
            '
            DoSet("PROSPER.SIN.SUM.VisMod", VisMod.Val)
            '
            ' FlowType: 0 = Tubing Flow
            '           1 = Annular Flow
            '
            DoSet("PROSPER.SIN.SUM.FlowType", FlowType.Val)
            '
            ' WellType: 0 = Producer
            '           1 = Inyector
            '           2 = Water Inyector
            '
            DoSet("PROSPER.SIN.SUM.WellType", WellType.Val)
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
            DoSet("PROSPER.SIN.SUM.LiftMethod", LiftMethod.Val)
            '
            ' LiftType: 0 = No Friction Loss In Annulus
            '           1 = Friction Loss In Annulus
            '           2 = Safety Equipment
            '
            DoSet("PROSPER.SIN.SUM.LiftType[1]", LiftType.Val)

            DoSet("PROSPER.SIN.SUM.Company", Company)
            DoSet("PROSPER.SIN.SUM.Field", Field)
            DoSet("PROSPER.SIN.SUM.Location", Locations)
            DoSet("PROSPER.SIN.SUM.Well", Well)
            DoSet("PROSPER.SIN.SUM.Platform", Platform)
            DoSet("PROSPER.SIN.SUM.Analyst", Analyst)
            DoSet("PROSPER.SIN.SUM.Date", DatGenDate)
            DoSet("PROSPER.Sin.Sum.Comments", Comenta)

            '
            ' Predict: 0 = Pressure Only
            '          1 = Pressure and Temperature (offshore)
            '          2 = Pressure and Temperature (on Land)        '
            DoSet("PROSPER.SIN.SUM.Predict", Predict.Val)
            '
            ' TempModel: 0 = Rough Approximation
            '            1 = Enthalpy Balance
            '            2 = Improved Approximation
            DoSet("PROSPER.SIN.SUM.TempModel", TempModel.Val)
            '
            ' RangeSystem: 0 = Full System
            '              1 = Pipeline Only
            DoSet("PROSPER.SIN.SUM.Range", RangeSystem.Val)
            '
            ' OutputRes: 0 = Show Calculating Data
            '            1 = Hide Calculating Data
            DoSet("PROSPER.SIN.SUM.Output", OutputRes.Val)
            '
            ' Completion: 0 = Cased Hole
            '             1 = Open hole
            DoSet("PROSPER.SIN.SUM.Completion", Completion.Val)
            '
            ' GravelPack: 0 = None
            '             1 = Gravel Pack
            '             2 = Pre Packed Screen
            '             3 = Wire Wrapped Screen
            '             4 = Slotted Liner
            DoSet("PROSPER.SIN.SUM.GravelPack", GravelPack.Val)
            '
            ' InflowType: 0 = Single Branch
            '             1 = MultiLateral Well
            DoSet("PROSPER.SIN.SUM.InflowType", InflowType.Val)
            '
            ' GasConing: 0 = No
            '            1 = Yes
            DoSet("PROSPER.SIN.SUM.GasConing", GasConing.Val)



        End Sub

        ''' <summary>
        ''' Se envían los datos a las condiciones del yacimiento para determinar cuanto gas esta disuelto en el aceite
        ''' </summary>
        ''' <returns> Regresa el valor de la RGA a condiciones de presión y temperatura del yacimiento</returns>
        Private Function RGA_PVT_PwsyTws() As Double

            ' RGA del PVT @ Pws y Tws
            Dim Solgor As Double

            ' Evaluación del PVT a Pres Yac y Temp Yac
            DoSet("PROSPER.PVT.Calc.TempMin", TRes.Val)
            DoSet("PROSPER.PVT.Calc.TempMax", TRes.Val)
            DoSet("PROSPER.PVT.Calc.TempStep", 1)

            DoSet("PROSPER.PVT.Calc.PresMin", PRes.Val)
            DoSet("PROSPER.PVT.Calc.PresMax", PRes.Val)
            DoSet("PROSPER.PVT.Calc.PresStep", 1)

            DoCmd("PROSPER.PVT.CALC")

            ' Valor calculado por Prosper
            Solgor = DoGet("PROSPER.PVT.Calc.Results[0].GOR")

            ' Relación Gas Aceite tomada de la columna de los datos del yacimiento debido a que se debe filtrar.
            DoSet("PROSPER.PVT.Input.Solgor", Solgor)

            Return Solgor

        End Function

        ''' <summary>
        ''' Se determina el flujo en el yacimiento
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function Yacimiento() As Boolean

            ' Resultados de la IPR
            Dim AOF As Double = 0
            ReDim IPR_RTEL(19)
            ReDim IPR_PWF(19)
            ReDim NumDatIPR(1)
            ReDim NomSerie(1)


            DoSet("PROSPER.Sin.IPR.Single.Pres", PRes.Val)
            DoSet("PROSPER.Sin.IPR.Single.Tres", TRes.Val)
            DoSet("PROSPER.SIN.IPR.Single.Wc", Wc.Val)
            DoSet("PROSPER.Sin.IPR.Single.totgor", GOR_Total)

            DoSet("PROSPER.SIN.IPR.Single.Compact", Compact.Val)
            DoSet("PROSPER.SIN.IPR.Single.IRELK", IRELK.Val)

            ' Metodo de IPR ->
            ' 0: Indice de Productividad
            ' 1: Vogel
            ' 2: Compuesto (Incluye Produccion de Agua)
            ' 3: Darcy
            ' 4: Fetkovich
            ' 5: Fetkovich Multigasto
            ' 6: Jones
            ' 7: Jones Multigasto
            ' 8: Transitorio
            ' 9: Pozo Fracturado Hidraulicamente
            ' 10: Pozo Horizontal sin Flujo en las Fronteras
            ' 11: Pozo Horizontal con Presion Constante en la Frontera
            ' 12: Yacimiento MultiCapa
            ' 13: Calculado en Forma Externa
            ' 14: Pozo Horizontal con Perdida de Presion en el Pozo
            ' 15: Yacimiento Multicapa con Perdida de Presion en el Pozo
            ' 16: SkinAide (ELF)
            ' 17: Doble Porosidad
            ' 18: Pozo Horizontal Atravesado por Fracturas Verticales
            DoSet("PROSPER.Sin.IPR.Single.MGSkinMethod", MGSkinMethod.Val)
            DoSet("PROSPER.Sin.IPR.Single.IprMethod", IPRMethod.Val)



            Select Case IPRMethod.Val
                Case 0 ' PI Entry
                    DoSet("PROSPER.SIN.IPR.Single.Pindex", PI.Val)
                Case 1 ' Vogel

                    DoSet("PROSPER.SIN.IPR.Single.Qtest", QTest.Val)
                    DoSet("PROSPER.Sin.IPR.Single.Ptest", Ptest.Val)

                Case 3 ' Darcy

                    ' Correlación para el cálculo del factor de daño Mecánico/Geometrico
                    ' 0: Proporcionado a mano
                    ' 1: Locke
                    ' 2: McLeod
                    ' 3: Karakas+Tariq
                    MGSkinMethod.Val = 0

                    ' Daño por desviación y penetración parcial
                    ' 0: Cinco / Martín-Bronz
                    ' 1: Wong-Cliford
                    ' 2: Cinco (2) / Martín-Bronz
                    DPSkinMethod.Val = 0

                    ' Datos del modelo de Darcy
                    DoSet("PROSPER.Sin.IPR.Single.ResPerm", ResPerm.Val)
                    DoSet("PROSPER.SIN.IPR.Single.Thickness", Thickness.Val)
                    DoSet("PROSPER.SIN.IPR.Single.Drainage", Drainage.Val)
                    DoSet("PROSPER.SIN.IPR.Single.Dietz", Dietz.Val)
                    DoSet("PROSPER.SIN.IPR.Single.WBR", WBR.Val)
                    DoSet("PROSPER.SIN.IPR.Single.Skin", Skin.Val)
                Case Else
                    Throw New Exception("Yacimiento: Error IprMethod = " & IPRMethod.ToString & " , aún no se ha implementado")
            End Select

            ' Ejecutar el calculo de la IPR
            DoCmd("PROSPER.IPR.CALC")

            ' Resultados de la IPR

            NumDatIPR(0) = DoGet("PROSPER.Sin.IPR.Single.RTLIST.Count")
            'ReDim Preserve Ql(1, NumCalcs)
            'ReDim Preserve Pwf(1, NumCalcs)

            For I = 0 To NumDatIPR(0) - 1
                IPR_RTEL(I) = DoGet("PROSPER.SIN.IPR.Single.RTLIST[" & CStr(I) & "]")
                IPR_PWF(I) = DoGet("PROSPER.Sin.IPR.Single.PWFLST[" & CStr(I) & "]")
            Next I

            '*** Get AOF
            AOF = DoGet("PROSPER.IPR.AOF[0]") ' De la capa 0

            ' Interpolación de la IPR para obtener Pwf
            'QLiqido = QTest

            ' Interpolación de la IPR para obtener Pwf
            'Pwf_Inter = FlaGr(Ql, Pwf, QLiqido, 2, NumCalcs, IErr)

            'DoCmd("PROSPER.Refresh")

        End Function

        ''' <summary>
        ''' Se envían a Prosper los datos del bombeo neumatico contínuo
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function Configura_BN() As Boolean
            'Configura el sistema artificial de producción de bombeo neumático

            ' Densidad del gas de bombeo neumatico
            DoSet("PROSPER.Sin.GLF.Gravity", Gravity.Val)
            '  % Mol de H2S
            DoSet("PROSPER.SIN.GLF.H2S", H2S.Val)
            '  % Mol de CO2
            DoSet("PROSPER.SIN.GLF.CO2", CO2.Val)
            '  % Mol de N2
            DoSet("PROSPER.SIN.GLF.N2", N2.Val)
            ' RGIL
            DoSet("PROSPER.SIN.GLF.GLRinj", GLRiny.Val)
            ' Qgi
            DoSet("PROSPER.SIN.GLF.GLRate", GLRate.Val)
            ' Emplear RGIL o Qgi
            '      0: Usar RGIL
            '      1: Usar Qgi
            DoSet("PROSPER.SIN.GLF.Entry", Entry.Val)
            ' Metodo de  calculo del BN.
            ' 0: Profundidad de Inyección Fija
            ' 1: Profundidad de Inyeccion Optima
            ' 2: Especificar la profundidad de las valvulas
            DoSet("PROSPER.SIN.GLF.Method", Method.Val)
            ' Profundidad de la valvula de inyeccion
            DoSet("PROSPER.SIN.GLF.ValveDepth", ValveDepth.Val)
        End Function
        Private Function Configura_BEC() As Boolean
            'Configura el sistema artificial de producción de Bombeo Electrocentrifugo
            'Bomba
            Dim Bb = DoGet("PROSPER.ESP.SELECTPUMP (" & CStr(Bomba_BEC.Val) & ")")
            'Motor-Potencia
            Dim MP = DoGet("PROSPER.ESP.SELECTMOTOR(" & CStr(Motor_BEC.Val) & "," & CStr(PotenciaMotor_BEC.Val) & ")")
            'Cable
            Dim Cb = DoGet("PROSPER.ESP.SELECTCABLE(" & CStr(Cable_BEC.Val) & ")")
            'Profundidad de la bomba en mD
            DoSet("PROSPER.SIN.ESP.Depth", Prof_BEC.Val)
            'Frecuencia de operación de la bambo en Hz
            DoSet("PROSPER.SIN.ESP.Frequency", Frec_BEC.Val)
            'Máximo diametro exterior de la bomba en pulgadas
            DoSet("PROSPER.SIN.ESP.MaxPumpOD", ODMax_BEC.Val)
            'Logitud del cable de potencia m
            DoSet("PROSPER.SIN.ESP.CableLength", LongCable_BEC.Val)
            'Eficiencia del separador del gas
            DoSet("PROSPER.SIN.ESP.Efficiency", EfiSepGas_BEC.Val)
            'Número de etapas de la bomba
            DoSet("PROSPER.SIN.ESP.Stages", Etapas_BEC.Val)
            'Voltaje en superficie en volts
            DoSet("PROSPER.SIN.ESP.Volts", VoltSup_BEC.Val)
            'Factor de desgaste de la bomba en fracciones
            DoSet("PROSPER.SIN.ESP.Wear", Desgaste_BEC.Val)
            'Modelo de reducción de gas
            DoSet("PROSPER.SIN.ESP.GDRflag", ReducGas_BEC.Val)
        End Function
        ''' <summary>
        ''' Se envían los datos del estado mecánico a Prosper
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function EdoMec_Dat() As Boolean

            Dim I As Integer = 0

            ' Datos del Estado Mecánico
            ' Type: 0: Árbol de válvulas
            '       1: TP
            '       2: Válvula de tormenta
            '       3: Restricción
            '       4: TR
            '
            For I = 0 To NumDatEdoMec.Val - 1
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].Label", Label(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].Type", DType.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].Depth", Depth.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].TID", TID.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].TIR", TIR.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].TOD", TOD.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].TOR", TOR.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].CID", CID.Val(I))
                DoSet("PROSPER.SIN.EQP.Down.Data[" & CStr(I) & "].CIR", CIR.Val(I))
            Next I

        End Function

        ''' <summary>
        ''' Se envían a Prosper los datos del registro de desviación
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function RegDesv() As Boolean

            Dim I As Integer = 0

            ' Registro de Desviaciones

            For I = 0 To NumDatTrayecto.Val - 1
                DoSet("Prosper.SIN.EQP.Devn.Data[" & CStr(I) & "].Enable", RDEnable.Val(I))
                DoSet("PROSPER.Sin.EQP.Devn.Data[" & CStr(I) & "].Md", RDMd.Val(I))
                DoSet("PROSPER.Sin.EQP.Devn.Data[" & CStr(I) & "].Tvd", RDTvd.Val(I))
            Next I

        End Function

        ''' <summary>
        ''' Se envía a Prosper los datos del perfil de temperatura
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function PerfilTemp() As Boolean
            Dim I As Integer = 0
            ' Perfil de Temperatura (Gradiente Geotermico)
            DoSet("PROSPER.SIN.EQP.Geo.Htc", Htc.Val)

            For I = 0 To NumDatTemp.Val - 1
                DoSet("PROSPER.SIN.EQP.Geo.Data[" & CStr(I) & "].Md", PTMd.Val(I))
                DoSet("PROSPER.SIN.EQP.Geo.Data[" & CStr(I) & "].Tmp", PTTmp.Val(I))
            Next I

        End Function
        ''' <summary>
        ''' Se envían a Prosper los datos de la línea de descarga
        ''' </summary>
        ''' <param name="TempTP"> Temperatura en la tubería de producción [oC]</param>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Public Shared Function TubDescarga(ByVal TempTP As Double) As Boolean
            ' Deshabilitar el equipo superficial
            DoSet("PROSPER.Sin.EQP.Surf.Disable", 0)
            DoSet("PROSPER.Sin.EQP.Surf.Tmp", TempTP)
            DoSet("PROSPER.Sin.EQP.Surf.Htc", 4)
            DoSet("PROSPER.Sin.EQP.Geo.Htc", 4)
        End Function

        ''' <summary>
        ''' Comparación de correlaciones y ajuste VLP/IPR.
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function Compara_Correlaciones_BN() As Boolean

            ' Ajuste VLP/IPR. 
            ' La sub rutina únicamente ajusta un solo juego de valores
            ' por lo que sólo se usa: DATA[0]

            Dim RATETYPE, ENABLE As Integer
            Dim VLPLABEL As String = ""
            Dim Htc As Double
            Dim TCCMsd, TMSDCC As Double
            ' Desde (DunsandRosModified) hasta (PetroleumExperts3)
            ReDim ProfDesa(9, 100)
            ReDim TVDCC(9, 100)
            ReDim PresWf(9, 100)
            ReDim NumDatCorr(9)
            ReDim NomCorr(9)


            ' Tipo de fluido producido -> 0: Liquido, 1: Aceite, 2: Gas.
            RATETYPE = 0
            ' Habilitar/Desabilitar los valores de DATA[i].
            ENABLE = 0
            ' Etiqueta
            VLPLABEL = "Prof. Estabilizacion"

            DoSet("PROSPER.ANL.VMT.RATETYPE", RATETYPE)
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].ENABLE", ENABLE)
            DoSet("PROSPER.ANL.VMT.Data[" & CStr(0) & "].label", VLPLABEL) ' "Prof. Estabilidad")

            ' Presion en la cabeza del pozo (Pth)
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].THPRES", THPres.Val)
            ' Temperatura en la cabeza del pozo Fluyendo (Thp) 
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].THTEMP", THTemp.Val)
            ' Corte de Agua. (Relación agua aceite WC)
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].WC", Wc.Val)
            ' Produccion de liquido
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].RATE", QTest.Val)
            ' Profundidad de medicion de la Pwf
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GDEPTH", NivMedDisp.Val)
            ' Presion de fondo fluyendo a la profundad de la medición
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GPRES", Ptest.Val)
            ' Presion de fondo estática (Pws) a la profundidad de la medición
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].PRES", PRes.Val)
            ' Relación gas aceite
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GOR", GOR_PTy)
            ' Relacion gas libre aceite (solo si se tiene conificacion de gas)
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GORFREE", GORFree)
            ' Gasto de Gas de Inyeccion.
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].IRATE", GLRate.Val)
            ' Profundidad de la valvula de inyeccion.
            DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].IDEPTH", ValveDepth.Val)



            ' Se procede a realizar el ajuste 
            DoCmd("PROSPER.ANL.VMT.UVAL")

            Htc = DoGet("Prosper.ANL.VMT.Data[" & CStr(0) & "].Uvalue")

            DoSet("PROSPER.SIN.EQP.Geo.Htc", Htc)


            ' Comparación de correlaciones
            ' Presión en la cabeza del pozo (Pth)
            DoSet("PROSPER.ANL.TCC.Pres", THPres.Val)
            ' Corte de Agua. (Relación agua aceite WC)
            DoSet("PROSPER.ANL.TCC.WC", Wc.Val)
            ' Producción de liquido
            DoSet("PROSPER.ANL.TCC.Rate", QTest.Val)
            ' Relación gas aceite
            DoSet("PROSPER.ANL.TCC.GOR ", GOR_PTy)
            ' Relación gas libre aceite (sólo si se tiene conificacion de gas)
            DoSet("PROSPER.ANL.TCC.GORFree", GORFree)

            DoSet("PROSPER.ANL.TCC.GLRate", GLRate.Val)
            DoSet("PROSPER.ANL.TCC.GLDepth", ValveDepth.Val)

            If Settings.GetBy("prosper_version") = "IPM 11" Then


            End If

            ' Tipo de producción
            ' 0 : Producción de liquido
            ' 1 : Producción de aceite
            DoSet("PROSPER.ANL.TCC.RateType", 0)

            '
            ' Correlación de la linea de descarga
            '
            ' 2  FancherBrown
            ' 4  MukerjeeBrill
            ' 5  BeggsandBrill
            ' 6  DuklerFlannigan
            ' 7  DuklerEatonFlannigan
            ' 13 BeggsandBrillGasHead
            ' 14 GREmodifiedbyPE
            ' 15 GREwithDSM
            ' 16 GREoriginal
            ' 17 GREwithAE
            ' 18 PetroleumExperts4
            ' 19 Hydro3P
            ' 20 PetroleumExperts5
            ' 21 OLGAS2P
            ' 22 OLGAS3P
            ' 23 OLGAS3PEXT
            '
            DoSet("PROSPER.ANL.TCC.Pipe", 5)

            ' EMR del estadomecánico
            TCCMsd = 1

            ' Datos medidos
            DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Msd", TCCMsd)
            DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Msd", NivMedDisp.Val)
            DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Prs", THPres.Val)
            DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Prs", Ptest.Val)

            ' Use the following list to to determine the value of corrname to select the correlation by name
            '
            ' 0       DunsandRosModified
            ' 1       HagedornBrown
            ' 2       FancherBrown
            ' 3       MukerjeeBrill
            ' 4       BeggsandBrill
            ' 5       PetroleumExperts
            ' 6       Orkiszewski
            ' 7       PetroleumExperts2
            ' 8       DunsandRosOriginal
            ' 9       PetroleumExperts3
            ' 10      GREmodifiedbyPE
            ' 11      PetroleumExperts4
            ' 12      Hydro3P
            ' 13      PetroleumExperts5
            ' 14      OLGAS2P
            ' 15      OLGAS3P
            ' 16      OLGAS3PEXT

            For I = 0 To 9 ' Solo hasta petroleum Expert 3
                DoSet("PROSPER.ANL.TCC.Corr[" & CStr(I) & "]", 1)
            Next I

            DoCmd("PROSPER.ANL.TCC.CALC")

            '
            ' Resultados de Comparación de Correlaciones 
            '
            Dim Indice, J As Integer
            Dim IndiceTmp As String
            ReDim NumCor(NumCorr.Val - 1)
            'Dim NumCor(NumCorr.Val - 1) As Integer
            Dim PwfCC(NumCorr.Val - 1), DifPwf(NumCorr.Val) As Double
            Dim LabelCC(NumCorr.Val - 1, 100) As String
            Dim TempCC(NumCorr.Val - 1, 100) As Double

            TMSDCC = Int(0)

            Try
                Dim max As Integer = 0

                ' Resultados: Escribe en la hoja comparacion de correlaciones
                For Indice = 0 To 9 'NumCorr.Val - 1 ' Correlacones a seleccionar (comienzan desde el cero)
                    IndiceTmp = CStr(IndiceCorr(Indice))
                    J = 0

                    Dim TotalCorrs As Integer = Integer.Parse(DoGet("PROSPER.OUT.TCC.Results[" + IndiceTmp.ToString() + "].COUNT"))

                    ' Do Until (TMSDCC >= Math.Round(NivMedDisp.Val - 0.5, 0)) ' For j = 1 To 100
                    For J = 0 To TotalCorrs - 1
                        'If J > max Then
                        '    max = J
                        '    ReDim Preserve LabelCC(NumCorr.Val - 1, J)
                        '    ReDim Preserve ProfDesa(NumCorr.Val - 1, J)
                        '    ReDim Preserve TVDCC(NumCorr.Val - 1, J)
                        '    ReDim Preserve PresWf(NumCorr.Val - 1, J)
                        '    ReDim Preserve TempCC(NumCorr.Val - 1, J)
                        'End If


                        ' Etiqueta
                        LabelCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Label[" & CStr(J) & "]")
                        ' Profundidad desarrollada
                        ProfDesa(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].MSD[" & CStr(J) & "]")
                        ' Profundidad vertical
                        TVDCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].TVD[" & CStr(J) & "]")
                        ' Presion
                        PresWf(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Pres[" & CStr(J) & "]")
                        ' Temperatura
                        TempCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Temp[" & CStr(J) & "]")
                        ' Profundidad desarrollada
                        TMSDCC = ProfDesa(Indice, J) 'DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].MSD[" & CStr(j - 1) & "]")
                        TMSDCC = Math.Round(TMSDCC, 0)
                        NumDatCorr(Indice) = J
                        'J = J + 1
                        ' Loop
                    Next
                    ' Presion en el intevalo medio productor
                    PwfCC(Indice) = PresWf(Indice, J - 1)
                    DifPwf(Indice) = Math.Abs(Ptest.Val - PwfCC(Indice))
                    NumCor(Indice) = Indice
                    NomCorr(Indice) = NombrCorr(Indice)
                    ' IPR = Qtest / (14.223 * DifPwf(Indice))  ' IPR (bl/día) / (lb/pg2)
                    TMSDCC = 0
                Next Indice


                ' Se ordenan las correlaciones de menor diferencia de presion a mayor diferencia DifPwf
                Call SortBubble(NumCorr.Val, NumCor, NomCorr, DifPwf, 3, True)

                'Se limpia la correlaciones
                For i = 0 To 9
                    DoSet("PROSPER.ANL.VMT.Corr[" & i & "]", 0)
                Next i
                ' Se seleccionan la correlaciones del mejor ajuste
                DoSet("PROSPER.ANL.VMT.Corr[" & CStr(NumCor(0)) & "]", 1)
                DoCmd("PROSPER.ANL.VMT.CALC")


                If Settings.GetBy("prosper_version") = "IPM 7.5" Then
                    ' Se seleccionan las tres primeras correlaciones
                    For Indice = 0 To 2
                        DoSet("PROSPER.ANL.VMT.Corr[" & CStr(NumCor(Indice)) & "]", 1)
                    Next Indice

                    DoCmd("PROSPER.ANL.VMT.CALC")
                    'Valores de Parametro 1 y Parametro 2

                    Dim TmpNomC(2) As String
                    Dim MsgTmp(2, 2) As String

                    For Indice = 0 To 2
                        TmpNomC(Indice) = NombrCorr_2(NumCor(Indice) + 1)
                        MsgTmp(Indice, 0) = DoGet("PROSPER.ANL.COR.Corr[{" & TmpNomC(Indice) & "}].A[" & CStr(0) & "]")
                        MsgTmp(Indice, 1) = DoGet("PROSPER.ANL.COR.Corr[{" & TmpNomC(Indice) & "}].A[" & CStr(1) & "]")
                        MsgTmp(Indice, 2) = DoGet("PROSPER.ANL.COR.Corr[{" & TmpNomC(Indice) & "}].A[" & CStr(2) & "]")
                    Next Indice
                End If


                'MsgBox(TmpNomC(0) & " : " & MsgTmp(0, 0) & MsgTmp(0, 1) & MsgTmp(0, 2) & Chr(13) & _
                '       TmpNomC(1) & " : " & MsgTmp(1, 0) & MsgTmp(1, 1) & MsgTmp(1, 2) & Chr(13) & _
                '       TmpNomC(2) & " : " & MsgTmp(2, 0) & MsgTmp(2, 1) & MsgTmp(2, 2))


                '' Se des seleccionan todas las correlaciones en "VLP Maching - Adjust IPR"
                For Indice = 0 To 23
                    DoSet("PROSPER.ANL.VMT.Corr[" & CStr(Indice) & "]", 0)
                Next Indice

                ' Se selecciona la mejor correlacion
                DoSet("PROSPER.ANL.VMT.Corr[" & CStr(NumCor(0)) & "]", 1)
                If Settings.GetBy("prosper_version") = "IPM 7.5" Then
                    DoCmd("PROSPER.ANL.VMT.CALC")
                End If

                CorrVFP = NumCor(0)

                Dim NombreCorrelacion As String = DoGet("PROSPER.ANL.VMT.Corrlabel[1],[1]")
                DoCmd("PROSPER.ANL.VMT.VLPIPR(" & CStr(NumCor(0) + 1) & ",1)")

            Catch ex As Exception
                Throw New Exception("Compara_Correlaciones_BN: " + ex.Message)
            End Try


        End Function

        Private Function Compara_Correlaciones_BEC() As Boolean


            Try
                ' Ajuste VLP/IPR. 
                ' La sub rutina únicamente ajusta un solo juego de valores
                ' por lo que sólo se usa: DATA[0]

                Dim RATETYPE, ENABLE As Integer
                Dim VLPLABEL As String = ""
                Dim Htc As Double
                Dim TCCMsd, TMSDCC As Double

                ' Tipo de fluido producido -> 0: Liquido, 1: Aceite, 2: Gas.
                RATETYPE = 0
                ' Habilitar/Desabilitar los valores de DATA[i].
                ENABLE = 0
                ' Etiqueta
                VLPLABEL = "Prof. Estabilizacion"

                DoSet("PROSPER.ANL.VMT.RATETYPE", RATETYPE)
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].ENABLE", ENABLE)
                DoSet("PROSPER.ANL.VMT.Data[" & CStr(0) & "].label", VLPLABEL) ' "Prof. Estabilidad")

                ' Presion en la cabeza del pozo (Pth)
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].THPRES", THPres.Val)
                ' Temperatura en la cabeza del pozo Fluyendo (Thp) 
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].THTEMP", THTemp.Val)
                ' Corte de Agua. (Relación agua aceite WC)
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].WC", Wc.Val)
                ' Produccion de liquido
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].RATE", QTest.Val)
                ' Profundidad de medicion de la Pwf
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GDEPTH", Prof_BEC.Val)
                ' Presion de fondo fluyendo a la profundad de la medición
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GPRES", PreDes_BEC.Val)
                ' Presion de fondo estática (Pws) a la profundidad de la medición
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].PRES", PRes.Val)
                ' Relación gas aceite
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GOR", GOR_PTy)
                ' Relacion gas libre aceite (solo si se tiene conificacion de gas)
                DoSet("PROSPER.ANL.VMT.DATA[" & CStr(0) & "].GORFREE", GORFree)
                'Freccuencia de operación 
                DoSet("PROSPER.ANL.VMT.Data[0].Freq", Frec_BEC.Val)
                'Factor de desgaste de la bomba
                DoSet("PROSPER.ANL.VMT.Data[0].Wear", Desgaste_BEC.Val)
                'Presión de succión de la bomba
                DoSet("PROSPER.ANL.VMT.Data[0].PIP", PreSuc_BEC.Val)
                'Presión de descarga de la bomba
                DoSet("PROSPER.ANL.VMT.Data[0].PDP", PreDes_BEC.Val)

                ' Se actualizan algunos cambios
                DoCmd("PROSPER.Refresh")
                ' Se procede a realizar el ajuste 
                DoCmd("PROSPER.ANL.VMT.UVAL")

                Htc = DoGet("Prosper.ANL.VMT.Data[" & CStr(0) & "].Uvalue")

                DoSet("PROSPER.SIN.EQP.Geo.Htc", Htc)

                ' Comparación de correlaciones
                ' Presión en la cabeza del pozo (Pth)
                DoSet("PROSPER.ANL.TCC.Pres", THPres.Val)
                ' Corte de Agua. (Relación agua aceite WC)
                DoSet("PROSPER.ANL.TCC.WC", Wc.Val)
                ' Producción de liquido
                DoSet("PROSPER.ANL.TCC.Rate", QTest.Val)
                ' Relación gas aceite
                DoSet("PROSPER.ANL.TCC.GOR ", GOR_PTy)
                ' Relación gas libre aceite (sólo si se tiene conificacion de gas)
                DoSet("PROSPER.ANL.TCC.GORFree", GORFree)

                ' Tipo de producción
                ' 0 : Producción de liquido
                ' 1 : Producción de aceite
                DoSet("PROSPER.ANL.TCC.RateType", 0)

                '
                ' Correlación de la linea de descarga
                '
                ' 2  FancherBrown
                ' 4  MukerjeeBrill
                ' 5  BeggsandBrill
                ' 6  DuklerFlannigan
                ' 7  DuklerEatonFlannigan
                ' 13 BeggsandBrillGasHead
                ' 14 GREmodifiedbyPE
                ' 15 GREwithDSM
                ' 16 GREoriginal
                ' 17 GREwithAE
                ' 18 PetroleumExperts4
                ' 19 Hydro3P
                ' 20 PetroleumExperts5
                ' 21 OLGAS2P
                ' 22 OLGAS3P
                ' 23 OLGAS3PEXT
                '
                DoSet("PROSPER.ANL.TCC.Pipe", 5)

                ' EMR del estadomecánico
                TCCMsd = 0

                ' Datos medidos
                'DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Msd", TCCMsd)
                'DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Msd", Prof_BEC.Val)
                'DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Prs", THPres.Val)
                'DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Prs", Ptest.Val)

                ' Use the following list to to determine the value of corrname to select the correlation by name
                '
                ' 0       PROSPER.ANL.VMT.Data[0].PIP
                ' 1       HagedornBrown
                ' 2       FancherBrown
                ' 3       MukerjeeBrill
                ' 4       BeggsandBrill
                ' 5       PetroleumExperts
                ' 6       Orkiszewski
                ' 7       PetroleumExperts2
                ' 8       DunsandRosOriginal
                ' 9       PetroleumExperts3
                ' 10      GREmodifiedbyPE
                ' 11      PetroleumExperts4
                ' 12      Hydro3P
                ' 13      PetroleumExperts5
                ' 14      OLGAS2P
                ' 15      OLGAS3P
                ' 16      OLGAS3PEXT

                For I = 0 To 9 ' Solo hasta petroleum Expert 3
                    DoSet("PROSPER.ANL.TCC.Corr[" & CStr(I) & "]", 1)
                Next I

                ''NUEVO MARDOQUEO
                'DoSet("PROSPER.ANL.TCC.Corr[" & NumCor(0) & "]", 1)
                'DoCmd("PROSPER.ANL.TCC.CALC")
                'Dim Pwfcalc As Double = DoGet("PROSPER.OUT.TCC.Results[" & NumCor(0) & "].Pres[160]")
                'DoSet("PROSPER.SIN.IPR.Single.Ptest", Pwfcalc)
                'DoCmd("PROSPER.IPR.CALC")
                ''TERMINA NUEVO MARDOQUEO

                ' Datos medidos
                DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Msd", TCCMsd)
                DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Msd", Prof_BEC.Val)
                DoSet("PROSPER.ANL.TCC.Comp[" & CStr(0) & "].Prs", THPres.Val)
                DoSet("PROSPER.ANL.TCC.Comp[" & CStr(1) & "].Prs", PreDes_BEC.Val)

                'For i = 0 To 9
                '    DoSet("PROSPER.ANL.VMT.Corr[" & CStr(i) & "]", 1)
                'Next i

                DoCmd("PROSPER.ANL.TCC.CALC")

                ' Resultados de Comparación de Correlaciones 
                '
                'Dim Indice, J As Integer
                'Dim IndiceTmp As String
                ''ReDim NumCor(NumCorr.Val - 1)
                ''ReDim ProfDesa(,)
                ''ReDim TVDCC(NumCorr.Val - 1, 100)
                ''ReDim PresWf(NumCorr.Val - 1, 100)
                'ReDim NumDatCorr(NumCorr.Val - 1)
                'ReDim NomCorr(NumCorr.Val - 1)
                'ReDim NumCor(NumCorr.Val - 1)
                'Dim PwfCC(NumCorr.Val - 1), DifPwf(NumCorr.Val) As Double
                'Dim LabelCC(,) As String
                'Dim TempCC(,) As Double

                Dim Indice, J As Integer
                Dim IndiceTmp As String
                ReDim NumCor(NumCorr.Val - 1)
                ReDim ProfDesa(NumCorr.Val - 1, 10)
                ReDim TVDCC(NumCorr.Val - 1, 10)
                ReDim PresWf(NumCorr.Val - 1, 10)
                ReDim NumDatCorr(NumCorr.Val - 1)
                ReDim NomCorr(NumCorr.Val - 1)
                'Dim NumCor(NumCorr.Val - 1) As Integer
                Dim PwfCC(NumCorr.Val - 1), DifPwf(NumCorr.Val) As Double
                Dim LabelCC(NumCorr.Val - 1, 10) As String
                Dim TempCC(NumCorr.Val - 1, 10) As Double

                TMSDCC = Int(0)
                ' Resultados: Escribe en la hoja comparacion de correlaciones
                Dim max As Integer = 0
                For Indice = 0 To NumCorr.Val - 1 ' Correlacones a seleccionar (comienzan desde el cero)
                    Dim ind As Boolean = True
                    IndiceTmp = CStr(IndiceCorr(Indice))
                    J = 0


                    Do Until (TMSDCC >= Math.Round(NivMedDisp.Val, 0)) ' For j = 1 To 100

                        If J > max Then
                            max = J
                            ReDim Preserve LabelCC(NumCorr.Val - 1, J)
                            ReDim Preserve ProfDesa(NumCorr.Val - 1, J)
                            ReDim Preserve TVDCC(NumCorr.Val - 1, J)
                            ReDim Preserve PresWf(NumCorr.Val - 1, J)
                            ReDim Preserve TempCC(NumCorr.Val - 1, J)
                        End If



                        LabelCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Label[" & CStr(J) & "]")
                        ' Profundidad desarrollada
                        ProfDesa(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].MSD[" & CStr(J) & "]")
                        ' Profundidad vertical
                        TVDCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].TVD[" & CStr(J) & "]")
                        ' Presion
                        PresWf(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Pres[" & CStr(J) & "]")
                        ' Temperatura
                        TempCC(Indice, J) = DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].Temp[" & CStr(J) & "]")


                        TMSDCC = ProfDesa(Indice, J) 'DoGet("PROSPER.OUT.TCC.Results[" & IndiceTmp & "].MSD[" & CStr(j - 1) & "]")
                        TMSDCC = Math.Round(TMSDCC, 0)
                        NumDatCorr(Indice) = J
                        J = J + 1
                        If (TMSDCC >= Math.Round(Prof_BEC.Val, 0) And ind = True) Then
                            ' Presion en el intevalo medio productor
                            PwfCC(Indice) = PresWf(Indice, J - 1)
                            DifPwf(Indice) = Math.Abs(Ptest.Val - PwfCC(Indice))
                            ind = False
                        End If
                    Loop
                    NumCor(Indice) = Indice
                    NomCorr(Indice) = NombrCorr(Indice)
                    ' IPR = Qtest / (14.223 * DifPwf(Indice))  ' IPR (bl/día) / (lb/pg2)
                    TMSDCC = 0
                Next Indice

                Call SortBubble(NumCorr.Val, NumCor, NomCorr, DifPwf, 3, True)

                'Se limpia la correlaciones
                For i = 0 To 9
                    DoSet("PROSPER.ANL.VMT.Corr[" & i & "]", 0)
                Next i
                ' Se seleccionan la correlaciones del mejor ajuste
                DoSet("PROSPER.ANL.VMT.Corr[" & CStr(NumCor(0)) & "]", 1)
                DoCmd("PROSPER.ANL.VMT.CALC")

                'Se selecciona la correlación ya ajustada
                For I = 0 To 9 ' Solo hasta petroleum Expert 3
                    DoSet("PROSPER.ANL.TCC.Corr[" & CStr(I) & "]", 0)
                Next I

                DoSet("PROSPER.ANL.TCC.Corr[" & NumCor(0) & "]", 1)
                DoCmd("PROSPER.ANL.TCC.CALC")
                Dim Pwfcalc As Double = DoGet("PROSPER.OUT.TCC.Results[" & IndiceCorr(NumCor(0)) & "].Pres[160]")
                DoSet("PROSPER.SIN.IPR.Single.Ptest", Pwfcalc)
                DoCmd("PROSPER.IPR.CALC")


                '' Se des seleccionan todas las correlaciones en "VLP Maching - Adjust IPR"
                For Indice = 0 To 23
                    DoSet("PROSPER.ANL.VMT.Corr[" & CStr(Indice) & "]", 0)
                Next Indice

                ' Se selecciona la mejor correlacion
                DoSet("PROSPER.ANL.VMT.Corr[" & CStr(NumCor(0)) & "]", 1)
                CorrVFP = NumCor(0)

                'Dim NombreCorrelacion As String = DoGet("PROSPER.ANL.VMT.Corrlabel[1],[1]")
                DoCmd("PROSPER.ANL.VMT.VLPIPR(" & CStr(NumCor(0) + 1) & ",0)")
                'DoCmd("PROSPER.ANL.VMT.VLPIPR(1,0)")



                Return True
            Catch ex As Exception
                Throw New Exception("Compara_Correlaciones_BEC: " + ex.Message)
            End Try

        End Function

        ''' <summary>
        ''' Analisis nodal con los valores del ajuste del modulo VLP/IPR Maching
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function VLPIPR() As Boolean
            Try
                ' Son 20 valores de producción de liquido [0-19]
                Dim ValAux As Double = 0
                ReDim VLPIPR_RTEL(1, 19)
                ReDim VLPIPR_PWF(1, 19)

                Dim version = Settings.GetBy("prosper_version")


                If version = "IPM 7.5" Then
                    'V7.5
                    For I = 0 To 19
                        ' QL bl / dia
                        ValAux = DoGet("PROSPER.OUT.VIM.Results[0].Rate[0][" & CStr(I) & "]")
                        VLPIPR_RTEL(0, I) = ValAux : VLPIPR_RTEL(1, I) = ValAux
                        ' Qo bl / dia
                        ' Pwf kg/cm2  VLP -> Tubería
                        VLPIPR_PWF(0, I) = DoGet("PROSPER.OUT.VIM.Results[0].VLPPres[0][" & CStr(I) & "]")
                        ' Pwf kg/cm2  IPR Yacimiento
                        VLPIPR_PWF(1, I) = DoGet("PROSPER.OUT.VIM.Results[0].IPRPres[0][" & CStr(I) & "]")
                    Next I
                Else
                    'V11
                    For I = 0 To 19
                        ' QL bl / dia
                        Dim qwater As Double = DoGet("PROSPER.OUT.VIM.Correlations[" & CStr(NumCor(0)) & "].Test[0].WatRate[" & CStr(I) & "]")
                        ValAux = DoGet("PROSPER.OUT.VIM.Correlations[" & CStr(NumCor(0)) & "].Test[0].OilRate[" & CStr(I) & "]") + qwater
                        VLPIPR_RTEL(0, I) = ValAux : VLPIPR_RTEL(1, I) = ValAux
                        ' Qo bl / dia
                        ' Pwf kg/cm2  VLP -> Tubería
                        VLPIPR_PWF(0, I) = DoGet("PROSPER.OUT.VIM.Correlations[" & CStr(NumCor(0)) & "].Test[0].VLPpres[" & CStr(I) & "]") / 14.22
                        ' Pwf kg/cm2  IPR Yacimiento
                        VLPIPR_PWF(1, I) = DoGet("PROSPER.OUT.VIM.Correlations[" & CStr(NumCor(0)) & "].Test[0].IPRpres[" & CStr(I) & "]") / 14.22
                    Next I


                End If

                Dim Ptest As Double = DoGet("PROSPER.OUT.VIM.Results[0].TestPres[0]")
                DoSet("PROSPER.SIN.IPR.Single.Ptest", Ptest)
                DoCmd("PROSPER.IPR.CALC")

                Return True
            Catch ex As Exception
                Throw New Exception("VLPIPR:" + ex.Message)
            End Try

        End Function

        ''' <summary>
        ''' Obtiene la curva del comportamiento del gas de BNC
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function Sensibilidad_BN() As Boolean
            ' Curva del comportamiento del gas de BN

            ' Analisis de Sensibilidad para el gas de Inyección
            ' Empleando el modulo -> System 3 Variables

            ' Presión en la cabeza del pozo (Pth)
            DoSet("PROSPER.ANL.SYS.Pres", THPres.Val)
            ' Corte de Agua. (Relación agua aceite WC)
            DoSet("PROSPER.ANL.SYS.WC", Wc.Val)
            ' Relación gas aceite total
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)

            '   You can also use the following syntax to reference the correlation by label
            '
            ' PROSPER.ANL.SYS.TubingLabel
            '
            ' Use the following list to select the correlation by name
            '
            ' 0       DunsandRosModified
            ' 1       HagedornBrown
            ' 2       FancherBrown
            ' 4       MukerjeeBrill
            ' 5       BeggsandBrill
            ' 8       PetroleumExperts
            ' 9       Orkiszewski
            ' 10      PetroleumExperts2
            ' 11      DunsandRosOriginal
            ' 12      PetroleumExperts3
            ' 14      GREmodifiedbyPE
            ' 18      PetroleumExperts4
            ' 19      Hydro3P
            ' 20      PetroleumExperts5
            ' 21      OLGAS2P
            ' 22      OLGAS3P
            ' 23      OLGAS3PEXT

            ' Correlación de flujo multifásico Vertical
            DoSet("PROSPER.ANL.SYS.Tubing", CorrVFP)

            ' Sensitivity Variables
            '
            ' Type    Index   Variable Name
            ' --------------------------------------------------------------
            '  1	  1	Reservoir Pressure
            ' 53	  2	Reservoir Temperature
            ' 51	  7	Reservoir Permeability
            ' 56	  8	Reservoir Thickness
            ' 57	  9	Drainage Area
            ' 58	 10	Dietz Shape Factor
            ' 59	 11	Wellbore Radius
            ' 16	  6	Water Cut
            ' 17	131	Gas Oil Ratio
            '  2	 27	Skin
            ' 40	155	Water Oil Ratio
            ' 19	156	Condensate Gas Ratio
            ' 24	132	GLR Free
            ' 25	133	Free Gas Rate
            ' 33	134	GOR Free
            ' 20	135	Total GOR
            ' 39	136	Dissolved and Free GLR
            ' 35	148	Operating Frequency
            ' 36	149	Gas Separator Efficiency
            ' 37	150	Pump Wear Factor
            ' 38	151	Pump Depth (Measured)
            ' 41	157	Number of Stages
            ' 27	145	First Node Pressure
            ' 26	144	Tubing Roughness
            '334	195	Downhole Heat Transfer Coefficient
            ' 21	147	Tubing/Pipe Diameter
            '341	202	Downhole Equipment
            '342	203	Surface Equipment
            '
            '
            ' Use      DoSet PROSPER.ANL.SYS.Sens.SensDB.Clear
            '          to reset ALL sensitivity variable Information


            'Se selecciona como parametro de sensibilidad el gasto de gas de inyección
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 16)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[2]", 1)

            '*** Limpiar los datos anteriores
            ' Qgi
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[].RESET", "")
            ' WC
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[].RESET", "")
            ' Pws
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[].RESET", "")

            '
            ' Se generan los 10 valores de Qgi
            ' En 2 corridas se generan 20 valores de WC
            '
            'Dim Delta_Wc, Wc_Max, Wc_Min As Double
            Dim Delta_Qgi As Double
            Dim WcVec(9) As Double
            ReDim QliqVLP(9, 19)
            ReDim PwfVLP(9, 19)
            ReDim QgiVec(9)
            ReDim QliqVec(9)

            Delta_Qgi = (QgiMax.Val - QgiMin.Val) / (10 - 1)
            For I = 0 To 9
                QgiVec(I) = QgiMin.Val + I * Delta_Qgi
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & I & "]", QgiVec(I))
            Next I
            '  Wc
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[0]", Wc.Val)

            ' Pws
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[0]", PRes.Val)

            ' Se realizan los calculos
            DoCmd("PROSPER.ANL.SYS.CALC")

            For j = 0 To 9
                QliqVec(j) = DoGet("PROSPER.OUT.SYS.Results[" & j & "][0][0].Sol.LiqRate")
            Next j
            For i = 0 To 9
                For j = 0 To 19
                    QliqVLP(i, j) = DoGet("PROSPER.OUT.SYS.Results[" & i & "][0][0].LiqRate[" & j & "]")
                    PwfVLP(i, j) = DoGet("PROSPER.OUT.SYS.Results[" & i & "][0][0].VLPpres[" & j & "]")
                Next j
            Next i
        End Function


        Public Function Sensibilidad_BN(ByVal Qgi_Min As Double, ByVal Qgi_Max As Double) As Boolean
            Try
                Me.Server = Connect(ProgramPath, ArchivoPVT)
                Dim TotalItems As Integer = 19
                ReDim QgiVec(19)
                ReDim QliqVec(19)



                DoCmd("PROSPER.SETUNITSYS(""Pemex"")")


                GOR_Total = DoGet("PROSPER.PVT.Calc.Results[0].GOR") 'RGA_PVT_PwsyTws()

                DoSet("PROSPER.ANL.SYS.Pres", THPres.Val)
                DoSet("PROSPER.ANL.SYS.WC", Wc.Val)
                DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)
                DoSet("PROSPER.ANL.SYS.SolutionNode", 1)
                DoSet("PROSPER.ANL.SYS.RateMethod", 2)
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)

                Dim incre As Double = (Qgi_Max - Qgi_Min) / 18
                For i = 0 To 18
                    Dim gas As Double = Qgi_Min + incre * i
                    'DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", gas)
                    QgiVec(i) = gas
                Next i

                'DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[19]", Qgi_afor)
                QgiVec(19) = GLRiny.Val

                'Dim narray = QgiVec.ToArray()
                Array.Sort(QgiVec)

                'QgiVec = QGIList.ToArray()



                For i = 0 To QgiVec.Length - 1
                    DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(i) & "]", QgiVec(i))
                Next


                ' DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & CStr(19) & "]", Qgi_afor)

                DoCmd("PROSPER.ANL.SYS.CALC")

                Dim Results = Integer.Parse(DoGet("PROSPER.OUT.SYS.Results.COUNT"))

                If Results = 0 Then
                    Throw New Exception("No hubo resultados en la operacion: PROSPER.OUT.SYS.Results")
                End If


                For i = 0 To Results - 1
                    QgiVec(i) = DoGet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" + i.ToString() + "]")
                    QliqVec(i) = DoGet("PROSPER.OUT.SYS.Results[" + i.ToString() + "].Sol.LiqRate")
                    'QgiVec(i) = DoGet("Prosper.ANL.SYS.Sens.SensDB.Sens[138].Vals[" + i.ToString() + "]")
                Next i
                'Qliq_aforo = DoGet("PROSPER.OUT.SYS.Results[19].Sol.LiqRate")

                Disconnect()
                Return True
            Catch ex As Exception
                Disconnect()
                Throw New Exception(ex.Message)
            End Try
        End Function

        Private Function Sensibilidad_BEC() As Boolean
            ' Curva del comportamiento del gas de BN

            ' Analisis de Sensibilidad para el gas de Inyección
            ' Empleando el modulo -> System 3 Variables
            Try
                ' Presión en la cabeza del pozo (Pth)
                DoSet("PROSPER.ANL.SYS.Pres", THPres.Val)
                ' Corte de Agua. (Relación agua aceite WC)
                DoSet("PROSPER.ANL.SYS.WC", Wc.Val)
                ' Relación gas aceite total
                DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)

                '   You can also use the following syntax to reference the correlation by label
                '
                ' PROSPER.ANL.SYS.TubingLabel
                '
                ' Use the following list to select the correlation by name
                '
                ' 0       DunsandRosModified
                ' 1       HagedornBrown
                ' 2       FancherBrown
                ' 4       MukerjeeBrill
                ' 5       BeggsandBrill
                ' 8       PetroleumExperts
                ' 9       Orkiszewski
                ' 10      PetroleumExperts2
                ' 11      DunsandRosOriginal
                ' 12      PetroleumExperts3
                ' 14      GREmodifiedbyPE
                ' 18      PetroleumExperts4
                ' 19      Hydro3P
                ' 20      PetroleumExperts5
                ' 21      OLGAS2P
                ' 22      OLGAS3P
                ' 23      OLGAS3PEXT

                ' Correlación de flujo multifásico Vertical
                DoSet("PROSPER.ANL.SYS.Tubing", CorrVFP)

                ' Sensitivity Variables
                '
                ' Type    Index   Variable Name
                ' --------------------------------------------------------------
                '  1	  1	Reservoir Pressure
                ' 53	  2	Reservoir Temperature
                ' 51	  7	Reservoir Permeability
                ' 56	  8	Reservoir Thickness
                ' 57	  9	Drainage Area
                ' 58	 10	Dietz Shape Factor
                ' 59	 11	Wellbore Radius
                ' 16	  6	Water Cut
                ' 17	131	Gas Oil Ratio
                '  2	 27	Skin
                ' 40	155	Water Oil Ratio
                ' 19	156	Condensate Gas Ratio
                ' 24	132	GLR Free
                ' 25	133	Free Gas Rate
                ' 33	134	GOR Free
                ' 20	135	Total GOR
                ' 39	136	Dissolved and Free GLR
                ' 35	148	Operating Frequency
                ' 36	149	Gas Separator Efficiency
                ' 37	150	Pump Wear Factor
                ' 38	151	Pump Depth (Measured)
                ' 41	157	Number of Stages
                ' 27	145	First Node Pressure
                ' 26	144	Tubing Roughness
                '334	195	Downhole Heat Transfer Coefficient
                ' 21	147	Tubing/Pipe Diameter
                '341	202	Downhole Equipment
                '342	203	Surface Equipment
                '
                '
                ' Use      DoSet PROSPER.ANL.SYS.Sens.SensDB.Clear
                '          to reset ALL sensitivity variable Information

                '*** Limpiar los datos anteriores
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")
                'Se selecciona como parametro de sensibilidad la frecuencia
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 35)
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 16)
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[2]", 1)

                ' Se generan los 10 valores de Qgi
                ' En 2 corridas se generan 20 valores de WC
                '
                'Dim Delta_Wc, Wc_Max, Wc_Min As Double
                Dim Delta_Frec As Double
                Dim WcVec(9) As Double
                ReDim QliqVLP(9, 19)
                ReDim PwfVLP(9, 19)
                ReDim Pdescarga_BEC(9, 19)
                ReDim FrecVec(9)
                ReDim QliqVec(9)

                Delta_Frec = (FrecMax.Val - FrecMin.Val) / (10 - 1)
                For I = 0 To 9
                    FrecVec(I) = FrecMin.Val + I * Delta_Frec
                    DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[148].Vals[" & I & "]", FrecVec(I))
                Next I
                '  Wc
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[0]", Wc.Val)

                ' Pws
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[0]", PRes.Val)

                ' Se realizan los calculos
                DoCmd("PROSPER.ANL.SYS.CALC")

                For j = 0 To 9
                    QliqVec(j) = DoGet("PROSPER.OUT.SYS.Results[" & j & "][0][0].Sol.LiqRate")
                Next j
                For i = 0 To 9
                    For j = 0 To 19
                        QliqVLP(i, j) = DoGet("PROSPER.OUT.SYS.Results[" & i & "][0][0].LiqRate[" & j & "]")
                        PwfVLP(i, j) = DoGet("PROSPER.OUT.SYS.Results[" & i & "][0][0].VLPpres[" & j & "]")
                        Pdescarga_BEC(i, j) = DoGet("PROSPER.OUT.SYS.Results[" & i & "][0][0].PDP[" & j & "]")
                    Next j
                Next i
            Catch ex As Exception
                Throw New Exception("Sensibilidad_BEC: " + ex.Message)
            End Try
        End Function

        ''' <summary>
        ''' Realiza un diagnóstico rápido de las condiciones de operacion del BNC.
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function QuickLook_BN() As Boolean
            ' Quick Look
            '
            Try
                ReDim MSDQL(1, 1000), PresQL(1, 1000), TVDQL(1, 1000), TempQL(1, 1000)
                ReDim GGTVD(1), GGPres(1)
                ReDim MSDGG(1000), PresGG(1000), TVDGG(1000)

                ' Use the following list to select the correlation by name
                '
                '  0      DunsandRosModified
                '  1      HagedornBrown
                '  2      FancherBrown
                '  4      MukerjeeBrill
                '  5      BeggsandBrill
                '  8      PetroleumExperts
                '  9      Orkiszewski
                ' 10      PetroleumExperts2
                ' 11      DunsandRosOriginal
                ' 12      PetroleumExperts3
                ' 14      GREmodifiedbyPE
                ' 18      PetroleumExperts4
                ' 19      Hydro3P
                ' 20      PetroleumExperts5
                ' 21      OLGAS2P
                ' 22      OLGAS3P
                ' 23      OLGAS3PEXT

                ' Correlación de flujo mulfifásico Vertical

                DoSet("PROSPER.ANL.QLG.Surface[0][0]", THPres.Val)

                DoSet("PROSPER.ANL.QLG.Surface[1][0]", THTemp.Val)

                DoSet("PROSPER.ANL.QLG.Surface[2][0]", QTest.Val)

                DoSet("PROSPER.ANL.QLG.Surface[3][0]", Wc.Val)

                DoSet("PROSPER.ANL.QLG.Surface[4][0]", Qg_Total + GLRate.Val)

                DoSet("PROSPER.ANL.QLG.Surface[5][0]", GLRate.Val)

                DoSet("PROSPER.ANL.QLG.Surface[6][0]", TRPres.Val)

                DoSet("PROSPER.ANL.QLG.Gaslift[0]", DiamValBNC.Val)

                DoSet("PROSPER.ANL.QLG.Gaslift[1]", ValveDepth.Val)

                DoSet("PROSPER.ANL.QLG.Tubing", 1)

                DoSet("PROSPER.ANL.QLG.Tubing", CorrVFP)
                ' Se realizan los calculos
                DoCmd("PROSPER.ANL.QLG.CALC")

                '
                ' Se obtienen los resultados del Quick Look
                '
                Dim Pwf_QL, TempValv, RGAQL, GORLibr, DPValv, PtrTeor As Double
                Dim DPYacim, IPQL, QgiCrit, PorQgiCrit, CoefTransfAjus As Double
                Dim Ok As Integer

                ' Presion a la prof de la valv.
                Pwf_QL = DoGet("PROSPER.OUT.QLG.Output[0]")
                ' Temperatura a la profundidad de la valvula
                TempValv = DoGet("PROSPER.OUT.QLG.Output[1]")
                ' Relacion gas aceite
                RGAQL = DoGet("PROSPER.OUT.QLG.Output[2]")
                ' GOR Libre
                GORLibr = DoGet("PROSPER.OUT.QLG.Output[3]")
                ' Delta P a travez de la valvula
                DPValv = DoGet("PROSPER.OUT.QLG.Output[4]")
                ' Presion en la TR teórica superf
                PtrTeor = DoGet("PROSPER.OUT.QLG.Output[5]")
                ' Delta P en el yacimiento
                DPYacim = DoGet("PROSPER.OUT.QLG.Output[6]")
                ' Indice de productividad
                IPQL = DoGet("PROSPER.OUT.QLG.Output[7]")
                ' Gasto de gas crítico
                QgiCrit = DoGet("PROSPER.OUT.QLG.Output[8]")
                ' Porcentaje de gvasto de gas crítico
                PorQgiCrit = DoGet("PROSPER.OUT.QLG.Output[9]")
                ' Coeficiente de transferencia de calor ajustdo
                CoefTransfAjus = DoGet("PROSPER.OUT.QLG.Output[10]")

                ' PNIMP: Profundidad del intervalo medio productor mD

                Dim LabelQL(1, 1000) As String
                Dim Indice, J As Integer
                Dim TMSDQL As Double

                Ok = False
                ' Resultados
                ' 0: Top - Down
                ' 1: Bottom - Up




                For Indice = 0 To 1

                    Dim NumGrad = Int32.Parse(DoGet("PROSPER.OUT.QLG.RESULTS[" + Indice.ToString() + "].COUNT"))
                    If NumGrad > 0 Then
                        For J = 0 To NumGrad - 1
                            LabelQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Label[" & CStr(J) & "]")
                            'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 4) = LabelQL(Indice, J)
                            ' Profundidad desarrollada
                            MSDQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].MSD[" & CStr(J) & "]")
                            'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 3) = MSDQL(Indice, J)
                            ' Profundidad Vertical
                            TVDQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].TVD[" & CStr(J) & "]")
                            'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 2) = TVDQL(Indice, J)
                            ' Presion
                            PresQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Pres[" & CStr(J) & "]")
                            'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 1) = PresQL(Indice, J)
                            ' Temperatura
                            TempQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Temp[" & CStr(J) & "]")
                            'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 0) = TempQL(Indice, J)
                            ' Nueva profundidad
                            TMSDQL = MSDQL(Indice, J)

                            If Indice = 0 Then
                                Ok = (TMSDQL >= NivMedDisp.Val - 0.1)
                            Else
                                Ok = (TMSDQL <= 0 + 0.1)
                            End If

                            NumDatGrad = NumGrad - 1
                            If Ok Then
                                Exit For
                            End If
                        Next
                    Else
                        Throw New Exception("QuickLook_BN (" + NumGrad.ToString() + "): Revisar GOR de entrada ")
                    End If





                    'If Indice = 0 Then TMSDQL = 0 Else TMSDQL = NivMedDisp.Val
                    'J = 0
                    'Do
                    '    ' Etiqueta
                    '    LabelQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Label[" & CStr(J) & "]")
                    '    'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 4) = LabelQL(Indice, J)
                    '    ' Profundidad desarrollada
                    '    MSDQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].MSD[" & CStr(J) & "]")
                    '    'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 3) = MSDQL(Indice, J)
                    '    ' Profundidad Vertical
                    '    TVDQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].TVD[" & CStr(J) & "]")
                    '    'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 2) = TVDQL(Indice, J)
                    '    ' Presion
                    '    PresQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Pres[" & CStr(J) & "]")
                    '    'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 1) = PresQL(Indice, J)
                    '    ' Temperatura
                    '    TempQL(Indice, J) = DoGet("PROSPER.OUT.QLG.Results[" & CStr(Indice) & "].Temp[" & CStr(J) & "]")
                    '    'Sheets(HojaQL).Cells(Reng + J, (5 * Indice) - 0) = TempQL(Indice, J)
                    '    ' Nueva profundidad
                    '    TMSDQL = MSDQL(Indice, J)
                    '    'MsgBox(TMSDQL.ToString & " : " & PNIMP.ToString & Chr(13) & _
                    '    '       Indice.ToString & ", " & J.ToString & ": " & LabelQL(Indice, J) & ", " & MSDQL(Indice, J))

                    '    J = J + 1
                    '    If Indice = 0 Then
                    '        Ok = (TMSDQL >= NivMedDisp.Val - 0.1)
                    '    Else
                    '        Ok = (TMSDQL <= 0 + 0.1)
                    '    End If
                    '    '          If J >= 87 Then
                    '    '             Ok = Ok
                    '    '          End If
                    'Loop Until Ok  '(TMSDQL >= PNIMP)
                Next Indice

                ' NumDatGrad = J - 1

                '
                ' Gradiente de gas medido
                '

                Dim TMSDGG As Double

                Indice = 0
                TMSDGG = 0

                Do Until (TMSDGG >= NivMedDisp.Val - 0.1)
                    ' Profundidad desarrollada
                    MSDGG(Indice) = DoGet("PROSPER.OUT.QLG.Results[0].GasGrad[" & CStr(Indice) & "].MSD")
                    'Sheets(HojaQL).Cells(Reng + Indice, ColGG) = MSDGG(Indice)
                    ' Profundidad Vertical
                    TVDGG(Indice) = DoGet("PROSPER.OUT.QLG.Results[0].GasGrad[" & CStr(Indice) & "].TVD")
                    'Sheets(HojaQL).Cells(Reng + Indice, ColGG + 1) = TVDGG(Indice)
                    ' Presion
                    PresGG(Indice) = DoGet("PROSPER.OUT.QLG.Results[0].GasGrad[" & CStr(Indice) & "].Pres")
                    'Sheets(HojaQL).Cells(Reng + Indice, ColGG + 2) = PresGG(Indice)
                    TMSDGG = MSDGG(Indice)
                    Indice = Indice + 1
                Loop

                NumDatGG = Indice - 1

                ' Gradiente teorico
                Dim GGTVD1, GGTVD2 As Double

                GGTVD1 = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[0].TVD")
                GGTVD2 = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[1].TVD")

                GGTVD(0) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[0].TVD")
                GGTVD(1) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[1].TVD")

                GGPres(0) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[0].Pres")
                GGPres(1) = DoGet("PROSPER.OUT.QLG.Results[1].GasGrad[1].Pres")
            Catch ex As Exception
                Throw New Exception("QuickLook_BN: " + ex.Message)
            End Try

        End Function

        Private Function QuickLook_BEC() As Boolean

            'Configura el Quicklook para bombeo electrocentrifugo

            Try

                ReDim MSDQL(1, 10), PresQL(1, 10), TempQL(1, 10), TVDQL(1, 10)



                'Presión en la cabeza del pozo

                DoSet("PROSPER.ANL.QLE.Quick[0]", THPres.Val)

                'Gasto de líquido

                DoSet("PROSPER.ANL.QLE.Quick[1]", QTest.Val)

                'Corte de Agua

                DoSet("PROSPER.ANL.QLE.Quick[2]", Wc.Val)

                'Relación de gas-aceite

                DoSet("PROSPER.ANL.QLE.Quick[3]", RGA_Aforo.Val)

                'Presión del yacimiento en el fondo del pozo

                DoSet("PROSPER.ANL.QLE.Quick[4]", PRes.Val)

                'Profundidad de la bomba

                DoSet("PROSPER.ANL.QLE.Quick[5]", Prof_BEC.Val)

                'Frecuencia de operación

                DoSet("PROSPER.ANL.QLE.Quick[6]", Frec_BEC.Val)

                'Longitud del cable

                DoSet("PROSPER.ANL.QLE.Quick[7]", LongCable_BEC.Val)

                'Eficiencia de separación de gas

                DoSet("PROSPER.ANL.QLE.Quick[8]", ReducGas_BEC.Val)

                'Número de etapas

                DoSet("PROSPER.ANL.QLE.Quick[9]", Etapas_BEC.Val)

                'Factor de desgaste de la bomba

                DoSet("PROSPER.ANL.QLE.Quick[10]", Desgaste_BEC.Val)

                'Corriente del BEC

                DoSet("PROSPER.ANL.QLE.Surf[0]", Corriente_BEC.Val)

                'Voltaje

                DoSet("PROSPER.ANL.QLE.Surf[0]", VoltSup_BEC.Val)

                'Potencia

                DoSet("PROSPER.ANL.QLE.Surf[0]", Potencia_BEC.Val)

                'Se selecciona la correlación que mejor ajusta

                DoSet("PROSPER.ANL.QLE.Tubing", CorrVFP)





                ' Se realizan los calculos

                DoCmd("PROSPER.ANL.QLE.CALC")



                ' Se obtienen los resultados del Quick Look

                '

                Dim Ok As Integer



                'Potencia requerida en la bomba

                HPreqBomba_BECq = DoGet("PROSPER.OUT.QLE.Output[0]")

                'Potencia requerida en el motor

                HPreqMotor_BECq = DoGet("PROSPER.OUT.QLE.Output[1]")

                'Eficiencia del motor

                EfiMotor_BECq = DoGet("PROSPER.OUT.QLE.Output[2]")

                'Voltaje requerido

                Voltreq_BECq = DoGet("PROSPER.OUT.QLE.Output[3]")

                'Presión de succión

                PreSuccion_BECq = DoGet("PROSPER.OUT.QLE.Output[4]")

                'Presión de descarga

                PreDescarga_BECq = DoGet("PROSPER.OUT.QLE.Output[5]")

                'Gasto de liquido a condiciones de fondo de pozo

                QliqCY_BECq = DoGet("PROSPER.OUT.QLE.Output[6]")

                'Facción de gas libre que entra en la bomba

                FgasinBomba_BECq = DoGet("PROSPER.OUT.QLE.Output[7]")

                'Cosumo de corriente

                KVA_BECq = DoGet("PROSPER.OUT.QLE.Output[8]")

                'Torque en el eje

                TorqEje_BECq = DoGet("PROSPER.OUT.QLE.Output[9]")





                ' PNIMP: Profundidad del intervalo medio productor mD



                'Dim LabelQL(1, 1000) As String

                'Dim TVDQL(1, 1000) As Double

                Dim Indice, J As Integer

                Dim TMSDQL As Double

                Dim max = 10

                NumDatGrad = 0

                NumDatGrad2 = 0

                Ok = False

                ' Resultados

                ' 0: Top - Down

                ' 1: Bottom - Up

                For Indice = 0 To 1

                    If Indice = 0 Then TMSDQL = 0 Else TMSDQL = NivMedDisp.Val 'OJO REVISAR

                    J = 0

                    Do

                        If J > max Then

                            max = J

                            ReDim Preserve MSDQL(1, J)

                            ReDim Preserve TVDQL(1, J)

                            ReDim Preserve PresQL(1, J)

                        End If

                        ' Profundidad desarrollada

                        MSDQL(Indice, J) = DoGet("PROSPER.OUT.QLE.Results[" & CStr(Indice) & "].MSD[" & CStr(J) & "]")

                        ' Profundidad Vertical

                        TVDQL(Indice, J) = DoGet("PROSPER.OUT.QLE.Results[" & CStr(Indice) & "].TVD[" & CStr(J) & "]")

                        ' Presion

                        PresQL(Indice, J) = DoGet("PROSPER.OUT.QLE.Results[" & CStr(Indice) & "].Pres[" & CStr(J) & "]")

                        ' Temperatura

                        'TempQL(Indice, J) = DoGet("PROSPER.OUT.QLE.Results[" & CStr(Indice) & "].Temp[" & CStr(J) & "]")

                        ' Nueva profundidad

                        TMSDQL = MSDQL(Indice, J)



                        J = J + 1

                        If Indice = 0 Then

                            NumDatGrad += 1

                            Ok = (TMSDQL >= NivMedDisp.Val - 0.1)

                        Else

                            NumDatGrad2 += 1

                            Ok = (TMSDQL >= NivMedDisp.Val - 0.1)

                            'Ok = (TMSDQL <= 0 + 0.1)

                        End If



                    Loop Until Ok

                Next Indice





                Dim Htcs As Double = DoGet("PROSPER.SIN.EQP.Geo.Htc")
                DoSet("PROSPER.SIN.SUM.TempModel", 2)
                Dim Htcincr As Double = Htcs / NumDatTemp.Val - 1
                For I = 0 To NumDatTemp.Val - 1
                    DoSet("PROSPER.SIN.EQP.Geo.Data[" & CStr(I) & "].Htc", Htcincr * (I + 1))
                Next I



                DoSet("PROSPER.ANL.GRD.Pres", THPres.Val)
                DoSet("PROSPER.ANL.GRD.Temp", TRes.Val)
                DoSet("PROSPER.ANL.GRD.WC", Wc.Val)
                DoSet("PROSPER.ANL.GRD.GOR", RGA_Aforo.Val)
                DoSet("PROSPER.ANL.GRD.Rate", QTest.Val)
                DoSet("PROSPER.ANL.GRD.Tubing", CorrVFP)
                DoCmd("PROSPER.ANL.GRD.CALC")



                NumDatGrad = 0
                NumDatGrad2 = 0
                max = 10

                For Indice = 0 To 1
                    If Indice = 0 Then TMSDQL = 0 Else TMSDQL = NivMedDisp.Val 'OJO REVISAR
                    J = 0
                    Do
                        If J > max Then
                            max = J
                            ReDim Preserve TempQL(1, J)

                        End If

                        ' Temperatura

                        TempQL(Indice, J) = DoGet("PROSPER.OUT.GRD.Results[0][0][0].Temp[" & CStr(J) & "]")
                        ' Nueva profundidad
                        TMSDQL = MSDQL(Indice, J)
                        J = J + 1
                        If Indice = 0 Then
                            NumDatGrad += 1
                            Ok = (TMSDQL >= NivMedDisp.Val - 0.1)
                        Else

                            NumDatGrad2 += 1
                            Ok = (TMSDQL >= NivMedDisp.Val - 0.1)
                            'Ok = (TMSDQL <= 0 + 0.1)

                        End If

                    Loop Until Ok

                Next Indice
                DoSet("PROSPER.SIN.SUM.TempModel", 0)
            Catch ex As Exception
                Throw New Exception("QuickLook_BEC: " + ex.Message)
            End Try
        End Function
        ''' <summary>
        ''' Se realiza un análisis de sensibilidad para diferentes gastos de gas de inyección y porcentajes de agua.
        ''' </summary>
        ''' <returns> Verdadero si encuentra un error o de lo contyrario regresa Falso</returns>
        Private Function SensAgua_BN() As Boolean
            ReDim Qgi_Res(9), Wc_Res(9), Pws_Res(0), Qliq_Res(9, 9, 0)
            '
            ' Analisis de sensibilidad para  diferentes porcentajes de agua
            '
            ' Empleando el modulo -> System 3 Variables

            ' Presión en la cabeza del pozo (Pth)
            DoSet("PROSPER.ANL.SYS.Pres", THPres.Val)
            ' Corte de Agua. (Relación agua aceite WC)
            DoSet("PROSPER.ANL.SYS.WC", Wc.Val)
            ' Relación gas aceite total
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)

            '   You can also use the following syntax to reference the correlation by label
            '
            ' PROSPER.ANL.SYS.TubingLabel
            '
            ' Use the following list to select the correlation by name
            '
            ' 0       DunsandRosModified
            ' 1       HagedornBrown
            ' 2       FancherBrown
            ' 4       MukerjeeBrill
            ' 5       BeggsandBrill
            ' 8       PetroleumExperts
            ' 9       Orkiszewski
            ' 10      PetroleumExperts2
            ' 11      DunsandRosOriginal
            ' 12      PetroleumExperts3
            ' 14      GREmodifiedbyPE
            ' 18      PetroleumExperts4
            ' 19      Hydro3P
            ' 20      PetroleumExperts5
            ' 21      OLGAS2P
            ' 22      OLGAS3P
            ' 23      OLGAS3PEXT

            ' Correlación de flujo multifásico Vertical
            DoSet("PROSPER.ANL.SYS.Tubing", CorrVFP)

            ' Sensitivity Variables
            '
            ' Type    Index   Variable Name
            ' --------------------------------------------------------------
            '    1     1 Reservoir Pressure     <----------------
            '   53     2 Reservoir Temperature
            '   51     7 Reservoir Permeability
            '   56     8 Reservoir Thickness
            '   57     9 Drainage Area
            '   58    10 Dietz Shape Factor
            '   59    11 Wellbore Radius
            '   16     6 Water Cut              <----------------
            '   17   131 Gas Oil Ratio
            '    2    27 Skin
            '   40   155 Water Oil Ratio
            '   19   156 Condensate Gas Ratio
            '   24   132 GLR Free
            '   25   133 Free Gas Rate
            '   33   134 GOR Free
            '   20   135 Total GOR
            '   39   136 Dissolved and Free GLR
            '   44   160 Gaslift Gas Specific Gravity
            '   22   138 Gaslift Gas Injection Rate    <---------------------
            '   23   139 GLR Injected
            '   31   140 Injection Depth
            '   27   145 First Node Pressure
            '   26   144 Tubing Roughness
            '  307   168 Pipe Roughness
            '  334   195 Downhole Heat Transfer Coefficient
            '  335   196 Surface Heat Transfer Coefficient
            '   28   146 Choke Size
            '   21   147 Tubing/Pipe Diameter
            '  341   202 Downhole Equipment
            '  342   203 Surface Equipment
            '
            '
            ' Use      DoSet PROSPER.ANL.SYS.Sens.SensDB.Clear
            '          to reset ALL sensitivity variable Information

            'Se selecciona como parametro de sensibilidad el gasto de gas de inyección
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 22)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 16)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[2]", 1)

            '*** Limpiar los datos anteriores
            ' Qgi
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[].RESET", "")
            ' WC
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[].RESET", "")
            ' Pws
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[].RESET", "")

            '
            ' Se generan los 10 valores de Qgi
            ' En 2 corridas se generan 20 valores de WC
            '

            Dim Delta_Qgi, Delta_Wc, Wc_Max, Wc_Min As Double
            Dim QgiVec(9), WcVec(9) As Double
            Wc_Min = 0
            Wc_Max = 90

            Delta_Qgi = (QgiMax.Val - QgiMin.Val) / (10 - 1)
            Delta_Wc = (Wc_Max - Wc_Min) / (10 - 1)

            For I = 0 To 9
                QgiVec(I) = QgiMin.Val + I * Delta_Qgi
                WcVec(I) = Wc_Min + I * Delta_Wc
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[138].Vals[" & I & "]", QgiVec(I))
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[" & I & "]", WcVec(I))
            Next I

            ' Pws
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[0]", PRes.Val)

            ' Se realizan los calculos
            DoCmd("PROSPER.ANL.SYS.CALC")

            ' Se obtienen los resultados parciales del análisis de sensibilidad al % de agua
            Resul_System3var(CorrVFP, THPres.Val, Wc.Val, GOR_Total, 9, 9, 0, Qgi_Res, Wc_Res, Pws_Res, Qliq_Res, 138)

        End Function

        Private Function SensAgua_BEC() As Boolean
            ReDim Frec_Res(9), Wc_Res(9), Pws_Res(0), Qliq_Res(9, 9, 0)
            '
            ' Analisis de sensibilidad para  diferentes porcentajes de agua
            '
            ' Empleando el modulo -> System 3 Variables

            ' Presión en la cabeza del pozo (Pth)
            DoSet("PROSPER.ANL.SYS.Pres", THPres.Val)
            ' Corte de Agua. (Relación agua aceite WC)
            DoSet("PROSPER.ANL.SYS.WC", Wc.Val)
            ' Relación gas aceite total
            DoSet("PROSPER.ANL.SYS.GOR", GOR_Total)

            '   You can also use the following syntax to reference the correlation by label
            '
            ' PROSPER.ANL.SYS.TubingLabel
            '
            ' Use the following list to select the correlation by name
            '
            ' 0       DunsandRosModified
            ' 1       HagedornBrown
            ' 2       FancherBrown
            ' 4       MukerjeeBrill
            ' 5       BeggsandBrill
            ' 8       PetroleumExperts
            ' 9       Orkiszewski
            ' 10      PetroleumExperts2
            ' 11      DunsandRosOriginal
            ' 12      PetroleumExperts3
            ' 14      GREmodifiedbyPE
            ' 18      PetroleumExperts4
            ' 19      Hydro3P
            ' 20      PetroleumExperts5
            ' 21      OLGAS2P
            ' 22      OLGAS3P
            ' 23      OLGAS3PEXT

            ' Correlación de flujo multifásico Vertical
            DoSet("PROSPER.ANL.SYS.Tubing", CorrVFP)

            ' Sensitivity Variables

            'Type	Index	Variable Name
            ' --------------------------------------------------------------
            '    1	  1	Reservoir Pressure
            '   53	  2	Reservoir Temperature
            '   51	  7	Reservoir Permeability
            '   56	  8	Reservoir Thickness
            '   57	  9	Drainage Area
            '   58	 10	Dietz Shape Factor
            '   59	 11	Wellbore Radius
            '   16	  6	Water Cut
            '   17	131	Gas Oil Ratio
            '    2	 27	Skin
            '   40	155	Water Oil Ratio
            '   19	156	Condensate Gas Ratio
            '   24	132	GLR Free
            '   25	133	Free Gas Rate
            '   33	134	GOR Free
            '   20	135	Total GOR
            '   39	136	Dissolved and Free GLR
            '   35	148	Operating Frequency
            '   36	149	Gas Separator Efficiency
            '   37	150	Pump Wear Factor
            '   38	151	Pump Depth (Measured)
            '   41	157	Number of Stages
            '   27	145	First Node Pressure
            '   26	144	Tubing Roughness
            '  334	195	Downhole Heat Transfer Coefficient
            '   21	147	Tubing/Pipe Diameter
            '  341	202	Downhole Equipment
            '  342	203	Surface Equipment

            '*** Limpiar los datos anteriores
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Clear", "")

            'Se selecciona como parametro de sensibilidad la freciencia del BEC
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[0]", 35)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[1]", 16)
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Vars[2]", 1)

            ' Se generan los 10 valores de Frecuencia
            '

            Dim Delta_frec, Delta_Wc, Wc_Max, Wc_Min As Double
            Dim WcVec(9) As Double
            ReDim QgiVec(9)
            Wc_Min = 0
            Wc_Max = 90

            Delta_frec = (FrecMax.Val - FrecMin.Val) / (10 - 1)
            Delta_Wc = (Wc_Max - Wc_Min) / (10 - 1)

            For I = 0 To 9
                FrecVec(I) = FrecMin.Val + I * Delta_frec
                WcVec(I) = Wc_Min + I * Delta_Wc
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[148].Vals[" & I & "]", FrecVec(I))
                DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[" & I & "]", WcVec(I))
            Next I

            ' Pws
            DoSet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[0]", PRes.Val)

            ' Se realizan los calculos
            DoCmd("PROSPER.ANL.SYS.CALC")

            ' Se obtienen los resultados parciales del análisis de sensibilidad al % de agua
            Resul_System3var(CorrVFP, THPres.Val, Wc.Val, GOR_Total, 9, 9, 0, Frec_Res, Wc_Res, Pws_Res, Qliq_Res, 148)
        End Function

        Public Function Carta_BEC() As Boolean
            Try
                Dim poli As New RegLinPol
                Xcarta_BEC = New ArrayList
                Ycarta_BEC = New ArrayList
                ReDim Rango_MinX_BEC(5), Rango_MinY_BEC(5), Mejor_EficX_BEC(5), Mejor_EficY_BEC(5), Rango_MaxX_BEC(5), Rango_MaxY_BEC(5)
                'Dim Bomba_aux As String
                Bombaaux_BEC = DoGet("PROSPER.ESP.PUMPNAME(" & CStr(Bomba_BEC.Val) & ")")
                Dim elementos As String() = Bombaaux_BEC.Split(" ")
                getBomba(elementos(0), elementos(1), elementos(2))
                Dim frecuencia = BOMBA.FRECUENCIAHZ
                Dim num_etapas = BOMBA.ETAPAS
                Dim rang_minimo = BOMBA.MIN_RAN_OP
                Dim rang_maximo = BOMBA.MAX_RAN_OP
                Dim mejor_eficiencia = BOMBA.MEJOR_EFICIENCIA
                Dim raiz = BOMBA.RAIZ
                Dim xau(99) As Double
                Dim yau(99) As Double
                Dim xx As Double = 0
                Dim frec(5) As Double
                frec(0) = 30 : frec(1) = 40 : frec(2) = 50 : frec(3) = 60 : frec(4) = 70 : frec(5) = 80

                'se obtinen las cuatro curvas a las freccuencias de 40, 50, 60 y 70 Hertz
                For j = 0 To 3
                    ReDim xau(99)
                    ReDim yau(99)
                    Dim incre_x As Double = (raiz * (frec(j + 1) / frecuencia)) / 99

                    For i = 0 To 99
                        xau(i) = xx + incre_x * i
                        yau(i) = (Etapas_BEC.Val / (num_etapas * 3.2808)) * (coeficientes(j).COEFICIENTE1 * xau(i) ^ 5 + coeficientes(j).COEFICIENTE2 * xau(i) ^ 4 + coeficientes(j).COEFICIENTE3 * xau(i) ^ 3 + coeficientes(j).COEFICIENTE4 * xau(i) ^ 2 + coeficientes(j).COEFICIENTE5 * xau(i) + coeficientes(j).COEFICIENTE6)
                    Next i
                    Xcarta_BEC.Add(xau)
                    Ycarta_BEC.Add(yau)
                Next j

                'se obtiene los cuatro puntos de la curva del rango mínimo de operación
                Dim rango_min_xau(3) As Double
                Dim rango_min_yau(3) As Double
                For i = 0 To 3
                    rango_min_xau(i) = rang_minimo * (frec(i + 1) / frecuencia)
                    rango_min_yau(i) = (Etapas_BEC.Val / (num_etapas * 3.2808)) * (coeficientes(i).COEFICIENTE1 * rango_min_xau(i) ^ 5 + coeficientes(i).COEFICIENTE2 * rango_min_xau(i) ^ 4 + coeficientes(i).COEFICIENTE3 * rango_min_xau(i) ^ 3 + coeficientes(i).COEFICIENTE4 * rango_min_xau(i) ^ 2 + coeficientes(i).COEFICIENTE5 * rango_min_xau(i) + coeficientes(i).COEFICIENTE6)
                Next i

                Dim coe() As Double
                poli.Grado = 2
                poli.Decim = 25
                poli.Ayuda = False
                poli.X = rango_min_xau
                poli.Y = rango_min_yau
                poli.Regres()
                coe = poli.Coef
                For i = 0 To 5
                    Rango_MinX_BEC(i) = rang_minimo * (frec(i) / frecuencia)
                    If (i = 0 Or i = 5) Then
                        For j = 0 To UBound(coe)
                            Rango_MinY_BEC(i) += coe(j) * Rango_MinX_BEC(i) ^ j
                        Next j
                    Else
                        Rango_MinY_BEC(i) = rango_min_yau(i - 1)
                    End If
                Next i


                'se obtiene los cuatro puntos de la curva del mejor eficiecia de la bomba
                Dim mejor_efic_xau(3) As Double
                Dim mejor_efic_yau(3) As Double
                For i = 0 To 3
                    mejor_efic_xau(i) = mejor_eficiencia * (frec(i + 1) / frecuencia)
                    mejor_efic_yau(i) = (Etapas_BEC.Val / (num_etapas * 3.2808)) * (coeficientes(i).COEFICIENTE1 * mejor_efic_xau(i) ^ 5 + coeficientes(i).COEFICIENTE2 * mejor_efic_xau(i) ^ 4 + coeficientes(i).COEFICIENTE3 * mejor_efic_xau(i) ^ 3 + coeficientes(i).COEFICIENTE4 * mejor_efic_xau(i) ^ 2 + coeficientes(i).COEFICIENTE5 * mejor_efic_xau(i) + coeficientes(i).COEFICIENTE6)
                Next i

                poli.X = mejor_efic_xau
                poli.Y = mejor_efic_yau
                poli.Regres()
                coe = poli.Coef
                For i = 0 To 5
                    Mejor_EficX_BEC(i) = mejor_eficiencia * (frec(i) / frecuencia)
                    If (i = 0 Or i = 5) Then
                        For j = 0 To UBound(coe)
                            Mejor_EficY_BEC(i) += coe(j) * Mejor_EficX_BEC(i) ^ j
                        Next j
                    Else
                        Mejor_EficY_BEC(i) = mejor_efic_yau(i - 1)
                    End If
                Next i

                'se obtiene los cuatro puntos de la curva del rango máximo de operación
                Dim rango_max_xau(3) As Double
                Dim rango_max_yau(3) As Double
                For i = 0 To 3
                    rango_max_xau(i) = rang_maximo * (frec(i + 1) / frecuencia)
                    rango_max_yau(i) = (Etapas_BEC.Val / (num_etapas * 3.2808)) * (coeficientes(i).COEFICIENTE1 * rango_max_xau(i) ^ 5 + coeficientes(i).COEFICIENTE2 * rango_max_xau(i) ^ 4 + coeficientes(i).COEFICIENTE3 * rango_max_xau(i) ^ 3 + coeficientes(i).COEFICIENTE4 * rango_max_xau(i) ^ 2 + coeficientes(i).COEFICIENTE5 * rango_max_xau(i) + coeficientes(i).COEFICIENTE6)
                Next i

                poli.X = rango_max_xau
                poli.Y = rango_max_yau
                poli.Regres()
                coe = poli.Coef
                For i = 0 To 5
                    Rango_MaxX_BEC(i) = rang_maximo * (frec(i) / frecuencia)
                    If (i = 0 Or i = 5) Then
                        For j = 0 To UBound(coe)
                            Rango_MaxY_BEC(i) += coe(j) * Rango_MaxX_BEC(i) ^ j
                        Next j
                    Else
                        Rango_MaxY_BEC(i) = rango_max_yau(i - 1)
                    End If
                Next i
                Dim JJJ As Integer = 1

                'DoCmd("PROSPER.MENU.OUTPUT.PLOT(7)")
                'Dim xxd = DoGet("PROSPER.MENU.OUTPUT.PLOT(7)")
            Catch ex As Exception
                Throw New Exception("Carta_BEC:" + ex.Message)
            End Try
        End Function

        Dim BOMBA As New BOMBA
        Dim coeficientes As New List(Of COEFICIENTES)
        Public Sub getBomba(Fabricante As String, MODELO As String, MED_PULGADAS As Double)
            Using contexto As New Entities_ModeloCI()
                'Dim fabricantes As CAT_FABRICANTE = contexto.CAT_FABRICANTE.Include("BOMBA.COEFICIENTES.DIVISIONES_COEFICIENTES").Where(Function(x) x.NOMBRE.Contains(Fabricante)).SingleOrDefault
                'bomba = From dd In datos.BOMBA Where dd.MODELO.Contains(modelo) And dd.MED_PULGADAS = dimension Select dd
                BOMBA = (From dd In contexto.BOMBA
                         Join fa In contexto.CAT_FABRICANTE On dd.IDFABRICANTE Equals fa.IDFABRICANTE
                         Where fa.NOMBRE.Equals(Fabricante) And dd.MODELO.Equals(MODELO) And dd.MED_PULGADAS = MED_PULGADAS
                         Select dd).FirstOrDefault


                If BOMBA IsNot Nothing Then
                    coeficientes = contexto.COEFICIENTES.Include("DIVISIONES_COEFICIENTES").Where(Function(w) w.IDBOMBA = BOMBA.IDBOMBA).OrderBy(Function(K) K.DIVISIONES_COEFICIENTES.NOMBRE).ToList
                End If
            End Using

        End Sub

        Public Function Diseño_BEC() As Double
            'Función que realiza el diseño del sistema BEC

            Dim Resul_Diseño As String = ""

            'Profundidad de la bomba en mD
            DoSet("PROSPER.SIN.ESP.Depth", Prof_BEC.Val)
            'Frecuencia de operación de la bambo en Hz
            DoSet("PROSPER.SIN.ESP.Frequency", Frec_BEC.Val)
            'Máximo diametro exterior de la bomba en pulgadas
            DoSet("PROSPER.SIN.ESP.MaxPumpOD", ODMax_BEC.Val)
            'Logitud del cable de potencia m
            DoSet("PROSPER.SIN.ESP.CableLength", LongCable_BEC.Val)
            'Eficiencia del separador del gas
            DoSet("PROSPER.SIN.ESP.Efficiency", EfiSepGas_BEC.Val)
            'Diseño del gasto
            DoSet("PROSPER.SIN.ESP.Rate", QTest.Val)
            'Corte de agua
            DoSet("PROSPER.SIN.ESP.WC", Wc.Val)
            'Total GOR
            DoSet("PROSPER.SIN.ESP.Solgor", GOR_Total)
            'Presión en la cabeza del pozo
            DoSet("PROSPER.SIN.ESP.Pres", THPres.Val)
            'Margen de seguridad de la potencia del motor
            DoSet("PROSPER.SIN.ESP.Safety", 0)
            'Factor de desgaste de la bomba en fracciones
            DoSet("PROSPER.SIN.ESP.Wear", Desgaste_BEC.Val)
            'Correlación de flujo vertica
            DoSet("PROSPER.SIN.ESP.Tubing", NumCorrVFP(0))
            'Correlación de flujo en la linea de descarga
            DoSet("PROSPER.SIN.ESP.Pipe", 5)
            'Modelo de reducción de gas
            DoSet("PROSPER.SIN.ESP.GDRflag", ReducGas_BEC.Val)

            'realiza el calculo
            DoCmd("PROSPER.ANL.ESP.CALC")

            'Resultados del diseño de sistema BEC

            'Presión de fondo fluyente
            Dim Pwf As Double = DoGet("PROSPER.OUT.ESP.ListBox[1]")
            'Presión de entrada en el bomba
            Dim PIP As Double = DoGet("PROSPER.OUT.ESP.ListBox[4]")
            'Temperatura de en la entrada de la bomba
            Dim PIT As Double = DoGet("PROSPER.OUT.ESP.ListBox[5]")
            'Gasto de líquido en la entrada de la bomba
            Dim PIR As Double = DoGet("PROSPER.OUT.ESP.ListBox[6]")
            'GOR libre en la entrada a la bomba
            Dim FGOREP As Double = DoGet("PROSPER.OUT.ESP.ListBox[7]")
            'Presión de descarga de la bomba
            Dim PDP As Double = DoGet("PROSPER.OUT.ESP.ListBox[8]")
            'Gasto de descarga de la bomba
            Dim PDR As Double = DoGet("PROSPER.OUT.ESP.ListBox[9]")
            'GOR total de descarga
            Dim GORAP As Double = DoGet("PROSPER.OUT.ESP.ListBox[10]")
            'Gasto masico
            Dim MFR As Double = DoGet("PROSPER.OUT.ESP.ListBox[11]")
            'Densidad total de la mezcla
            Dim TFG As Double = DoGet("PROSPER.OUT.ESP.ListBox[12]")
            'Gasto promedio en el fondo
            Dim ADR As Double = DoGet("PROSPER.OUT.ESP.ListBox[13]")
            'Carga requerida
            Dim HR As Double = DoGet("PROSPER.OUT.ESP.ListBox[14]")
            'Carga actual requerida
            Dim AHR As Double = DoGet("PROSPER.OUT.ESP.ListBox[15]")
            'Potencia del fluido requerido
            Dim FPR As Double = DoGet("PROSPER.OUT.ESP.ListBox[16]")
            'Relación Gas Líquido en la entrada de la bomba
            Dim GLRPI As Double = DoGet("PROSPER.OUT.ESP.ListBox[17]")
            'Fracción de gas en la entrada a la bomba
            Dim GFPI As Double = DoGet("PROSPER.OUT.ESP.ListBox[18]")
            'Bo en la entrada de la bomba
            Dim BoPI As Double = DoGet("PROSPER.OUT.ESP.ListBox[19]")
            'Bg en la entrada de la bomba
            Dim BgPI As Double = DoGet("PROSPER.OUT.ESP.ListBox[20]")
            'Temperatura promedio en el cable
            Dim ACT As Double = DoGet("PROSPER.OUT.ESP.ListBox[21]")

            Resul_Diseño &= GetFrmt("Presión en cabeza del pozo =" & CStr(THPres.Val) & " (Kg/cm2 g)", 2)
            Resul_Diseño &= GetFrmt("Presión de fondo fluyendo =" & CStr(Pwf) & " (Kg/cm2 g)", 5)
            Resul_Diseño &= GetFrmt("Corte de agua =" & CStr(Wc.Val) & " (porcentaje)", 5)
            Resul_Diseño &= GetFrmt("Frecuencia de la bomba =" & CStr(Frec_BEC.Val) & " (Hertz)", 5)
            Resul_Diseño &= GetFrmt("Presión de entrada a la bomba =" & CStr(PIP) & " (Kg/cm2 g)", 5)
            Resul_Diseño &= GetFrmt("Temperatura de entrada a la bomba =" & CStr(PIT) & " (°C)", 5)
            Resul_Diseño &= GetFrmt("Gasto de entrada a la bomba =" & CStr(PIR) & " (RB/day)", 5)
            Resul_Diseño &= GetFrmt("GOR libre que entra a la bomba =" & CStr(FGOREP) & " (m3/m3)", 5)
            Resul_Diseño &= GetFrmt("Presión de descarga =" & CStr(PDP) & " (Kg/cm2 g)", 5)
            Resul_Diseño &= GetFrmt("Gasto de descarga =" & CStr(PDR) & " (RB/day)", 5)
            Resul_Diseño &= GetFrmt("GOR total en cima de la bomba =" & CStr(GORAP) & " (m3/m3)", 5)
            Resul_Diseño &= GetFrmt("Gasto masico =" & CStr(MFR) & " (lbm/day)", 5)
            Resul_Diseño &= GetFrmt("Densidad relativa total de la mezcla de los fluidos =" & CStr(TFG) & " (Fracción)", 5)
            Resul_Diseño &= GetFrmt("Gasto promedio en el fondo del pozo =" & CStr(ADR) & " (RB/day)", 5)
            Resul_Diseño &= GetFrmt("Carga requerida =" & CStr(HR) & " (m)", 5)
            Resul_Diseño &= GetFrmt("Carga requerida actual =" & CStr(AHR) & " (m)", 5)
            Resul_Diseño &= GetFrmt("Potencia hidraulica requerida =" & CStr(FPR) & " (hp)", 5)
            Resul_Diseño &= GetFrmt("Relación gas-líquido en la entrada en la bomba (V/V) =" & CStr(GLRPI) & " (fraction)", 5)
            Resul_Diseño &= GetFrmt("Fracción de gas en la entrada de la bomba =" & CStr(GFPI) & " (fraction)", 5)
            Resul_Diseño &= GetFrmt("Bo en la entrada de la bomba =" & CStr(BoPI) & " (RB/STB)", 5)
            Resul_Diseño &= GetFrmt("Bg en la entrada de la boma =" & CStr(BgPI) & " (ft3/scf)", 5)
            Resul_Diseño &= GetFrmt("Temperatura promedio del cable =" & CStr(ACT) & " (deg C)", 7)

        End Function

        ''' <summary>
        ''' Se obtienen los resultados parciales del análisis de sensibilidad al % de agua
        ''' </summary>
        ''' <param name="CorrVert"></param>
        ''' <param name="Pth"></param>
        ''' <param name="Wc"></param>
        ''' <param name="GORTOT"></param>
        ''' <param name="NumVar1"></param>
        ''' <param name="NumVar2"></param>
        ''' <param name="NumVar3"></param>
        ''' <param name="Var1"></param>
        ''' <param name="Var2"></param>
        ''' <param name="Var3"></param>
        ''' <param name="SolLiqRate"></param>
        ''' <remarks></remarks>
        Public Shared Sub Resul_System3var(ByVal CorrVert As Integer,
                                           ByVal Pth As Double, ByVal Wc As Double, ByVal GORTOT As Double,
                                           ByVal NumVar1 As Integer, NumVar2 As Integer, NumVar3 As Integer,
                                           ByRef Var1() As Double, ByRef Var2() As Double, ByRef Var3() As Double,
                                           ByRef SolLiqRate(,,) As Double, ByVal ind As Integer)

            Dim I, J, K, L As Integer
            'Dim QLiqAnl(NumVar1, NumVar2, NumVar3, 19), QAcAnl(NumVar1, NumVar2, NumVar3, 19), VLPPres(NumVar1, NumVar2, NumVar3, 19), IPRPres(NumVar1, NumVar2, NumVar3, 19) As Double
            Dim SolGasRate(9, 9, 9), SolOilRate(9, 9, 9), SolWatRate(9, 9, 9), SolBHP(9, 9, 9)
            Dim SolWHPress(9, 9, 9), SolWHTempe(9, 9, 9) As Double

            For I = 0 To NumVar1
                ' Volumen de gas de inyección
                Var1(I) = DoGet("PROSPER.ANL.SYS.Sens.SensDB.Sens[" & ind & "].Vals[" & I & "]")
                For J = 0 To NumVar2
                    Var2(J) = DoGet("PROSPER.ANL.SYS.Sens.SensDB.Sens[6].Vals[" & J & "]")
                    For K = 0 To NumVar3
                        Var3(K) = DoGet("PROSPER.ANL.SYS.Sens.SensDB.Sens[1].Vals[" & K & "]")
                        'For L = 0 To 19
                        '    ' Producción de líquido
                        '    If L = 17 Then Stop
                        '    QLiqAnl(I, J, K, L) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].LiqRate[" & L & "]")
                        '    QAcAnl(I, J, K, L) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].OilRate[" & L & "]")
                        '    VLPPres(I, J, K, L) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].VLPpres[" & L & "]")
                        '    IPRPres(I, J, K, L) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].IPRpres[" & L & "]")
                        'Next L
                        SolLiqRate(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.LiqRate")
                        'MsgBox(I.ToString & ", " & J.ToString & ", " & K.ToString & ": " & SolLiqRate(I, J, K).ToString)
                        SolGasRate(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.GasRate")
                        SolOilRate(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.OilRate")
                        SolWatRate(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.WatRate")
                        SolBHP(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.BHP")
                        SolWHPress(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.WHPressure")
                        SolWHTempe(I, J, K) = DoGet("PROSPER.OUT.SYS.Results[" & I & "][" & J & "][" & K & "].Sol.WHTemperature")
                    Next K
                Next J
            Next I
        End Sub

        ''' <summary>
        ''' Se obtiene el máximo o el mínimo de un vector
        ''' </summary>
        ''' <param name="NumDat"> Número de datros en el vector</param>
        ''' <param name="Val"> Arreglo en donde se va a buscar</param>
        ''' <param name="Max"> Valor booleano, verdadfero se obtiene el valor máxomo, falso se obtiene el valor minimo</param>
        ''' <returns> Valor encongrado, ya sea máximo o mínimo</returns>
        Public Shared Function MaxMin(ByVal NumDat As Integer, ByVal Val() As Double, ByVal Max As Boolean) As Double
            Dim I As Integer
            Dim V As Double
            V = Val(0)
            For I = 0 To NumDat
                If Max Then
                    If V < Val(I) Then V = Val(I)
                Else
                    If V > Val(I) Then V = Val(I)
                End If
            Next I
            Return V
        End Function

        ''' <summary>
        ''' Obtiene el nombre de la correlación de flujo multifásico vertical
        ''' </summary>
        ''' <param name="Cual"> Valor entero que determina la correlación</param>
        ''' <returns> Regresa una cadena con el nombre de la correlación</returns>
        Public Shared Function CorrelacionFM(ByVal Cual As Integer) As String
            Dim Aux As String = ""
            Select Case Cual
                Case 0 : Aux = "Duns - Ros Modificada"
                Case 1 : Aux = "Hagedorn y Brown"
                Case 2 : Aux = "Fancher y Brown"
                Case 3 : Aux = "Gray"
                Case 4 : Aux = "Mukerjee - Brill"
                Case 5 : Aux = "Beggs y Brill"
                Case 6 : Aux = "Dukler - Flannigan"
                Case 7 : Aux = "Dukler - Eaton -Flannigan"
                Case 8 : Aux = "Petroleum Experts"
                Case 9 : Aux = "Orkiszewski"
                Case 10 : Aux = "Petroleum Experts 2"
                Case 11 : Aux = "Duns - Ros Original"
                Case 12 : Aux = "Petroleum Experts 3"
                Case 13 : Aux = "Beggs y Brill Gas Head"
                Case 14 : Aux = "GRE modificada por Petroleum Experts"
                Case 15 : Aux = "GRE con DSM"
                Case 16 : Aux = "GRE original"
                Case 17 : Aux = "GRE con AE"
                Case 18 : Aux = "Petroleum Experts4"
                Case 19 : Aux = "Hydro 3P"
            End Select
            Return Aux
        End Function

        ''' <summary>
        ''' Obtiene el indice de la correlación de flujo multifásico seleccionada
        ''' </summary>
        ''' <param name="Cual"> valor numerico del 0 al 13</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Shared Function IndiceCorr(ByVal Cual As Integer) As Integer
            '  0 DunsandRosModified
            '  1 HagedornBrown
            '  2 FancherBrown
            '  3 Gray
            '  4 MukerjeeBrill
            '  5 BeggsandBrill
            '  6 DuklerFlannigan
            '  7 DuklerEatonFlannigan
            '  8 PetroleumExperts
            '  9 Orkiszewski
            ' 10 PetroleumExperts2
            ' 11 DunsandRosOriginal
            ' 12 PetroleumExperts3
            ' 13 BeggsandBrillGasHead
            ' 14 GREmodifiedbyPE
            ' 15 GREwithDSM
            ' 16 GREoriginal
            ' 17 GREwithAE
            ' 18 PetroleumExperts4
            ' 19 Hydro3P
            Dim Indice As Integer
            Select Case Cual
                Case 0 : Indice = 0   ': Correlac = "DunsandRosModified"
                Case 1 : Indice = 1   ': Correlac = " HagedornBrown"
                Case 2 : Indice = 2   ': Correlac = " FancherBrown"
                Case 3 : Indice = 4   ': Correlac = " MukerjeeBrill"
                Case 4 : Indice = 5   ': Correlac = " BeggsandBrill"
                Case 5 : Indice = 8   ': Correlac = " PetroleumExperts"
                Case 6 : Indice = 9   ': Correlac = " Orkiszewski"
                Case 7 : Indice = 10  ': Correlac = " PetroleumExperts2"
                Case 8 : Indice = 11  ': Correlac = " DunsandRosOriginal"
                Case 9 : Indice = 12  ': Correlac = " PetroleumExperts3"
                Case 10 : Indice = 14 ': Correlac = " GREmodifiedbyPE"
                Case 11 : Indice = 18 ': Correlac = " PetroleumExperts4"
                Case 12 : Indice = 19 ': Correlac = " Hydro3P"
                Case 13 : Indice = 20 ': Correlac = " PetroleumExperts5"
                Case Else
                    Throw New Exception("Sin correlacion de FMV")
            End Select
            Return Indice
        End Function

        ''' <summary>
        ''' Ordena o clasifica tres vectores tomando como base uno de ellos, ya sea de mayor a menor o de menor a mayor
        ''' </summary>
        ''' <param name="NPunt"> Número de puntos de los tres vectores</param>
        ''' <param name="Vec_1"> Primer vector</param>
        ''' <param name="Vec_2"> Segundo vector</param>
        ''' <param name="Vec_3"> Tercer vector</param>
        ''' <param name="Cual"> Determina cual vector sera tomado como base</param>
        ''' <param name="Menor_Mayor"> Verdadero: Ordena de menor a mayor, Falso: ordena de mayor a menor</param>
        Public Function SortBubble(ByVal NPunt As Integer, ByVal Vec_1() As Integer, ByVal Vec_2() As String, ByRef Vec_3() As Double,
                                     ByVal Cual As Integer, ByVal Menor_Mayor As Boolean)

            ' ============================== BubbleSort ==================================
            '    The BubbleSort algorithm cycles through SortArray, comparing adjacent
            '    elements and swapping pairs that are out of order.  It continues to
            '    do this until no pairs are swapped.
            ' ============================================================================
            Dim Limit As Integer = 0
            Dim Switch1 As Integer = 0
            Dim Row As Integer = 0
            Dim Verifica As Boolean = False

            Try
                Limit = NPunt - 1 ' Numero de puntos
                Do
                    Switch1 = 0
                    For Row = 0 To (Limit - 1)
                        ' Two adjacent elements are out of order, so swap their values
                        Select Case Cual
                            Case 1
                                ' Por el vector 1: Numero de la correlación dentro de PROSPER
                                If Menor_Mayor Then
                                    Verifica = Vec_1(Row) > Vec_1(Row + 1)
                                Else
                                    Verifica = Vec_1(Row) < Vec_1(Row + 1)
                                End If
                            Case 2
                                ' Por el vector 2: Nombre de la correlación
                                If Menor_Mayor Then
                                    Verifica = Vec_2(Row) > Vec_2(Row + 1)
                                Else
                                    Verifica = Vec_2(Row) < Vec_2(Row + 1)
                                End If
                            Case 3
                                ' Por el vector 3: diferencia de presión
                                If Menor_Mayor Then
                                    Verifica = Vec_3(Row) > Vec_3(Row + 1)
                                Else
                                    Verifica = Vec_3(Row) < Vec_3(Row + 1)
                                End If
                                'MsgBox(Row.ToString)
                            Case Else
                                MsgErrCalc &= GetFrmt("Función: SortBubble", 2)
                                MsgErrCalc &= GetFrmt("No se ha seleccionado que columna ordenar", 2)
                                IErr = True
                                Exit Function
                        End Select
                        If Verifica Then
                            'MsgBox("Swap: " & Row.ToString & " -> " & (Row + 1))
                            SwapSortBubble(Vec_1, Vec_2, Vec_3, Row, Switch1)
                            Verifica = False
                        End If
                    Next Row
                    ' Sort on next pass only to where the last switch was made:
                    Limit = Switch1
                Loop While Switch1
            Catch ex As Exception
                Throw New Exception(ex.ToString)
            End Try
        End Function

        ''' <summary>
        ''' Determina el nombre de la correlacion de flujo multifásico vertical
        ''' </summary>
        ''' <param name="Cual"> Indice de la correlación</param>
        ''' <returns> Nombre de la correlación deseada</returns>
        Public Function NombrCorr(Cual As Integer) As String
            Dim Correlac As String = ""
            Select Case Cual
                Case 0 : Correlac = "DunsandRosModified"
                Case 1 : Correlac = "HagedornBrown"
                Case 2 : Correlac = "FancherBrown"
                Case 3 : Correlac = "MukerjeeBrill"
                Case 4 : Correlac = "BeggsandBrill"
                Case 5 : Correlac = "PetroleumExperts"
                Case 6 : Correlac = "Orkiszewski"
                Case 7 : Correlac = "PetroleumExperts2"
                Case 8 : Correlac = "DunsandRosOriginal"
                Case 9 : Correlac = "PetroleumExperts3"
                Case 10 : Correlac = "GREmodifiedbyPE"
                Case 11 : Correlac = "PetroleumExperts4"
                Case 12 : Correlac = "Hydro3P"
                Case 13 : Correlac = "PetroleumExperts5"
                Case Else
                    IErr = True
                    Correlac = "Sin correlacion de FM"
            End Select
            Return Correlac
        End Function

        ''' <summary>
        ''' Determina el índice de la correlación de flujo multifasico vertical empleado en Prosper
        ''' </summary>
        ''' <param name="Cual"> Índice de la correlación asignado en la presente clase</param>
        ''' <returns> Indice de la Correlación de FMV empleadp en Prosper</returns>
        Public Shared Function NumCorrVFP(ByVal Cual As Integer) As Integer
            '   You can also use the following syntax to reference the correlation by label
            '
            ' PROSPER.ANL.SYS.TubingLabel
            '
            ' Use the following list to select the correlation by name
            '
            ' 0       DunsandRosModified
            ' 1       HagedornBrown
            ' 2       FancherBrown
            ' 4       MukerjeeBrill
            ' 5       BeggsandBrill
            ' 8       PetroleumExperts
            ' 9       Orkiszewski
            ' 10      PetroleumExperts2
            ' 11      DunsandRosOriginal
            ' 12      PetroleumExperts3
            ' 14      GREmodifiedbyPE
            ' 18      PetroleumExperts4
            ' 19      Hydro3P
            ' 20      PetroleumExperts5
            ' 21      OLGAS2P
            ' 22      OLGAS3P
            ' 23      OLGAS3PEXT
            Dim Correla As Integer
            Select Case Cual
                Case 0 : Correla = 0    ' DunsandRosModified
                Case 1 : Correla = 1    ' HagedornBrown
                Case 2 : Correla = 2    ' FancherBrown
                Case 3 : Correla = 4    ' MukerjeeBrill
                Case 4 : Correla = 5    ' BeggsandBrill
                Case 5 : Correla = 8    ' PetroleumExperts
                Case 6 : Correla = 9    ' Orkiszewski
                Case 7 : Correla = 10   ' PetroleumExperts2
                Case 8 : Correla = 11   ' DunsandRosOriginal
                Case 9 : Correla = 12   ' PetroleumExperts3
                Case 10 : Correla = 14  ' GREmodifiedbyPE
                Case 11 : Correla = 18  ' PetroleumExperts4
                Case 12 : Correla = 19  ' Hydro3P
                Case 13 : Correla = 20  ' PetroleumExperts5
            End Select
            Return Correla
        End Function

        ''' <summary>
        ''' Determina el nombre de la correlación de flujo multifásico vertical asignada en Prosper 
        ''' </summary>
        ''' <param name="Cual"> Número secuancial de correlación</param>
        ''' <returns> Nombre de la correlacion de FMV asignada en Prosper</returns>
        Public Shared Function NombrCorr_2(ByVal Cual As Integer) As String
            Dim Correlac As String = ""
            Select Case Cual
                Case 1
                    'PROSPER.ANL.COR.Corr[{Duns and Ros Modified}].A[0]
                    Correlac = "Duns and Ros Modified"
                Case 2
                    'PROSPER.ANL.COR.Corr[{Hagedorn Brown}].A[0]
                    Correlac = "Hagedorn Brown"
                Case 3
                    'PROSPER.ANL.COR.Corr[{Fancher Brown}].A[0]
                    Correlac = "Fancher Brown"
                Case 4
                    'PROSPER.ANL.COR.Corr[{Mukerjee Brill}].A[0]
                    Correlac = "Mukerjee Brill"
                Case 5
                    'PROSPER.ANL.COR.Corr[{Beggs and Brill}].A[0]
                    Correlac = "Beggs and Brill"
                Case 6
                    'PROSPER.ANL.COR.Corr[{Petroleum Experts}].A[0]
                    Correlac = "Petroleum Experts"
                Case 7
                    'PROSPER.ANL.COR.Corr[{Orkiszewski}].A[0]
                    Correlac = "Orkiszewski"
                Case 8
                    'PROSPER.ANL.COR.Corr[{Petroleum Experts 2}].A[0]
                    Correlac = "Petroleum Experts 2"
                Case 9
                    'PROSPER.ANL.COR.Corr[{Duns and Ros Original}].A[0]
                    Correlac = "Duns and Ros Original"
                Case 10
                    'PROSPER.ANL.COR.Corr[{Petroleum Experts 3}].A[0]
                    Correlac = "Petroleum Experts 3"
                Case 11
                    'PROSPER.ANL.COR.Corr[{GRE (modified by PE)}].A[0]
                    Correlac = "GRE (modified by PE)"
                    'PROSPER.ANL.COR.Corr[{GRE (modified by PE)}].A[0]
                Case 12
                    'PROSPER.ANL.COR.Corr[{Petroleum Experts 4}].A[0]
                    Correlac = "Petroleum Experts 4"
                Case 13
                    'PROSPER.ANL.COR.Corr[{Hydro-3P}].A[0]
                    Correlac = "Hydro-3P"
                Case 14
                    'PROSPER.ANL.COR.Corr[{Petroleum Experts 5}].A[0]
                    Correlac = "Petroleum Experts 5"
            End Select
            Return Correlac
        End Function

        ''' <summary>
        ''' Tres vectores A, B y C que se quieren intercambiar del lugar I al lugar J
        ''' </summary>
        ''' <param name="A"> Primer vector de tipo entero</param>
        ''' <param name="B"> Segundo vector de tipo String</param>
        ''' <param name="C"> Tercer vector de tipo doble</param>
        ''' <param name="I"> Indice que se quiere cambiar</param>
        ''' <param name="J"> Indice por el cual se quiere cambiar</param>
        ''' <remarks></remarks>
        Public Shared Sub SwapSortBubble(ByRef A() As Integer, ByRef B() As String, ByRef C() As Double,
                                         ByVal I As Integer, ByRef J As Integer)
            ' Siempre será necesario cambiar esta subrutina debido a que se debe adaptar a el tipo declarado
            '
            ' Agregar todos los vectores necearios y que correspondan a el tipo declarado
            '
            'MsgBox(A(I).ToString & " : " & A(I + 1).ToString)
            Call Swap(A(I), A(I + 1))
            'MsgBox(A(I).ToString & " : " & A(I + 1).ToString)

            'MsgBox(B(I).ToString & " : " & B(I + 1).ToString)
            Call Swap(B(I), B(I + 1))
            'MsgBox(B(I).ToString & " : " & B(I + 1).ToString)

            'MsgBox(C(I).ToString & " : " & C(I + 1).ToString)
            Call Swap(C(I), C(I + 1))
            'MsgBox(C(I).ToString & " : " & C(I + 1).ToString)

            J = I ' Esta instrucción no se debe cambiar ni quitar
        End Sub

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="Valor_1"></param>
        ''' <param name="Valor_2"></param>
        ''' <remarks></remarks>
        Public Shared Sub Swap(ByRef Valor_1 As Object, ByRef Valor_2 As Object)
            Dim Valor_3 As Object
            Valor_3 = Valor_1
            Valor_1 = Valor_2
            Valor_2 = Valor_3
        End Sub

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="Vec"></param>
        ''' <remarks></remarks>
        Public Shared Sub VerifQgi(Vec() As Double)

            Dim I As Integer
            Const Incr As Double = 0.1

            ' El valor a verificar se encuentra en el 10mo lugar
            For I = 0 To 8
                If Vec(I) = Vec(9) Then Vec(I) = Vec(I) + Incr
            Next I

        End Sub

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="Yn"></param>
        ''' <param name="Yo"></param>
        ''' <param name="AXLen"></param>
        ''' <param name="Inc"></param>
        ''' <param name="Dltv"></param>
        ''' <param name="Fstv"></param>
        ''' <remarks></remarks>
        Public Shared Sub Escala(ByRef Yn As Double, ByRef Yo As Double,
                          ByVal AXLen As Long, ByVal Inc As Long,
                          ByRef Dltv As Long, ByRef Fstv As Long)

            '----------------------------------------------------------------------------
            '
            '   (c) Faustino Alonso Fuentes Nucamendi          - Junio, 1988
            '       Consultores Petroleros Internacionales
            '
            ' ---------------------------------------------------------------------------
            ' Yn    : Valor Maximo
            ' Yo    : Valor minimo
            ' AXLEN : Longitud en pulgadas sobre el cual el arreglo va a ser escalado.
            ' INC   : Incremento de localizacion de los puntos
            ' Dltv  :
            ' Fstv  :
            '
            '----------------------------------------------------------------------------
            Dim Fad As Double
            Dim I, Isi, P, T As Long
            Dim Sigue As Boolean

            Dim Saver(35) As Long

            '   ....Se inicializa el arreglo con los valores de Delta Basicos
            '

            Saver(1) = 1.0 : Saver(2) = 2.0 : Saver(3) = 4.0 : Saver(4) = 5.0 : Saver(5) = 8.0
            Saver(6) = 10.0 : Saver(7) = 20.0 : Saver(8) = 50.0 : Saver(9) = 100.0
            Saver(10) = 200.0 : Saver(11) = 500.0 : Saver(12) = 1000.0 : Saver(13) = 2000.0
            Saver(14) = 5000.0 : Saver(15) = 10000.0 : Saver(16) = 20000.0
            Saver(17) = 50000.0 : Saver(18) = 100000.0 : Saver(19) = 200000.0
            Saver(20) = 500000.0 : Saver(21) = 1000000.0 : Saver(22) = 2000000.0
            Saver(23) = 5000000.0 : Saver(24) = 10000000.0 : Saver(25) = 20000000.0
            Saver(26) = 50000000.0 : Saver(27) = 100000000.0 : Saver(28) = 200000000.0
            Saver(29) = 500000000.0 : Saver(30) = 1000000000.0 : Saver(31) = 2000000000.0
            Saver(32) = 5000000000.0 : Saver(33) = 10000000000.0 : Saver(34) = 20000000000.0
            Saver(35) = 50000000000.0

            Fad = 0.01
            Fstv = Yo
            If Yo < 0 Then Fad = Fad - 1
            Dltv = (Yn - Fstv) / AXLen
            If Dltv > 0 Then
Ciclo1:
                I = Fix((Math.Log(Dltv) / Math.Log(10)) + 1000)
                P = 10 ^ (I - 1000)
                Dltv = Dltv / P - 0.01
                For I = 1 To 6
                    Isi = I
                    If (Saver(I) - Dltv) >= 0 Then Exit For
                Next I
                Do
                    Dltv = Saver(Isi) * P
                    Fstv = Dltv * Fix(Yo / Dltv + Fad)
                    T = Fstv + (AXLen + 0.01) * Dltv
                    If (T - Yn) < 0 Then
                        Fstv = P * Fix(Yo / P + Fad)
                        T = Fstv + (AXLen + 0.01) * Dltv
                        If (T - Yn) < 0 Then
                            Isi = Isi + 1
                            Sigue = True
                        Else
                            Sigue = False
                        End If
                    Else
                        Sigue = False
                    End If
                Loop While Sigue
                Fstv = Fstv - Fix((AXLen + (Fstv - Yn) / Dltv) / 2) * Dltv
                If (Yo * Fstv) < 0 Then Fstv = 0
                If Inc <= 0 Then
                    Fstv = Fstv + Fix(AXLen + 0.5) * Dltv
                    Dltv = -Dltv
                End If
            Else
                Dltv = 2 * Fstv
                Dltv = Math.Abs(Dltv / AXLen) + 1
                GoTo Ciclo1
            End If
        End Sub

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="Maxx"></param>
        ''' <param name="Minn"></param>
        ''' <param name="Fstv"></param>
        ''' <param name="Dltv"></param>
        ''' <param name="NoDivide"></param>
        ''' <param name="TipoArr"></param>
        ''' <remarks></remarks>
        Public Shared Sub DivideMe(ByRef Maxx As Long, ByRef Minn As Long,
                            ByVal Fstv As Long, ByVal Dltv As Long,
                            ByRef NoDivide As Long,
                            ByVal TipoArr As Long)


            '----------------------------------------------------------------------------
            '
            ' (c)   Faustino Alonso Fuentes Nucamendi          -  Junio 1988.
            '
            '----------------------------------------------------------------------------
            '     Determina el numero de divisiones de la grafica , asi como los valores
            '  del maximo y del minimo de los valores a graficar
            '  NoDivide: NUMERO DE DIVISIONES A REALIZAR EN LA GRAFICA
            '
            '----------------------------------------------------------------------------

            Select Case TipoArr
                Case 1 ' ESCALAMIENTO DE MAXIMO A MINIMO
                    NoDivide = Math.Abs(Fstv / Dltv)
                    Maxx = Fstv
                    Minn = Fstv + NoDivide * Dltv
                Case 2
                    NoDivide = Math.Abs(Int((Maxx - Fstv) / Dltv)) + 1
                    Maxx = Fstv + NoDivide * Dltv
                    Minn = Fstv
                Case Else
            End Select
        End Sub

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Function getAyuda() As String

            Dim textAyuda As String = ""
            'Dim title As String = "Ayuda 1/3"
            'Dim style As MsgBoxStyle = MsgBoxStyle.DefaultButton1 Or MsgBoxStyle.Information Or MsgBoxStyle.OkCancel
            'Dim response As MsgBoxResult

            textAyuda &= GetFrmt("Clase: Ayuda Modelo de Prosper                                                     ", 2)
            textAyuda &= GetFrmt("Crea un modelo de Prosper a partir de los datos almacenados en una base de datos.  ", 2)
            textAyuda &= GetFrmt("Datos de Entrada:                                                                  ", 2)
            textAyuda &= GetFrmt("Ayuda       : Despliega el presente mensaje.                                       ", 5)
            textAyuda &= GetFrmt("Diagnostics : Despliega que valores tienen los datos de entrada.                   ", 5)
            textAyuda &= GetFrmt("ProgramPath : Ruta donde se encuentra el archivo Prosper.exe                       ", 5)
            textAyuda &= GetFrmt("ArchivoPVT  : Ruta donde se encuentra el archivo del ajuste PVT del campo.         ", 6)
            textAyuda &= GetFrmt("Datos Generales:                                                                   ", 2)
            textAyuda &= GetFrmt("Fluid       : 0 = Oil and Water. 1 = Dry and Wet Gas. 2 = Retrograde Condensate.   ", 5)
            textAyuda &= GetFrmt("PVTModel    : 0 = Black Oil. 1 = Equation of State.                                ", 5)
            textAyuda &= GetFrmt("Separator   : 0 = Single Stage Separator. 1 = Two Stage Separator.                 ", 5)
            textAyuda &= GetFrmt("Emulsion    : 0 = No. 1 = Emulsion + Pump Viscosity Correction.                    ", 5)
            textAyuda &= GetFrmt("Hydrate     : 0 = Disable Warning. 1 = Enable Warning.                             ", 5)
            textAyuda &= GetFrmt("WatVis      : 0 = Use Default Correlation. 1 = Use Pressure Corrected Correlation. ", 5)
            textAyuda &= GetFrmt("VisMod      : 0 = Newtonian Fluid. 1 = Non Newtonian Fluid.                        ", 5)
            textAyuda &= GetFrmt("FlowType    : 0 = Tubing Flow. 1 = Annular Flow.                                   ", 5)
            textAyuda &= GetFrmt("WellType    : 0 = Producer. 1 = Inyector. 2 = Water Inyector                       ", 5)
            textAyuda &= GetFrmt("LiftMethod  : 0 = None. 1 = Gas Lift (Continous). 2 = Electrical Sumersible Pump.  ", 5)
            textAyuda &= GetFrmt("3 = Hydraulic Drive Downhole Pump. 4 = Progressive Cavity Pump.", 7)
            textAyuda &= GetFrmt("5 = Coiled Tubing Gas Lift. 6 = Diluent Injection. 7 = Jet Pump.", 7)
            textAyuda &= GetFrmt("8 = MultiPhase Pump. 9 = Sucker Rod Pump. 10 = Gas Lift (Intermttent).", 7)
            textAyuda &= GetFrmt("LiftType    : 0 = No Friction Loss In Annulus. 1 = Friction Loss In Annulus.       ", 5)
            textAyuda &= GetFrmt("2 = Safety Equipment.", 7)
            textAyuda &= GetFrmt("Predict     : 0 = Pressure Only. 1 = Pressure and Temperature (offshore).          ", 5)
            textAyuda &= GetFrmt("2 = Pressure and Temperature (on Land).", 7)
            textAyuda &= GetFrmt("TempModel   : 0 = Rough Approximation. 1 = Enthalpy Balance.                       ", 5)
            textAyuda &= GetFrmt("2 = Improved Approximation.", 7)
            textAyuda &= GetFrmt("RangeSystem : 0 = Full System. 1 = Pipeline Only.                                  ", 5)
            textAyuda &= GetFrmt("OutputRes   : 0 = Show Calculating Data. 1 = Hide Calculating Data.                ", 5)
            textAyuda &= GetFrmt("Completion  : 0 = Cased Hole. 1 = Open hole.                                       ", 5)
            textAyuda &= GetFrmt("GravelPack  : 0 = None. 1 = Gravel Pack. 2 = Pre Packed Screen.                    ", 5)
            textAyuda &= GetFrmt("3 = Wire Wrapped Screen. 4 = Slotted Liner                                         ", 7)

            'response = MsgBox(textAyuda, style, title)
            'If response = MsgBoxResult.Cancel Then
            '    Return
            'Else
            '    textAyuda = ""
            '    title = "Ayuda 2/3"
            'End If

            'textAyuda = "Clase -> Crea.Modelo: Clase que crea un Modelo de Prosper" & Chr(13) & Chr(13) &
            '"Datos de Entrda :" & Chr(13) & Chr(13) & Chr(9) &
            '"Ayuda As Boolean : Despliega esta ayuda. " & Chr(13) & Chr(9) &
            '"Diagnostics As Boolean : Despliega que valores tienen los datos de entrada." & Chr(13) & Chr(13) & Chr(9) &
            '"ProgramPath As String : Ruta donde se encuentra el archivo Prosper.exe" & Chr(13) & Chr(9) &
            '"ArchivoPVT As String : Ruta donde se encuentra el archivo del ajuste PVT del campo." & Chr(13) & Chr(13) &
            '"Datos Generales :" & Chr(13) & Chr(13) & Chr(9) &
            '"Fluid As Byte : 0 = Oil and Water. 1 = Dry and Wet Gas. 2 = Retrograde Condensate." & Chr(13) & Chr(9) &
            '"PVTModel As Byte : 0 = Black Oil. 1 = Equation of State." & Chr(13) & Chr(9) &
            '"Separator As Byte : 0 = Single Stage Separator. 1 = Two Stage Separator." & Chr(13) & Chr(9) &
            '"Emulsion As Byte : 0 = No. 1 = Emulsion + Pump Viscosity Correction." & Chr(13) & Chr(9) &
            '"Hydrate As Byte : 0 = Disable Warning. 1 = Enable Warning" & Chr(13) & Chr(9) &
            '"WatVis As Byte : 0 = Use Default Correlation. 1 = Use Pressure Corrected Correlation." & Chr(13) & Chr(9) &
            '"VisMod As Byte : 0 = Newtonian Fluid. 1 = Non Newtonian Fluid" & Chr(13) & Chr(9) &
            '"FlowType As Byte : 0 = Tubing Flow. 1 = Annular Flow" & Chr(13) & Chr(9) &
            '"WellType As Byte : 0 = Producer. 1 = Inyector. 2 = Water Inyector" & Chr(13) & Chr(9) &
            '"LiftMethod As Byte : 0 = None. 1 = Gas Lift (Continous). 2 = Electrical Sumersible Pump." & Chr(13) & Chr(9) & Chr(9) & Chr(9) &
            '"LiftType As Byte : 0 = No Friction Loss In Annulus. 1 = Friction Loss In Annulus. 2 = Safety Equipment." & Chr(13) & Chr(9) &
            '"Predict As Byte : 0 = Pressure Only. 1 = Pressure and Temperature (offshore). 2 = Pressure and Temperature (on Land)." & Chr(13) & Chr(9) &
            '"TempModel As Byte : 0 = Rough Approximation. 1 = Enthalpy Balance. 2 = Improved Approximation." & Chr(13) & Chr(9) &
            '"RangeSystem As Byte : 0 = Full System. 1 = Pipeline Only." & Chr(13) & Chr(9) &
            '"OutputRes As Byte : 0 = Show Calculating Data. 1 = Hide Calculating Data." & Chr(13) & Chr(9) &

            textAyuda &= GetFrmt("Completion   : 0 = Cased Hole. 1 = Open hole.                                             ", 5)
            textAyuda &= GetFrmt("GravelPack   : 0 = None. 1 = Gravel Pack. 2 = Pre Packed Screen. 3 = Wire Wrapped Screen. ", 5)
            textAyuda &= GetFrmt("4 = Slotted Liner.", 7)
            textAyuda &= GetFrmt("InflowType   : 0 = Single Branch. 1 = MultiLateral Well.                                  ", 5)
            textAyuda &= GetFrmt("GasConing    : 0 = No. 1 = Yes                                                            ", 5)
            textAyuda &= GetFrmt("Company      : Pemex Exploración y producción.                                            ", 5)
            textAyuda &= GetFrmt("Field        : Nomnbre del campo.                                                         ", 5)
            textAyuda &= GetFrmt("Locations    : Nombre del activo.                                                         ", 5)
            textAyuda &= GetFrmt("Well         : Nombre del pozo.                                                           ", 5)
            textAyuda &= GetFrmt("Platform     : Nombre de la plataforma o macropera.                                       ", 5)
            textAyuda &= GetFrmt("Analyst      : Nombre del analista.                                                       ", 5)
            textAyuda &= GetFrmt("DatGenDate   : fecha de creacion del modelo.                                              ", 5)
            textAyuda &= GetFrmt("Comenta      : Comentarios.                                                               ", 6)
            textAyuda &= GetFrmt(StrDup(125, "-"), 1)
            textAyuda &= GetFrmt("Datos del Yacimiento:                                                                     ", 2)
            textAyuda &= GetFrmt("IPRMethod    : 0 = PI Entry. 1 = Vogel. 2 = Composite. 3 = Darcy. 4: Fetkovich.           ", 5)
            textAyuda &= GetFrmt("PRes         : Presión de Fondo Estática ....................................... [kg/cm2] ", 5)
            textAyuda &= GetFrmt("TRes         : Temperatura del Yacimiento .......................................... [oC] ", 5)
            textAyuda &= GetFrmt("Wc           : Porcentaje de agua en la capa ........................................ [%] ", 5)
            textAyuda &= GetFrmt("TotGor       : Relación Gas Aceite Total ........................................ [m3/m3] ", 5)
            textAyuda &= GetFrmt("Compact      : Modelo de reduccion de permeabilidad por compactación. 0 = No. 1 = Yes.    ", 5)
            textAyuda &= GetFrmt("IRelK        : Considerar las permeabilidades relativas. 0 = No. 1 = Yes.                 ", 6)
            textAyuda &= GetFrmt(StrDup(125, "-"), 1)
            textAyuda &= GetFrmt("Datos del modelo de Vogel.                                                                ", 2)
            textAyuda &= GetFrmt("MGSkinMethod : Daño Mecánico / Geometrico                                                 ", 5)
            textAyuda &= GetFrmt("0 = Enter Skin by Hand. 1 = Loke. 2 = MacLeod.  3 = Karakas - Tariq                       ", 7)
            textAyuda &= GetFrmt("DPSkinMethod : Daño por desviación y penetración parcial                                  ", 5)
            textAyuda &= GetFrmt("0 = Cinco / Martín-Bronz. 1 = Wong-Cliford. 2 = Cinco (2) / Martín-Bronz                  ", 7)
            textAyuda &= GetFrmt("ResPerm      : Permeabilidad del yacimiento ........................................ [md] ", 5)
            textAyuda &= GetFrmt("Thickness    : Espesor del yacimiento ............................................... [m] ", 5)
            textAyuda &= GetFrmt("Drainage     : Area de drene ...................................................... [km2] ", 5)

            'textAyuda = "Completion As Byte : 0 = Cased Hole. 1 = Open hole." & Chr(13) & Chr(9) &
            '"GravelPack As Byte : 0 = None. 1 = Gravel Pack. 2 = Pre Packed Screen. 3 = Wire Wrapped Screen. 4 = Slotted Liner." & Chr(13) & Chr(9) &
            '"InflowType As Byte : 0 = Single Branch. 1 = MultiLateral Well." & Chr(13) & Chr(9) &
            '"GasConing As Byte : 0 = No. 1 = Yes" & Chr(13) & Chr(9) &
            '"Company As String : Pemex Exploración y producción" & Chr(13) & Chr(9) &
            '"Field As String : Nomnbre del campo" & Chr(13) & Chr(9) &
            '"Locations As String : Nombre del Activo" & Chr(13) & Chr(9) &
            '"Well As String : Nombre del pozo"
            ' Daño Mecánico / Geometrico
            ' 0: Enter Skin by Hand
            ' 1: Loke
            ' 2: MacLeod
            ' 3: Karakas - Tariq
            'textAyuda = "Datos Generales :" & Chr(13) & Chr(13) & Chr(9) &
            '"Platform As String : Nombre de la plataforma o macropera" & Chr(13) & Chr(9) &
            '"Analyst As String : Nombre del analista" & Chr(13) & Chr(9) &
            '"DatGenDate As String : fecha de creacion del modelo" & Chr(13) & Chr(9) &
            '"Comenta As String : Comentarios" & Chr(13) & Chr(13) &
            '"Datos del yacimiento :" & Chr(13) & Chr(13) & Chr(9) &
            '"IPRMethod as Byte: 0 = PI Entry. 1 = Vogel. 2 = Composite." & Chr(13) & Chr(9) & Chr(9) & Chr(9) &
            '                   "3 = Darcy. 4: Fetkovich." & Chr(13) & Chr(9) &
            '"PRes As Double : Presión de Fondo Estática kg/cm2" & Chr(13) & Chr(9) &

            '"TRes As Double : Temperatura del Yacimiento oC" & Chr(13) & Chr(9) &
            '"Wc As Double : Porcentaje de agua en la capa % " & Chr(13) & Chr(9) &
            '"TotGor As Double : Relación Gas Aceite Total m3/m3" & Chr(13) & Chr(9) &
            '"Compact As Byte: Modelo de reduccion de permeabilidad" & Chr(13) & Chr(9) & Chr(9) & Chr(9) &
            '                 "por compactación. 0 = No. 1 = Yes" & Chr(13) & Chr(9) &
            '"IRelK As Byte: Considerar las permeabilidades relativas" & Chr(13) & Chr(9) & Chr(9) & Chr(9) &
            '                 "0 = No. 1 = Yes" & Chr(13) & Chr(13) &
            '"Datos del modelo de Vogel" & Chr(13) & Chr(13) & Chr(9) &
            '"QTest As Double : Gasto de líquido bl/día" & Chr(13) & Chr(9) &
            '"Ptest As Double : Presión de fondo fluyendo kg/cm2" & Chr(13) & Chr(13) &
            '"Datos del modelo de Darcy" & Chr(13) & Chr(13) & Chr(9) &
            '"MGSkinMethod As Byte: Daño Mecánico / Geometrico" & Chr(13) & Chr(9) & Chr(9) &
            '                "0 = Enter Skin by Hand.  1 = Loke" & Chr(13) & Chr(9) & Chr(9) &
            '                "2 = MacLeod.  3 = Karakas - Tariq" & Chr(13) & Chr(9) &
            '"DPSkinMethod As Byte: Daño por desviación y" & Chr(13) & Chr(9) & Chr(9) &
            '                "penetración parcial" & Chr(13) & Chr(9) & Chr(9) &
            '                "0 = Cinco / Martín-Bronz.  1 = Wong-Cliford" & Chr(13) & Chr(9) & Chr(9) &
            '                "2 = Cinco (2) / Martín-Bronz" & Chr(13) & Chr(9) &
            '"ResPerm As Double: Permeabilidad md " & Chr(13) & Chr(9) &
            '"Thickness As Double: Espesor del yacimiento m " & Chr(13) & Chr(9) &
            '"Drainage As Double: Area de drene km2 " & Chr(13) & Chr(9)

            'response = MsgBox(textAyuda, style, title)
            'If response = MsgBoxResult.Cancel Then
            '    Return
            'Else
            '    textAyuda = ""
            '    title = "Ayuda 3/3"
            'End If

            textAyuda &= GetFrmt(StrDup(125, "-"), 1)
            textAyuda &= GetFrmt("Datos del modelo de Darcy:                                                                ", 2)
            textAyuda &= GetFrmt("Dietz : Factor de forma .......................................................... [adim] ", 5)
            textAyuda &= GetFrmt("WBR   : Radio del pozo ............................................................. [pg] ", 5)
            textAyuda &= GetFrmt("Skin  : Factor de daño ........................................................... [adim] ", 6)
            textAyuda &= GetFrmt("Datos del Bombeo Neumatico Continuo:                                                      ", 2)
            textAyuda &= GetFrmt("Entry      : Emplear RGIL. 0 = No, emplear Qgi. 1 = Yes, emplear RGIL.                    ", 5)
            textAyuda &= GetFrmt("Method     : Metodo de  calculo del BN. 0: Profundidad de Inyección Fija.                 ", 5)
            textAyuda &= GetFrmt("1: Profundidad de Inyeccion Optima. 2: Especificar la profundidad de las valvulas.        ", 7)
            textAyuda &= GetFrmt("Gravity    : Densidad del gas de bombeo neumático ................................ [adim] ", 5)
            textAyuda &= GetFrmt("H2S        : % Mol de H2S                                                                 ", 5)
            textAyuda &= GetFrmt("CO2        : % Mol de CO2                                                                 ", 5)
            textAyuda &= GetFrmt("N2         : % Mol de N2                                                                  ", 5)
            textAyuda &= GetFrmt("Qgi        : Gasto de gas de inyección .......................................... [MMPCD] ", 5)
            textAyuda &= GetFrmt("GLRate     : Realación gas inyectado liquido .................................... [m3/m3] ", 5)
            textAyuda &= GetFrmt("ValveDepth : Profundidad de la válvula de inyeccion ................................. [m] ", 5)
            textAyuda &= GetFrmt("DiamValBNC : Diámetro de la válvula de BNC ................................. [64 avos pg] ", 5)
            textAyuda &= GetFrmt(StrDup(125, "-"), 1)
            textAyuda &= GetFrmt(" ", 5)
            textAyuda &= GetFrmt(" ", 5)
            textAyuda &= GetFrmt(" ", 5)

            'textAyuda = "Datos del modelo de Darcy" & Chr(13) & Chr(13) & Chr(9) &
            '"Dietz As Double: Factor de forma" & Chr(13) & Chr(9) &
            '"WBR As Double: Radio del pozo pg" & Chr(13) & Chr(9) &
            '"Skin As Double: Factor de daño" & Chr(13) & Chr(13) &
            '"Datos del Bombeo Neumatico Continuo" & Chr(13) & Chr(13) & Chr(9) &
            '"Entry As Byte: Emplear RGIL" & Chr(13) & Chr(9) & Chr(9) &
            '                 "0 = No, emplear Qgi. 1 = Yes, emplear RGIL" & Chr(13) & Chr(9) &
            '"Method As Byte: Metodo de  calculo del BN." & Chr(13) & Chr(9) & Chr(9) &
            '                 "0: Profundidad de Inyección Fija." & Chr(13) & Chr(9) & Chr(9) &
            '                 "1: Profundidad de Inyeccion Optima" & Chr(13) & Chr(9) & Chr(9) &
            '                 "2: Especificar la profundidad de las valvulas" & Chr(13) & Chr(9) &
            '"Gravity As Double: Densidad del gas de bombeo neumatico" & Chr(13) & Chr(9) &
            '"H2S As Double: % Mol de H2S" & Chr(13) & Chr(9) &
            '"CO2 As Double: % Mol de CO2" & Chr(13) & Chr(9) &
            '"N2 As Double: % Mol de N2" & Chr(13) & Chr(9) &
            '"Qgi As Double: Gasto de gas de inyección MMPCD" & Chr(13) & Chr(9) &
            '"GLRate As Double: Realación gas inyectado liquido m3/m3" & Chr(13) & Chr(9) &
            '"ValveDepth As Double:Profundidad de la valvula" & Chr(13) & Chr(9) & Chr(9) &
            '"de inyeccion m" & Chr(13) & Chr(9) &
            '"DiamValBNC As Double: Diametro de la válvula" & Chr(13) & Chr(9) & Chr(9) &
            '                      "de BNC 64 avos pg" & Chr(13) & Chr(9)

            'response = MsgBox(textAyuda, style, title)
            'If response = MsgBoxResult.Cancel Then Return

            Return textAyuda

        End Function

        Private Function getDiagnostics() As String '(ByVal ModeloProsper As Crea)

            Dim textDiagn As String = ""
            'Dim title As String = "Diagnóstico 1/2"
            'Dim style As MsgBoxStyle = MsgBoxStyle.DefaultButton1 Or MsgBoxStyle.Information Or MsgBoxStyle.OkCancel
            'Dim response As MsgBoxResult

            'MPrsp.NumDatEdoMec = NumDatEdoMec
            'MPrsp.NumDatTrayecto = NumDatTrayecto
            'MPrsp.NumDatTemp = NumDatTemp

            Try

                textDiagn &= GetFrmt("Clase: Modelo de Prosper.                                                                      ", 1)
                textDiagn &= GetFrmt("Verificación de datos de entrada.                                                              ", 2)
                'textDiagn &= GetFrmt(StrDup(125, "-"), 1)
                textDiagn &= GetFrmt("Datos de Entrada:                                                                              ", 2)
                textDiagn &= GetFrmt("ProgramPath = " & ProgramPath, 5)
                textDiagn &= GetFrmt("ArchivoPVT  = " & ArchivoPVT, 6)
                'textDiagn &= GetFrmt(StrDup(125, "-"), 1)
                textDiagn &= GetFrmt("Datos Generales:                                                                               ", 2)
                textDiagn &= GetFrmt("Fluid = " & CStr(Fluid.Val) & "   -> " & GetFluid(Fluid.Val), 5)
                textDiagn &= GetFrmt("PVTModel = " & CStr(PVTModel.Val) & "   -> " & GetPVTModel(PVTModel.Val), 5)
                textDiagn &= GetFrmt("Separator = " & CStr(Separator.Val) & "   -> " & GetSeparator(Separator.Val), 5)
                textDiagn &= GetFrmt("Emulsion = " & CStr(Emulsion.Val) & "   -> " & GetEmulsion(Emulsion.Val), 5)
                textDiagn &= GetFrmt("Hydrate = " & CStr(Hydrate.Val) & "   -> " & GetHydrate(Hydrate.Val), 5)
                textDiagn &= GetFrmt("WatVis = " & CStr(WatVis.Val) & "   -> " & GetWatVis(WatVis.Val), 5)
                textDiagn &= GetFrmt("VisMod = " & CStr(VisMod.Val) & "   -> " & GetVisMod(VisMod.Val), 5)
                textDiagn &= GetFrmt("FlowType = " & CStr(FlowType.Val) & "   -> " & GetFlowType(FlowType.Val), 5)
                textDiagn &= GetFrmt("WellType = " & CStr(WellType.Val) & "   -> " & GetWellType(WellType.Val), 5)
                textDiagn &= GetFrmt("LiftMethod = " & CStr(LiftMethod.Val) & "   -> " & GetLiftMethod(LiftMethod.Val), 5)
                textDiagn &= GetFrmt("LiftType = " & CStr(LiftType.Val) & "   -> " & GetLiftType(LiftType.Val), 5)
                textDiagn &= GetFrmt("Predict = " & CStr(Predict.Val) & "   -> " & GetPredict(Predict.Val), 5)
                textDiagn &= GetFrmt("TempModel = " & CStr(TempModel.Val) & "   -> " & GetTempModel(TempModel.Val), 5)
                textDiagn &= GetFrmt("RangeSystem = " & CStr(RangeSystem.Val) & "   -> " & GetRangeSystem(RangeSystem.Val), 5)
                textDiagn &= GetFrmt("OutputRes = " & CStr(OutputRes.Val) & "   -> " & GetOutputRes(OutputRes.Val), 5)
                textDiagn &= GetFrmt("Completion = " & CStr(Completion.Val) & "   -> " & GetCompletion(Completion.Val), 5)
                textDiagn &= GetFrmt("GravelPack = " & CStr(GravelPack.Val) & "   -> " & GetGravelPack(GravelPack.Val), 5)
                textDiagn &= GetFrmt("InflowType = " & CStr(InflowType.Val) & "   -> " & GetInflowType(InflowType.Val), 5)
                textDiagn &= GetFrmt("GasConing = " & CStr(GasConing.Val) & "   -> " & GetGasConing(GasConing.Val), 5)
                textDiagn &= GetFrmt("Company = " & Company, 5)
                textDiagn &= GetFrmt("Field = " & Field, 5)
                textDiagn &= GetFrmt("Locations = " & Locations, 5)
                textDiagn &= GetFrmt("Well = " & Well, 5)
                textDiagn &= GetFrmt("Platform = " & Platform, 5)
                textDiagn &= GetFrmt("Analyst = " & Analyst, 5)
                textDiagn &= GetFrmt("DatGenDate = " & DatGenDate, 5)
                textDiagn &= GetFrmt("Comenta = " & Comenta, 5)
                'textDiagn &= GetFrmt(StrDup(125, "-"), 1)

                'response = MsgBox(textDiagn, style, title)
                'If response = MsgBoxResult.Cancel Then
                '    Return
                'Else
                '    textDiagn = ""
                '    title = "Diagnóstico 2/2"
                'End If

                textDiagn &= GetFrmt("Datos del Yacimiento:                                                                     ", 2)
                textDiagn &= GetFrmt("IPRMethod  = " & CStr(IPRMethod.Val) & "   -> " & GetIPRMethod(IPRMethod.Val), 5)
                textDiagn &= GetFrmt("Presión de Fondo Estática =  " & CStr(PRes.Val) & " [kg/cm2]", 5)
                textDiagn &= GetFrmt("Temperatura del Yacimiento =  " & CStr(TRes.Val) & " [oC]", 5)
                textDiagn &= GetFrmt("Porcentaje de agua en la capa =  " & CStr(Wc.Val) & " [%]", 5)
                textDiagn &= GetFrmt("Relación Gas Aceite Total =  " & CStr(TotGor.Val) & " [m3/m3]", 5)
                textDiagn &= GetFrmt("Compact  = " & CStr(Compact.Val) & "   -> " & GetCompact(Compact.Val), 5)
                textDiagn &= GetFrmt("IRelK  = " & CStr(IRELK.Val) & "   -> " & GetIRelK(IRELK.Val), 6)
                'textDiagn &= GetFrmt(StrDup(125, "-"), 1)
                textDiagn &= GetFrmt("Datos del modelo de Vogel.                                                                ", 2)
                textDiagn &= GetFrmt("MGSkinMethod  = " & CStr(MGSkinMethod.Val) & "   -> " & GetMGSkinMethod(MGSkinMethod.Val), 5)
                textDiagn &= GetFrmt("DPSkinMethod  = " & CStr(DPSkinMethod.Val) & "   -> " & GetDPSkinMethod(DPSkinMethod.Val), 6)
                textDiagn &= GetFrmt("Datos del modelo de Darcy:                                                                ", 2)
                textDiagn &= GetFrmt("Permeabilidad del yacimiento =  " & CStr(ResPerm.Val) & " [md]", 5)
                textDiagn &= GetFrmt("Espesor del yacimiento =  " & CStr(Thickness.Val) & " [m]", 5)
                textDiagn &= GetFrmt("Area de drene =  " & CStr(Drainage.Val) & " [km2]", 5)
                textDiagn &= GetFrmt("Factor de forma =  " & CStr(Dietz.Val) & " [adim]", 5)
                textDiagn &= GetFrmt("Radio del pozo =  " & CStr(WBR.Val) & " [pg]", 5)
                textDiagn &= GetFrmt("Factor de daño total =  " & CStr(Skin.Val) & " [adim]", 6)
                textDiagn &= GetFrmt("Datos del Bombeo Neumatico Continuo:                                                      ", 2)
                textDiagn &= GetFrmt("Entry  = " & CStr(Entry.Val) & "   -> " & GetEntry(Entry.Val), 5)
                textDiagn &= GetFrmt("Method  = " & CStr(Method.Val) & "   -> " & GetMethod(Method.Val), 5)
                textDiagn &= GetFrmt("Densidad del gas de bombeo neumático =  " & CStr(Gravity.Val) & " [adim]", 5)
                textDiagn &= GetFrmt("% Mol de H2S =  " & CStr(H2S.Val) & " [%]", 5)
                textDiagn &= GetFrmt("% Mol de CO2 =  " & CStr(CO2.Val) & " [%]", 5)
                textDiagn &= GetFrmt("% Mol de N2 =  " & CStr(N2.Val) & " [%]", 5)
                textDiagn &= GetFrmt("Gasto de gas de inyección =  " & CStr(GLRiny.Val) & " [MMPCD]", 5)
                textDiagn &= GetFrmt("Realación gas inyectado liquido =  " & CStr(GLRate.Val) & " [m3/m3]", 5)
                textDiagn &= GetFrmt("Profundidad de la válvula de inyeccion =  " & CStr(ValveDepth.Val) & " [m]", 5)
                textDiagn &= GetFrmt("Diámetro de la válvula de BNC =  " & CStr(DiamValBNC.Val) & " [64 avos pg]", 5)

                'textDiagn &= GetFrmt(" =  " & CStr() & " []", 5)

                'response = MsgBox(textDiagn, style, title)
                'If response = MsgBoxResult.Cancel Then
                '    Return
                'Else
                '    textDiagn = ""
                'End If

            Catch ex As Exception
                Throw New Exception(ex.ToString)
            End Try
            Return textDiagn
        End Function

        Private Function GetFluid(ByVal Fluid As Byte) As String
            Dim msg As String = ""
            Select Case Fluid
                Case 0 : msg = "Oil and Water"
                Case 1 : msg = "Dry and Wet Gas"
                Case 2 : msg = "Retrograde Condensate"
                Case Else : msg = "Tipo de fluido no definido, valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetPVTModel(ByVal PVTModel As Byte) As String
            Dim msg As String = ""
            Select Case PVTModel
                Case 0 : msg = "Black Oil"
                Case 1 : msg = "Equation of State"
                Case Else : msg = "Modelo PVT no definido, valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetSeparator(ByVal Separator As Byte) As String
            Dim msg As String = ""
            Select Case Separator
                Case 0 : msg = "Single Stage Separator"
                Case 1 : msg = "Two Stage Separator"
                Case Else : msg = "Condiciones de separación no definidas: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetEmulsion(ByVal Emulsion As Byte) As String
            Dim msg As String = ""
            Select Case Emulsion
                Case 0 : msg = "No"
                Case 1 : msg = "Emulsion + Pump Viscosity Correction"
                Case Else : msg = "Considerar Emulsiones no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetHydrate(ByVal Hydrate As Byte) As String
            Dim msg As String = ""
            Select Case Hydrate
                Case 0 : msg = "Disable Warning"
                Case 1 : msg = "Enable Warning"
                Case Else : msg = "Avisos de Emulsiones no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetWatVis(ByVal WatVis As Byte) As String
            Dim msg As String = ""
            Select Case WatVis
                Case 0 : msg = "Use Default Correlation"
                Case 1 : msg = "Use Pressure Corrected Correlation"
                Case Else : msg = "Correlación de viscosidad del agua no definida: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetVisMod(ByVal VisMod As Byte) As String
            Dim msg As String = ""
            Select Case VisMod
                Case 0 : msg = "Newtonian Fluid"
                Case 1 : msg = "Non Newtonian Fluid"
                Case Else : msg = "Correlación de viscosidad del aceite no definida: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetFlowType(ByVal FlowType As Byte) As String
            Dim msg As String = ""
            Select Case FlowType
                Case 0 : msg = "Tubing Flow"
                Case 1 : msg = "Annular Flow"
                Case Else : msg = "Tipo de flujo en el pozo no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetWellType(ByVal WellType As Byte) As String
            Dim msg As String = ""
            Select Case WellType
                Case 0 : msg = "Producer"
                Case 1 : msg = "Inyector"
                Case 2 : msg = "Water Inyector"
                Case Else : msg = "Tipo de pozo no definido: Valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetLiftMethod(ByVal LiftMethod As Byte) As String
            Dim msg As String = ""
            Select Case LiftMethod
                Case 0 : msg = "None"
                Case 1 : msg = "Gas Lift (Continous)"
                Case 2 : msg = "Electrical Sumersible Pump"
                Case 3 : msg = "Hydraulic Drive Downhole Pump"
                Case 4 : msg = "Progressive Cavity Pump"
                Case 5 : msg = "Coiled Tubing Gas Lift"
                Case 6 : msg = "Diluent Injection"
                Case 7 : msg = "Jet Pump"
                Case 8 : msg = "MultiPhase Pump"
                Case 9 : msg = "Sucker Rod Pump"
                Case 10 : msg = "Gas Lift (Intermttent)"
                Case Else : msg = "Tipo sistema artificial no definido: Valores validos: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10."
            End Select
            Return msg
        End Function

        Private Function GetLiftType(ByVal LiftType As Byte) As String
            Dim msg As String = ""
            Select Case LiftType
                Case 0 : msg = "No Friction Loss In Annulus"
                Case 1 : msg = "Friction Loss In Annulus"
                Case 2 : msg = "Safety Equipment"
                Case Else : msg = "Tipo de calculo de perdidas de presion en el espaxcio anulal no definido: Valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetPredict(ByVal Predict As Byte) As String
            Dim msg As String = ""
            Select Case Predict
                Case 0 : msg = "Pressure Only"
                Case 1 : msg = "Pressure and Temperature (offshore)"
                Case 2 : msg = "Pressure and Temperature (on Land)"
                Case Else : msg = "Tipo de cálculo de presión y/o temperatura no definido: Valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetTempModel(ByVal TempModel As Byte) As String
            Dim msg As String = ""
            Select Case TempModel
                Case 0 : msg = "Rough Approximation"
                Case 1 : msg = "Enthalpy Balance"
                Case 2 : msg = "Improved Approximation"
                Case Else : msg = "Correlacion de temperatura no definida: Valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetRangeSystem(ByVal RangeSystem As Byte) As String
            Dim msg As String = ""
            Select Case RangeSystem
                Case 0 : msg = "Full System"
                Case 1 : msg = "Pipeline Only"
                Case Else : msg = "Rango de los calculos no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetOutputRes(ByVal OutputRes As Byte) As String
            Dim msg As String = ""
            Select Case OutputRes
                Case 0 : msg = "Show Calculating Data"
                Case 1 : msg = "Hide Calculating Data"
                Case Else : msg = "Opcion de desplegado de resultados no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetCompletion(ByVal Completion As Byte) As String
            Dim msg As String = ""
            Select Case Completion
                Case 0 : msg = "Cased Hole"
                Case 1 : msg = "Open hole"
                Case Else : msg = "Tipo de terminación no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetGravelPack(ByVal GravelPack As Byte) As String
            Dim msg As String = ""
            Select Case GravelPack
                Case 0 : msg = "None"
                Case 1 : msg = "Gravel Pack"
                Case 2 : msg = "Pre Packed Screen"
                Case 3 : msg = "Wire Wrapped Screen"
                Case 4 : msg = "Slotted Liner"
                Case Else : msg = "Tipo de terminación no definido: Valores validos: 0, 1, 2, 3, 4."
            End Select
            Return msg
        End Function

        Private Function GetInflowType(ByVal InflowType As Byte) As String
            Dim msg As String = ""
            Select Case InflowType
                Case 0 : msg = "Single Branch"
                Case 1 : msg = "MultiLateral Well"
                Case Else : msg = "Tipo de capa del yacimiento no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetGasConing(ByVal GasConing As Byte) As String
            Dim msg As String = ""
            Select Case GasConing
                Case 0 : msg = "No"
                Case 1 : msg = "Yes"
                Case Else : msg = "Considerar conificación de gas no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetIPRMethod(ByVal IPRMethod As Byte) As String
            Dim msg As String = ""
            Select Case IPRMethod
                Case 0 : msg = "PI Entry"
                Case 1 : msg = "Vogel"
                Case 2 : msg = "Composite"
                Case 3 : msg = "Darcy"
                Case 4 : msg = "Fetkovich"
                Case Else : msg = "Correlación de flujo en el yacimiento no definida: Valores validos: 0, 1, 2, 3, 4."
            End Select
            Return msg
        End Function

        Private Function GetCompact(ByVal Compact As Byte) As String
            Dim msg As String = ""
            Select Case Compact
                Case 0 : msg = "No"
                Case 1 : msg = "Yes"
                Case Else : msg = "Modelo de reduccion de permeabilidad por compactación no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetIRelK(ByVal IRelK As Byte) As String
            Dim msg As String = ""
            Select Case IRelK
                Case 0 : msg = "No"
                Case 1 : msg = "Yes"
                Case Else : msg = "Considerar las permeabilidades relativas no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetMGSkinMethod(ByVal MGSkinMethod As Byte) As String
            Dim msg As String = ""
            Select Case MGSkinMethod
                Case 0 : msg = "Enter Skin by Hand"
                Case 1 : msg = "Loke"
                Case 2 : msg = "MacLeod"
                Case 3 : msg = "Karakas - Tariq"
                Case Else : msg = "Correlación de Daño Mecánico/Geometrico no definido: Valores validos: 0, 1, 2, 3."
            End Select
            Return msg
        End Function

        Private Function GetDPSkinMethod(ByVal DPSkinMethod As Byte) As String
            Dim msg As String = ""
            Select Case DPSkinMethod
                Case 0 : msg = "Cinco / Martín-Bronz"
                Case 1 : msg = "Wong-Cliford"
                Case 2 : msg = "Cinco (2) / Martín-Bronz"
                Case Else : msg = "Correlación de Daño por desviación y penetración parcial no definido: Valores validos: 0, 1, 2."
            End Select
            Return msg
        End Function

        Private Function GetEntry(ByVal Entry As Byte) As String
            Dim msg As String = ""
            Select Case Entry
                Case 0 : msg = "No, emplear Qgi"
                Case 1 : msg = "Yes, emplear RGIL"
                Case Else : msg = "Emplear RGIL no definido: Valores validos: 0, 1."
            End Select
            Return msg
        End Function

        Private Function GetMethod(ByVal Method As Byte) As String
            Dim msg As String = ""
            Select Case Method
                Case 0 : msg = "Profundidad de Inyección Fija"
                Case 1 : msg = "Profundidad de Inyeccion Optima"
                Case 2 : msg = "Especificar la profundidad de las valvulas"
                Case Else : msg = "Método de cálculo de BN no definido: Valores validos: 0, 1, 2."
            End Select
            Return msg
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

    ''' <summary>
    ''' Estructura para los datos de tipo Doble
    ''' </summary>
    ''' <remarks></remarks>
    Class MaxMinDouble
        Public Nomb As String
        Public Val As Double
        Public Max As Double
        Public Min As Double
    End Class

    ''' <summary>
    ''' Estructura para los datos de tipo Byte
    ''' </summary>
    ''' <remarks></remarks>
    Class MaxMinByte
        Public Nomb As String
        Public Val As Byte
        Public Max As Byte
        Public Min As Byte
    End Class

    ''' <summary>
    ''' Estructura de los datos tipo Entero
    ''' </summary>
    ''' <remarks></remarks>
    Class MaxMinInteger
        Public Nomb As String
        Public Val As Integer
        Public Max As Integer
        Public Min As Integer
    End Class

    ''' <summary>
    ''' Estructura para los datos de tipo arreglo
    ''' </summary>
    ''' <remarks></remarks>
    Class MaxMinArreglo
        Public Nomb As String
        Public Val As Object
        Public Max As Double
        Public Min As Double
    End Class

    Public Sub Parametros_de_ajuste()
        '
        Dim ParametrosModelo As Hashtable = New Hashtable
        Dim Correlacion As String
        Dim ind_Cor As Integer = 0
        Dim Corr As Integer

        Dim Pozo As String
        Pozo = DoGet("PROSPER.SIN.SUM.Well")

        'Datos PVT
        Dim GOR As Double = DoGet("PROSPER.PVT.Input.Solgor")
        Dim API As Double = DoGet("PROSPER.PVT.Input.Api")
        Dim DenRelGas As Double = DoGet("PROSPER.PVT.Input.Grvgas")
        Dim Salinidad As Double = DoGet("PROSPER.PVT.Input.Watsal")
        Dim H2S As Double = DoGet("PROSPER.PVT.Input.H2s")
        Dim C02 As Double = DoGet("PROSPER.PVT.Input.Co2")
        Dim N2 As Double = DoGet("PROSPER.PVT.Input.N2")
        Dim CorrPb As Double = DoGet("PROSPER.PVT.Input.PBcorr")
        Dim CorrMuo As Double = DoGet("PROSPER.PVT.Input.UOcorr")
        Dim Temp As Double = DoGet("PROSPER.PVT.Match.Data[0][0][0]")
        Dim Pb As Double = DoGet("PROSPER.PVT.Match.Data[0][0][2]")
        Dim Presion() As Double
        Dim Rs() As Double
        Dim Bo() As Double
        Dim VisAceite() As Double
        Dim kk As Integer
        ParametrosModelo.Add("GOR", GOR)
        ParametrosModelo.Add("API", API)
        ParametrosModelo.Add("DenRelGas", DenRelGas)
        ParametrosModelo.Add("Salinidad", Salinidad)
        ParametrosModelo.Add("H2S", H2S)
        ParametrosModelo.Add("C02", C02)
        ParametrosModelo.Add("N2", N2)
        ParametrosModelo.Add("Corr_Pb", CorrPb)
        ParametrosModelo.Add("Corr_ViscOil", CorrMuo)
        ParametrosModelo.Add("Temp_PVT", Temp)
        ParametrosModelo.Add("Pb_PVT", Pb)

        Do
            Dim pres As Double = DoGet("PROSPER.PVT.Match.Data[0][kk][1]")
            If (pres = 0) Then Exit Do
            ReDim Preserve Presion(kk)
            ReDim Preserve Rs(kk)
            ReDim Preserve Bo(kk)
            ReDim Preserve VisAceite(kk)
            Presion(kk) = DoGet("PROSPER.PVT.Match.Data[0][" & CStr(kk) & "][1]")
            Rs(kk) = DoGet("PROSPER.PVT.Match.Data[0][" & CStr(kk) & "][11]")
            Bo(kk) = DoGet("PROSPER.PVT.Match.Data[0][" & CStr(kk) & "][4]")
            VisAceite(kk) = DoGet("PROSPER.PVT.Match.Data[0][" & CStr(kk) & "][5]")
            kk += 1
        Loop Until (kk > 14)

        'IPR
        Dim IPR As Integer = DoGet("PROSPER.SIN.IPR.Single.IprMethod")
        Dim Py As Double = DoGet("PROSPER.SIN.IPR.Single.Pres")
        Dim Ty As Double = DoGet("PROSPER.SIN.IPR.Single.Tres")
        Dim Wc As Double = DoGet("PROSPER.SIN.IPR.Single.Tres")
        Dim Qliq_Test As Double = DoGet("PROSPER.SIN.IPR.Single.Qtest")
        Dim Pwf_Test As Double = DoGet("PROSPER.SIN.IPR.Single.Ptest")

        ParametrosModelo.Add("Metodo_IPR", IPR)
        ParametrosModelo.Add("Presión_yac.", Py)
        ParametrosModelo.Add("Temp_yac.", Ty)
        ParametrosModelo.Add("Coste_Agua", Wc)
        ParametrosModelo.Add("Qliq_Test", Qliq_Test)
        ParametrosModelo.Add("Pwf_Test", Pwf_Test)

        'Gaslift Input Data
        Dim Sg_Gaslift As Double = DoGet("PROSPER.SIN.GLF.Gravity")
        Dim H2s_Gaslift As Double = DoGet("PROSPER.SIN.GLF.H2S")
        Dim C02_Gaslift As Double = DoGet("PROSPER.SIN.GLF.CO2")
        Dim N2_Gaslift As Double = DoGet("PROSPER.SIN.GLF.N2")
        Dim GLR_Gaslift As Double = DoGet("PROSPER.SIN.GLF.GLRinj")
        Dim Qgi_Gaslift As Double = DoGet("PROSPER.SIN.GLF.GLRate")
        Dim GLR_Rate_Gaslift As Double = DoGet("PROSPER.SIN.GLF.Entry")
        Dim Metodo_Gaslift As Double = DoGet("PROSPER.SIN.GLF.Method")
        Dim Prof_Valvula As Double = DoGet("PROSPER.SIN.GLF.ValveDepth")

        ParametrosModelo.Add("Sg_Gaslift", Sg_Gaslift)
        ParametrosModelo.Add("H2s_Gaslift", H2s_Gaslift)
        ParametrosModelo.Add("C02_Gaslift", C02_Gaslift)
        ParametrosModelo.Add("N2_Gaslift", N2_Gaslift)
        ParametrosModelo.Add("GLR_Gaslift", GLR_Gaslift)
        ParametrosModelo.Add("Qgi_Gaslift", Qgi_Gaslift)
        ParametrosModelo.Add("GLR_Rate_Gaslift", GLR_Rate_Gaslift)
        ParametrosModelo.Add("Metodo_Gaslift", Metodo_Gaslift)
        ParametrosModelo.Add("Prof_Valvula", Prof_Valvula)


        Dim HTC As Double = DoGet("PROSPER.SIN.EQP.Geo.Htc")
        ParametrosModelo.Add("Coef. Transf. de Calor", HTC)

        Do
            Corr = DoGet("PROSPER.ANL.VMT.Corr[" & CStr(ind_Cor) & "]")
            If (Corr = 1) Then Exit Do
            ind_Cor += 1
        Loop While (ind_Cor <= 16)

        Select Case ind_Cor
            Case 0
                Correlacion = "DunsandRosModified"
            Case 1
                Correlacion = "HagedornBrown"
            Case 2
                Correlacion = "FancherBrown"
            Case 3
                Correlacion = "MukerjeeBrill"
            Case 4
                Correlacion = "BeggsandBrill"
            Case 5
                Correlacion = "PetroleumExperts"
            Case 6
                Correlacion = "Orkiszewski"
            Case 7
                Correlacion = "PetroleumExperts2"
            Case 8
                Correlacion = "DunsandRosOriginal"
            Case 9
                Correlacion = "PetroleumExperts3"
            Case 10
                Correlacion = "GREmodifiedbyPE"
            Case 11
                Correlacion = "PetroleumExperts4"
            Case 12
                Correlacion = "Hydro3P"
            Case 13
                Correlacion = "PetroleumExperts5"
            Case 14
                Correlacion = "PE6HeavyOil"
            Case 15
                Correlacion = "OLGAS3P"
            Case 16
                Correlacion = "OLGAS3PEXT"
            Case Else
                Correlacion = "Ninguno"
        End Select

        ParametrosModelo.Add("Correlación", Correlacion)
        'Quick_Look
        Dim Pwh As Double = DoGet("PROSPER.ANL.QLG.Surface[0][0]")
        Dim Twh As Double = DoGet("PROSPER.ANL.QLG.Surface[1][0]")
        Dim Qliq As Double = DoGet("PROSPER.ANL.QLG.Surface[2][0]")
        Dim TotQgas As Double = DoGet("PROSPER.ANL.QLG.Surface[4][0]")
        Dim Qginy As Double = DoGet("PROSPER.ANL.QLG.Surface[5][0]")
        Dim Ptr As Double = DoGet("PROSPER.ANL.QLG.Surface[6][0]")
        Dim Orif As Double = DoGet("PROSPER.ANL.QLG.Gaslift[0]")
        Dim ProfIny As Double = DoGet("PROSPER.ANL.QLG.Gaslift[1]")

        ParametrosModelo.Add("Pwh", Pwh)
        ParametrosModelo.Add("Twh", Twh)
        ParametrosModelo.Add("Qliq", Qliq)
        ParametrosModelo.Add("TotQgas", TotQgas)
        ParametrosModelo.Add("Qginy", Qginy)
        ParametrosModelo.Add("Ptr", Ptr)
        ParametrosModelo.Add("Diámetro_Orificio", Orif)
        ParametrosModelo.Add("Prof._Inyeccion", ProfIny)

    End Sub

    Public Sub PVT(ByVal GOR As Double, ByVal API As Double, ByVal Drg As Double, ByVal Salinidad As Double, ByVal H2S As Double, ByVal CO2 As Double, ByVal N2 As Double, ByVal IcorPRB As Integer, ByVal IcorVis As Integer, ByVal Tpvt As Double, ByVal Psat As Double, ByVal Pprueba() As Double, ByVal Rs() As Double, ByVal Bo() As Double)

        DoSet("PROSPER.PVT.Input.Solgor", GOR)
        DoSet("PROSPER.PVT.Input.Api", API)
        DoSet("PROSPER.PVT.Input.Grvgas", Drg)
        DoSet("PROSPER.PVT.Input.Watsal", Salinidad)
        DoSet("PROSPER.PVT.Input.H2s", H2S)
        DoSet("PROSPER.PVT.Input.Co2", CO2)
        DoSet("PROSPER.PVT.Input.N2", N2)
        DoSet("PROSPER.PVT.Input.PBcorr", IcorPRB)
        DoSet("PROSPER.PVT.Input.UOcorr", IcorVis)

        DoSet("PROSPER.PVT.Match.Data[0][0][0]", Tpvt)
        DoSet("PROSPER.PVT.Match.Data[0][0][2]", Psat)
        For i = 0 To UBound(Pprueba)
            DoSet("PROSPER.PVT.Match.Data[0][" & CStr(i) & "][1]", Pprueba(i))
            DoSet("PROSPER.PVT.Match.Data[0][" & CStr(i) & "][11]", Rs(i))
            DoSet("PROSPER.PVT.Match.Data[0][" & CStr(i) & "][1]", Bo(i))
        Next i
        DoCmd("PROSPER.PVT.MATCHALL")
    End Sub
End Class




