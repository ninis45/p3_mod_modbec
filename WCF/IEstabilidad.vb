Imports System.ServiceModel

' NOTA: puede usar el comando "Cambiar nombre" del menú contextual para cambiar el nombre de interfaz "IEstabilidad" en el código y en el archivo de configuración a la vez.
<ServiceContract()>
Public Interface IEstabilidad

    <OperationContract()>
    Sub DoWork()
    <OperationContract()>
    Function Recomendacion() As String
    ' <OperationContract()>
    'Function Pozo(ByVal IdConfiguracion As String) As List(Of ModeloProsper.Clases.EstabilidadDePozos.Parametros_Estabilidad)
    <OperationContract()>
    Function Pozo(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of ModeloProsper.Clases.EstabilidadDePozos.Parametros_Estabilidad)
    <OperationContract()>
    Function Mapa(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of ModeloProsper.Clases.MapaEstabilidad.Parametros_Estabilidad)
    <OperationContract()>
    Function Execute(ByVal IdModPozo As String) As Boolean
End Interface
