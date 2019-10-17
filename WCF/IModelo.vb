Imports System.ServiceModel
Imports ModeloCI

' NOTA: puede usar el comando "Cambiar nombre" del menú contextual para cambiar el nombre de interfaz "IModelo" en el código y en el archivo de configuración a la vez.
<ServiceContract()>
Public Interface IModelo



    <OperationContract()>
    Function Monitor(ByRef OpenServer As String) As List(Of String)

    <OperationContract()>
    Sub Program()

    <OperationContract()>
    Sub Reset(ByVal IdModPozo As String, ByVal MaxIntentos As Integer)

    <OperationContract()>
    Sub Delete(ByVal IdModPozo As String, ByVal IdUsuario As String)

    <OperationContract()>
    Function Execute(ByVal IdModPozo As String, ByVal User As String) As Boolean

    '<OperationContract()>
    'Function SensGas(ByVal IdModPozo As String, ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double) As List(Of Gas)

    <OperationContract()>
    Function Sensibilidad_BN(ByVal IdModPozo As String) As Boolean
    <OperationContract>
    Function Sensibilidad_BNF(ByVal Qgi As Double, ByVal Pwh As Double, ByVal WC As Double, ByVal QGI_Min As Double, ByVal QGI_Max As Double, ByVal FileD As Byte(), ByVal FileName As String) As List(Of Gas)

    <OperationContract()>
    Sub ShutDown()
End Interface
