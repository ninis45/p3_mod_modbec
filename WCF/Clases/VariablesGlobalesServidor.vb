Module VariablesGlobalesServidor
    Public DireccionProper As String = "C:\Program Files (x86)\Petroleum Experts\IPM 7.5\prosper.EXE"
    Public DireccionPVT As String = "C:\Rabasa.Out"
    Public StringConexion As String = "SERVER=10.85.35.113\SQLDESA01;User=CamposInteligentes;Password=ciGaitep; Database=CI;"
    Public objConsulta As New BaseDatosServidor(StringConexion)
End Module
