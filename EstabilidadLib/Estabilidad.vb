Public Class Estabilidad
    Public Property Recomendacion As String

    Public Function Pozo(ByVal IdModPozo As String)

    End Function
    Public Function Pozo(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of EstabilidadLib.Clases.EstabilidadDePozos.Parametros_Estabilidad)




        Dim objpozo As EstabilidadLib.Clases.EstabilidadDePozos = New EstabilidadLib.Clases.EstabilidadDePozos(Ptr_min, Ptr_max, Ptr_num, Pwh_Min, Pwh_Max, Pwh_num, Qgi_min, Qgi_max, Qgi_num, Dvalve_min, Dvalve_max, Dvalve_num, DiaVal_min, DiaVal_max, DiaVal_num, Wc_min, Wc_max, Wc_num, Cd, Ptr_Act, Pwh_Act, Qgi_Act, Dvalve_Act, DiaValve_Act, Wc_Act, Caso)

        Try

            objpozo.Principal()

            Recomendacion = objpozo.recomendaciones
            Return objpozo.ListaParametrosEstabilidad
        Catch ex As Exception


            Throw New Exception(ex.Message)
        End Try



    End Function


    Public Function Mapa(ByVal Ptr_min As Double, ByVal Ptr_max As Double, ByVal Ptr_num As Integer, ByVal Pwh_Min As Double, ByVal Pwh_Max As Double, ByVal Pwh_num As Integer, ByVal Qgi_min As Double, ByVal Qgi_max As Double, ByVal Qgi_num As Integer, ByVal Dvalve_min As Double, ByVal Dvalve_max As Double, ByVal Dvalve_num As Integer, ByVal DiaVal_min As Double, ByVal DiaVal_max As Double, ByVal DiaVal_num As Integer, ByVal Wc_min As Double, ByVal Wc_max As Double, ByVal Wc_num As Integer, ByVal Cd As Double, ByVal Ptr_Act As Double, ByVal Pwh_Act As Double, ByVal Qgi_Act As Double, ByVal Dvalve_Act As Double, DiaValve_Act As Double, Wc_Act As Double, ByVal Caso As Integer, ByVal Metodologia As Integer) As List(Of EstabilidadLib.Clases.MapaEstabilidad.Parametros_Estabilidad)
        Dim objmapa As EstabilidadLib.Clases.MapaEstabilidad = New EstabilidadLib.Clases.MapaEstabilidad(Ptr_min, Ptr_max, Ptr_num, Pwh_Min, Pwh_Max, Pwh_num, Qgi_min, Qgi_max, Qgi_num, Dvalve_min, Dvalve_max, Dvalve_num, DiaVal_min, DiaVal_max, DiaVal_num, Wc_min, Wc_max, Wc_num, Cd, Ptr_Act, Pwh_Act, Qgi_Act, Dvalve_Act, DiaValve_Act, Wc_Act, Caso, "")
        Try
            objmapa.GENERAL()

            Recomendacion = objmapa.recomendaciones
            Return objmapa.ListaParametrosEstabilidad
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try

    End Function



End Class
